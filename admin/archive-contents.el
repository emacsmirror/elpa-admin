;;; archive-contents.el --- Auto-generate an Emacs Lisp package archive.  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2020  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'lisp-mnt)
(require 'package)
(require 'pcase)


(defconst archive--release-subdir "archive/"
  "Subdirectory where the ELPA release files (tarballs, ...) will be placed.")
(defconst archive--devel-subdir "archive-devel/"
  "Subdirectory where the ELPA bleeding edge files (tarballs, ...) will be placed.")
(defconst archive--name "NonGNU")
(defconst archive--gitrepo "emacs/nongnu.git")
(defconst archive--url "http://elpa.gnu.org/nongnu/")



(defvar archive--debug nil)
(defun archive--message (&rest args)
  (when archive--debug (apply #'message args)))

(defconst archive-re-no-dot "\\`\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regular expression matching all files except \".\" and \"..\".")

(defun archive--version-to-list (vers)
  (when vers
    (let ((l (version-to-list vers)))
      ;; Signal an error for things like "1.02" which is parsed as "1.2".
      (cl-assert (equal vers (package-version-join l)) nil
                 "Unsupported version syntax %S" vers)
      l)))

(defun archive--convert-require (elt)
  (let ((vers (archive--version-to-list (car (cdr elt)))))
    (if vers
        (list (car elt) vers)
      (list (car elt)))))

(defun archive--dirname (dir &optional base)
  (file-name-as-directory (expand-file-name dir base)))

(defun archive--delete-elc-files (dir &optional only-orphans)
  "Recursively delete all .elc files in DIR.
Delete backup files also."
  (dolist (f (directory-files dir t archive-re-no-dot))
    (cond ((file-directory-p f)
	   (archive--delete-elc-files f))
	  ((or (and (string-match "\\.elc\\'" f)
                    (not (and only-orphans
                              (file-readable-p (replace-match ".el" t t f)))))
	       (backup-file-name-p f))
	   (delete-file f)))))

(defun batch-make-archive ()
  "Process package content directories and generate the archive-contents file."
  (let ((packages '(1))) ; format-version.
    (dolist (dir (directory-files default-directory nil archive-re-no-dot))
      (condition-case v
	  (if (not (file-directory-p dir))
	      (message "Skipping non-package file %s" dir)
	    (let* ((pkg (file-name-nondirectory dir))
		   (autoloads-file (expand-file-name (concat pkg "-autoloads.el") dir)))
	      ;; Omit autoloads and .elc files from the package.
              (when (file-exists-p autoloads-file)
                (delete-file autoloads-file))
	      (archive--delete-elc-files dir)
	      (let ((metadata (or (with-demoted-errors
                                    ;;(format "batch-make-archive %s: %%s" dir)
                                    (archive--metadata dir pkg))
                                  '(nil "0"))))
                ;; (nth 1 metadata) is nil for "org" which is the only package
                ;; still using the "org-pkg.el file to specify the metadata.
                (if (and (nth 1 metadata)
                         (or (equal (nth 1 metadata) "0")
                             ;; Old deprecated convention.
                             (< (string-to-number (nth 1 metadata)) 0)))
                    (progn ;; Negative version: don't publish this package yet!
                      (message "Package %s not released yet!" dir)
                      (delete-directory dir 'recursive))
                  (push (if (car metadata)
                            (apply #'archive--process-simple-package
                                   dir pkg (cdr metadata))
                          (when (nth 1 metadata)
                            (archive--write-pkg-file dir pkg metadata))
                          (archive--process-multi-file-package dir pkg))
                        packages)))))
	((debug error) (error "Error in %s: %S" dir v))))
    (with-temp-buffer
      (pp (nreverse packages) (current-buffer))
      (write-region nil nil "archive-contents"))))

(defun archive--update-archive-contents (pkg-desc dir)
  "Update the `archive-contents' file in DIR with new package PKG-DESC."
  (let* ((filename (expand-file-name "archive-contents" dir))
         (ac (if (file-exists-p filename)
                 (archive--form-from-file-contents filename)
               '(1))))
    (archive--message "current AC: %S" ac)
    (setf (alist-get (car pkg-desc) (cdr ac)) (cdr pkg-desc))
    (setf (cdr ac) (sort (cdr ac)
                         (lambda (x y)
                           (string-lessp (symbol-name (car x)) (symbol-name (car y))))))
    (archive--message "new AC: %S" ac)
    (with-temp-buffer
      (pp ac (current-buffer))
      (write-region nil nil filename)
      (let ((default-directory (expand-file-name dir)))
        (archive--html-make-index (cdr ac))))))

(defun archive--get-release-revision (dir pkgname &optional vers version-map)
  "Get the REVISION that corresponds to current release.
This is either found from VERS in VERSION-MAP or by looking at the last
commit which modified the \"Version:\" pseudo header."
  (while (and version-map
              (not (member vers (car version-map))))
    (pop version-map))
  (or (nth 2 (car version-map))
      (let* ((default-directory (archive--dirname dir))
             (release-rev
              (with-temp-buffer
                (if (equal 0         ;Don't signal an error if call errors out.
                     (archive--call
                      (current-buffer)
                      "git" "log" "-n1" "--oneline" "--no-patch"
                      "--pretty=format:%H"
                      "-L" (concat "/^;;* *\\(Package-\\)\\?Version:/,+1:"
                                   pkgname ".el")))
                    (buffer-string)
                  (cons 'error (buffer-string))))))
        (if (stringp release-rev)
            (progn
              (archive--message "Found release rev: %S" release-rev)
              release-rev)
          (archive--message "Can't find release rev: %s" (cdr release-rev))
          nil))))

(defun archive--select-revision (dir pkgname rev)
  "Checkout revision REV in DIR of PKGNAME."
  (let ((cur-rev (vc-working-revision
                  (expand-file-name (concat pkgname ".el") dir))))
    (if (equal rev cur-rev)
        (archive--message "Current revision is already desired revision!")
      (with-temp-buffer
        (let ((default-directory (archive--dirname dir)))
          (archive--call (current-buffer) "git" "status" "--porcelain")
          (if (not (zerop (buffer-size)))
              (error "git-status not clean:\n%s" (buffer-string))
            (archive--call (current-buffer) "git" "reset" "--merge" rev)
            (archive--message "Reverted to release revision %s\n%s"
                              rev (buffer-string))))))))

(defun archive--make-one-tarball (tarball dir pkgname metadata
                                          &optional revision-function)
  "Create file TARBALL for PKGNAME if not done yet."
  (archive--message "Building tarball %s..." tarball)
  (if (file-readable-p tarball)
      (archive--message "Tarball %s already built!" tarball)
    (let* ((destdir (file-name-directory tarball))
           (_ (unless (file-directory-p destdir) (make-directory destdir)))
           (vers (nth 1 metadata))
           (elpaignore (expand-file-name ".elpaignore" dir))
           (re (concat "\\`" (regexp-quote pkgname)
                       "-\\(.*\\)\\.tar\\(\\.[a-z]*z\\)?"))
           (oldtarballs
            (mapcar
             (lambda (file)
               (string-match re file)
               (cons (match-string 1 file) file))
             (directory-files destdir nil re))))
      (delete-file (expand-file-name (format "%s-pkg.el" pkgname) dir))
      (when revision-function
        (archive--select-revision dir pkgname (funcall revision-function)))
      ;; FIXME: Build Info files and corresponding `dir' file.
      (archive--write-pkg-file dir pkgname metadata)
      ;; FIXME: Allow renaming files or selecting a subset of the files!
      (archive--call nil "tar"
                     "--exclude-vcs"
                     "-X" (if (file-readable-p elpaignore)
                              elpaignore "/dev/null")
                     "--transform"
                     (format "s|^packages/%s|%s-%s|" pkgname pkgname vers)
                     "-cf" tarball
                     (concat "packages/" pkgname))
      (let* ((pkgdesc
              ;; FIXME: `archive--write-pkg-file' wrote the metadata to
              ;; <pkg>-pkg.el and then `archive--process-multi-file-package'
              ;; reads it back.  We could/should skip the middle man.
              (archive--process-multi-file-package
               dir pkgname 'dont-rename)))
        (archive--message "%s: %S" pkgname pkgdesc)
        (archive--update-archive-contents pkgdesc destdir)
        ;; FIXME: Send email announcement!
        (let ((link (expand-file-name (format "%s.tar" pkgname) destdir)))
          (when (file-exists-p link) (delete-file link))
          (make-symbolic-link (file-name-nondirectory tarball) link))
        (dolist (oldtarball oldtarballs)
          ;; lzip compress oldtarballs.
          (let ((file (cdr oldtarball)))
            (when (string-match "\\.tar\\'" file)
              (archive--call nil "lzip" (expand-file-name file destdir))
              (setf (cdr oldtarball) (concat file ".lz")))))
        (let* ((default-directory (expand-file-name destdir)))
          ;; Apparently this also creates the <pkg>-readme.txt file.
          (archive--html-make-pkg pkgdesc
                                  `((,vers . ,(file-name-nondirectory tarball))
                                    . ,oldtarballs)
                                  dir))
        (message "Built new package %s!" tarball)
        ))))

(defun archive--get-devel-version (dir)
  "Compute the date-based pseudo-version used for devel builds."
  (let* ((default-directory (archive--dirname dir))
         (gitdate
          (with-temp-buffer
            (archive--call (current-buffer)
                           "git" "show" "--pretty=format:%cI" "--no-patch")
            (buffer-string)))
         (verdate
          ;; Convert Git's date into something that looks like a version number.
          ;; While we're at it, convert Git's date into its UTC equivalent,
          ;; to try and make sure time-versions are monotone.
          (let ((process-environment (cons "TZ=UTC" process-environment)))
            (with-temp-buffer
              (archive--call (current-buffer)
                             "date" "-d" gitdate "+%Y%m%d.%H%M%S")
              (buffer-string)))))
    ;; Get rid of leading zeros since ELPA's version numbers don't allow them.
    (replace-regexp-in-string "\\(?:\\`\\|[^0-9]\\)0+" "\\1"
                              ;; Remove trailing newline or anything untoward.
                              (replace-regexp-in-string "[^.0-9]+" ""
                                                        verdate))))

(defun archive--get-package-spec (pkgname)
  "Retrieve the property list for PKGNAME from `externals-list'."
  (let* ((specs (archive--form-from-file-contents "externals-list"))
         (spec (assoc pkgname specs)))
    (if (null spec)
        (error "Unknown package `%S`" pkgname)
      (cdr spec))))

(defun batch-make-all-packages (&rest _)
  "Check all the packages and build the relevant new tarballs."
  (let* ((specs (archive--form-from-file-contents "externals-list")))
    (dolist (spec specs)
      (with-demoted-errors "Build error: %S"
        (archive--make-one-package (format "%s" (car spec)))))))

(defun batch-make-one-package (&rest _)
  "Build the new tarballs (if needed) for one particular package,"
  (archive--make-one-package (pop command-line-args-left)))

(defun archive--make-one-package (pkgname)
  "Build the new tarballs (if needed) for PKGNAME."
  (let* ((dir (expand-file-name pkgname "packages")))
    (archive--message "Checking package %s for updates..." pkgname)
    (archive--external-package-sync pkgname)
    (let* ((pkg-spec (archive--get-package-spec pkgname))
           (_ (archive--message "pkg-spec for %s: %S" pkgname pkg-spec))
           (version-map (plist-get pkg-spec :version-map))
           (metadata (archive--metadata dir pkgname version-map))
           (vers (nth 1 metadata)))
      (archive--message "metadata = %S" metadata)
      (if (null metadata)
          (error "No metadata found for package: %s" pkgname)
        ;; Disregard the simple/multi distinction.  This might have been useful
        ;; in a distant past, but nowadays it's just unneeded extra complexity.
        (setf (car metadata) nil)
        ;; First, try and build the devel tarball
        ;; Do it before building the release tarball, because building
        ;; the release tarball may revert to some older commit.
        (let* ((date-version (archive--get-devel-version dir))
               ;; Add a ".0." so that when the version number goes from
               ;; NN.MM to NN.MM.1 we don't end up with the devel build
               ;; of NN.MM comparing as more recent than NN.MM.1.
               (devel-vers (concat vers ".0." date-version))
               (tarball (concat archive--devel-subdir
                                (format "%s-%s.tar" pkgname devel-vers)))
               (archive--name (concat archive--name "-devel")))
          (archive--make-one-tarball tarball
                                     dir pkgname
                                     `(nil ,devel-vers . ,(nthcdr 2 metadata))))
        ;; Try and build the latest release tarball.
        (cond
         ((or (equal vers "0")
              (let ((dont-release (plist-get pkg-spec :dont-release)))
                (when dont-release (string-match dont-release vers))))
          (archive--message "Package %s not released yet!" pkgname))
         (t
          (let ((tarball (concat archive--release-subdir
                                 (format "%s-%s.tar" pkgname vers))))
            (archive--make-one-tarball tarball
                                       dir pkgname metadata
                                       (lambda ()
                                         (archive--get-release-revision
                                          dir pkgname vers version-map))))))))))

(defun archive--call (destination program &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (apply #'call-process program nil destination nil args))

(defconst archive--revno-re "[0-9a-f]+")

(defun archive-prepare-packages (srcdir)
  "Prepare the `packages' directory inside the Git checkout.
Expects to be called from within the `packages' directory.
\"Prepare\" here is for subsequent construction of the packages and archive,
so it is meant to refresh any generated files we may need.
Currently only refreshes the ChangeLog files."
  (setq srcdir (archive--dirname srcdir))
  (let* ((wit ".changelog-witness")
         (prevno (with-temp-buffer
                   (insert-file-contents wit)
                   (if (looking-at (concat archive--revno-re "$"))
                       (match-string 0)
                     (error "Can't find previous revision name"))))
         (new-revno
          (or (with-temp-buffer
                (let ((default-directory srcdir))
                  (archive--call '(t) "git" "rev-parse" "HEAD")
                  (goto-char (point-min))
                  (when (looking-at (concat archive--revno-re "$"))
                    (match-string 0))))
              (error "Couldn't find the current revision's name")))
         (pkgs '()))
    (unless (equal prevno new-revno)
      (with-temp-buffer
        (let ((default-directory srcdir))
          (unless (zerop (archive--call '(t) "git" "diff"
                                       "--dirstat=cumulative,0"
                                       prevno))
            (error "Error signaled by git diff --dirstat %d" prevno)))
        (goto-char (point-min))
        (while (re-search-forward "^[ \t.0-9%]* packages/\\([-[:alnum:]]+\\)/$"
                                  nil t)
          (push (match-string 1) pkgs))))
    (let ((default-directory (expand-file-name "packages/")))
      (dolist (pkg pkgs)
        (condition-case v
            (when (file-directory-p pkg)
              (archive--make-changelog pkg (expand-file-name "packages/"
                                                             srcdir)))
          (error (message
		  "Error in archive-prepare-packages for package %S:\n  %S"
                  pkg v)))))
    (write-region new-revno nil wit nil 'quiet)
    ;; Also update the ChangeLog of external packages.
    (let ((default-directory (expand-file-name "packages/")))
      (dolist (dir (directory-files "."))
        (and (not (member dir '("." "..")))
             (file-directory-p dir)
             (let* ((gitdir (expand-file-name
                             (concat "packages/" dir "/.git")
                             srcdir))
                    (index (cond
                            ((file-directory-p gitdir)
                             (expand-file-name
                              (concat "packages/" dir "/.git/index")
                              srcdir))
                            ((file-readable-p gitdir)
                             (with-temp-buffer
                               (insert-file-contents gitdir)
                               (goto-char (point-min))
                               (if (looking-at "gitdir:[ \t]*")
                                   (progn
                                     (delete-region (match-beginning 0)
                                                    (match-end 0))
                                     (expand-file-name "index" (buffer-string)))
                                 (message "Can't find gitdir in %S" gitdir)
                                 nil)))
                            (t nil)))
                    (cl (expand-file-name "ChangeLog" dir)))
               (and index
                    (file-exists-p index)
                    (or (not (file-exists-p cl))
                        (file-newer-than-file-p index cl))))
             (archive--make-changelog
              dir (expand-file-name "packages/" srcdir)))))
    ))

(defconst archive-default-url-format (concat archive--url "%s.html"))
(defconst archive-default-url-re (format archive-default-url-format ".*"))


(defun archive--override-version (version-map orig-fun header)
  (let ((str (funcall orig-fun header)))
    (or (if (or (equal header "version")
                (and str (equal header "package-version")))
            (cadr (assoc str version-map)))
        str)))

(defun archive--metadata (dir pkg &optional version-map)
  "Return a list (SIMPLE VERSION DESCRIPTION REQ EXTRAS),
where SIMPLE is non-nil if the package is simple;
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((mainfile (expand-file-name (concat pkg ".el") dir))
         (files (directory-files dir nil "\\`dir\\'\\|\\.el\\'")))
    (setq files (delete (concat pkg "-pkg.el") files))
    (setq files (delete (concat pkg "-autoloads.el") files))
    (cond
     ((file-exists-p mainfile)
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
        (let* ((pkg-desc
                (unwind-protect
                    (progn
                      (when version-map
                        (advice-add 'lm-header :around
                                    (apply-partially
                                     #'archive--override-version
                                     version-map)))
                      (package-buffer-info))
                  (advice-remove 'lm-header
                                 #'archive--override-version)))
               (extras (package-desc-extras pkg-desc))
               (version (package-desc-version pkg-desc))
               (keywords (lm-keywords-list))
               ;; (_ (archive--version-to-list version)) ; Sanity check!
               (pt (lm-header "package-type"))
               (simple (if pt (equal pt "simple") (= (length files) 1)))
               (found-url (alist-get :url extras))
               (found-keywords (alist-get :keywords extras)))

          (when (and keywords (not found-keywords))
            ;; Using an old package-buffer-info which doesn't include
            ;; keywords.  Fix it by hand.
            (push (cons :keywords keywords) extras))
          (unless found-url
            ;; Provide a good default URL.
            (push (cons :url (format archive-default-url-format pkg)) extras))
          (list simple
		(package-version-join version)
		(package-desc-summary pkg-desc)
                (package-desc-reqs pkg-desc)
                extras))))
     (t
      (error "Can't find main file %s file in %s" mainfile dir)))))

(defun archive--process-simple-package (dir pkg vers desc req extras)
  "Deploy the contents of DIR into the archive as a simple package.
Rename DIR/PKG.el to PKG-VERS.el, delete DIR, and return the descriptor."
  ;; Write DIR/foo.el to foo-VERS.el and delete DIR
  (let ((src (expand-file-name (concat pkg ".el") dir)))
    (funcall (if (file-symlink-p src) #'copy-file #'rename-file)
	     src (concat pkg "-" vers ".el")))
  ;; Add the content of the ChangeLog.
  (let ((cl (expand-file-name "ChangeLog" dir)))
    (with-current-buffer (find-file-noselect (concat pkg "-" vers ".el"))
      (goto-char (point-max))
      (re-search-backward "^;;;.*ends here")
      (re-search-backward "^(provide")
      (skip-chars-backward " \t\n")
      (insert "\n\n;;;; ChangeLog:\n\n")
      (let* ((start (point))
             (end (copy-marker start t)))
        (condition-case nil
            (insert-file-contents cl)
          (file-error (message "Can't find %S's ChangeLog file" pkg)))
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (while (progn (forward-line -1) (>= (point) start))
          (insert ";; ")))
      (set (make-local-variable 'backup-inhibited) t)
      (basic-save-buffer)               ;Less chatty than save-buffer.
      (kill-buffer)))
  (delete-directory dir t)
  (cons (intern pkg) (vector (archive--version-to-list vers)
                             req desc 'single extras)))

(defun archive--make-changelog (dir srcdir)
  "Export Git log info of DIR into a ChangeLog file."
  (message "Refreshing ChangeLog in %S" dir)
  (let ((default-directory (archive--dirname dir)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (when (file-readable-p "ChangeLog") (insert-file-contents "ChangeLog"))
        (let ((old-md5 (md5 (current-buffer))))
          (erase-buffer)
          (let ((default-directory (archive--dirname dir srcdir)))
            (archive--call (current-buffer) ; hmm, why not use ‘t’ here? --ttn
                          "git" "log" "--date=short"
                          "--format=%cd  %aN  <%ae>%n%n%w(80,8,8)%B%n"
                          "."))
          (tabify (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil t)
            (replace-match "\n\n"))
          (if (equal old-md5 (md5 (current-buffer)))
              (message "ChangeLog's md5 unchanged for %S" dir)
            (write-region (point-min) (point-max) "ChangeLog" nil 'quiet)))))))

(defun archive--alist-to-plist-args (alist)
  (mapcar (lambda (x)
            (if (and (not (consp x))
                     (or (keywordp x)
                         (not (symbolp x))
                         (memq x '(nil t))))
                x `',x))
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(defun archive--plist-args-to-alist (plist)
  (let (alist)
    (while plist
      (let ((value (cadr plist)))
        (when value
          (cl-assert (keywordp (car plist)))
          (push (cons (car plist)
                      (if (eq 'quote (car-safe value)) (cadr value) value))
                alist)))
      (setq plist (cddr plist)))
    alist))

(defun archive--process-multi-file-package (dir pkg &optional dont-rename)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and return the descriptor."
  (let* ((exp (archive--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
         (req-exp (nth 4 exp))
	 (req (mapcar #'archive--convert-require
                      (if (eq 'quote (car-safe req-exp)) (nth 1 req-exp)
                        (when req-exp
                          (error "REQ should be a quoted constant: %S"
                                 req-exp)))))
         (extras (archive--plist-args-to-alist (nthcdr 5 exp))))
    (unless (equal (nth 1 exp) pkg)
      (error (format "Package name %s doesn't match file name %s"
		     (nth 1 exp) pkg)))
    (unless dont-rename (rename-file dir (concat pkg "-" vers)))
    (cons (intern pkg) (vector (archive--version-to-list vers)
                               req (nth 3 exp) 'tar extras))))

(defun archive--form-from-file-contents (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    ;; This is unnecessary because ‘with-temp-buffer’ generates a new
    ;; (empty) buffer, and ‘insert-file-contents’ inserts after point.
    ;; In other words, point is alraedy at bob.
    ;;- (goto-char (point-min))
    (read (current-buffer))))

(defun archive--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (unless (file-exists-p pkg-file)
      (error "File not found: %s" pkg-file))
    (archive--form-from-file-contents pkg-file)))

(defun archive-refresh-pkg-file ()
  ;; Note: Used via --batch by GNUmakefile rule.
  (let* ((dir (directory-file-name default-directory))
         (pkg (file-name-nondirectory dir)))
    (archive--write-pkg-file dir pkg (archive--metadata dir pkg))))

(defun archive--write-pkg-file (pkg-dir name metadata)
  ;; FIXME: Use package-generate-description-file!
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (write-region
     (concat (format ";; Generated package description from %s.el  -*- no-byte-compile: t -*-\n"
		     name)
	     (prin1-to-string
              (cl-destructuring-bind (version desc requires extras)
                  (cdr metadata)
                (nconc
                 (list 'define-package
                       name
                       version
                       desc
                       (list 'quote
                             ;; Turn version lists into string form.
                             (mapcar
                              (lambda (elt)
                                (list (car elt)
                                      (package-version-join (cadr elt))))
                              requires)))
                 (archive--alist-to-plist-args extras))))
	     "\n")
     nil
     pkg-file)))

;;; Make the HTML pages for online browsing.

(defun archive--html-header (title &optional header)
  (format "<!DOCTYPE HTML PUBLIC>
<html lang=\"en\" xml:lang=\"en\">
    <head>
        <title>%s</title>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <link rel=\"shortcut icon\" type=\"image/png\" href=\"../favicon.png\">
        <link rel=\"stylesheet\" href=\"//code.cdn.mozilla.net/fonts/fira.css\">
        <link rel=\"stylesheet\" type=\"text/css\" href=\"../layout.css\">
        <script src=\"../javascript/jquery.min.js\" type=\"text/javascript\"></script>
        <script src=\"../javascript/jquery.filtertable.min.js\" type=\"text/javascript\"></script>
        <script src=\"../javascript/package-search.js\" type=\"text/javascript\"></script>
        <meta name=\"viewport\" content=\"initial-scale=1.0,maximum-scale=1.0,width=device-width\" />
    </head>
    <body>

        <div class=\"wrapper\">

            <div class=\"header small\">
                <div class=\"container\">
                    <h1>%s</h1>
                </div>
            </div>

            <div class=\"container\">\n"
          title (or header title)))

(defun archive--html-bytes-format (bytes) ;Aka memory-usage-format.
  (setq bytes (/ bytes 1024.0))
  (let ((units '("KiB" "MiB" "GiB" "TiB")))
    (while (>= bytes 1024)
      (setq bytes (/ bytes 1024.0))
      (setq units (cdr units)))
    (cond
     ((>= bytes 100) (format "%4.0f&nbsp;%s" bytes (car units)))
     ((>= bytes 10) (format "%4.1f&nbsp;%s" bytes (car units)))
     (t (format "%4.2f&nbsp;%s" bytes (car units))))))

(defun archive--get-prop (prop name srcdir mainsrcfile)
  (let ((kprop (intern (format ":%s" (downcase prop)))))
    (or
     (let ((pkgdescfile (expand-file-name (format "%s-pkg.el" name)
                                          srcdir)))
       (when (file-readable-p pkgdescfile)
         (let* ((desc (archive--form-from-file-contents pkgdescfile))
                (val-exp (plist-get (cdr desc) kprop)))
           (if (eq 'quote (car-safe val-exp))
               (cadr val-exp)
             val-exp))))
     (when (file-readable-p mainsrcfile)
       (with-temp-buffer
         (insert-file-contents mainsrcfile)
         (lm-header prop))))))

(defun archive--get-section (hsection fsection srcdir mainsrcfile)
  (when (consp fsection)
    (while (cdr-safe fsection)
      (setq fsection
            (if (file-readable-p (expand-file-name (car fsection) srcdir))
                (car fsection)
              (cdr fsection))))
    (when (consp fsection) (setq fsection (car fsection))))
  (cond
   ((file-readable-p (expand-file-name fsection srcdir))
    (with-temp-buffer
      (insert-file-contents (expand-file-name fsection srcdir))
      (buffer-string)))
   ((file-readable-p mainsrcfile)
    (with-temp-buffer
      (insert-file-contents mainsrcfile)
      (emacs-lisp-mode)       ;lm-section-start needs the outline-mode setting.
      (let ((start (lm-section-start hsection)))
        (when start
          (insert
           (prog1
               (buffer-substring start (lm-section-end hsection))
             (erase-buffer)))
          (emacs-lisp-mode)
          (goto-char (point-min))
          (delete-region (point) (line-beginning-position 2))
          (uncomment-region (point-min) (point-max))
          (when (looking-at "^\\([ \t]*\n\\)+")
            (replace-match ""))
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (delete-region (point) (point-max))
          (buffer-string)))))))

(defun archive--quote (txt)
  (replace-regexp-in-string "<" "&lt;"
                            (replace-regexp-in-string "&" "&amp;" txt)))

(defun archive--read-externals-list (&optional dir)
  (archive--form-from-file-contents
   (expand-file-name "externals-list" dir)))

(defun archive--insert-repolinks (name srcdir _mainsrcfile url)
  (when url
    (insert (format "<dt>Home page</dt> <dd><a href=%S>%s</a></dd>\n"
                    url (archive--quote url)))
    (when (string-match archive-default-url-re url)
      (setq url nil)))
  (let* ((externals (archive--read-externals-list
                     (expand-file-name "../../../elpa" srcdir)))
         (extern-desc (assoc name externals))
         (git-sv "http://git.savannah.gnu.org/")
         (urls
          (if (eq (nth 1 extern-desc) :core)
              (let* ((files (nth 2 extern-desc))
                     (file (if (listp files)
                               (directory-file-name
                                (file-name-directory
                                 (try-completion "" files)))
                             files)))
                (mapcar (lambda (s) (concat s file))
                        `("cgit/emacs.git/tree/"
                          ,(if (listp files)
                               "gitweb/?p=emacs.git;a=tree;f="
                             "gitweb/?p=emacs.git;a=blob;f="))))
            (mapcar (lambda (s) (format s archive--gitrepo name))
                    (if (eq (nth 1 extern-desc) :external)
                        '("cgit/%s/?h=externals/%s"
                          "gitweb/?p=%s;a=shortlog;h=refs/heads/externals/%s")
                      '("cgit/%s/tree/packages/%s"
                        "gitweb/?p=%s;a=tree;f=packages/%s"))))))
    (insert (format
             (concat (format "<dt>Browse %srepository</dt> <dd>" (if url "ELPA's " ""))
                     "<a href=%S>%s</a> or <a href=%S>%s</a></dd>\n")
             (concat git-sv (nth 0 urls))
             'CGit
             (concat git-sv (nth 1 urls))
             'Gitweb))))

(defun archive--html-make-pkg (pkg files &optional srcdir)
  (let* ((name (symbol-name (car pkg)))
         (latest (package-version-join (aref (cdr pkg) 0)))
         (srcdir (or srcdir
                     (expand-file-name name "../../build/packages")))
         (mainsrcfile (expand-file-name (format "%s.el" name) srcdir))
         (desc (aref (cdr pkg) 2)))
    (with-temp-buffer
      (insert (archive--html-header
               (format "%s ELPA - %s" archive--name name)
               (format "<a href=\"index.html\">%s ELPA</a> - %s"
                       archive--name name)))
      (insert (format "<h2 class=\"package\">%s</h2>" name))
      (insert "<dl>")
      (insert (format "<dt>Description</dt><dd>%s</dd>\n" (archive--quote desc)))
      (if (zerop (length latest))
          (insert "<dd>This package "
                  (if files (concat "is not in " archive--name " ELPA any more")
                    "has not been released yet")
                  ".</dd>\n")
        (let* ((file (cdr (assoc latest files)))
               (attrs (file-attributes file)))
          (insert (format "<dt>Latest</dt> <dd><a href=%S>%s</a>, %s, %s</dd>\n"
                          file (archive--quote file)
                          (format-time-string "%Y-%b-%d" (nth 5 attrs))
                          (archive--html-bytes-format (nth 7 attrs))))))
      (let ((maint (archive--get-prop "Maintainer" name srcdir mainsrcfile)))
        (when maint
          (when (consp maint)
            (archive--message "maint=%S" maint)
            (setq maint (concat (if (car maint) (concat (car maint) " "))
                                "<" (cdr maint) ">")))
          (insert (format "<dt>Maintainer</dt> <dd>%s</dd>\n" (archive--quote maint)))))
      (archive--insert-repolinks
       name srcdir mainsrcfile
       (or (cdr (assoc :url (aref (cdr pkg) 4)))
           (archive--get-prop "URL" name srcdir mainsrcfile)))
      (insert "</dl>")
      (insert (format "<p>To install this package, run in Emacs:</p>
                       <pre>M-x <span class=\"kw\">package-install</span> RET <span class=\"kw\">%s</span> RET</pre>"
                      name))
      (let ((rm (archive--get-section
                 "Commentary" '("README" "README.rst"
                                ;; Most README.md files seem to be currently
                                ;; worse than the Commentary: section :-(
                                ;; "README.md"
                                "README.org")
                 srcdir mainsrcfile)))
        (when rm
          (write-region rm nil (concat name "-readme.txt"))
          (insert "<h2>Full description</h2><pre>\n" (archive--quote rm)
                  "\n</pre>\n")))
      (unless (< (length files) (if (zerop (length latest)) 1 2))
        (insert (format "<h2>Old versions</h2><table>\n"))
        (dolist (file
                 (sort files (lambda (f1 f2) (version< (car f2) (car f1)))))
          (unless (equal (pop file) latest)
            (let ((attrs (file-attributes file)))
              (insert (format "<tr><td><a href=%S>%s</a></td><td>%s</td><td>%s</td>\n"
                              file (archive--quote file)
                              (format-time-string "%Y-%b-%d" (nth 5 attrs))
                              (archive--html-bytes-format (nth 7 attrs)))))))
        (insert "</table>\n"))
      (let ((news (archive--get-section
                   "News" '("NEWS" "NEWS.rst" "NEWS.md" "NEWS.org")
                   srcdir mainsrcfile)))
        (when news
          (insert "<h2>News</h2><pre>\n" (archive--quote news) "\n</pre>\n")))
      (insert "</body>\n")
      (write-region (point-min) (point-max) (concat name ".html")))))

(defun archive--html-make-index (pkgs)
  (with-temp-buffer
    (insert (archive--html-header (concat archive--name " ELPA Packages")))
    (insert "<table>\n")
    (insert "<tr><th>Package</th><th>Version</th><th>Description</th></tr>\n")
    (dolist (pkg pkgs)
      (insert (format "<tr><td><a href=\"%s.html\">%s</a></td><td>%s</td><td>%s</td></tr>\n"
                      (car pkg) (car pkg)
                      (package-version-join (aref (cdr pkg) 0))
                      (aref (cdr pkg) 2))))
    (insert "                </table>
            </div>
            <div class=\"push\"></div>
        </div>

        <div class=\"footer\">
            <div class=\"container\">
                <p>Copyright 2016 <a href=\"https://fsf.org\">Free Software Foundation</a>, Inc.</p>
                <p>Design provided by <a href=\"http://nicolas.petton.fr\">Nicolas Petton</a></p>
                <p>
                   This website is licensed under the
                   <a href=\"https://creativecommons.org/licenses/by-nd/3.0/us/\">CC BY-ND 3.0</a>
                   US License.
                </p>
            </div>
        </div>

</body>\n")
    (write-region (point-min) (point-max) "index.html")))

(defun batch-html-make-index ()
  (let ((packages (make-hash-table :test #'equal))
        (archive-contents
         ;; Skip the first element which is a version number.
         (cdr (archive--form-from-file-contents "archive-contents"))))
    (dolist (subdir (directory-files "../../build/packages" nil))
      (cond
       ((member subdir '("." ".." "elpa.rss" "index.html" "archive-contents")))
       (t (puthash subdir nil packages))))
    (dolist (file (directory-files default-directory nil))
      (cond
       ((member file '("." ".." "elpa.rss" "index.html" "archive-contents")))
       ((string-match "\\.html\\'" file))
       ((string-match "\\.sig\\'" file))
       ((string-match "-readme\\.txt\\'" file)
        (let ((name (substring file 0 (match-beginning 0))))
          (puthash name (gethash name packages) packages)))
       ((string-match "-\\([0-9][^-]*\\)\\.\\(tar\\|el\\)\\'" file)
        (let ((name (substring file 0 (match-beginning 0)))
              (version (match-string 1 file)))
          (push (cons version file) (gethash name packages))))
       (t (message "Unknown file %S" file))))
    (maphash (lambda (pkg-name files)
               (archive--html-make-pkg
                (let ((pkg (intern pkg-name)))
                  (or (assq pkg archive-contents)
                      ;; Add entries for packages that are either not yet
                      ;; released or not released any more.
                      ;; FIXME: Get actual description!
                      (let ((entry (cons pkg (vector nil nil "" nil nil))))
                        (setq archive-contents
                              ;; Add entry at the end.
                              (nconc archive-contents (list entry)))
                        entry)))
                files))
             packages)
    (archive--html-make-index archive-contents)))

(defun archive--pull (dirname)
  (let ((default-directory (archive--dirname dirname)))
    (with-temp-buffer
      (cond
       ((file-directory-p ".git")
        (message "Running git pull in %S" default-directory)
        (archive--call t "git" "pull"))
       ((file-exists-p ".git")
        (if (with-temp-buffer
              (archive--call t "git" "status" "--branch" "--porcelain=2")
              (goto-char (point-min))
              ;; Nothing to pull (nor push, actually).
              (search-forward "\n# branch.ab +0 -0" nil t))
            (message "%s up-to-date" dirname)
          (message "Updating worktree in %S" default-directory)
          (archive--call t "git" "merge")))
       (t (error "No .git in %S" default-directory)))
      (unless (and (eobp) (bobp))
        (message "Updated %s:%s%s" dirname
                 (if (and (eobp) (bolp)
                          (eq (line-beginning-position 0) (point-min)))
                     " " "\n")
                 (buffer-string))))))

;;; Maintain external packages.

(defconst archive--elpa-git-url "git://git.sv.gnu.org/emacs/elpa")
(defconst archive--emacs-git-url "git://git.sv.gnu.org/emacs.git")

(defun archive--sync-emacs-repo ()
  "Sync Emacs repository, if applicable.
Return non-nil if there's an \"emacs\" repository present."
  ;; Support for :core packages is important for elpa.gnu.org, but for other
  ;; cases such as "in-place installation", it's rather secondary since
  ;; those users can just as well use a development version of Emacs to get
  ;; those packages.
  ;; So make the handling of :core packages depend on whether or not the user
  ;; has setup a clone of Emacs under the "emacs" subdirectory.
  (let ((emacs-repo-root (expand-file-name "emacs")))
    (if (not (file-directory-p emacs-repo-root))
        (progn (message "No \"emacs\" subdir: will skip :core packages")
               nil)
      (archive--pull emacs-repo-root)
      t)))

(defun archive--find-non-trivial-file (dir)
  (catch 'found-important-file
    (dolist (file (directory-files-recursively dir ".*"))
      (unless (or (member file '("." ".."))
                  (string-match "\\.elc\\'" file)
                  (string-match "-autoloads.el\\'" file)
                  (string-match "-pkg.el\\'" file)
                  (file-symlink-p file))
        (throw 'found-important-file file)))
    nil))

(defun archive--cleanup-packages (externals-list with-core)
  "Remove unknown subdirectories of `packages/'.
This is any subdirectory inside `packages/' that's not under
version control nor listed in EXTERNALS-LIST.
If WITH-CORE is non-nil, it means we manage :core packages as well."
  (when (file-directory-p (expand-file-name "packages/"))
    (let ((default-directory (expand-file-name "packages/")))
      (dolist (dir (directory-files "."))
        (cond
         ((file-symlink-p dir)
          ;; There are normally no such thing, but the user may elect to
          ;; add symlinks to other projects.  If so, update them, as if they
          ;; were "externals".
          (when (file-directory-p (expand-file-name ".git" dir))
            (archive--pull dir)))
         ((or (not (file-directory-p dir)) )
          ;; We only add/remove plain directories in elpa/packages (not
          ;; symlinks).
          nil)
         ((member dir '("." "..")) nil)
         ((assoc dir externals-list) nil)
         ((file-directory-p (expand-file-name (format "%s/.git" dir)))
          (let ((status
                 (with-temp-buffer
                   (let ((default-directory (archive--dirname dir)))
                     (archive--call t "git" "status" "--porcelain")
                     (buffer-string)))))
            (if (zerop (length status))
                (progn (delete-directory dir 'recursive t)
                       (message "Deleted all of %s" dir))
              (message "Keeping leftover unclean %s:\n%s" dir status))))
         ;; Check if `dir' is under version control.
         ((and with-core
               (not (zerop (archive--call nil "git" "ls-files"
                                         "--error-unmatch" dir))))
          ;; Not under version control.  Check if it only contains
          ;; symlinks and generated files, in which case it is probably
          ;; a leftover :core package that can safely be deleted.
          ;; (let ((file (archive--find-non-trivial-file dir)))
          ;;   (if file
          ;;       (message "Keeping %s for non-trivial file \"%s\"" dir file)
          ;;     (progn
          ;;       (message "Deleted untracked package %s" dir)
          ;;       (delete-directory dir 'recursive t))))
          ))))))

(defvar archive--use-worktree nil)
(defun archive--use-worktree-p ()
  (unless archive--use-worktree
    (setq archive--use-worktree
          (list
           (ignore-errors
             (zerop (archive--call nil "git" "worktree" "list"))))))
  (car archive--use-worktree))

(defun archive--external-package-sync (name)
  "Sync external package named NAME."
  (let ((default-directory (expand-file-name "packages/")))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (cond ((not (file-exists-p name))
           (let* ((branch (concat "externals/" name))
                  (output
                   (with-temp-buffer
                     (if (archive--use-worktree-p)
                         (archive--call t "git" "worktree" "add"
                                       "-B" branch
                                       name (concat "origin/" branch))
                       (archive--call t "git" "clone"
                                     "--reference" ".." "--single-branch"
                                     "--branch" branch
                                     archive--elpa-git-url name))
                     (buffer-string))))
             (message "Cloning branch %s:\n%s" name output)))
          ((not (file-exists-p (concat name "/.git")))
           (message "%s is in the way of an external, please remove!" name))
          (t (archive--pull name)))))

(defun archive--core-package-empty-dest-p (dest)
  "Return non-nil if DEST is an empty variant."
  (member dest (list "" "." nil)))

(defun archive--core-package-link-file
    (source dest emacs-repo-root package-root exclude-regexp)
  "Link file from SOURCE to DEST ensuring subdirectories."
  (unless (string-match-p exclude-regexp source)
    (let* ((absolute-package-file-name
	    (if (equal "" dest)
		;; Calling expand-file-name would remove the trailing / !
		package-root
              (expand-file-name dest package-root)))
           (absolute-core-file-name
            (expand-file-name source emacs-repo-root))
           (directory (file-name-directory absolute-package-file-name)))
      (when (fboundp 'file-name-quote)  ;Not yet available on elpa.gnu.org
        (setq directory (file-name-quote directory)))
      (unless (file-directory-p directory)
        (make-directory directory t))
      (condition-case err
	  (make-symbolic-link absolute-core-file-name
			      absolute-package-file-name t)
	(file-error
         (message "Error: can't symlink to %S from %S:\n  %S"
                  absolute-core-file-name absolute-package-file-name err)
	 (copy-file absolute-core-file-name
		    (if (file-directory-p absolute-package-file-name)
			(file-name-as-directory absolute-package-file-name)
		      absolute-package-file-name)))))
    (message "  %s -> %s" source (if (archive--core-package-empty-dest-p dest)
                                     (file-name-nondirectory source)
                                   dest))))

(defun archive--core-package-link-directory
    (source dest emacs-repo-root package-root exclude-regexp)
  "Link directory files from SOURCE to DEST ensuring subdirectories."
  (let ((stack (list source))
        (base source)
        (absolute-source))
    (while stack
      (setq source (pop stack)
            absolute-source (expand-file-name source emacs-repo-root))
      (if (file-directory-p absolute-source)
          (dolist (file (directory-files absolute-source))
            (unless (member file (list "." ".."))
              (push (concat (file-name-as-directory source) file) stack)))
        (let* ((base (file-name-as-directory base))
               (source-sans-base (substring source (length base)))
               (package-file-name
                (if (archive--core-package-empty-dest-p dest)
                    ;; Link to root with its original filename.
                    source-sans-base
                  (concat
                   ;; Prepend the destination, allowing for directory rename.
                   (file-name-as-directory dest) source-sans-base))))
          (archive--core-package-link-file
           source package-file-name
           emacs-repo-root package-root exclude-regexp))))))

(defun archive--core-package-sync (definition)
  "Sync core package from DEFINITION."
  (pcase-let*
      ((`(,name . (:core ,file-patterns :excludes ,excludes)) definition)
       (emacs-repo-root (expand-file-name "emacs"))
       (package-root (archive--dirname name "packages"))
       (default-directory package-root)
       (exclude-regexp
        (mapconcat #'identity
                   (mapcar #'wildcard-to-regexp
                           (append '("*.elc" "*~") excludes nil))
                   "\\|"))
       (file-patterns
        (mapcar
         (lambda (file-pattern)
           (pcase file-pattern
             ((pred (stringp)) (cons file-pattern ""))
             (`(,file ,dest . ,_) (cons file dest))
             (_ (error "Unrecognized file format for package %s: %S"
                       name file-pattern))))
         (if (stringp file-patterns)
             ;; Files may be just a string, normalize.
             (list file-patterns)
           file-patterns))))
    (message "Linking files for package: %s" name)
    (when (file-directory-p package-root)
      (delete-directory package-root t))
    (make-directory package-root t)
    (dolist (file-pattern file-patterns)
      (pcase-let* ((`(,file . ,dest) file-pattern))
        (if (file-directory-p (expand-file-name file emacs-repo-root))
            (archive--core-package-link-directory
             file dest emacs-repo-root package-root exclude-regexp)
          (archive--core-package-link-file
           file dest emacs-repo-root package-root exclude-regexp))))))

(defun archive-add/remove/update-externals ()
  "Remove non-package directories and fetch external packages."
  (let ((externals-list (archive--read-externals-list)))
    (let ((with-core (archive--sync-emacs-repo)))
      (archive--cleanup-packages externals-list with-core)
      (pcase-dolist ((and definition `(,name ,kind ,_url)) externals-list)
        (pcase kind
          (`:subtree nil)               ;Nothing to do.
          (`:external (archive--external-package-sync name))
          (`:core (when with-core (archive--core-package-sync definition)))
          (_ (message "Unknown external package kind `%S' for %s"
                      kind name)))))))

;;; Manage .gitignore

(defun archive-gitignore-externals (elf gf)
  (let ((pkgs (cl-loop
               for (name kind . _) in (archive--read-externals-list
                                       (file-name-directory elf))
               when (memq kind '(:external :core))
               collect name)))
    (with-current-buffer (find-file-noselect gf)
      (goto-char (point-min))
      (when (re-search-forward
             "#.*External.*git.*\n\\(packages/[^*/\n]+/?\n\\)+"
             nil 'move)
        (replace-match ""))
      (insert "# External packages with their own .git tree [autogenerated].\n"
              (mapconcat (lambda (p) (format "packages/%s/\n" p))
                         (sort pkgs #'string<)
                         ""))
      (save-buffer))))

(provide 'archive-contents)
;;; archive-contents.el ends here
