;;; elpa-admin.el --- Auto-generate an Emacs Lisp package archive  -*- lexical-binding:t -*-

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

;;;; TODO

;; - support for conveniently rebuilding individual files like
;;   index.html, archive-contents, or <pkg>.html
;; - render the README and News in the HTML rather than as <pre> block!

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)


(defvar elpaa--release-subdir "archive/"
  "Subdirectory where the ELPA release files (tarballs, ...) will be placed.")
(defvar elpaa--devel-subdir "archive-devel/"
  "Subdirectory where the ELPA bleeding edge files (tarballs, ...) will be placed.")
(defvar elpaa--name "NonGNU")
(defvar elpaa--gitrepo "emacs/nongnu.git")
(defvar elpaa--url "https://elpa.gnu.org/nongnu/")

(defvar elpaa--branch-prefix "externals/")
(defvar elpaa--release-branch-prefix "externals-release/")

(defvar elpaa--specs-file "externals-list")
(defvar elpaa--copyright-file "copyright_exceptions")
(defvar elpaa--email-to nil) ;;"gnu-emacs-sources@gnu.org"
(defvar elpaa--email-from nil) ;;"ELPA update <do.not.reply@elpa.gnu.org>"
(defvar elpaa--email-reply-to nil)

(defvar elpaa--sandbox t
  "If non-nil, run some of the less trusted commands in a sandbox.
This is recommended when building packages from untrusted sources,
but this requires Bubblewrap to be installed and has only been tested
on some Debian systems.")

(defvar elpaa--debug nil)

(defun elpaa-read-config (&optional file)
  (let ((config (elpaa--form-from-file-contents (or file "elpa-config"))))
    (pcase-dolist (`(,var ,val) config)
      (cl-assert (or (stringp val) (booleanp val)) t)
      (setf (pcase-exhaustive var
              ('name			elpaa--name)
              ('gitrepo			elpaa--gitrepo)
              ('url			elpaa--url)
              ('branch-prefix		elpaa--branch-prefix)
              ('release-branch-prefix	elpaa--release-branch-prefix)
              ('specs-file		elpaa--specs-file)
              ('copyright-file		elpaa--copyright-file)
              ('email-to		elpaa--email-to)
              ('email-from		elpaa--email-from)
              ('email-reply-to		elpaa--email-reply-to)
              ('sandbox			elpaa--sandbox)
              ('debug			elpaa--debug))
            val))))

(defun elpaa--message (&rest args)
  (when elpaa--debug (apply #'message args)))

(defconst elpaa--re-no-dot "\\`\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regular expression matching all files except \".\" and \"..\".")

(defun elpaa--version-to-list (vers)
  (when vers
    (let ((l (version-to-list vers)))
      ;; Signal an error for things like "1.02" which is parsed as "1.2".
      (cl-assert (equal vers (package-version-join l)) nil
                 "Unsupported version syntax %S" vers)
      l)))

(defun elpaa--convert-require (elt)
  (let ((vers (elpaa--version-to-list (car (cdr elt)))))
    (if vers
        (list (car elt) vers)
      (list (car elt)))))

(defun elpaa--dirname (dir &optional base)
  (file-name-as-directory (expand-file-name dir base)))

(defun elpaa--delete-elc-files (dir &optional only-orphans)
  "Recursively delete all .elc files in DIR.
Delete backup files also."
  (dolist (f (directory-files dir t elpaa--re-no-dot))
    (cond ((file-directory-p f)
	   (elpaa--delete-elc-files f))
	  ((or (and (string-match "\\.elc\\'" f)
                    (not (and only-orphans
                              (file-readable-p (replace-match ".el" t t f)))))
	       (backup-file-name-p f))
	   (delete-file f)))))

(defun elpaa--update-archive-contents (pkg-desc dir)
  "Update the `archive-contents' file in DIR with new package PKG-DESC."
  (let* ((filename (expand-file-name "archive-contents" dir))
         (ac (if (file-exists-p filename)
                 (elpaa--form-from-file-contents filename)
               '(1))))
    (elpaa--message "current AC: %S" ac)
    (setf (alist-get (car pkg-desc) (cdr ac)) (cdr pkg-desc))
    (setf (cdr ac) (sort (cdr ac)
                         (lambda (x y)
                           (string-lessp (symbol-name (car x)) (symbol-name (car y))))))
    (elpaa--message "new AC: %S" ac)
    (with-temp-buffer
      (pp ac (current-buffer))
      (write-region nil nil filename)
      (let ((default-directory (expand-file-name dir)))
        (elpaa--html-make-index (cdr ac))))))

(defun elpaa--get-specs ()
  (elpaa--form-from-file-contents "externals-list"))

(defun elpaa--spec-get (pkg-spec prop &optional default)
  (or (plist-get (cdr pkg-spec) prop) default))

(defun elpaa--main-file (pkg-spec)
  (or (elpaa--spec-get pkg-spec :main-file)
      (concat (car pkg-spec) ".el")))

(defun elpaa--get-release-revision (dir pkg-spec &optional vers version-map)
  "Get the REVISION that corresponds to current release.
This is either found from VERS in VERSION-MAP or by looking at the last
commit which modified the \"Version:\" pseudo header."
  (while (and version-map
              (not (member vers (car version-map))))
    (pop version-map))
  (or (nth 2 (car version-map))
      (let* ((default-directory (elpaa--dirname dir))
             (release-rev
              (with-temp-buffer
                (if (equal 0         ;Don't signal an error if call errors out.
                     (elpaa--call
                      (current-buffer)
                      "git" "log" "-n1" "--oneline" "--no-patch"
                      "--pretty=format:%H"
                      "-L" (concat "/^;;* *\\(Package-\\)\\?Version:/,+1:"
                                   (elpaa--main-file pkg-spec))))
                    (buffer-string)
                  (cons 'error (buffer-string))))))
        (if (stringp release-rev)
            (progn
              (elpaa--message "Found release rev: %S" release-rev)
              release-rev)
          (elpaa--message "Can't find release rev: %s" (cdr release-rev))
          nil))))

(defun elpaa--get-last-release (pkg-spec)
  "Return (VERSION . REV) of the last release.
Assumes that the current worktree holds a snapshot version."
  (with-temp-buffer
    (let* ((default-directory (elpaa--dirname (car pkg-spec) "packages"))
           (release-branch (elpaa--spec-get pkg-spec :release-branch))
           (L-spec (concat "/^;;* *\\(Package-\\)\\?Version:/,+1:"
                           (elpaa--main-file pkg-spec)))
           (search-start-rev
            (or (if release-branch
                    (concat "refs/remotes/origin/"
                            elpaa--release-branch-prefix (car pkg-spec)))
                (if (not (equal 0     ;Don't signal an error if call errors out.
                                (elpaa--call
                                 (current-buffer)
                                 "git" "log" "-n1" "--oneline" "--no-patch"
                                 "--pretty=format:%H"
                                 "-L" L-spec)))
                    (progn
                      (elpaa--message "Error in git-log:\n" (buffer-string))
                      nil)
                  (goto-char (point-min))
                  (concat
                   ;; This is the rev of the last change to Version:
                   (buffer-substring (point) (line-end-position))
                   "~1")))))
      (erase-buffer)
      (if (not (equal 0              ;Don't signal an error if call errors out.
                      (elpaa--call
                       (current-buffer)
                       "git" "log" "-n1" "--oneline"
                       "--pretty=format:%H"
                       "-L" L-spec
                       search-start-rev)))
          (progn
            (elpaa--message "Error in git-log:\n" (buffer-string))
            nil)
        (goto-char (point-min))
        (let ((rev (buffer-substring (point) (line-end-position)))
              (case-fold-search t))
          (if (not (re-search-forward "^\\+.*Version:[ \t]*\\(.+?\\)[ \t]*$"
                                      nil t))
              (elpaa--message "No previous release version found")
            (let* ((vers (match-string 1))
                   (vl (condition-case err (version-to-list vers)
                         (error (elpaa--message "Error: %S" err) nil))))
              (cond
               ((null vl)
                (elpaa--message "Invalid previous release version"))
               ((member -4 vl)
                (elpaa--message "Previous version was also snapshot"))
               (t
                (cons (package-version-join vl) rev))))))))))

(defun elpaa--select-revision (dir pkg-spec rev)
  "Checkout revision REV in DIR of PKG-SPEC.
Do it without leaving the current branch."
  (let ((cur-rev
         ;; FIXME: Emacs-26's `vc-git-working-revision' ignores its arg and
         ;; uses uses the `default-directory' to get the revision.
         (let* ((ftn (file-truename
                      (expand-file-name (elpaa--main-file pkg-spec) dir)))
                (default-directory (file-name-directory ftn)))
           (vc-working-revision ftn))))
    (if (equal rev cur-rev)
        (elpaa--message "Current revision is already desired revision!")
      (with-temp-buffer
        (let ((default-directory (elpaa--dirname dir)))
          (elpaa--call t "git" "reset" "--merge" rev)
          (elpaa--message "Reverted to release revision %s\n%s"
                          rev (buffer-string)))))))

(defun elpaa--make-tar-transform (pkgname r)
  (let ((from (nth 0 r)) (to (nth 1 r)))
    (cl-assert (not (string-match "[][*\\|?]" from)))
    (cl-assert (not (string-match "[][*\\|?]" to)))
    (format "--transform=s|^packages/%s/%s|packages/%s/%s|"
            pkgname
            (if (string-match "/\\'" from)
                (concat (substring from 0 -1) "\\($\\|/\\)")
              (concat from "$"))
            pkgname to)))

(defvar elpaa--temp-files)

(defun elpaa--temp-file (f)
  (when (boundp 'elpaa--temp-files)
    (push (if (stringp f) (expand-file-name f) f) elpaa--temp-files)))

(defmacro elpaa--with-temp-files (dir &rest body)
  (declare (debug t) (indent 1))
  `(elpaa--call-with-temp-files ,dir (lambda () . ,body)))

(defun elpaa--call-with-temp-files (dir f)
  (let ((elpaa--temp-files nil))
    (unwind-protect
        (progn
          (elpaa--clean dir)
          (funcall f))
      (elpaa--message "Deleting temp files: %S" elpaa--temp-files)
      (dolist (f elpaa--temp-files)
        (if (stringp f)
            (delete-file f)
          (funcall f))))))

(defun elpaa--clean (dir)
  "Try and bring DIR to a pristine state without losing too much info."
  (let* ((default-directory (elpaa--dirname dir))
         (generated-files
          (directory-files "." nil
                           "-\\(pkg\\|autoloads\\)\\.el\\'\\|\\.elc\\'")))
    (mapc #'delete-file generated-files)
    (when (file-exists-p ".git")
      (with-temp-buffer
        (elpaa--call t "git" "status" "--porcelain")
        (unless (zerop (buffer-size))
          (elpaa--call t "git" "add" ".")
          (if (zerop
               (elpaa--call t "git" "stash" "push" "-m"
                            "Saved changes while building tarball"))
              (elpaa--temp-file
               (lambda ()
                 (with-temp-buffer
                   (let* ((default-directory (elpaa--dirname dir)))
                     (elpaa--call t "git" "stash" "apply")
                     (elpaa--call t "git" "stash" "drop"))
                   (elpaa--message "%s" (buffer-string)))))
            (error "Can't stash pending changes!:\n%s" (buffer-string))))
        (elpaa--call t "git" "clean" "-x" "-d" "-f")
        (elpaa--temp-file
         (lambda ()
           (with-temp-buffer
             (let* ((default-directory (elpaa--dirname dir)))
               (elpaa--call t "git" "clean" "-x" "-d" "-f")
               (elpaa--call t "git" "checkout" "--" "."))
             (elpaa--message "%s" (buffer-string)))))))))

(defun elpaa--make-one-tarball ( tarball dir pkg-spec metadata
                                 &optional revision-function)
  "Create file TARBALL for PKGNAME if not done yet.
Return non-nil if a new tarball was created."
  (elpaa--message "Building tarball %s..." tarball)
  (if (or (file-readable-p tarball)
          (file-readable-p (replace-regexp-in-string
                            "\\.tar\\'" ".el" tarball)))
      (progn
        (elpaa--message "Tarball %s already built!" tarball)
        nil)
    (elpaa--with-temp-files
        dir
      (let* ((destdir (file-name-directory tarball))
             (pkgname (car pkg-spec))
             (_ (unless (file-directory-p destdir) (make-directory destdir)))
             (vers (nth 1 metadata))
             (elpaignore (expand-file-name ".elpaignore" dir))
             (ignores (elpaa--spec-get pkg-spec :ignored-files))
             (renames (elpaa--spec-get pkg-spec :renames))
             (re (concat "\\`" (regexp-quote pkgname)
                         "-\\([0-9].*\\)\\.\\(tar\\|el\\)\\(\\.[a-z]*z\\)?\\'"))
             (oldtarballs
              (mapcar
               (lambda (file)
                 (string-match re file)
                 (cons (match-string 1 file) file))
               (directory-files destdir nil re))))
        (when revision-function
          (elpaa--select-revision dir pkg-spec (funcall revision-function)))
        (elpaa--copyright-check pkg-spec)
        ;; FIXME: Build Info files and corresponding `dir' file.
        (elpaa--build-Info pkg-spec dir)
        (elpaa--make pkg-spec dir)
        (elpaa--write-pkg-file dir pkgname metadata)
        ;; FIXME: Allow renaming files or selecting a subset of the files!
        (cl-assert (not (string-match "[][*\\|?]" pkgname)))
        (cl-assert (not (string-match "[][*\\|?]" vers)))
        (apply #'elpaa--call
               nil "tar"
               `("--exclude-vcs"
                 ,@(cond
                    (ignores
                     (mapcar (lambda (i) (format "--exclude=packages/%s/%s" pkgname i))
                             ignores))
                    ((file-readable-p elpaignore) `("-X" ,elpaignore)))
                 ,@(mapcar (lambda (r) (elpaa--make-tar-transform pkgname r)) renames)
                 "--transform"
                 ,(format "s|^packages/%s|%s-%s|" pkgname pkgname vers)
                 "-chf" ,tarball
                 ,(concat "packages/" pkgname)))
        (let* ((pkgdesc
                ;; FIXME: `elpaa--write-pkg-file' wrote the metadata to
                ;; <pkg>-pkg.el and then `elpaa--process-multi-file-package'
                ;; reads it back.  We could/should skip the middle man.
                (elpaa--process-multi-file-package
                 dir pkgname 'dont-rename)))
          (elpaa--message "%s: %S" pkgname pkgdesc)
          (elpaa--update-archive-contents pkgdesc destdir)
          (when (and nil revision-function) ;FIXME: Circumstantial evidence.
            ;; Various problems:
            ;; - If "make build/foo" is used by the developers in order to test
            ;;   the build of their package, they'll end up with those spurious
            ;;   tags which may end up spreading to unintended places.
            ;; - The tags created in elpa.gnu.org won't spread to nongnu.git
            ;;   because that account can't push to git.sv.gnu.org anyway.
            (let ((default-directory (elpaa--dirname dir)))
              (elpaa--call nil "git" "tag" "-f"
                           (format "%s-release/%s-%s"
                                   elpaa--name pkgname vers))))
          (let ((link (expand-file-name (format "%s.tar" pkgname) destdir)))
            (when (file-symlink-p link) (delete-file link))
            (make-symbolic-link (file-name-nondirectory tarball) link))
          (dolist (oldtarball oldtarballs)
            ;; Compress oldtarballs.
            (let ((file (cdr oldtarball)))
              (when (string-match "\\.\\(tar\\|el\\)\\'" file)
                ;; Don't compress the file we just created.
                (unless (equal file (file-name-nondirectory tarball))
                  ;; (elpaa--message "not equal %s and %s" file tarball)
                  (elpaa--call nil "lzip" (expand-file-name file destdir))
                  (setf (cdr oldtarball) (concat file ".lz"))))))
          (let* ((default-directory (expand-file-name destdir)))
            ;; Apparently this also creates the <pkg>-readme.txt file.
            (elpaa--html-make-pkg pkgdesc pkg-spec
                                  `((,vers . ,(file-name-nondirectory tarball))
                                    . ,oldtarballs)
                                  dir))
          (message "Built new package %s!" tarball)
          'new)))))

(defun elpaa--get-devel-version (dir pkg-spec)
  "Compute the date-based pseudo-version used for devel builds."
  (let* ((ftn (file-truename      ;; Follow symlinks!
              (expand-file-name (elpaa--main-file pkg-spec) dir)))
        (default-directory (file-name-directory ftn))
         (gitdate
          (with-temp-buffer
           (if (plist-get (cdr pkg-spec) :core)
               ;; For core packages, don't use the date of the last
               ;; commit to the branch, but that of the last commit
               ;; to the main file.
               (elpaa--call t "git" "log" "--pretty=format:%cI" "--no-patch"
                            "-1" "--" (file-name-nondirectory ftn))
             (elpaa--call t "git" "show" "--pretty=format:%cI" "--no-patch"))
            (buffer-string)))
         (verdate
          ;; Convert Git's date into something that looks like a version number.
          ;; While we're at it, convert Git's date into its UTC equivalent,
          ;; to try and make sure time-versions are monotone.
          (let ((process-environment (cons "TZ=UTC" process-environment)))
            (with-temp-buffer
              (elpaa--call t "date" "-d" gitdate "+%Y%m%d.%H%M%S")
              (buffer-string)))))
    ;; Get rid of leading zeros since ELPA's version numbers don't allow them.
    (replace-regexp-in-string "\\(?:\\`\\|[^0-9]\\)0+" "\\1"
                              ;; Remove trailing newline or anything untoward.
                              (replace-regexp-in-string "[^.0-9]+" ""
                                                        verdate))))

(defun elpaa--get-package-spec (pkgname)
  "Retrieve the property list for PKGNAME from `externals-list'."
  (let* ((specs (elpaa--get-specs))
         (spec (assoc pkgname specs)))
    (if (null spec)
        (error "Unknown package `%S`" pkgname)
      spec)))

(defun elpaa-batch-make-all-packages (&rest _)
  "Check all the packages and build the relevant new tarballs."
  (elpaa-read-config)
  (let* ((specs (elpaa--get-specs)))
    (dolist (spec specs)
      (condition-case err
          (elpaa--make-one-package spec)
        (error (message "Build error for %s: %S" (car spec) err))))))

(defun elpaa-batch-make-one-package (&rest _)
  "Build the new tarballs (if needed) for one particular package."
  (elpaa-read-config)
  (while command-line-args-left
    (elpaa--make-one-package (elpaa--get-package-spec
                                (pop command-line-args-left)))))

(defun elpaa--make-one-package (pkg-spec)
  "Build the new tarballs (if needed) for PKG-SPEC."
  (elpaa--message "Checking package %s for updates..." (car pkg-spec))
  (let* ((pkgname (car pkg-spec))
         (dir (expand-file-name pkgname "packages"))
         (_ (if (eq (nth 1 pkg-spec) :core)
                (elpaa--core-package-sync pkg-spec)
              (elpaa--external-package-sync pkg-spec)))
         (_ (elpaa--message "pkg-spec for %s: %S" pkgname pkg-spec))
         (metadata (elpaa--metadata dir pkg-spec))
         (vers (nth 1 metadata)))
    (elpaa--message "metadata = %S" metadata)
    (if (null metadata)
        (error "No metadata found for package: %s" pkgname)
      ;; Disregard the simple/multi distinction.  This might have been useful
      ;; in a distant past, but nowadays it's just unneeded extra complexity.
      (setf (car metadata) nil)
      ;; First, try and build the devel tarball
      ;; Do it before building the release tarball, because building
      ;; the release tarball may revert to some older commit.
      (let* ((date-version (elpaa--get-devel-version dir pkg-spec))
             ;; Add a ".0." so that when the version number goes from
             ;; NN.MM to NN.MM.1 we don't end up with the devel build
             ;; of NN.MM comparing as more recent than NN.MM.1.
             ;; But be careful to turn "2.3" into "2.3.0.DATE"
             ;; and "2.3b" into "2.3b0.DATE".
             (devel-vers
              (concat vers (if (string-match "[0-9]\\'" vers) ".")
                      "0." date-version))
             (tarball (concat elpaa--devel-subdir
                              (format "%s-%s.tar" pkgname devel-vers)))
             (new
              (let ((elpaa--name (concat elpaa--name "-devel")))
                ;; Build the archive-devel tarball.
                (elpaa--make-one-tarball tarball
                                           dir pkg-spec
                                           `(nil ,devel-vers
                                                 . ,(nthcdr 2 metadata))))))

        ;; Try and build the latest release tarball.
        (cond
         ((or (equal vers "0")
              ;; -4 is used for "NN.MMsnapshot" and "NN.MM-git"
              (member '-4 (version-to-list vers)))
          (cond
           ((equal vers "0")
            (elpaa--message "Package %s not released yet!" pkgname))
           ((not new)
            (elpaa--message "Nothing new for package %s!" pkgname))
           (t
            ;; If this revision is a snapshot, check to see if there's
            ;; a previous non-snapshot revision and build it if needed.
            (let* ((last-rel (elpaa--get-last-release pkg-spec))
                   (tarball (concat elpaa--release-subdir
                                    (format "%s-%s.tar"
                                            pkgname (car last-rel))))
                   (metadata `(nil ,(car last-rel) . ,(nthcdr 2 metadata))))
              (if (not last-rel)
                  (elpaa--message "Package %s not released yet!" pkgname)
                (when (elpaa--make-one-tarball
                       tarball dir pkg-spec metadata
                       (lambda () (cdr last-rel)))
                  (elpaa--release-email pkg-spec metadata dir)))))))
         (t
          (let ((tarball (concat elpaa--release-subdir
                                 (format "%s-%s.tar" pkgname vers))))
            (when (elpaa--make-one-tarball
                   tarball dir pkg-spec metadata
                   (lambda ()
                     (elpaa--get-release-revision
                      dir pkg-spec vers
                      (plist-get (cdr pkg-spec) :version-map))))
              (elpaa--release-email pkg-spec metadata dir)))))))))

(defun elpaa--call (destination program &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (elpaa--message "call-process %s %S" program args)
  (apply #'call-process program nil destination nil args))

(defconst elpaa--bwrap-args
  '("--unshare-all"
    "--ro-bind" "/lib" "/lib"
    "--ro-bind" "/lib64" "/lib64"
    "--ro-bind" "/usr" "/usr"
    "--ro-bind" "/etc/alternatives" "/etc/alternatives"
    "--ro-bind" "/etc/emacs" "/etc/emacs"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"))

(defun elpaa--call-sandboxed (destination &rest args)
  "Like ‘elpaa--call’ but sandboxed.
More specifically, uses Bubblewrap such that the command is
confined to write to the  is writable.
Signal an error if the command did not finish with exit code 0."
  (if (not elpaa--sandbox)
      (apply #'elpaa--call destination args)
    (elpaa--message "call-sandboxed %S" args)
    (let ((exitcode
           (apply #'elpaa--call destination "bwrap"
                  (append elpaa--bwrap-args
                          `("--bind" ,default-directory ,default-directory)
                          args))))
      (unless (eq exitcode 0)
        (if (eq destination t)
            (error "Error-indicating exit code in elpaa--call-sandboxed:\n%s"
                   (buffer-string))
          (error "Error-indicating exit code in elpaa--call-sandboxed"))))))

(defun elpaa--default-url (pkgname) (concat elpaa--url pkgname ".html"))
(defun elpaa--default-url-re () (elpaa--default-url ".*"))


(defun elpaa--override-version (pkg-spec orig-fun header)
  (let ((str (funcall orig-fun header)))
    (or (if (or (equal header "version")
                (and str (equal header "package-version")))
            (let ((version-map (plist-get (cdr pkg-spec) :version-map))
                  (dont-release (plist-get (cdr pkg-spec) :dont-release)))
              (or (cadr (assoc str version-map))
                  (and str dont-release
                       (string-match dont-release str)
                       (replace-match "snapshot" t t str)))))
        str)))

;; Some packages use version numbers which `version-to-list' doesn't
;; recognize out of the box.  So here we help.

(add-to-list 'version-regexp-alist '("^[-.+ ]*beta-?$" . -2)) ;"1.0.0-beta-3"
(add-to-list 'version-regexp-alist '("^[-.+ ]*dev$" . -4))    ;2.5-dev

(defun elpaa--metadata (dir pkg-spec)
  "Return a list (SIMPLE VERSION DESCRIPTION REQ EXTRAS),
where SIMPLE is non-nil if the package is simple;
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((pkg (car pkg-spec))
         (mainfile (expand-file-name (elpaa--main-file pkg-spec) dir))
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
                      (when (or (plist-get (cdr pkg-spec) :version-map)
                                (plist-get (cdr pkg-spec) :dont-release))
                        (advice-add 'lm-header :around
                                    (apply-partially
                                     #'elpaa--override-version
                                     pkg-spec)))
                      (package-buffer-info))
                  (advice-remove 'lm-header
                                 #'elpaa--override-version)))
               (extras (package-desc-extras pkg-desc))
               (version (package-desc-version pkg-desc))
               (keywords (lm-keywords-list))
               ;; (_ (elpaa--version-to-list version)) ; Sanity check!
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
            (push (cons :url (elpaa--default-url pkg)) extras))
          (list simple
		(package-version-join version)
		(package-desc-summary pkg-desc)
                (package-desc-reqs pkg-desc)
                extras))))
     (t
      (error "Can't find main file %s file in %s" mainfile dir)))))

(defun elpaa--alist-to-plist-args (alist)
  (mapcar (lambda (x)
            (if (and (not (consp x))
                     (or (keywordp x)
                         (not (symbolp x))
                         (memq x '(nil t))))
                x `',x))
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(defun elpaa--plist-args-to-alist (plist)
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

(defun elpaa--process-multi-file-package (dir pkg &optional dont-rename)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and return the descriptor."
  (let* ((exp (elpaa--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
         (req-exp (nth 4 exp))
	 (req (mapcar #'elpaa--convert-require
                      (if (eq 'quote (car-safe req-exp)) (nth 1 req-exp)
                        (when req-exp
                          (error "REQ should be a quoted constant: %S"
                                 req-exp)))))
         (extras (elpaa--plist-args-to-alist (nthcdr 5 exp))))
    (unless (equal (nth 1 exp) pkg)
      (error (format "Package name %s doesn't match file name %s"
		     (nth 1 exp) pkg)))
    (unless dont-rename (rename-file dir (concat pkg "-" vers)))
    (cons (intern pkg) (vector (elpaa--version-to-list vers)
                               req (nth 3 exp) 'tar extras))))

(defun elpaa--form-from-file-contents (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    ;; This is unnecessary because ‘with-temp-buffer’ generates a new
    ;; (empty) buffer, and ‘insert-file-contents’ inserts after point.
    ;; In other words, point is alraedy at bob.
    ;;- (goto-char (point-min))
    (read (current-buffer))))

(defun elpaa--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (unless (file-exists-p pkg-file)
      (error "File not found: %s" pkg-file))
    (elpaa--form-from-file-contents pkg-file)))

(defun elpaa--write-pkg-file (pkg-dir name metadata)
  ;; FIXME: Use package-generate-description-file!
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (elpaa--temp-file pkg-file)
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
                 (elpaa--alist-to-plist-args extras))))
	     "\n")
     nil
     pkg-file)))

(defun elpaa-batch-generate-description-file (&rest _)
  "(Re)build the <PKG>-pkg.el file for particular packages."
  (while command-line-args-left
    (let* ((file (pop command-line-args-left))
           (dir (file-name-directory file))
           (pkg (file-name-nondirectory (directory-file-name dir)))
           (pkg-spec (elpaa--get-package-spec pkg)))
      (elpaa--write-pkg-file dir pkg
                               (elpaa--metadata dir pkg-spec)))))

;;; Make the HTML pages for online browsing.

(defun elpaa--html-header (title &optional header)
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

(defun elpaa--html-bytes-format (bytes) ;Aka memory-usage-format.
  (if (null bytes)
      "??KiB"
    (setq bytes (/ bytes 1024.0))
    (let ((units '("KiB" "MiB" "GiB" "TiB")))
      (while (>= bytes 1024)
        (setq bytes (/ bytes 1024.0))
        (setq units (cdr units)))
      (cond
       ((>= bytes 100) (format "%4.0f&nbsp;%s" bytes (car units)))
       ((>= bytes 10) (format "%4.1f&nbsp;%s" bytes (car units)))
       (t (format "%4.2f&nbsp;%s" bytes (car units)))))))

(defun elpaa--get-prop (prop name srcdir mainsrcfile)
  (let ((kprop (intern (format ":%s" (downcase prop)))))
    (or
     (let ((pkgdescfile (expand-file-name (format "%s-pkg.el" name)
                                          srcdir)))
       (when (file-readable-p pkgdescfile)
         (let* ((desc (elpaa--form-from-file-contents pkgdescfile))
                (val-exp (plist-get (cdr desc) kprop)))
           (if (eq 'quote (car-safe val-exp))
               (cadr val-exp)
             val-exp))))
     (when (file-readable-p mainsrcfile)
       (with-temp-buffer
         (insert-file-contents mainsrcfile)
         (lm-header prop))))))

(defun elpaa--get-section (hsection fsection srcdir pkg-spec)
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
   ((file-readable-p (elpaa--main-file pkg-spec))
    (with-temp-buffer
      (insert-file-contents (elpaa--main-file pkg-spec))
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

(defun elpaa--get-README (pkg-spec dir)
  (elpaa--get-section
   "Commentary" (elpaa--spec-get pkg-spec :readme
                                 '("README" "README.rst"
                                   ;; Most README.md files seem to be currently
                                   ;; worse than the Commentary: section :-(
                                   ;; "README.md"
                                   "README.org"))
   dir pkg-spec))

(defun elpaa--get-NEWS (pkg-spec dir)
  (let ((text
         (elpaa--get-section
          "News" (elpaa--spec-get pkg-spec :news
                                  '("NEWS" "NEWS.rst" "NEWS.md" "NEWS.org"))
          dir pkg-spec)))
    (if (< (length text) 4000)
        text
      (concat (substring text 0 4000) "...\n...\n"))))


(defun elpaa--html-quote (txt)
  (replace-regexp-in-string "<" "&lt;"
                            (replace-regexp-in-string "&" "&amp;" txt)))

(defun elpaa--insert-repolinks (pkg-spec url)
  (when url
    (insert (format "<dt>Home page</dt> <dd><a href=%S>%s</a></dd>\n"
                    url (elpaa--html-quote url)))
    (when (string-match (elpaa--default-url-re) url)
      (setq url nil)))
  (let* ((git-sv "http://git.savannah.gnu.org/")
         (urls
          (if (eq (nth 1 pkg-spec) :core)
              (let* ((files (nth 2 pkg-spec))
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
            (mapcar (lambda (s)
                      (format s elpaa--gitrepo
                              elpaa--branch-prefix
                              (car pkg-spec)))
                    '("cgit/%s/?h=%s%s"
                      "gitweb/?p=%s;a=shortlog;h=refs/heads/%s%s")))))
    (insert (format
             (concat (format "<dt>Browse %srepository</dt> <dd>" (if url "ELPA's " ""))
                     "<a href=%S>%s</a> or <a href=%S>%s</a></dd>\n")
             (concat git-sv (nth 0 urls))
             'CGit
             (concat git-sv (nth 1 urls))
             'Gitweb))))

(defun elpaa--html-make-pkg (pkg pkg-spec files srcdir)
  (let* ((name (symbol-name (car pkg)))
         (latest (package-version-join (aref (cdr pkg) 0)))
         (mainsrcfile (expand-file-name (elpaa--main-file pkg-spec) srcdir))
         (desc (aref (cdr pkg) 2)))
    (cl-assert (equal name (car pkg-spec)))
    (with-temp-buffer
      (insert (elpaa--html-header
               (format "%s ELPA - %s" elpaa--name name)
               (format "<a href=\"index.html\">%s ELPA</a> - %s"
                       elpaa--name name)))
      (insert (format "<h2 class=\"package\">%s</h2>" name))
      (insert "<dl>")
      (insert (format "<dt>Description</dt><dd>%s</dd>\n" (elpaa--html-quote desc)))
      (if (zerop (length latest))
          (insert "<dd>This package "
                  (if files (concat "is not in " elpaa--name " ELPA any more")
                    "has not been released yet")
                  ".</dd>\n")
        (let* ((file (cdr (assoc latest files)))
               (attrs (file-attributes file)))
          (insert (format "<dt>Latest</dt> <dd><a href=%S>%s</a>, %s, %s</dd>\n"
                          file (elpaa--html-quote file)
                          (format-time-string "%Y-%b-%d" (nth 5 attrs))
                          (elpaa--html-bytes-format (nth 7 attrs))))))
      (let ((maint (elpaa--get-prop "Maintainer" name srcdir mainsrcfile)))
        (when maint
          (when (consp maint)
            (elpaa--message "maint=%S" maint)
            (setq maint (concat (if (car maint) (concat (car maint) " "))
                                "<" (cdr maint) ">")))
          (insert (format "<dt>Maintainer</dt> <dd>%s</dd>\n" (elpaa--html-quote maint)))))
      (elpaa--insert-repolinks
       pkg-spec
       (or (cdr (assoc :url (aref (cdr pkg) 4)))
           (elpaa--get-prop "URL" name srcdir mainsrcfile)))
      (insert "</dl>")
      (insert (format "<p>To install this package, run in Emacs:</p>
                       <pre>M-x <span class=\"kw\">package-install</span> RET <span class=\"kw\">%s</span> RET</pre>"
                      name))
      (let ((rm (elpaa--get-README pkg-spec srcdir)))
        (when rm
          (write-region rm nil (concat name "-readme.txt"))
          (insert "<h2>Full description</h2><pre>\n" (elpaa--html-quote rm)
                  "\n</pre>\n")))
      ;; (message "latest=%S; files=%S" latest files)
      (unless (< (length files) (if (zerop (length latest)) 1 2))
        (insert (format "<h2>Old versions</h2><table>\n"))
        (dolist (file
                 (sort files (lambda (f1 f2) (version< (car f2) (car f1)))))
          (unless (equal (pop file) latest)
            (let ((attrs (file-attributes file)))
              (insert (format "<tr><td><a href=%S>%s</a></td><td>%s</td><td>%s</td>\n"
                              file (elpaa--html-quote file)
                              (format-time-string "%Y-%b-%d" (nth 5 attrs))
                              (elpaa--html-bytes-format (nth 7 attrs)))))))
        (insert "</table>\n"))
      (let ((news (elpaa--get-NEWS pkg-spec srcdir)))
        (when news
          (insert "<h2>News</h2><pre>\n" (elpaa--html-quote news) "\n</pre>\n")))
      (insert "</body>\n")
      (write-region (point-min) (point-max) (concat name ".html")))))

(defun elpaa--html-make-index (pkgs)
  (with-temp-buffer
    (insert (elpaa--html-header (concat elpaa--name " ELPA Packages")))
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

(defun elpaa--pull (dirname)
  (let ((default-directory (elpaa--dirname dirname)))
    (with-temp-buffer
      ;; Undo any local changes to `<pkg>-pkg.el', in case it's under
      ;; version control.
      (let ((elpaa--debug nil))
        (elpaa--call t "git" "checkout" "--"
                     (concat (file-name-nondirectory dirname) "-pkg.el")))
      (erase-buffer)              ;Throw away the error message we usually get.
      (cond
       ((file-directory-p ".git")
        (message "Running git pull in %S" default-directory)
        (elpaa--call t "git" "pull"))
       ((file-exists-p ".git")
        (if (with-temp-buffer
              (let ((elpaa--debug nil))
                (elpaa--call t "git" "status" "--branch" "--porcelain=2"))
              (goto-char (point-min))
              ;; Nothing to pull (nor push, actually).
              (search-forward "\n# branch.ab +0 -0" nil t))
            (elpaa--message "%s up-to-date" dirname)
          (message "Updating worktree in %S" default-directory)
          (elpaa--call t "git" "merge")))
       (t (error "No .git in %S" default-directory)))
      (unless (and (eobp) (bobp))
        (message "Updated %s:%s%s" dirname
                 (if (and (eobp) (bolp)
                          (eq (line-beginning-position 0) (point-min)))
                     " " "\n")
                 (buffer-string))))))

;;; Maintain external packages.

(defun elpaa--sync-emacs-repo ()
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
      (elpaa--pull emacs-repo-root)
      t)))

(defun elpaa--find-non-trivial-file (dir)
  (catch 'found-important-file
    (dolist (file (directory-files-recursively dir ".*"))
      (unless (or (member file '("." ".."))
                  (string-match "\\.elc\\'" file)
                  (string-match "-autoloads.el\\'" file)
                  (string-match "-pkg.el\\'" file)
                  (file-symlink-p file))
        (throw 'found-important-file file)))
    nil))

(defun elpaa--cleanup-packages (externals-list with-core)
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
            (elpaa--pull dir)))
         ((or (not (file-directory-p dir)) )
          ;; We only add/remove plain directories in elpa/packages (not
          ;; symlinks).
          nil)
         ((member dir '("." "..")) nil)
         ((assoc dir externals-list) nil)
         ((file-directory-p (expand-file-name (format "%s/.git" dir)))
          (let ((status
                 (with-temp-buffer
                   (let ((default-directory (elpaa--dirname dir)))
                     (elpaa--call t "git" "status" "--porcelain")
                     (buffer-string)))))
            (if (zerop (length status))
                (progn (delete-directory dir 'recursive t)
                       (message "Deleted all of %s" dir))
              (message "Keeping leftover unclean %s:\n%s" dir status))))
         ;; Check if `dir' is under version control.
         ((and with-core
               (not (zerop (elpaa--call nil "git" "ls-files"
                                         "--error-unmatch" dir))))
          ;; Not under version control.  Check if it only contains
          ;; symlinks and generated files, in which case it is probably
          ;; a leftover :core package that can safely be deleted.
          ;; (let ((file (elpaa--find-non-trivial-file dir)))
          ;;   (if file
          ;;       (message "Keeping %s for non-trivial file \"%s\"" dir file)
          ;;     (progn
          ;;       (message "Deleted untracked package %s" dir)
          ;;       (delete-directory dir 'recursive t))))
          ))))))


(defun elpaa--external-package-sync (pkg-spec)
  "Sync external package named PKG-SPEC."
  (let ((name (car pkg-spec))
        (default-directory (expand-file-name "packages/")))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (cond ((not (file-exists-p name))
           (let* ((branch (concat elpaa--branch-prefix name))
                  (add-branches
                   (lambda ()
                     (let ((pos (point)))
                       (elpaa--call t "git" "config"
                                    "--get-all" "remote.origin.fetch")
                       (unless (or (= (point) pos)
                                   (re-search-backward "\\*$" pos t))
                         (elpaa--call t "git" "remote" "set-branches"
                                      "--add" "origin" branch)
                         (when (elpaa--spec-get pkg-spec :release-branch)
                           (elpaa--call t "git" "remote" "set-branches"
                                        "--add" "origin"
                                        (concat elpaa--release-branch-prefix
                                                name)))
                         (elpaa--call t "git" "fetch" "origin")))))

                  (output
                   (with-temp-buffer
                     (cond
                      ((or (elpaa--git-branch-p (elpaa--ortb pkg-spec))
                           (progn
                             (funcall add-branches)
                             (elpaa--git-branch-p (elpaa--ortb pkg-spec))))
                       (elpaa--call t "git" "worktree" "add"
                                      "-B" branch
                                      name (elpaa--ortb pkg-spec)))
                      ((elpaa--git-branch-p branch)
                       (elpaa--call t "git" "worktree" "add" name branch))
                      ((elpaa--git-branch-p (elpaa--urtb pkg-spec))
                       (elpaa--call t "git" "worktree" "add"
                                      "-B" branch "--no-track"
                                      name (elpaa--urtb pkg-spec)))
                      (t
                       (error "No branch %s for the worktree of %s:\n%s"
                              branch name (buffer-string))))
                     (buffer-string))))
             (message "Cloning branch %s:\n%s" name output)))
          ((not (file-exists-p (concat name "/.git")))
           (message "%s is in the way of an external, please remove!" name))
          (t (elpaa--pull name)))))

(defun elpaa--core-package-empty-dest-p (dest)
  "Return non-nil if DEST is an empty variant."
  (member dest (list "" "." nil)))

(defun elpaa--core-package-link-file
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
    (message "  %s -> %s" source (if (elpaa--core-package-empty-dest-p dest)
                                     (file-name-nondirectory source)
                                   dest))))

(defun elpaa--core-package-link-directory
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
                (if (elpaa--core-package-empty-dest-p dest)
                    ;; Link to root with its original filename.
                    source-sans-base
                  (concat
                   ;; Prepend the destination, allowing for directory rename.
                   (file-name-as-directory dest) source-sans-base))))
          (elpaa--core-package-link-file
           source package-file-name
           emacs-repo-root package-root exclude-regexp))))))

(defun elpaa--core-package-sync (definition)
  "Sync core package from DEFINITION."
  (pcase-let*
      ((`(,name . (:core ,file-patterns :excludes ,excludes)) definition)
       (emacs-repo-root (expand-file-name "emacs"))
       (package-root (elpaa--dirname name "packages"))
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
            (elpaa--core-package-link-directory
             file dest emacs-repo-root package-root exclude-regexp)
          (elpaa--core-package-link-file
           file dest emacs-repo-root package-root exclude-regexp))))))

(defun elpaa-add/remove/update-externals ()
  "Remove non-package directories and fetch external packages."
  (elpaa-read-config)
  (let ((command-line-args-left '("-")))
    (elpaa-batch-archive-update-worktrees)))

(defun elpaa-batch-archive-update-worktrees (&rest _)
  (elpaa-read-config)
  (let ((specs (elpaa--get-specs))
        (pkgs command-line-args-left)
        (with-core (elpaa--sync-emacs-repo)))
    (setq command-line-args-left nil)
    (if (equal pkgs '("-")) (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (assoc pkg specs))
             (kind (nth 1 pkg-spec)))
        (pcase kind
          (`:external (elpaa--external-package-sync pkg-spec))
          (`:core (when with-core (elpaa--core-package-sync pkg-spec)))
          (_ (if pkg-spec
                 (message "Unknown package kind `%S' for %s" kind pkg)
               (message "Unknown package %s" pkg))))))))

;;; Check copyrights

(defun elpaa--copyright-files (pkg-spec)
  "Return the list of ELisp files in the package PKG-SPEC."
  (let* ((pkgname (car pkg-spec))
         (default-directory (elpaa--dirname pkgname "packages"))
         (ignores (elpaa--spec-get pkg-spec :ignored-files))
         (all-ignores '("." ".." ".git" "test" ".dir-locals.el"))
         (dir-files (lambda (d)
                      (cl-set-difference (directory-files d)
                                         all-ignores :test #'equal)))
         (pending (cl-set-difference
                   (funcall dir-files ".")
                   (list (concat pkgname "-pkg.el")
                         (concat pkgname "-autoloads.el"))
                   :test #'equal))
         (files '()))
    (while pending
      (pcase (pop pending)
        ((pred (lambda (f) (member f ignores))))
        ((and d (guard (and (file-directory-p d) (not (file-symlink-p d)))))
         (setq pending (nconc (mapcar (lambda (f) (concat d "/" f))
                                      (funcall dir-files d))
                              pending)))
        ((and (pred (string-match "\\.el\\'")) f)
         (push f files))))
    files))

(defun elpaa--copyright-collect (pkg-spec)
  ;; This is crude but is only meant to catch the all too common mistakes where
  ;; we forget to update the copyright information after transferring the
  ;; copyright to the FSF.
  (with-temp-buffer
    (let* ((pkgname (car pkg-spec))
           (elpaa--debug nil)
           (files (mapcar (lambda (f) (concat pkgname "/" f))
                          (elpaa--copyright-files pkg-spec)))
           (default-directory (elpaa--dirname "packages")))
      ;; Look for ELisp files which omit a copyright line for the FSF.
      (apply #'elpaa--call t "grep" "-L" "Free Software Foundation, Inc" files)
      ;; Look for *other* lines attributing copyright to someone else.
      (dolist (file files)
        (elpaa--call t "sed" "-n"
                     "-e" "/[Cc]opyright.*, *[1-9][-0-9]*,\\?$/N"
                     "-e" "/Free Software Foundation/d"
                     ;; FIXME: This tends to suffer from misc false positives.
                     "-e" (format "s|^\\(.*;.*[Cc]opyright\\)|%s:\\1|p"
                                  (replace-regexp-in-string "|" "_" file))
                     file)))
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defun elpaa--copyright-filter (collected)
  (let ((res '()))
    (with-current-buffer (find-file-noselect elpaa--copyright-file)
      (dolist (line (split-string collected "\n" t))
        (goto-char (point-min))
        (unless (re-search-forward (concat "^" (regexp-quote line) "$") nil t)
          (push line res))))
    res))

(defun elpaa--copyright-check (pkg-spec)
  "Check the copyright notices, if applicable."
  (when (file-readable-p elpaa--copyright-file)
    (let* ((collected (elpaa--copyright-collect pkg-spec))
           (filtered (elpaa--copyright-filter collected)))
      (when filtered
        (message "Problem with copyright notices:\n%s"
                 (mapconcat (lambda (line)
                              (if (string-match ":" line) line
                                (concat "Missing copyright notice in " line)))
                            filtered "\n"))
        (error "Abort")))))

(defun elpaa-batch-copyright-check (&rest _)
  (elpaa-read-config)
  (let ((specs (elpaa--get-specs))
        (pkgs command-line-args-left))
    (setq command-line-args-left nil)
    (when (equal pkgs '("-"))
      (setq pkgs (delq nil (mapcar (lambda (spec)
                                     (when (file-directory-p
                                            (elpaa--dirname (car spec)
                                                            "packages"))
                                       (car spec)))
                                   specs))))
    (dolist (pkg pkgs)
      (ignore-error 'error
        (elpaa--copyright-check (assoc pkg specs))))))

;;; Announcement emails

(defun elpaa--release-email (pkg-spec metadata dir)
  (when elpaa--email-to
    (with-temp-buffer
      (message-mode)
      (let* ((version (nth 1 metadata))
             (pkgname (car pkg-spec))
             (name (capitalize pkgname)))
        (message-setup `((From    . ,elpaa--email-from)
                         (To      . ,elpaa--email-to)
                         (Subject . ,(format "[%s ELPA] %s version %s"
                                             elpaa--name name version))
                         ,@(if elpaa--email-reply-to
                               `((Reply-To . ,elpaa--email-reply-to)))))
        (insert "Version " version
                " of package " name
                " has just been released in " elpaa--name " ELPA.
You can now find it in M-x package-list RET.

" name " describes itself as:
  " (nth 2 metadata) "

More at " (elpaa--default-url pkgname))
        (let ((news (elpaa--get-NEWS pkg-spec dir)))
          (when news
            (insert "\n\nRecent NEWS:\n\n" news)))
        ;; (pop-to-buffer (current-buffer)) (debug t)
        (message-send)
        ))))

;;; Build Info files from Texinfo

(defun elpaa--build-Info (pkg-spec dir)
  (let* ((default-directory (elpaa--dirname dir))
         (docfile (elpaa--spec-get pkg-spec :doc)))
    (when (and docfile (file-readable-p docfile)
               (string-match "\\.org\\'" docfile))
      (with-temp-buffer
        (elpaa--call-sandboxed
         t "emacs" "--batch" "-l" "ox-texinfo" docfile
         "--eval" "(message \"ELPATEXI=%s\" (org-texinfo-export-to-texinfo))")
        (message "%s" (buffer-string))
        (goto-char (point-max))
        (when (re-search-backward "ELPATEXI=\\(.*\\)\n?" nil t)
          (setq docfile (concat (file-name-directory docfile)
                                (match-string 1)))
          (elpaa--temp-file docfile))))

    (when (and docfile (file-readable-p docfile)
               (string-match "\\.texi\\(nfo\\)?\\'" docfile))
      (let ((info-file (concat
                        (file-name-sans-extension
                         (file-name-nondirectory docfile))
                        ".info")))
        (elpaa--temp-file info-file)
        (with-temp-buffer
          (elpaa--call-sandboxed
           t "makeinfo" "--no-split" docfile "-o" info-file)
          (message "%s" (buffer-string)))
        (setq docfile info-file)))

    (when (and docfile (not (string-match "\\.info\\'" docfile)))
      (error "Not a supported doc format: %s" docfile))

    (when (and docfile (file-readable-p docfile)
               (file-name-directory docfile))
      ;; The built-in support for Info files in package.el only
      ;; works for Info file that are in the top-level directory.
      ;; FIXME: We could just not use it, but then we'd need to do
      ;; something like add a dummy .el file at toplevel with
      ;; an ;;;###autoload cookie which adds the right directory to
      ;; Info-directory-list.  This would have the advantage that
      ;;   emacs -l .../<pkg>-autoloads.el
      ;; would properly setup the Info reader, tho!
      (let ((info-file (file-name-nondirectory docfile)))
        (elpaa--temp-file info-file)
        (copy-file docfile info-file)
        (setq docfile info-file)))

    (when (and docfile (file-readable-p docfile))
      (let ((dir-file (expand-file-name "dir")))
        (elpaa--temp-file dir-file)
        (with-temp-buffer
          (elpaa--call-sandboxed
           t "install-info" (concat "--dir=" dir-file) docfile)
          (message "%s" (buffer-string)))))))

(defun elpaa--make (pkg-spec dir)
  (let ((target (elpaa--spec-get pkg-spec :make)))
    (when target
      (with-temp-buffer
        (let ((default-directory (elpaa--dirname dir)))
          (elpaa--call-sandboxed t "make" target)
          (elpaa--message "%s" (buffer-string)))))))

;;; Fetch updates from upstream

(defun elpaa--branch (pkg-spec)
  (elpaa--spec-get pkg-spec :branch "master"))

(defun elpaa--urtb (pkg-spec &optional branch)
  "Return our upstream remote tracking branch for PKG-SPEC."
  (format "refs/remotes/upstream/%s/%s" (car pkg-spec)
          (or branch (elpaa--branch pkg-spec))))

(defun elpaa--ortb (pkg-spec)
  "Return our origin remote tracking branch for PKG-SPEC."
  ;; We can't use the shorthand "origin/%s%s" when we pass it to
  ;; `git-show-ref'.
  (format "refs/remotes/origin/%s%s" elpaa--branch-prefix (car pkg-spec)))

(defun elpaa--git-branch-p (branch)
  "Return non-nil iff BRANCH is an existing branch."
  (equal 0 (elpaa--call t "git" "show-ref" "--verify" "--quiet" branch)))

(defun elpaa--fetch (pkg-spec &optional k)
  (let* ((pkg (car pkg-spec))
         (url (elpaa--spec-get pkg-spec :external))
         (branch (elpaa--branch pkg-spec))
         (release-branch (elpaa--spec-get pkg-spec :release-branch))
         (urtb (elpaa--urtb pkg-spec))
         (refspec (format "refs/heads/%s:%s" branch urtb))
         (release-refspec (if release-branch
                              (format "refs/heads/%s:%s"
                                      release-branch
                                      (elpaa--urtb pkg-spec release-branch)))))
    (if (not url)
        (message "Missing upstream URL in externals-list for %s" pkg)
      (message "Fetching updates for %s..." pkg)
      (with-temp-buffer
        (cond
         ((not (equal 0 (apply #'elpaa--call
                               t "git" "fetch" "--no-tags"
                               url refspec
                               (if release-refspec
                                   (list release-refspec)))))
          (message "Fetch error for %s:\n%s" pkg (buffer-string)))
         ((let* ((ortb (elpaa--ortb pkg-spec))
                 (exists (elpaa--git-branch-p ortb)))
            (not (equal 0 (elpaa--call t "git" "log"
                                         (if exists
                                             (format "%s...%s" ortb urtb)
                                           urtb)))))
          (message "Log error for %s:\n%s" pkg (buffer-string)))
         ((eq (point-min) (point-max))
          (message "No pending upstream changes for %s" pkg))
         (t (message "%s" (buffer-string))
            (when k (funcall k pkg-spec))))))))

(defun elpaa--push (pkg-spec)
  (let* ((pkg (car pkg-spec))
         ;; (url (plist-get (cdr pkg-spec) :external))
         ;; (branch (elpaa--branch pkg-spec))
         (release-branch (elpaa--spec-get pkg-spec :release-branch))
         (ortb (elpaa--ortb pkg-spec))
         (urtb (elpaa--urtb pkg-spec)))
    ;; FIXME: Arrange to merge if it's not a fast-forward.
    (with-temp-buffer
      (cond
       ((zerop (elpaa--call t "git" "merge-base" "--is-ancestor" urtb ortb))
        (message "Nothing to push for %s" pkg))
       ((and
         (not (zerop (elpaa--call t "git" "merge-base" "--is-ancestor"
                                  ortb urtb)))
         (elpaa--git-branch-p ortb))
        (message "Can't push %s: not a fast-forward" pkg))
       ((equal 0 (apply #'elpaa--call
                        t "git" "push" "--set-upstream"
                        "origin"
                        (format "%s:refs/heads/%s%s"
                                urtb elpaa--branch-prefix pkg)
                        (when release-branch
                          (list
                           (format "%s:refs/heads/%s%s"
                                   (elpaa--urtb pkg-spec release-branch)
                                   elpaa--release-branch-prefix pkg)))))
        (message "Pushed %s successfully:\n%s" pkg (buffer-string))
        (elpaa--external-package-sync pkg-spec))
       (t
        (message "Push error for %s:\n%s" pkg (buffer-string)))))))

(defun elpaa--batch-fetch-and (k)
  (let ((specs (elpaa--get-specs))
        (pkgs command-line-args-left))
    (setq command-line-args-left nil)
    (if (equal pkgs '("-")) (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (assoc pkg specs)))
        (if (not pkg-spec) (message "Unknown package: %s" pkg)
          ;; (unless (file-directory-p (expand-file-name pkg "packages"))
          ;;   (elpaa--external-package-sync pkg-spec))
          (elpaa--fetch pkg-spec k))))))

(defun elpaa-batch-fetch-and-show (&rest _)
  (elpaa-read-config)
  (elpaa--batch-fetch-and #'ignore))

(defun elpaa-batch-fetch-and-push (&rest _)
  (elpaa-read-config)
  (elpaa--batch-fetch-and #'elpaa--push))

;;; ERT test support

(defun elpaa-ert-package-install (top-directory package)
  ;; blitz default value and set up from elpa.
  (setq package-archives
        `(("local-elpa"
	   . ,(expand-file-name "packages" top-directory)))
	package-user-dir (make-temp-file "elpa-test" t))
  (package-initialize)
  (package-refresh-contents)
  (package-install package))

(defun elpaa-ert-test-find-tests (package-directory package)
  (append
   `(,(expand-file-name
       (concat (symbol-name package) "-autoloads.el") package-directory))
   (or
    (directory-files package-directory t ".*-test.el$")
    (directory-files package-directory t ".*-tests.el$")
    (let ((dir-test (expand-file-name "test" package-directory)))
      (when (file-directory-p dir-test)
	(directory-files dir-test t directory-files-no-dot-files-regexp)))
    (let ((dir-tests (expand-file-name "tests" package-directory)))
      (when (file-directory-p dir-tests)
	(directory-files dir-tests t directory-files-no-dot-files-regexp))))))

(defun elpaa-ert-load-tests (package-directory package)
  (mapc
   (lambda (file)
     (let ((force-load-messages t))
       (load-file file)))
   (elpaa-ert-test-find-tests package-directory package)))

(defun elpaa-ert-test-package (top-directory package)
  (elpaa-ert-package-install top-directory package)
  (elpaa-ert-load-tests
   (expand-file-name (concat "packages/" (symbol-name package)) top-directory)
   package)

  (ert-run-tests-batch-and-exit t))


(provide 'elpa-admin)
;;; elpa-admin.el ends here
