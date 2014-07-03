;;; archive-contents.el --- Auto-generate an Emacs Lisp package archive.  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2014  Free Software Foundation, Inc

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

(eval-when-compile (require 'cl))
(require 'lisp-mnt)
(require 'package)
(require 'pcase)

(defconst archive-contents-subdirectory-regexp
  "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)")

(defconst archive-re-no-dot "\\`\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regular expression matching all files except \".\" and \"..\".")

(defun archive--version-to-list (vers)
  (when vers
    (let ((l (version-to-list vers)))
      ;; Signal an error for things like "1.02" which is parsed as "1.2".
      (assert (equal vers (package-version-join l)) nil
              "Unsupported version syntax %S" vers)
      l)))

(defun archive--convert-require (elt)
  (list (car elt)
	(archive--version-to-list (car (cdr elt)))))

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
	      (if (file-exists-p autoloads-file)
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
                          (if (nth 1 metadata)
                              (apply #'archive--write-pkg-file
                                     dir pkg (cdr metadata)))
                          (archive--process-multi-file-package dir pkg))
                        packages)))))
	((debug error) (error "Error in %s: %S" dir v))))
    (with-temp-buffer
      (pp (nreverse packages) (current-buffer))
      (write-region nil nil "archive-contents"))))

(defconst archive--revno-re "[0-9a-f]+")

(defun archive-prepare-packages (srcdir)
  "Prepare the `packages' directory inside the Git checkout.
Expects to be called from within the `packages' directory.
\"Prepare\" here is for subsequent construction of the packages and archive,
so it is meant to refresh any generated files we may need.
Currently only refreshes the ChangeLog files."
  (setq srcdir (file-name-as-directory (expand-file-name srcdir)))
  (let* ((wit ".changelog-witness")
         (prevno (with-temp-buffer
                   (insert-file-contents wit)
                   (if (looking-at (concat archive--revno-re "$"))
                       (match-string 0)
                     (error "Can't find previous revision name"))))
         (new-revno
          (or (with-temp-buffer
                (let ((default-directory srcdir))
                  (call-process "git" nil '(t) nil "rev-parse" "HEAD")
                  (goto-char (point-min))
                  (when (looking-at (concat archive--revno-re "$"))
                    (match-string 0))))
              (error "Couldn't find the current revision's name")))
         (pkgs '()))
    (unless (equal prevno new-revno)
      (with-temp-buffer
        (let ((default-directory srcdir))
          (unless (zerop (call-process "git" nil '(t) nil "diff"
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
            (if (file-directory-p pkg)
                (archive--make-changelog pkg (expand-file-name "packages/"
                                                               srcdir)))
          (error (message "Error: %S" v)))))
    (write-region new-revno nil wit nil 'quiet)
    ;; Also update the ChangeLog of external packages.
    (let ((default-directory (expand-file-name "packages/")))
      (dolist (dir (directory-files "."))
        (and (not (member dir '("." "..")))
             (file-directory-p dir)
             (let ((index (expand-file-name
                           (concat "packages/" dir "/.git/index")
                           srcdir))
                   (cl (expand-file-name "ChangeLog" dir)))
               (and (file-exists-p index)
                    (or (not (file-exists-p cl))
                        (file-newer-than-file-p index cl))))
             (archive--make-changelog
              dir (expand-file-name "packages/" srcdir)))))
    ))

(defconst archive-default-url-format "http://elpa.gnu.org/packages/%s.html")
(defconst archive-default-url-re (format archive-default-url-format ".*"))

(defun archive--metadata (dir pkg)
  "Return a list (SIMPLE VERSION DESCRIPTION REQ EXTRAS),
where SIMPLE is non-nil if the package is simple;
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((mainfile (expand-file-name (concat pkg ".el") dir))
         (files (directory-files dir nil "\\.el\\'")))
    (setq files (delete (concat pkg "-pkg.el") files))
    (setq files (delete (concat pkg "-autoloads.el") files))
    (cond
     ((file-exists-p mainfile)
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
	(if (not (looking-at ";;;.*---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$"))
            (error "Can't parse first line of %s" mainfile)
          ;; Grab the other fields, which are not mandatory.
          (let* ((description (match-string 1))
                 (pv )
                 (version
                  (or (lm-header "package-version")
                      (lm-header "version")
                      (unless (equal pkg "org")
                        (error "Missing `version' header"))))
                 (_ (archive--version-to-list version)) ; Sanity check!
                 (requires-str (lm-header "package-requires"))
                 (pt (lm-header "package-type"))
                 (simple (if pt (equal pt "simple") (= (length files) 1)))
                 (keywords (lm-keywords-list))
                 (url (or (lm-header "url")
                          (format archive-default-url-format pkg)))
                 (req
                  (if requires-str
                      (mapcar 'archive--convert-require
                              (car (read-from-string requires-str))))))
            (list simple version description req
                  ;; extra parameters
                  (list (cons :url url)
                        (cons :keywords keywords)))))))
     (t
      (error "Can find main file %s file in %s" mainfile dir)))))

(defun archive--process-simple-package (dir pkg vers desc req extras)
  "Deploy the contents of DIR into the archive as a simple package.
Rename DIR/PKG.el to PKG-VERS.el, delete DIR, and return the descriptor."
  ;; Write DIR/foo.el to foo-VERS.el and delete DIR
  (rename-file (expand-file-name (concat pkg ".el") dir)
	       (concat pkg "-" vers ".el"))
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
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (if (file-readable-p "ChangeLog") (insert-file-contents "ChangeLog"))
        (let ((old-md5 (md5 (current-buffer))))
          (erase-buffer)
          (let ((default-directory
                  (file-name-as-directory (expand-file-name dir srcdir))))
            (call-process "git" nil (current-buffer) nil
                          "log" "--date=short"
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

(defun archive--process-multi-file-package (dir pkg)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and return the descriptor."
  (let* ((exp (archive--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
         (req-exp (nth 4 exp))
	 (req (mapcar 'archive--convert-require
                      (if (eq 'quote (car-safe req-exp)) (nth 1 req-exp)
                        (when req-exp
                          (error "REQ should be a quoted constant: %S"
                                 req-exp)))))
         (extras (archive--plist-args-to-alist (nthcdr 5 exp))))
    (unless (equal (nth 1 exp) pkg)
      (error (format "Package name %s doesn't match file name %s"
		     (nth 1 exp) pkg)))
    (rename-file dir (concat pkg "-" vers))
    (cons (intern pkg) (vector (archive--version-to-list vers)
                               req (nth 3 exp) 'tar extras))))

(defun archive--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (with-temp-buffer
      (unless (file-exists-p pkg-file)
	(error "File not found: %s" pkg-file))
      (insert-file-contents pkg-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun archive--refresh-pkg-file ()
  (let* ((dir (directory-file-name default-directory))
         (pkg (file-name-nondirectory dir)))
    (apply #'archive--write-pkg-file dir pkg
           (cdr (archive--metadata dir pkg)))))

(defun archive--write-pkg-file (pkg-dir name version desc requires extras)
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (write-region
     (concat (format ";; Generated package description from %s.el\n"
		     name)
	     (prin1-to-string
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
               (archive--alist-to-plist-args extras)))
	     "\n")
     nil
     pkg-file)))

;;; Make the HTML pages for online browsing.

(defun archive--html-header (title)
  (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">
<html>
<head>
  <title>%s</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
</head>
<body>
<h1 align=\"center\">%s</h1>\n"
          title title))

(defun archive--html-bytes-format (bytes) ;Aka memory-usage-format.
  (setq bytes (/ bytes 1024.0))
  (let ((units '(;; "B"
                 "kB" "MB" "GB" "TB")))
    (while (>= bytes 1024)
      (setq bytes (/ bytes 1024.0))
      (setq units (cdr units)))
    (cond
     ;; ((integerp bytes) (format "%4d%s" bytes (car units)))
     ((>= bytes 100) (format "%4.0f%s" bytes (car units)))
     ((>= bytes 10) (format "%4.1f%s" bytes (car units)))
     (t (format "%4.2f%s" bytes (car units))))))

(defun archive--get-prop (prop name srcdir mainsrcfile)
  (let ((kprop (intern (format ":%s" (downcase prop)))))
    (or
     (let ((pkgdescfile (expand-file-name (format "%s-pkg.el" name)
                                          srcdir)))
       (when (file-readable-p pkgdescfile)
         (with-temp-buffer
           (insert-file-contents pkgdescfile)
           (let ((desc (read (current-buffer))))
             (plist-get (cdr desc) kprop)))))
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

(defun archive--insert-repolinks (name srcdir mainsrcfile url)
  (when url
    (insert (format "<p>Home page: <a href=%S>%s</a></p>\n"
                    url (archive--quote url)))
    (when (string-match archive-default-url-re url)
      (setq url nil)))
  (let* ((externals
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "../../../elpa/externals-list" srcdir))
            (read (current-buffer))))
         (external (eq :external (nth 1 (assoc name externals))))
         (git-sv "http://git.savannah.gnu.org/")
         (urls (if external
                   '("cgit/emacs/elpa.git/?h=externals/"
                     "gitweb/?p=emacs/elpa.git;a=shortlog;h=refs/heads/externals/")
                 '("cgit/emacs/elpa.git/tree/packages/"
                   "gitweb/?p=emacs/elpa.git;a=tree;f=packages/"))))
    (insert (format
             (concat (format "<p>Browse %srepository: " (if url "ELPA's " ""))
                     "<a href=%S>%s</a> or <a href=%S>%s</a></p>\n")
             (concat git-sv (nth 0 urls) name)
             'CGit
             (concat git-sv (nth 1 urls) name)
             'Gitweb))))

(defun archive--html-make-pkg (pkg files)
  (let* ((name (symbol-name (car pkg)))
         (latest (package-version-join (aref (cdr pkg) 0)))
         (srcdir (expand-file-name name "../../build/packages"))
         (mainsrcfile (expand-file-name (format "%s.el" name) srcdir))
         (desc (aref (cdr pkg) 2)))
    (with-temp-buffer
      (insert (archive--html-header (format "GNU ELPA - %s" name)))
      (insert (format "<p>Description: %s</p>\n" (archive--quote desc)))
      (if (zerop (length latest))
          (insert "<p>This package "
                  (if files "is not in GNU ELPA any more"
                    "has not been released yet")
                  ".</p>\n")
        (let* ((file (cdr (assoc latest files)))
               (attrs (file-attributes file)))
          (insert (format "<p>Latest: <a href=%S>%s</a>, %s, %s</p>\n"
                          file (archive--quote file)
                          (format-time-string "%Y-%b-%d" (nth 5 attrs))
                          (archive--html-bytes-format (nth 7 attrs))))))
      (let ((maint (archive--get-prop "Maintainer" name srcdir mainsrcfile)))
        (when maint
          (insert (format "<p>Maintainer: %s</p>\n" (archive--quote maint)))))
      (archive--insert-repolinks
       name srcdir mainsrcfile
       (or (cdr (assoc :url (aref (cdr pkg) 4)))
           (archive--get-prop "URL" name srcdir mainsrcfile)))
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
        (insert (format "<h2>Old versions</h2><table cellpadding=\"3\" border=\"1\">\n"))
        (dolist (file files)
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
    (insert (archive--html-header "GNU ELPA Packages"))
    (insert "<table cellpadding=\"3\" border=\"1\">\n")
    (insert "<tr><th>Package</th><th>Version</th><th>Description</th></tr>\n")
    (dolist (pkg pkgs)
      (insert (format "<tr><td><a href=\"%s.html\">%s</a></td><td>%s</td><td>%s</td></tr>\n"
                      (car pkg) (car pkg)
                      (package-version-join (aref (cdr pkg) 0))
                      (aref (cdr pkg) 2))))
    (insert "</table></body>\n")
    (write-region (point-min) (point-max) "index.html")))

(defun batch-html-make-index ()
  (let ((packages (make-hash-table :test #'equal))
        (archive-contents
         (with-temp-buffer
           (insert-file-contents "archive-contents")
           (goto-char (point-min))
           ;; Skip the first element which is a version number.
           (cdr (read (current-buffer))))))
    (dolist (subdir (directory-files "../../build/packages" nil))
      (cond
       ((member subdir '("." ".." "elpa.rss" "index.html" "archive-contents")))
       (t (puthash subdir nil packages))))
    (dolist (file (directory-files default-directory nil))
      (cond
       ((member file '("." ".." "elpa.rss" "index.html" "archive-contents")))
       ((string-match "\\.html\\'" file))
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

;;; Maintain external packages.

(defconst archive--elpa-git-url "git://git.sv.gnu.org/emacs/elpa")

(defun archive-add/remove/update-externals ()
  (let ((exts (with-current-buffer (find-file-noselect "externals-list")
                (goto-char (point-min))
                (read (current-buffer)))))
    (let ((default-directory (expand-file-name "packages/")))
      ;; Remove "old/odd" externals.
      (dolist (dir (directory-files "."))
        (cond
         ((member dir '("." "..")) nil)
         ((assoc dir exts) nil)
         ((file-directory-p (expand-file-name (format "%s/.git" dir)))
          (let ((status
                 (with-temp-buffer
                   (let ((default-directory (file-name-as-directory
                                             (expand-file-name dir))))
                     (call-process "git" nil t nil "status" "--porcelain")
                     (buffer-string)))))
            (if (zerop (length status))
                (progn (delete-directory dir 'recursive t)
                       (message "Deleted all of %s" dir))
              (message "Keeping leftover unclean %s:\n%s" dir status))))))
      (pcase-dolist (`(,dir ,kind ,_url) exts)
        (cond
         ((eq kind :subtree) nil)       ;Nothing to do.
         ((not (eq kind :external))
          (message "Unknown external package kind `%S' for %s" kind dir))
         ((not (file-exists-p dir))
          (let* ((branch (concat "externals/" dir))
                 (output
                  (with-temp-buffer
                    ;; FIXME: Use git-new-workdir!
                    (call-process "git" nil t nil "clone"
                                  "--reference" ".." "--single-branch"
                                  "--branch" branch
                                  archive--elpa-git-url dir)
                    (buffer-string))))
            (message "Cloning branch %s:\n%s" dir output)))
         ((not (file-directory-p (concat dir "/.git")))
          (message "%s is in the way of an external, please remove!" dir))
         (t
          (let ((default-directory (file-name-as-directory
                                    (expand-file-name dir))))
            (with-temp-buffer
              (message "Running git pull in %S" default-directory)
              (call-process "git" nil t nil "pull")
              (message "Updated %s:%s" dir (buffer-string))))
          ))))))

(provide 'archive-contents)
;;; archive-contents.el ends here
