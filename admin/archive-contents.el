;;; archive-contents.el --- Auto-generate an Emacs Lisp package archive.  -*- lexical-binding:t -*-

;; Copyright (C) 2011, 2012, 2013  Free Software Foundation, Inc

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

(defun archive--convert-require (elt)
  (list (car elt)
	(version-to-list (car (cdr elt)))))

(defun archive--strip-rcs-id (str)
  "Strip RCS version ID from the version string STR.
If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (when str
    (when (string-match "\\`[ \t]*[$]Revision:[ \t]+" str)
      (setq str (substring str (match-end 0))))
    (condition-case nil
	(if (version-to-list str)
	    str)
      (error nil))))

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
		   (autoloads-file (expand-file-name (concat pkg "-autoloads.el") dir))
		   simple-p)
	      ;; Omit autoloads and .elc files from the package.
	      (if (file-exists-p autoloads-file)
		  (delete-file autoloads-file))
	      (archive--delete-elc-files dir)
	      ;; Test whether this is a simple or multi-file package.
	      (setq simple-p (archive--simple-package-p dir pkg))
	      (push (if simple-p
			(apply #'archive--process-simple-package
			       dir pkg simple-p)
		      (archive--process-multi-file-package dir pkg))
		    packages)))
	(error (error "Error in %s: %S" dir v))))
    (with-temp-buffer
      (pp (nreverse packages) (current-buffer))
      (write-region nil nil "archive-contents"))))

(defun batch-prepare-packages ()
  "Prepare the `packages' directory inside the Git checkout.
Expects to be called from within the `packages' directory.
\"Prepare\" here is for subsequent construction of the packages and archive,
so it is meant to refresh any generated files we may need.
Currently only refreshes the ChangeLog files."
  (let* ((wit ".changelog-witness")
         (prevno (or (with-temp-buffer
                       (ignore-errors (insert-file-contents wit))
                       (when (looking-at "[1-9][0-9]*\\'")
                         (string-to-number (match-string 0))))
                     1))
         (new-revno
          (or (with-temp-buffer
                (call-process "bzr" nil '(t) nil "revno")
                (goto-char (point-min))
                (when (looking-at "[1-9][0-9]*$")
                  (string-to-number (match-string 0))))
              (error "bzr revno did not return a number as expected")))
         (pkgs '()))
    (unless (= prevno new-revno)
      (with-temp-buffer
        (unless (zerop (call-process "bzr" nil '(t) nil "log" "-v"
                                     (format "-r%d.." (1+ prevno))))
          (error "Error signaled by bzr log -v -r%d.." (1+ prevno)))
        (goto-char (point-min))
        (while (re-search-forward "^  packages/\\([-[:alnum:]]+\\)/" nil t)
          (pushnew (match-string 1) pkgs :test #'equal))))
    (dolist (pkg pkgs)
      (condition-case v
          (if (file-directory-p pkg)
              (archive--make-changelog pkg))
        (error (message "Error: %S" v))))
    (write-region (number-to-string new-revno) nil wit nil 'quiet)))

(defun archive--simple-package-p (dir pkg)
  "Test whether DIR contains a simple package named PKG.
If so, return a list (VERSION DESCRIPTION REQ COMMENTARY), where
VERSION is the version string of the simple package, DESCRIPTION
is the brief description of the package, REQ is a list of
requirements, and COMMENTARY is the package commentary.
Otherwise, return nil."
  (let* ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir))
	 (mainfile (expand-file-name (concat pkg ".el") dir))
         (files (directory-files dir nil archive-re-no-dot))
	 version description req commentary)
    (dolist (file (prog1 files (setq files ())))
      (unless (string-match "\\(?:\\.elc\\|~\\)\\'" file)
        (push file files)))
    (setq files (delete (concat pkg "-pkg.el") files))
    (setq files (delete (concat pkg "-autoloads.el") files))
    (setq files (delete "ChangeLog" files))
    (cond
     ((and (or (not (file-exists-p pkg-file))
               (= (length files) 1))
           (file-exists-p mainfile))
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
	(if (not (looking-at ";;;.*---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$"))
            (error "Can't parse first line of %s" mainfile)
          (setq description (match-string 1))
          (setq version
                (or (archive--strip-rcs-id (lm-header "package-version"))
                    (archive--strip-rcs-id (lm-header "version"))
                    (error "Missing `version' header")))
          ;; Grab the other fields, which are not mandatory.
          (let ((requires-str (lm-header "package-requires")))
            (if requires-str
                (setq req (mapcar 'archive--convert-require
                                  (car (read-from-string requires-str))))))
          (setq commentary (lm-commentary))
          (list version description req commentary))))
     ((not (file-exists-p pkg-file))
      (error "Can find single file nor package desc file in %s" dir)))))

(defun archive--process-simple-package (dir pkg vers desc req commentary)
  "Deploy the contents of DIR into the archive as a simple package.
Rename DIR/PKG.el to PKG-VERS.el, delete DIR, and write the
package commentary to PKG-readme.txt.  Return the descriptor."
  ;; Write the readme file.
  (with-temp-buffer
    (erase-buffer)
    (emacs-lisp-mode)
    (insert (or commentary
		(prog1 "No description"
		  (message "Missing commentary in package %s" pkg))))
    (goto-char (point-min))
    (while (looking-at ";*[ \t]*\\(commentary[: \t]*\\)?\n")
      (delete-region (match-beginning 0)
		     (match-end 0)))
    (uncomment-region (point-min) (point-max))
    (goto-char (point-max))
    (while (progn (forward-line -1)
		  (looking-at "[ \t]*\n"))
      (delete-region (match-beginning 0)
		     (match-end 0)))
    (write-region nil nil (concat pkg "-readme.txt")))
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
        (insert-file-contents cl)
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (while (progn (forward-line -1) (>= (point) start))
          (insert ";; ")))
      (set (make-local-variable 'backup-inhibited) t)
      (save-buffer)
      (kill-buffer)))
  (delete-directory dir t)
  (cons (intern pkg) (vector (version-to-list vers) req desc 'single)))

(defun archive--make-changelog (dir)
  "Export Bzr log info of DIR into a ChangeLog file."
  (message "Refreshing ChangeLog in %S" dir)
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (if (file-readable-p "ChangeLog") (insert-file-contents "ChangeLog"))
        (let ((old-md5 (md5 (current-buffer))))
          (erase-buffer)
          ;; git --no-pager log --date=short --format="%cd  %aN  <%ae>%n%n%w(80,8,8)%B%n" | sed 's/^        /\t/'
          (call-process "bzr" nil (current-buffer) nil
                        "log" "--gnu-changelog" ".")
          (if (equal old-md5 (md5 (current-buffer)))
              (message "ChangeLog's md5 unchanged for %S" dir)
            (write-region (point-min) (point-max) "ChangeLog" nil 'quiet)))))))

(defun archive--process-multi-file-package (dir pkg)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and write the package commentary to
PKG-readme.txt.  Return the descriptor."
  (let* ((exp (archive--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
	 (req (mapcar 'archive--convert-require (nth 4 exp)))
	 (readme (expand-file-name "README" dir)))
    (unless (equal (nth 1 exp) pkg)
      (error (format "Package name %s doesn't match file name %s"
		     (nth 1 exp) pkg)))
    ;; Write the readme file.
    (when (file-exists-p readme)
      (copy-file readme (concat pkg "-readme.txt") 'ok-if-already-exists))
    (rename-file dir (concat pkg "-" vers))
    (cons (intern pkg) (vector (version-to-list vers) req (nth 3 exp) 'tar))))

(defun archive--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (with-temp-buffer
      (unless (file-exists-p pkg-file)
	(error "File not found: %s" pkg-file))
      (insert-file-contents pkg-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun batch-make-site-dir (package-dir site-dir)
  (require 'package)
  (setq package-dir (expand-file-name package-dir default-directory))
  (setq site-dir (expand-file-name site-dir default-directory))
  (dolist (dir (directory-files package-dir t archive-re-no-dot))
    (if (not (file-directory-p dir))
        (message "Skipping non-package file %s" dir)
      (let* ((pkg (file-name-nondirectory dir))
             (autoloads-file (expand-file-name
                              (concat pkg "-autoloads.el") dir))
             simple-p version)
        ;; Omit autoloads and .elc files from the package.
        (if (file-exists-p autoloads-file)
            (delete-file autoloads-file))
        (archive--delete-elc-files dir 'only-orphans)
        ;; Test whether this is a simple or multi-file package.
        (setq simple-p (archive--simple-package-p dir pkg))
        (if simple-p
            (progn
              (apply 'archive--write-pkg-file dir pkg simple-p)
              (setq version (car simple-p)))
          (setq version
                (nth 2 (archive--multi-file-package-def dir pkg))))
        (make-symbolic-link (expand-file-name dir package-dir)
                            (expand-file-name (concat pkg "-" version)
                                              site-dir)
                            t)
        (let ((make-backup-files nil))
          (package-generate-autoloads pkg dir))
        (let ((load-path (cons dir load-path)))
          ;; FIXME: Don't compile the -pkg.el files!
          (byte-recompile-directory dir 0))))))

(defun archive--refresh-pkg-file ()
  (let* ((dir (directory-file-name default-directory))
         (pkg (file-name-nondirectory dir))
         (simple-p (archive--simple-package-p dir pkg)))
    (if simple-p
        (progn
          ;; (message "Refreshing pkg description of %s" pkg)
          (apply 'archive--write-pkg-file dir pkg simple-p))
      ;; (message "Not refreshing pkg description of %s" pkg)
      )))

(defun batch-make-site-package (sdir)
  (let* ((dest (car (file-attributes sdir)))
         (pkg (file-name-nondirectory (directory-file-name (or dest sdir))))
         (dir (or dest sdir)))
    (let ((make-backup-files nil))
      (package-generate-autoloads pkg dir))
    (let ((load-path (cons dir load-path)))
      ;; FIXME: Don't compile the -pkg.el files!
      (byte-recompile-directory dir 0))))

(defun archive--write-pkg-file (pkg-dir name version desc requires &rest ignored)
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (write-region
     (concat (format ";; Generated package description from %s.el\n"
		     name)
	     (prin1-to-string
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
			   requires))))
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
<h1 align=\"center\">%s</h1>"
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
  (cond
   ((file-readable-p (expand-file-name fsection srcdir))
    (with-temp-buffer
      (insert-file-contents (expand-file-name fsection srcdir))
      (buffer-string)))
   ((file-readable-p mainsrcfile)
    (with-temp-buffer
      (insert-file-contents mainsrcfile)
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
  (replace-regexp-in-string "<" "&lt"
                            (replace-regexp-in-string "&" "&amp" txt)))

(defun archive--html-make-pkg (pkg files)
  (let* ((name (symbol-name (car pkg)))
         (latest (package-version-join (aref (cdr pkg) 0)))
         (srcdir (expand-file-name name "../../build/packages"))
         (mainsrcfile (expand-file-name (format "%s.el" name) srcdir))
         (desc (aref (cdr pkg) 2)))
    (with-temp-buffer
      (insert (archive--html-header (format "GNU ELPA - %s" name)))
      (insert (format "<p>Description: %s</p>\n" (archive--quote desc)))
      (let* ((file (cdr (assoc latest files)))
             (attrs (file-attributes file)))
        (insert (format "<p>Latest: <a href=%S>%s</a>, %s, %s</p>\n"
                        file (archive--quote file)
                        (format-time-string "%Y-%b-%d" (nth 5 attrs))
                        (archive--html-bytes-format (nth 7 attrs)))))
      (let ((maint (archive--get-prop "Maintainer" name srcdir mainsrcfile)))
        (when maint
          (insert (format "<p>Maintainer: %s</p>\n" (archive--quote maint)))))
      (let* ((urlkind "Origin")
             (url
              (or (archive--get-prop "URL" name srcdir mainsrcfile)
                  (progn
                    (setq urlkind "Repository")
                    (concat "http://bzr.sv.gnu.org/lh/emacs/elpa/files/head:/packages/" name)))))
        (insert (format "<p>%s: <a href=%S>%s</a></p>"
                        urlkind url (archive--quote url))))
      (let ((readme (archive--get-section "Commentary" "README" srcdir mainsrcfile)))
        (when readme
          (insert "<h2>Full description</h2><pre>\n" (archive--quote readme)
                  "\n</pre>\n")))
      (unless (< (length files) 2)
        (insert (format "<h2>Old versions</h2><table cellpadding=\"3\" border=\"1\">\n"))
        (dolist (file files)
          (unless (equal (pop file) latest)
            (let ((attrs (file-attributes file)))
              (insert (format "<tr><td><a href=%S>%s</a></td><td>%s</td><td>%s</td>\n"
                              file (archive--quote file)
                              (format-time-string "%Y-%b-%d" (nth 5 attrs))
                              (archive--html-bytes-format (nth 7 attrs)))))))
        (insert "</table>\n"))
      (let ((news (archive--get-section "News" "NEWS" srcdir mainsrcfile)))
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
    (dolist (pkg archive-contents)
      (archive--html-make-pkg pkg (gethash (symbol-name (car pkg)) packages)))
    ;; FIXME: Add (old?) packages that are in `packages' but not in
    ;; archive-contents.
    (archive--html-make-index archive-contents)))

;;; Maintain external packages.

(defun archive-add/remove-externals ()
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
      (pcase-dolist (`(,dir ,kind ,url) exts)
        (cond
         ((not (file-exists-p dir))
          (let* ((branch (concat "externals/" dir))
                 (output
                  (with-temp-buffer
                    ;; FIXME: Use git-new-workdir!
                    (call-process "git" nil t nil "branch" "--track"
                                  branch (concat "origin/" branch))
                    (call-process "git" nil t nil "clone"
                                  "--shared" "--branch" branch "../" dir)
                    ;; (let ((default-directory (file-name-as-directory
                    ;;                           (expand-file-name dir))))
                    ;;   (call-process "git" nil t nil "branch"
                    ;;                 "-m" branch "master"))
                    (buffer-string))))
            (message "Cloning branch %s:\n%s" dir output)))
         ((not (file-directory-p (concat dir "/.git")))
          (message "%s is in the way of an external, please remove!" dir))
         (t
          (let ((default-directory (expand-file-name dir)))
            (with-temp-buffer
              (call-process "git" nil t nil "pull")
              (message "Updated %s:%s" dir (buffer-string))))
          ))))))

(provide 'archive-contents)
;;; archive-contents.el ends here
