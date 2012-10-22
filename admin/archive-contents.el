;;; archive-contents.el --- Auto-generate an Emacs Lisp package archive.

;; Copyright (C) 2011, 2012  Free Software Foundation, Inc

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

(require 'lisp-mnt)
(require 'package)

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
	      (error "Skipping non-package file %s" dir)
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
			(apply 'archive--process-simple-package
			       dir pkg simple-p)
		      (archive--process-multi-file-package dir pkg))
		    packages)))
	;; Error handler
	(error (message "%s" (cadr v)))))
    (with-temp-buffer
      (pp (nreverse packages) (current-buffer))
      (write-region nil nil "archive-contents"))))

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
      (unless (string-match "\\.elc\\'" file)
        (push file files)))
    (when (and (or (not (file-exists-p pkg-file))
		   (= (length files) 2))
	       (file-exists-p mainfile))
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
	(and (looking-at ";;;.*---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$")
	     (progn
	       (setq description (match-string 1))
	       (setq version
		     (or (archive--strip-rcs-id (lm-header "package-version"))
			 (archive--strip-rcs-id (lm-header "version"))
                         "0.0")))
	     (progn
	       ;; Grab the other fields, which are not mandatory.
	       (let ((requires-str (lm-header "package-requires")))
		 (if requires-str
		     (setq req (mapcar 'archive--convert-require
				       (car (read-from-string requires-str))))))
	       (setq commentary (lm-commentary))
	       (list version description req commentary)))))))

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
  (delete-directory dir t)
  (cons (intern pkg) (vector (version-to-list vers) req desc 'single)))

(defun archive--make-changelog (dir)
  "Export Bzr log info of DIR into a ChangeLog file."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (call-process "bzr" nil '(:file "ChangeLog") nil
                  "log" "--gnu-changelog" ".")))

(defun archive--process-multi-file-package (dir pkg)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and write the package commentary to
PKG-readme.txt.  Return the descriptor."
  (let* ((exp (archive--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
	 (req (mapcar 'archive--convert-require (nth 4 exp)))
	 (readme (expand-file-name "README" dir)))
    (archive--make-changelog dir)
    (unless (equal (nth 1 exp) pkg)
      (error (format "Package name %s doesn't match file name %s"
		     (nth 1 exp) pkg)))
    ;; Write the readme file.
    (when (file-exists-p readme)
      (copy-file readme (concat pkg "-readme.txt") 'ok-if-already-exists))
    (rename-file dir (concat pkg "-" vers))
    (cons (intern pkg) (vector (version-to-list vers) req (nth 3 exp) 'tar))))

(defun archive--multi-file-package-def (dir pkg)
  "Reurn the `define-package' form in the file DIR/PKG-pkg.el."
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
    (condition-case v
	(if (not (file-directory-p dir))
	    (error "Skipping non-package file %s" dir)
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
	      (byte-recompile-directory dir 0))))
     ;; Error handler
     (error (message "%s" (cadr v))))))

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


;; Local Variables:
;; no-byte-compile: t
;; lexical-binding: t
;; End:

(provide 'archive-contents)
;;; archive-contents.el ends here
