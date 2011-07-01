;;; archive-contents.el --- Auto-generate the `archive-contents' file

;; Copyright (C) 2011  Free Software Foundation, Inc

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

(defconst archive-contents-subdirectory-regexp
  "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)")

(defun archive-contents--convert-require (elt)
  (list (car elt)
	(version-to-list (car (cdr elt)))))

(defun archive-contents--strip-rcs-id (str)
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

(defun batch-make-archive-contents ()
  (let ((packages '(1))) ; format-version.
    (dolist (file (directory-files default-directory))
      (condition-case nil
	(cond
	 ((memq file '("." ".." "elpa.rss" "archive-contents"))
	  nil)
	 ;; Multi-file package
	 ((file-directory-p file)
	  (let* ((pkg (file-name-nondirectory file))
		 (exp
		  (with-temp-buffer
		    (insert-file-contents
		     (expand-file-name (concat pkg "-pkg.el") file))
		    (goto-char (point-min))
		    (read (current-buffer))))
		 (vers (nth 2 exp))
		 (req (mapcar 'archive-contents--convert-require
			      (nth 4 exp)))
		 (readme (expand-file-name "README" file)))
	    (when (file-exists-p readme)
	      (copy-file readme
			 (concat pkg "-readme.txt")
			 'ok-if-already-exists))
	    (unless (equal (nth 1 exp) pkg)
	      (error (format "Package name %s doesn't match file name %s"
			     (nth 1 exp) file)))
	    (push (cons (intern pkg)
			(vector (version-to-list vers) req (nth 3 exp) 'tar))
		  packages))
	  (rename-file file (concat pkg "-" vers)))
	 ;; Simple package
	 ((string-match "\\([^/]+\\)\\.el\\'" file)
	  (let* ((pkg (match-string 1 file))
		 (desc
		  (with-temp-buffer
		    (insert-file-contents file)
		    (goto-char (point-min))
		    (unless (looking-at ";;;.*---[ \t]*\\(.*\\)\\(-\\*-.*-\\*-[ \t]*\\)?$")
		      (error "Incorrectly formatted header in %s" file))
		    (prog1 (match-string 1)
		      (let ((commentary (lm-commentary)))
			(with-current-buffer (find-file-noselect
					      (concat pkg "-readme.txt"))
			  (erase-buffer)
			  (emacs-lisp-mode)
			  (insert (or commentary
				      (prog1 "No description"
					(message "Missing Commentary in %s"
						 file))))
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
			  (save-buffer))))))
		 (vers (or (archive-contents--strip-rcs-id (lm-header "package-version"))
			   (archive-contents--strip-rcs-id (lm-header "version"))
			   (error "Missing version number in %s" file)))
		 (requires-str (lm-header "package-requires"))
		 (req (if requires-str
			  (mapcar 'archive-contents--convert-require
				  (car (read-from-string requires-str))))))
	    (push (cons (intern pkg)
			(vector (version-to-list vers) req desc 'single))
		  packages)
	    (rename-file file (concat (or (file-name-directory file) "")
				      pkg "-" vers ".el"))))
	 ((not (or (string-match "\\.elc\\'" file)
		   (string-match "-readme\\.txt\\'" file)))
	  (message "Unknown file %s" file)))
	;; Error handler
	(error (message (cadr v)))))
    (with-current-buffer (find-file-noselect "archive-contents")
      (erase-buffer)
      (pp (nreverse packages) (current-buffer))
      (save-buffer))))

;; Local Variables:
;; no-byte-compile: t
;; lexical-binding: t
;; End:

(provide 'archive-contents)
;;; archive-contents.el ends here
