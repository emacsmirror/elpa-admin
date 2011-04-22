;;; archive-contents.el --- Auto-generate the `archive-contents' file -*- lexical-binding: t -*-

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

(defun batch-make-archive-contents ()
  (let ((packages '(1))) ; format-version.
    (dolist (file (directory-files default-directory))
      (pcase file
       ((or `"." `".." `"elpa.rss" `"archive-contents") nil)
       ((pred file-directory-p)
        (if (not (string-match (concat archive-contents-subdirectory-regexp "\\'")
			       file))
            (message "Unknown package directory name format %s" file)
          (let* ((pkg  (match-string 1 file))
                 (vers (match-string 2 file))
                 (exp
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name (concat pkg "-pkg.el") file))
                    (goto-char (point-min))
                    (read (current-buffer))))
		 (req (mapcar 'archive-contents--convert-require
			      (nth 4 exp)))
		 (readme (expand-file-name "README" file)))
	    (when (file-exists-p readme)
	      (copy-file readme
			 (concat pkg "-readme.txt")
			 'ok-if-already-exists))
            (unless (equal (nth 1 exp) pkg)
              (message "Package name %s doesn't match file name %s"
                       (nth 1 exp) file))
            (unless (equal (nth 2 exp) vers)
              (message "Package version %s doesn't match file name %s"
                       (nth 2 exp) file))
            (push (cons (intern pkg)
                        (vector (version-to-list vers)
                                req
                                (nth 3 exp)
                                'tar))
                  packages))))
       ;; Simple package
       ((pred (string-match "\\.el\\'"))
        (if (not (string-match "-\\([0-9.]+\\)\\.el\\'" file))
            (message "Unknown package file name format %s" file)
          (let* ((pkg (substring file 0 (match-beginning 0)))
                 (vers (match-string 1 file))
                 (desc
                  (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (if (not (looking-at ";;;.*---[ \t]*\\(.*\\)\\(-\\*-.*-\\*-[ \t]*\\)?$"))
                        (message "Incorrectly formatted header in %s" file)
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
                            (save-buffer)))))))
		 (requires-str (lm-header "package-requires"))
		 (req (if requires-str
			  (mapcar 'archive-contents--convert-require
				  (car (read-from-string requires-str))))))
            (push (cons (intern pkg)
                        (vector (version-to-list vers)
				req
                                desc
                                'single))
                  packages))))
       ((pred (string-match "\\.elc\\'")) nil)
       ((pred (string-match "-readme\\.txt\\'")) nil)
       (t
        (message "Unknown file %s" file))))
    (with-current-buffer (find-file-noselect "archive-contents")
      (erase-buffer)
      (pp (nreverse packages) (current-buffer))
      (save-buffer))))

(provide 'archive-contents)
;;; archive-contents.el ends here
