;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2016-2017, Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun ert-support-package-install (top-directory package)
  ;; blitz default value and set up from elpa.
  (setq package-archives
        `(("local-elpa"
	   . ,(expand-file-name "archive/packages" top-directory)))
	package-user-dir (make-temp-file "elpa-test" t))
  (package-initialize)
  (package-refresh-contents)
  (package-install package))

(defun ert-support-test-find-tests (package-directory package)
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

(defun ert-support-load-tests (package-directory package)
  (mapc
   (lambda (file)
     (let ((force-load-messages t))
       (load-file file)))
   (ert-support-test-find-tests package-directory package)))

(defun ert-support-test-package (top-directory package)
  (ert-support-package-install top-directory package)
  (ert-support-load-tests
   (expand-file-name (concat "packages/" (symbol-name package)) top-directory)
   package)

  (ert-run-tests-batch-and-exit t))
