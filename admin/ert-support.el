;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2016, Free Software Foundation, Inc.

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
        `(("local-elpa" . ,(concat top-directory "/archive/packages"))))
  (setq package-user-dir
        (make-temp-file "elpa-test" t))
  (package-initialize)
  (package-refresh-contents)
  (package-install package))

(defun ert-support-test-find-tests (package-directory package)
  (or
   (directory-files package-directory nil ".*-test.el$")
   (directory-files package-directory nil ".*-tests.el$")
   (let ((dir-test
          (concat package-directory "/test")))
     (when (file-exists-p dir-test)
       (directory-files dir-test)))
   (let ((dir-tests
          (concat package-directory "/tests")))
     (when (file-exists-p dir-tests)
       (directory-files dir-tests)))))

(defun ert-support-load-tests (package-directory package)
  (mapc
   (lambda(file)
     (message "Loading test file... %s" (concat package-directory file))
     (load-file (concat package-directory file)))
   (ert-support-test-find-tests package-directory package)))

(defun ert-support-test-package (top-directory package)
  (ert-support-package-install top-directory package)
  (ert-support-load-tests
   (concat top-directory "/packages/" (symbol-name package) "/")
   package)

  (ert-run-tests-batch-and-exit t))
