;;; elpa-admin.el --- Auto-generate an Emacs Lisp package archive  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2022  Free Software Foundation, Inc

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;;; TODO

;; - bug#45345: [elpa-archive] "make build/<package>" should not pull
;;   unconditionally
;; - bug#45346: make it easier to ignore all the files in <directory>
;;   except for a few exceptions.
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

(defvar elpaa--branch-prefix "elpa/")
(defvar elpaa--release-branch-prefix "elpa-release/")

(defvar elpaa--specs-file "elpa-packages")
(defvar elpaa--copyright-file "copyright_exceptions")
(defvar elpaa--email-to nil) ;;"gnu-emacs-sources@gnu.org"
(defvar elpaa--email-from nil) ;;"ELPA update <do.not.reply@elpa.gnu.org>"
(defvar elpaa--email-reply-to nil)

(defvar elpaa--sandbox-extra-ro-dirs nil)

(defvar elpaa--sandbox
  ;; Currently sandboxing is implemented using `bwrap' which AFAIK doesn't
  ;; exist for w32 nor for macos, so there's no point defaulting to non-nil
  ;; on those platforms.
  ;; On GNU/linux we do default to non-nil regardless if we find `bwrap' in
  ;; $PATH just out of paranoia (in case `bwrap' ends up missing by accident).
  (not (memq system-type '(darwin ms-dos windows-nt cygwin)))
  "If non-nil, run some of the less trusted commands in a sandbox.
This is recommended when building packages from untrusted sources,
but this requires Bubblewrap (https://github.com/containers/bubblewrap)
to be installed and has only been tested on some Debian systems.")

(defvar elpaa--doc-subdirectory "doc/"
  "Directory in which to place HTML docs for the ELPA website.
If nil, don't build the docs in the first place.
Directory is relative to the tarball directory.
Can be set in elpa-config via `doc-dir'.")

(defvar elpaa--debug (getenv "ELPA_DEBUG")
  "Non-nil means to print debug messages.")

(defvar elpaa--org-export-options
  '(:with-author nil :with-creator nil :with-broken-links t)
  "Options used common to all Org export backends.
See variable `org-export-options-alist'.")

(unless (fboundp 'ignore-error)
  (defmacro ignore-error (condition &rest body)
    `(condition-case nil (progn ,@body) (,condition nil))))

(defun elpaa--form-from-file-contents (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    ;; This is unnecessary because ‘with-temp-buffer’ generates a new
    ;; (empty) buffer, and ‘insert-file-contents’ inserts after point.
    ;; In other words, point is alraedy at bob.
    ;;- (goto-char (point-min))
    (read (current-buffer))))

(defun elpaa-read-config (file)
  (let ((config (elpaa--form-from-file-contents file)))
    (pcase-dolist (`(,var ,val) config)
      (cl-assert (or (stringp val) (booleanp val)
                     (and (consp val) (cl-every #'stringp val)))
                 t)
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
              ('sandbox-extra-ro-dirs	elpaa--sandbox-extra-ro-dirs)
              ('doc-dir                 elpaa--doc-subdirectory)
              ('debug			elpaa--debug))
            val))))

(when (file-readable-p "elpa-config") (elpaa-read-config "elpa-config"))

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
  (elpaa--form-from-file-contents elpaa--specs-file))

(defun elpaa--spec-get (pkg-spec prop &optional default)
  (or (plist-get (cdr pkg-spec) prop) default))

(defun elpaa--spec-member (pkg-spec prop)
  (plist-member (cdr pkg-spec) prop))

(defun elpaa--main-file (pkg-spec)
  (or (elpaa--spec-get pkg-spec :main-file)
      (let ((ldir (elpaa--spec-get pkg-spec :lisp-dir)))
        (concat (if ldir (file-name-as-directory ldir)) (car pkg-spec) ".el"))))

(defun elpaa--get-last-release-commit (pkg-spec &optional from)
  "Return the commit that last changed `Version:'.
FROM is the start revision.  Return nil if not found."
  (with-temp-buffer
    (if (equal 0     ;Don't signal an error if call errors out.
               (elpaa--call
                (current-buffer)
                "git" "log" "-n1" "--oneline" "--no-patch"
                "--pretty=format:%H"
                (when (elpaa--spec-get pkg-spec :merge)
                  ;; Finding "the" revision when there's a merge involved is
                  ;; fundamentally unreliable.
                  ;; Ideally we should probably signal an error when the commit
                  ;; we found is not on all paths from FROM to avoid making an
                  ;; arbitrary choice.
                  ;; For `:merge'd packages, the commit that flipped `Version:'
                  ;; is usually not what we want, since that one was on the
                  ;; upstream branch, without our own changes.
                  ;; We use `--first-parent' for this reason, so it prefers
                  ;; the corresponding merge commit (which is not ideal either
                  ;; but is arguably the best we can do in that case).
                  "--first-parent")
                "-L" (concat "/^;;* *\\(Package-\\)\\?Version:/,+1:"
                             (file-name-nondirectory
                              (elpaa--main-file pkg-spec)))
                from))
        ;; The --no-patch (aka -s) option does not work
        ;; with "git log -L<from>,<to>:<path>" before git
        ;; version 2.22; so capture only the first line.
        (buffer-substring-no-properties
         (goto-char (point-min)) (line-end-position))
      (elpaa--message "Can't find release rev: %s" (buffer-string))
      nil)))

(defun elpaa--get-release-revision (dir pkg-spec &optional vers version-map)
  "Get the REVISION that corresponds to current release.
This is either found from VERS in VERSION-MAP or by looking at the last
commit which modified the \"Version:\" pseudo header."
  (while (and version-map
              (not (member vers (car version-map))))
    (pop version-map))
  (or (nth 2 (car version-map))
      (and (elpaa--spec-get pkg-spec :rolling-release) "HEAD")
      ;; When the mainfile is a symlink (e.g. for :core packages), run Git
      ;; in the directory that holds the actual file, otherwise Git won't
      ;; know what file we're talking about.
      (let* ((mainfile (file-truename
                        (expand-file-name (elpaa--main-file pkg-spec)
                                          (elpaa--dirname dir))))
             (default-directory (file-name-directory mainfile)))
        (elpaa--get-last-release-commit pkg-spec))))

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
               ((or (equal vers "0") (< (apply #'min vl) 0))
                ;; FIXME: Maybe we could look further into the past?
                (elpaa--message "Previous version was also snapshot"))
               (t
                (cons (package-version-join vl) rev))))))))))

(defun elpaa--select-revision (dir pkg-spec rev)
  "Checkout revision REV in DIR of PKG-SPEC.
Do it without leaving the current branch.  If REV is nil, then
use the revision that is already checked out.  If REV is a
function, then call it with no arguments and use the value it
returns.  Return the selected revision."
  (let* ((ftn (file-truename
               (expand-file-name (elpaa--main-file pkg-spec) dir)))
         ;; FIXME: Emacs-26's `vc-git-working-revision' ignores its arg and
         ;; uses uses the `default-directory' to get the revision.
         (default-directory (file-name-directory ftn))
         (cur-rev (vc-working-revision ftn)))
    (when (functionp rev)
      (setq rev (funcall rev)))
    ;; Don't fail in case `rev' is not known.
    (if (or (not rev) (equal rev cur-rev))
        (elpaa--message "Current revision is already desired revision!")
      (with-temp-buffer
        ;; Run it within the true-filename directory holding the mainfile,
        ;; so that for :core packages we properly affect the Emacs tree.
        (elpaa--call t "git" "reset" "--merge" rev)
        (elpaa--message "Reverted to release revision %s\n%s"
                        rev (buffer-string)))
      ;; We should make sure we go back to the head of the branch afterwards,
      ;; tho it's convenient to do it more lazily, e.g. in case of error it
      ;; can make it easier to diagnose the problem.
      ;; But for `:core' packages it's important because the same tree
      ;; may be used for another package in the same run, so we'd otherwise
      ;; end up (re)building old versions.
      ;; FIXME: We should probably fix this better in
      ;; `elpaa--get-release-revision' and/or `elpaa--get-last-release'
      ;; not to depend on the current in-tree revision.
      (when (eq :core (cadr pkg-spec))
        (elpaa--temp-file
         (lambda ()
           (let ((default-directory (file-name-directory ftn)))
             (with-temp-buffer
               (elpaa--call t "git" "merge")
               (elpaa--message "Restored the head revision\n%s"
                               (buffer-string))))))))
    (or rev cur-rev)))

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
          (if (zerop
               (elpaa--call t "git" "stash" "push" "-u" "-m"
                            "Saved changes while building tarball"
                            ;; Don't stash the deletion of <foo>-pkg.el,
                            ;; since it would cause a merge conflict
                            ;; later in "stash apply".
                            ;; Don't know when this was introduced into Git
                            ;; and it seems somewhat fiddly (e.g. git-2.30.2
                            ;; gives a weird behavior if the "*" is missing),
                            ;; so not sure it's worth the trouble:
                            ;;
                            ;;     "--" "*" ":(exclude,glob)*-pkg.el"
                            ))
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

(defvar elpaa--keep-max 20)

(defun elpaa--keep-old (oldtarballs n)
  "Select N tarballs to keep among those in OLDTARBALLS."
  ;; It's not clear which ones to select.  My main goal here was to try and keep
  ;; more of the last releases than of the old releases, and also to favor the
  ;; last release in a given line, so for example for Emacs releases, we might
  ;; prefer to keep: 24.5 24.4 24.3 24.2 24.1 23.4 22.3 21.3 20.4
  ;; rather than   : 24.3 24.1 23.3 23.2 23.1 21.1 20.3 20.2 20.1
  ;; Also, we want this to work for "any" release numbering scheme, including
  ;; the pseudo release numbers YYYYMMMDD used for snapshots.
  ;;
  ;; I'm not very satisfied with the code below:
  ;; - It was tested mostly on sets where N is significantly smaller than the
  ;;   input set size, whereas in practice it'll probably mostly be used with
  ;;   N being 20 and OLDTARBALLS containing 21 elements, so... we'll see.
  ;; - I don't think this algorithm enjoys any kind of "stability" property
  ;;   such as a guarantee that if you first select 50 elements and then you
  ;;   select 20 elements out of that you get the same result as if you
  ;;   directly selected 20 elements from the original set.
  (cl-assert (natnump n))
  (cond
   ((< n 1) nil)
   ((not (nthcdr n oldtarballs)) oldtarballs) ;; We can keep them all.
   (t
    (setq oldtarballs (nreverse
                       (sort (copy-sequence oldtarballs)
                             (lambda (t1 t2)
                               (version<= (car t1) (car t2))))))
    (cond
     ((< n 2)
      ;; If we have to pick one, keep the latest.
      (list (car oldtarballs)))
     ((< n 3)
      ;; If there's only room for 2 elements, keep the first and the last.
      (cons (car oldtarballs) (last oldtarballs)))
     (t
      ;; The general idea here is to split the input into buckets
      ;; which represent a kind of "logarithm of distance to the latest"
      ;; and then we pick the same number of elements from each bucket
      ;; (the log(distance) is actually taken to be the length of the common
      ;; prefix between the two versions).
      (let* ((latest (pop oldtarballs))
             (vers (car latest))
             (buckets ())
             (kept (list latest)))
        (dolist (oldtarball oldtarballs)
          (let* ((tvers (car oldtarball))
                 (common-prefix (try-completion "" (list vers tvers))))
            (push oldtarball (alist-get (length common-prefix) buckets))))

        ;; Make sure there are fewer buckets than target elements.
        (while (> (length buckets) (- n (length kept)))
          ;; (message "Too many buckets (%s/%s): Merging...."
          ;;          (length buckets) (- n (length kept)))
          (let ((target-size (1+ (/ (length oldtarballs) n)))
                (new t))
            (dolist (bucket (prog1 buckets (setq buckets nil)))
              (if (or new (> (length bucket) target-size))
                  (progn (push bucket buckets) (setq new nil))
                (setq new t)
                (setf (cdar buckets) (nconc (cdr bucket) (cdar buckets)))))))

        ;; "Spread" some buckets: for a two-level release numbering scheme,
        ;; we might end up with 2 buckets: one with the latest minor releases
        ;; and the other with everything else.  When we recurse on the
        ;; "everything else", the same will tend to happen again, and overall
        ;; this tends to select too many "recent minor releases" in favor of
        ;; keeping older major releases.
        ;; We try to compensate here by splitting "furtherest" buckets into
        ;; smaller buckets based on the first char that differs between their
        ;; release number.
        (setq buckets (sort buckets (lambda (b1 b2) (<= (car b1) (car b2)))))
        (while
            (let* ((bucket (car buckets))
                   (len (length (try-completion "" bucket)))
                   (newbuckets ()))
              (dolist (oldtarball (cdr bucket))
                (let ((tvers (car oldtarball)))
                  (push oldtarball
                        (alist-get (substring tvers 0
                                              (min (length tvers) (1+ len)))
                                   newbuckets nil nil #'equal))))
              (when (< (+ (length newbuckets) (length (cdr buckets)))
                       (- n (length kept)))
                ;; (message "Spreading one bucket")
                (setq buckets (nconc (cdr buckets)
                                     (mapcar (lambda (b)
                                               (cons (length (car b)) (cdr b)))
                                             newbuckets)))
                t)))
        ;; Finally, evenly select elements from every bucket.
        (setq buckets (sort buckets (lambda (b1 b2) (<= (length b1) (length b2)))))
        (while buckets
          (let ((bucket-size (/ (- n (length kept)) (length buckets)))
                (bucket (cdr (pop buckets))))
            (setq kept (nconc (elpaa--keep-old bucket
                                               bucket-size)
                              kept))))
        kept))))))

(defun elpaa--prune-old-tarballs (tarball oldtarballs destdir &optional minage)
  ;; Make sure we don't count ourselves among the "old" tarballs.
  (let ((self (rassoc (file-name-nondirectory tarball) oldtarballs)))
    (when self
      (setq oldtarballs (delq self oldtarballs))))
  (with-demoted-errors "elpaa--prune-old-tarballs: %S"
    (when (nthcdr elpaa--keep-max oldtarballs)
      (let* ((keep (elpaa--keep-old oldtarballs elpaa--keep-max))
             (keep (nreverse (sort keep
                                   (lambda (t1 t2) (version<= (car t1) (car t2)))))))
        (message "Keeping: %s" (mapcar #'cdr keep))
        (dolist (oldtarball oldtarballs)
          (unless (memq oldtarball keep)
            (cl-assert (not (equal (cdr oldtarball)
                                   (file-name-nondirectory tarball))))
            (message "Deleting %s" (cdr oldtarball))
            (let* ((olddir (expand-file-name "old" destdir))
                   (filename (cdr oldtarball))
                   (sig (concat (if (member (file-name-extension filename)
                                            '("lz" "gz"))
                                    (file-name-sans-extension filename)
                                  filename)
                                ".sig"))
                   (mvfun (lambda (f)
                            (let* ((src (expand-file-name f destdir))
                                   (fa (file-attributes src)))
                              (cond
                               ((not fa)
                                (message "Not existing/moving: %S" src))
                               ((and minage
                                     (< (float-time
                                         (time-subtract
                                          (current-time)
                                          (file-attribute-modification-time
                                           fa)))
                                        ;; One year.
                                        minage))
                                (message "File too young: %S" src))
                               (t
                                (rename-file src
                                             (expand-file-name f olddir))))))))
              (make-directory olddir t)
              (funcall mvfun filename)
              (funcall mvfun sig))))
        (setq oldtarballs keep)))
    (dolist (oldtarball oldtarballs)
      ;; Compress oldtarballs.
      (let ((file (cdr oldtarball)))
        (when (string-match "\\.\\(tar\\|el\\)\\'" file)
          ;; Make sure we don't compress the file we just created.
          (cl-assert (not (equal file (file-name-nondirectory tarball))))
          ;; (elpaa--message "not equal %s and %s" file tarball)
          (elpaa--call nil "lzip" (expand-file-name file destdir))
          (setf (cdr oldtarball) (concat file ".lz"))))))
  oldtarballs)

(defun elpaa--make-one-tarball ( tarball dir pkg-spec metadata-or-version
                                 &optional revision-function tarball-only)
  "Create file TARBALL for PKG-SPEC if not done yet.
Return non-nil if a new tarball was created.  Also create some
auxillary files unless TARBALL-ONLY is non-nil ."
  (elpaa--message "Building tarball %s..." tarball)
  (if (and (or (file-readable-p tarball)
               (file-readable-p (replace-regexp-in-string
				 "\\.tar\\'" ".el" tarball)))
	   (or tarball-only
	       ;; Even if the above exists, then we might still have
	       ;; to call `elpaa--make-one-tarball-1' because that
	       ;; is the only place where `elpaa--html-make-pkg' is
	       ;; called and that in turn is where these files are
	       ;; created:
               (let ((pkgname (car pkg-spec))
		     (default-directory
		       (expand-file-name (file-name-directory tarball))))
		 (and (file-readable-p (concat pkgname "-readme.txt"))
		      (file-readable-p (concat pkgname ".html"))
		      (file-readable-p (concat pkgname ".svg"))))))
      (progn
        (elpaa--message "Tarball %s already built!" tarball)
        nil)
    (message "======== Building tarball %s..." tarball)
    (let ((res nil))
      (unwind-protect
          (condition-case-unless-debug err
              (setq res (elpaa--make-one-tarball-1
                         tarball dir pkg-spec metadata-or-version
                         revision-function tarball-only))
            (error (message "Build error for %s: %S" tarball err)
                   nil))
        (message (if res "######## Built new package %s!"
                   "######## Build of package %s FAILED!!")
                 tarball)))))

(defun elpaa--make-one-tarball-1 ( tarball dir pkg-spec metadata-or-version
                                 &optional revision-function tarball-only)
  (elpaa--with-temp-files
   dir
   (let* ((destdir (file-name-directory tarball))
          (pkgname (car pkg-spec))
          (_ (when (and destdir (not (file-directory-p destdir)))
               (make-directory destdir)))
          (revision (elpaa--select-revision dir pkg-spec revision-function))
          (metadata
           (if (stringp metadata-or-version)
               ;; Re-read the metadata after `elpaa--select-revision'.
               (let ((metadata (elpaa--metadata dir pkg-spec)))
                 (unless (equal metadata-or-version (nth 1 metadata))
                   ;; It's probably an error if it happens, but let's
                   ;; see first when it happens.
                   (elpaa--message "Error: version disagreement at %S: %S"
                                   metadata-or-version metadata))
                 ;; Use the arg-provided version in case of disagreement.
                 `(nil ,metadata-or-version . ,(nthcdr 2 metadata)))
             metadata-or-version))
          (vers (nth 1 metadata))
          (elpaignore (expand-file-name ".elpaignore" dir))
          (ignores (elpaa--spec-get pkg-spec :ignored-files))
          (renames (elpaa--spec-get pkg-spec :renames))
          (ldir    (elpaa--spec-get pkg-spec :lisp-dir))
          (re (concat "\\`" (regexp-quote pkgname)
                      "-\\([0-9].*\\)\\.\\(tar\\|el\\)\\(\\.[a-z]*z\\)?\\'"))
          (oldtarballs
           (unless tarball-only
             (mapcar
              (lambda (file)
                (string-match re file)
                (cons (match-string 1 file) file))
              (directory-files destdir nil re))))
          rendered)
     (when ldir
       (cl-pushnew (list (file-name-as-directory ldir) "") renames
                   :test #'equal))
     (elpaa--copyright-check pkg-spec)
     (let ((process-environment (elpaa--makeenv vers revision)))
       ;; Run `make' before building the Info file, so that the `make'
       ;; rule can be used to build the Info/Texinfo file.
       (elpaa--make pkg-spec dir)
       (elpaa--build-Info pkg-spec dir destdir))
     (elpaa--write-pkg-file dir pkgname metadata revision)
     (setq rendered (elpaa--write-plain-readme dir pkg-spec))
     ;; FIXME: Allow renaming files or selecting a subset of the files!
     (cl-assert (not (string-match "[][*\\|?]" pkgname)))
     (cl-assert (not (string-match "[][*\\|?]" vers)))
     (apply #'elpaa--call
            nil "tar"
            `("--exclude-vcs"
              ,@(mapcar (lambda (i) (format "--exclude=packages/%s/%s" pkgname i))
                        ignores)
              ,@(when (file-readable-p elpaignore) `("-X" ,elpaignore))
              ,@(mapcar (lambda (r) (elpaa--make-tar-transform pkgname r))
                        renames)
              "--transform"
              ,(format "s|^packages/%s|%s-%s|" pkgname pkgname vers)
              "-chf" ,tarball
              ,(concat "packages/" pkgname)))
     (cl-assert (file-readable-p tarball))
     (unless tarball-only
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
	   (ignore-error file-error     ;E.g. under w32!
	     (make-symbolic-link (file-name-nondirectory tarball) link)))
         (setq oldtarballs
               (let ((elpaa--keep-max
                      ;; In the devel directory, don't bother keeping so
                      ;; many tarballs.
                      (if revision-function elpaa--keep-max
                        (/ elpaa--keep-max 2))))
                 (elpaa--prune-old-tarballs tarball oldtarballs destdir
                                            ;; Keep release versions at
                                            ;; least 2 years.
                                            (if revision-function
                                                (* 60 60 24 365 2)))))
         (let ((default-directory (expand-file-name destdir)))
           ;; This also creates <pkg>-readme.txt and <pkg>.svg.
           (elpaa--html-make-pkg pkgdesc pkg-spec
                                 `((,vers . ,(file-name-nondirectory tarball))
                                   . ,oldtarballs)
                                 dir rendered))))
     'new)))

(defun elpaa--makeenv (version revision)
  "Set the PACKAGE_VERSION and PACKAGE_REVISION environment variables.
Set them to the values specified by VERSION and REVISION in a copy
of the current `process-environment'.  Return the modified copy."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "PACKAGE_VERSION" version)
    (setenv "PACKAGE_REVISION" revision)
    process-environment))

(defun elpaa--git-date-to-timestamp (gitdate)
  "Convert date from git (ISO 6401) to a timestamp."
  (unless (string-match (rx bos
                            (group-n 1 (+ digit)) "-"
                            (group-n 2 (+ digit)) "-"
                            (group-n 3 (+ digit)) "T"
                            (group-n 4 (+ digit)) ":"
                            (group-n 5 (+ digit)) ":"
                            (group-n 6 (+ digit))
                            (? "+"
                               (group-n 7 (+ digit)) ":"
                               (group-n 8 (+ digit))))
                        gitdate)
    (error "Unknown date format: %S" gitdate))
  (let* ((field
          (lambda (group)
            (and (match-beginning group)
                 (string-to-number (match-string group gitdate)))))
         (y (funcall field 1))
         (mo (funcall field 2))
         (d (funcall field 3))
         (h (funcall field 4))
         (mi (funcall field 5))
         (s (funcall field 6))
         (zh (funcall field 7))
         (zm (funcall field 8))
         (zs (if zh
                 (* 60 (+ (* zh 60) zm))
               0)))
    (encode-time (list s mi h d mo y nil nil zs))))

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
          (format-time-string "%Y%m%d.%H%M%S"
                              (elpaa--git-date-to-timestamp gitdate)
                              0)))
    ;; Get rid of leading zeros since ELPA's version numbers don't allow them.
    (replace-regexp-in-string "\\(\\`\\|[^0-9]\\)0+\\([0-9]\\)" "\\1\\2"
                              ;; Remove trailing newline or anything untoward.
                              (replace-regexp-in-string "[^.0-9]+" ""
                                                        verdate))))

(defun elpaa--get-package-spec (pkgname &optional noerror)
  "Retrieve the property list for PKGNAME from `elpaa--specs-file'."
  (let* ((specs (elpaa--get-specs))
         (spec (assoc pkgname specs)))
    (if (null spec)
	(if (not noerror)
            (error "Unknown package %S" pkgname)
	  (message "Unknown package %S" pkgname)
	  (list pkgname))
      spec)))

(defun elpaa--publish-package-specs (specs)
  "Process and publish SPECS in elpa-packages.eld files."
  (with-temp-buffer
    ;; Remove :core packages, handle :url nil and and compress the
    ;; contents of the "elpa-packages"
    (prin1
     (mapcan
      (lambda (spec)
        (pcase spec
          (`(,name :url nil . ,rest)
           `((,name :url ,(concat "https://git.sv.gnu.org/git/" elpaa--gitrepo)
                    :branch ,(concat elpaa--branch-prefix (car spec))
                    ,@rest)))
          (`(,_ :core ,_ . ,_) nil)       ;not supported
          (_ (list spec))))
      specs)
     (current-buffer))
    (write-region nil nil (expand-file-name "elpa-packages.eld" elpaa--release-subdir))
    (write-region nil nil (expand-file-name "elpa-packages.eld" elpaa--devel-subdir))))

(defun elpaa-batch-make-all-packages (&rest _)
  "Check all the packages and build the relevant new tarballs."
  (let ((specs (elpaa--get-specs)))
    (dolist (spec specs)
      (condition-case err
          (elpaa--make-one-package spec)
        (error (message "Build error for %s: %S" (car spec) err))))
    (elpaa--publish-package-specs specs)))

(defun elpaa-batch-make-one-package (&rest _)
  "Build the new tarballs (if needed) for one particular package."
  (while command-line-args-left
    (elpaa--make-one-package (elpaa--get-package-spec
                                (pop command-line-args-left)))))

(defun elpaa-batch-make-one-tarball (&rest _)
  "Build a tarball for a particular package."
  (while command-line-args-left
    (let* ((tarball (pop command-line-args-left))
           (pkgname (file-name-sans-extension tarball))
           (pkg-spec (elpaa--get-package-spec pkgname)))
      (delete-file tarball)
      (elpaa--make-one-package pkg-spec tarball))))

(defun elpaa--string-width (str)
  "Determine string width in pixels of STR."
  (with-temp-buffer
    ;; Current (2021) ImageMagick recommends using the "magick"
    ;; driver, rather than "convert" directly, but Debian doesn't
    ;; provide it yet.
    (elpaa--call (current-buffer)
                 "convert" "-debug" "annotate" "xc:" "-font" "DejaVu-Sans"
                 "-pointsize" "110" "-annotate" "0" str "null:")
    (goto-char (point-min))
    (if (not (re-search-forward "Metrics:.*?width: \\([0-9]+\\)"))
        (error "Could not determine string width")
      (let ((width (string-to-number (match-string 1))))
        ;; This test aims to catch the case where the font is missing,
        ;; but it seems it only works in some cases :-(
        (if (and (> (string-width str) 0) (not (> width 0)))
            (progn (message "convert:\n%s" (buffer-string))
                   (error "Could not determine string width"))
          width)))))

(defun elpaa--make-badge (file left right)
  "Make badge svg FILE with LEFT and RIGHT string."
  (let* ((lw (elpaa--string-width left))
         (rw (elpaa--string-width right))
         (pad (elpaa--string-width "x"))
         (color "#bb3955")
         (width (/ (+ lw rw (* 4 pad)) 10))
         (offset -10) ;; Small alignment correction
         (ctx `((offset . ,offset)
                (left . ,left)
                (right . ,right)
                (lw . ,lw)
                (rw . ,rw)
                (width . ,width)
                (color . ,color)
                (pad . ,pad))))
    ;; FIXME: Use `svg.el'?
    (with-temp-buffer
      (insert
       (replace-regexp-in-string
        "{\\([^}]+\\)}"
        (lambda (str)
          (elpaa--html-quote
           (format "%s" (eval (read (match-string 1 str)) ctx))))
        (eval-when-compile
          (replace-regexp-in-string
           "[ \t\n]+" " "
           (replace-regexp-in-string
            "'" "\""
            "<?xml version='1.0'?>
<svg xmlns='http://www.w3.org/2000/svg'
     xmlns:xlink='http://www.w3.org/1999/xlink'
     width='{width}'
     height='20'
     role='img'
     aria-label='{left}: {right}'>
  <title>{left}: {right}</title>
  <linearGradient id='s' x2='0' y2='100%'>
    <stop offset='0' stop-color='#bbb' stop-opacity='.1'/>
    <stop offset='1' stop-opacity='.1'/>
  </linearGradient>
  <clipPath id='r'>
    <rect width='{width}' height='20' rx='3' fill='#fff'/>
  </clipPath>
  <g clip-path='url(#r)'>
    <rect width='{(/ (+ lw (* 2 pad)) 10)}'
          height='20' fill='#555'/>
    <rect x='{(1- (/ (+ lw (* 2 pad)) 10))}'
          width='{width}' height='20' fill='{color}'/>
    <rect width='{width}' height='20' fill='url(#s)'/>
  </g>
  <g fill='#fff'
     text-anchor='middle'
     font-family='Verdana,Geneva,DejaVu Sans,sans-serif'
     font-size='110'
     text-rendering='geometricPrecision'>
    <text aria-hidden='true'
          x='{(+ (/ lw 2) pad offset)}'
          y='150'
          fill='#010101' fill-opacity='.3'
          transform='scale(.1)' textLength='{lw}'>{left}</text>
    <text x='{(+ (/ lw 2) pad offset)}'
          y='140' transform='scale(.1)'
          fill='#fff'
          textLength='{lw}'>{left}</text>
    <text aria-hidden='true'
          x='{(+ lw (/ rw 2) (* 3 pad) offset)}'
          y='150'
          fill='#010101'  fill-opacity='.3'
          transform='scale(.1)' textLength='{rw}'>{right}</text>
    <text x='{(+ lw (/ rw 2) (* 3 pad) offset)}'
          y='140'
          transform='scale(.1)'
          fill='#fff' textLength='{rw}'>{right}</text>
  </g>
</svg>")))))
      (write-region (point-min) (point-max) file))))

(defun elpaa--make-one-package (pkg-spec &optional tarball-only)
  "Build the new tarballs (if needed) for PKG-SPEC.
If TARBALL-ONLY is non-nil, don't try and select some other revision and
place the resulting tarball into the file named TARBALL-ONLY."
  (elpaa--message "Checking package %s for updates..." (car pkg-spec))
  (let* ((pkgname (car pkg-spec))
         (dir (expand-file-name pkgname "packages"))
         (_ (cond
             (tarball-only nil)
             ((eq (nth 1 pkg-spec) :core) (elpaa--core-package-sync pkg-spec))
             (t (elpaa--worktree-sync pkg-spec))))
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
             (tarball (or tarball-only
                          (concat elpaa--devel-subdir
                                  (format "%s-%s.tar" pkgname devel-vers))))
             (new
              (let ((elpaa--name (concat elpaa--name "-devel")))
                ;; Build the archive-devel tarball.
                (elpaa--make-one-tarball tarball
                                         dir pkg-spec
                                         `(nil ,devel-vers
                                               . ,(nthcdr 2 metadata))
                                         nil tarball-only)))
             (rolling-release (elpaa--spec-get pkg-spec :rolling-release)))

        ;; Try and build the latest release tarball.
        (cond
         (tarball-only nil)
         ((equal vers "0")
          (elpaa--message "Package %s not released yet!" pkgname))
         ;; negative version numbers are used for pre-releases
         ;; (i.e. snapshots, alpha, beta, and rc).
         ((< (apply #'min (version-to-list vers)) 0)
          (cond
           ((not (or new
                     ;; Even if there's nothing new on the devel branch,
                     ;; there can be something new on the release branch.
                     (elpaa--spec-get pkg-spec :release-branch)))
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
                       tarball dir pkg-spec (car last-rel)
                       (lambda () (cdr last-rel)))
                  ;; FIXME: This `metadata' reflects that of the HEAD rather
                  ;; than that of the release commit.  It might actually be
                  ;; beneficial in case the `Maintainer:' was updated after
                  ;; the release commit, but it can probably bite us :-(
                  (elpaa--release-email pkg-spec metadata dir)))))))
         ((and (stringp rolling-release)
               (not (version= rolling-release vers)))
          (elpaa--message "Expected version %s, but got %s for package %s!"
                          rolling-release vers  pkgname))
         (t
          (when rolling-release
            (setq vers devel-vers))
          (let ((tarball (concat elpaa--release-subdir
                                 (format "%s-%s.tar" pkgname vers))))
            (when (elpaa--make-one-tarball
                   tarball dir pkg-spec vers
                   (lambda ()
                     (elpaa--get-release-revision
                      dir pkg-spec vers
                      (plist-get (cdr pkg-spec) :version-map))))
              (elpaa--release-email pkg-spec metadata dir)))))))))

(defun elpaa--call (destination program &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (elpaa--message "call-process %s %S" program args)
  (apply #'call-process program nil destination nil (delq nil args)))

(defconst elpaa--bwrap-args
  '("--unshare-all"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"))

(defvar elpaa--sandbox-ro-binds
  '("/lib" "/lib64" "/bin" "/usr" "/etc/alternatives" "/etc/emacs" "/gnu"))

(defun elpaa--call-sandboxed (destination &rest args)
  "Like ‘elpaa--call’ but sandboxed.
More specifically, uses Bubblewrap such that the command is
confined to only have write access to the `default-directory'.
Signal an error if the command did not finish with exit code 0."
  (if (not elpaa--sandbox)
      (apply #'elpaa--call destination args)
    (elpaa--message "call-sandboxed %S" args)
    (let ((dd (expand-file-name default-directory))) ;No `~' allowed!
      (setq args (nconc `("--bind" ,dd ,dd) args)))
    ;; Add read-only dirs in reverse order.
    (dolist (b (append elpaa--sandbox-ro-binds
                       elpaa--sandbox-extra-ro-dirs))
      (when (file-exists-p b)         ;`brwap' burps on binds that don't exist!
        (setq b (expand-file-name b))
        (setq args (nconc `("--ro-bind" ,b ,b) args))))
    (let ((exitcode
           (apply #'elpaa--call destination "bwrap"
                  (append elpaa--bwrap-args args))))
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

(defvar version-regexp-alist version-regexp-alist) ;; Make it writable!
(add-to-list 'version-regexp-alist '("^[-.+ ]*beta-?$" . -2)) ;"1.0.0-beta-3"
(add-to-list 'version-regexp-alist '("^[-.+ ]*dev$" . -4))    ;2.5-dev

(defun elpaa--metadata (dir pkg-spec)
  "Return a list (SIMPLE VERSION DESCRIPTION REQ EXTRAS).
SIMPLE is non-nil if the package is simple;
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

(defun elpaa--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (unless (file-exists-p pkg-file)
      (error "File not found: %s" pkg-file))
    (elpaa--form-from-file-contents pkg-file)))

(defun elpaa--write-pkg-file (pkg-dir name metadata &optional revision)
  (setf (alist-get :commit (nth 4 metadata))
        (or revision
            ;; FIXME: Emacs-26's `vc-git-working-revision' ignores its
            ;; arg and uses the `default-directory' to get the revision.
            ;; Similar to the kludge in `elpaa--select-revision'.
            (let ((default-directory pkg-dir))
              (vc-working-revision pkg-dir))))
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

(defun elpaa--write-plain-readme (pkg-dir pkg-spec)
  "Render a plain text readme from PKG-SPEC in PKG-DIR.
This is only done if necessary, that is if the readme contents
are not already taken to be formatted in plain text or when the
readme file has an unconventional name"
  (let ((readme-content (elpaa--get-README pkg-spec pkg-dir)))
    (cond
     ((eq (car readme-content) 'text/x-org)
      (let ((rendered (elpaa--section-to-plain-text readme-content)))
        (write-region rendered nil (expand-file-name "README-elpa" pkg-dir))
        rendered))
     ((let* ((readme-file (elpaa--spec-get pkg-spec :readme))
             (known-readme-names            ;see `package--get-description'
              '("README-elpa"
                "README-elpa.md"
                "README"
                "README.rst"
                "README.org")))
        (when (and readme-file
                   (not (eq readme-file 'ignore))
                   (not (member readme-file known-readme-names)))
          (let ((default-directory pkg-dir))
            ;; It's tempting to use a symlink, but our tarballs should not
            ;; contain symlinks (so they work under w32, for instance,
            ;; and also because I'm not sure how well tar-untar-buffer
            ;; handles symlinks).
            (copy-file readme-file "README-elpa"))
          (cdr readme-content))))
     ((cdr readme-content)))))

(defun elpaa-batch-generate-description-file (&rest _)
  "(Re)build the <PKG>-pkg.el file for particular packages."
  (while command-line-args-left
    (let* ((file (pop command-line-args-left))
           (dir (file-name-directory file))
           (pkg (file-name-nondirectory (directory-file-name dir)))
           (pkg-spec (elpaa--get-package-spec pkg 'noerror)))
      (elpaa--write-pkg-file dir pkg
                             (elpaa--metadata dir pkg-spec)))))

;;; Make the HTML pages for online browsing.

(defun elpaa--html-header (title &optional header head-extra)
  (format "<!DOCTYPE HTML PUBLIC>
<html lang=\"en\" xml:lang=\"en\">
    <head>
        <title>%s</title>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <link rel=\"shortcut icon\" type=\"image/png\" href=\"../favicon.png\">
        <link rel=\"stylesheet\" href=\"//code.cdn.mozilla.net/fonts/fira.css\">
        <link rel=\"stylesheet\" type=\"text/css\" href=\"../layout.css\">%s
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
          title (or head-extra "") (or header title)))

(defvar elpaa--index-javascript-headers "
        <script src=\"../javascript/jquery.min.js\" type=\"text/javascript\"></script>
        <script src=\"../javascript/jquery.filtertable.min.js\" type=\"text/javascript\"></script>
        <script src=\"../javascript/package-search.js\" type=\"text/javascript\"></script>")

(defun elpaa--html-footer ()
  (format "\n
        <div class=\"footer\">
            <div class=\"container\">
                <p>Last refreshed on %s</p>
                <p>Copyright 2016-%s <a href=\"https://fsf.org\">Free Software Foundation</a>, Inc.</p>
                <p>Design provided by <a href=\"https://nicolas.petton.fr\">Nicolas Petton</a></p>
                <p>
                  This website is licensed under the
                  <a href=\"https://creativecommons.org/licenses/by-nd/4.0/\">CC BY-ND 4.0</a>
                  International License.
                </p>
                <p><a href=\"/jslicense.html\" data-jslicense=\"1\">JavaScript Licenses</a></p>
            </div>
        </div>

</body>\n"
          (format-time-string "%Y-%b-%d %R %Z" nil t)
          (format-time-string "%Y" nil t)))

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

(cl-defgeneric elpaa--section-to-plain-text (section)
  "Return SECTION as plain text.
SECTION should be a cons as returned by `elpaa--get-section',
which see."
  (cdr section))

(cl-defmethod elpaa--section-to-plain-text ((section (head text/x-org)))
  (elpaa--export-org (cdr section) 'ascii
                     :ext-plist (append '(:ascii-charset utf-8)
                                        elpaa--org-export-options)))

(cl-defgeneric elpaa--section-to-html (section)
  "Return SECTION as HTML.
SECTION should be a cons as returned by `elpaa--get-section',
which see."
  (concat "<pre>\n"
          (elpaa--html-quote (cdr section))
          "\n</pre>\n"))

(cl-defmethod elpaa--section-to-html ((section (head text/x-org)))
  ;; FIXME: When the HTML refers to files (typically pictures), we should
  ;; make those links works.
  (elpaa--export-org (cdr section) 'html
                     :body-only t
                     :ext-plist (append '(:html-toplevel-hlevel 3)
                                        elpaa--org-export-options)))

(defvar elpaa-markdown-command
  (if (executable-find "markdown2")
      ;; Presumably https://github.com/trentm/python-markdown2.
      ;; Stay conservative in the set of extensions we support.
      '("markdown2" "-x" "code-friendly,tables")
    '("markdown")))

(cl-defmethod elpaa--section-to-html ((section (head text/markdown)))
  (with-temp-buffer
    (let ((input-filename
           (make-temp-file (expand-file-name "elpaa--export-input"))))
      (unwind-protect
          (progn
            (write-region (cdr section) nil input-filename)
            (apply #'elpaa--call-sandboxed t
                   `(,@elpaa-markdown-command ,input-filename)))
        (delete-file input-filename)))
    ;; Adjust headings since this HTML fragment will be inserted
    ;; inside an <h2> section.
    ;; FIXME: It would be much better to tell the conversion tool
    ;; to do that, or maybe to use CSS to get the same result.
    ;; Especially since this naive regexp search may match
    ;; false positives!
    (goto-char (point-min))
    (while (re-search-forward "</?h\\([1-9][0-9]*\\)>" nil t)
      (replace-match (number-to-string
                      (+ 2 (string-to-number (match-string 1))))
                     t t nil 1))
    (buffer-string)))

(defun elpaa--extension-to-mime (ext)
  (pcase ext
    ;; FIXME: On my Debian machine, `mailcap-extension-to-mime' tells me
    ;; "org" is `application/vnd.lotus-organizer'.
    ("org" 'text/x-org)
    ;; FIXME: Apparently on some systems, `mailcap-extension-to-mime'
    ;; returns nil for this one.
    ((or "md" "markdown") 'text/markdown)
    (_
     (require 'mailcap)
     (let ((mt (if ext (mailcap-extension-to-mime ext))))
         (if mt (intern mt) 'text/plain)))))

(defun elpaa--get-section (header file srcdir pkg-spec)
  "Return specified section for PKG-SPEC.
Returns (TYPE . CONTENT) cons, where TYPE is a MIME-type string,
and CONTENT is the content string.  If FILE is readable in
SRCDIR, return its contents.  Otherwise return section under
HEADER in package's main file."
  (when (consp file)
    (while (cdr-safe file)
      (setq file
            (if (file-readable-p (expand-file-name (car file) srcdir))
                (car file)
              (cdr file))))
    (when (consp file) (setq file (car file))))
  (cond
   ((when file
      (let ((file (expand-file-name file srcdir)))
        (and (file-readable-p file) (file-regular-p file))))
    ;; Return FILE's contents.
    (let ((type (elpaa--extension-to-mime (file-name-extension file)))
          (content (with-temp-buffer
                     (insert-file-contents (expand-file-name file srcdir))
                     (buffer-string))))
      (cons type content)))
   ((file-readable-p (expand-file-name (elpaa--main-file pkg-spec) srcdir))
    ;; Return specified section from package's main source file.
    (with-temp-buffer
      (let ((type 'text/plain))
        (insert-file-contents
         (expand-file-name (elpaa--main-file pkg-spec) srcdir))
        (emacs-lisp-mode)       ;lm-section-start needs the outline-mode setting.
        (let ((start (lm-section-start header)))
          (when start
            ;; FIXME: Emacs<28 had a bug in `lm-section-end', so cook up
            ;; our own ad-hoc replacement.
            (goto-char start) (forward-line 1)
            (re-search-forward "^\\(;;;[^;\n]\\|[^; \n]\\)" nil t)
            (insert
             (prog1
                 (buffer-substring start (match-beginning 0))
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
            (cons type (buffer-string)))))))))

(cl-defun elpaa--export-org (content backend &key body-only ext-plist)
  "Return Org CONTENT as an exported string.
BACKEND and EXT-PLIST are passed to `org-export-as', which see.
Uses `elpaa--call-sandboxed', since exporting with Org may run
arbitrary code."
  (declare (indent defun))
  (cl-check-type backend symbol)
  (cl-assert (memq body-only '(nil t)) t
             "BODY-ONLY may only be nil or t")
  ;; "emacs --batch" loads site-init files, which may pollute output,
  ;; so we write it to a temp file.
  (let ((input-filename
         (make-temp-file (expand-file-name "elpaa--export-input")))
        (output-filename
         (make-temp-file (expand-file-name "elpaa--export-output"))))
    (unwind-protect
        (progn
          (write-region content nil input-filename)
          (with-temp-buffer
            (elpaa--call-sandboxed
             t "emacs" "--batch" "-l" (format "ox-%S" backend)
             input-filename
             "--eval" "(setq org-babel-confirm-evaluate-answer-no t)"
             "--eval" (format "(write-region (org-export-as '%s nil nil %S '%S) nil %S)"
                              backend body-only ext-plist output-filename)))
          (with-temp-buffer
            (insert-file-contents output-filename)
            (buffer-string)))
      (delete-file input-filename)
      (delete-file output-filename))))

(defun elpaa--get-README (pkg-spec dir)
  (let ((readme (elpaa--spec-get pkg-spec :readme
                                 '("README" "README.rst"
                                   ;; Most README.md files seem to be
                                   ;; currently worse than the Commentary:
                                   ;; section :-( "README.md"
                                   "README.org"))))
    (or (elpaa--get-section
         "Commentary" (unless (eq readme 'ignore) readme)
         dir pkg-spec)
        '(text/plain . "No description available."))))

(defun elpaa--get-NEWS (pkg-spec dir)
  (let* ((news
          (elpaa--get-section
           "News" (elpaa--spec-get pkg-spec :news
                                   '("NEWS" "NEWS.rst" "NEWS.md" "NEWS.org"))
           dir pkg-spec)))
    (if (< (length (cdr news)) 4000)
        news
      (with-temp-buffer
        (insert (cdr news))
        (goto-char (point-min))
        (forward-char 4000)
        (forward-line 1)
        (cons (car news)
              (concat (buffer-substring (point-min) (point))
                      "...\n...\n"))))))


(defun elpaa--html-quote (txt)
  (replace-regexp-in-string "<" "&lt;"
                            (replace-regexp-in-string "&" "&amp;" txt)))

(defun elpaa--insert-repolinks (pkg-spec url)
  (when url
    (insert (format "<dt>Website</dt> <dd><a href=%S>%s</a></dd>\n"
                    url (elpaa--html-quote url)))
    (when (string-match (elpaa--default-url-re) url)
      (setq url nil)))
  (let* ((git-sv "https://git.savannah.gnu.org/")
         (urls
          (if (eq (nth 1 pkg-spec) :core)
              (let* ((files (nth 2 pkg-spec))
                     (file (if (listp files)
                               (directory-file-name
                                (or (file-name-directory
                                     (try-completion "" files))
                                    ""))
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

(defun elpaa--get-docfiles (pkg-spec)
  (let ((files (elpaa--spec-get pkg-spec :doc)))
    (if (listp files) files (list files))))

(defun elpaa--doc-html-file (docfile)
  (concat (file-name-base docfile) ".html"))

(defun elpaa--html-insert-docs (pkg-spec)
  (let ((docfiles (reverse (plist-get (cdr pkg-spec) :internal--html-docs)))
	;; `html-dir' is relative to the tarball directory, so html
	;; references on mirrors work.  It does not include the
	;; package name, so cross references among package docs work.
	(html-dir (when elpaa--doc-subdirectory
	            (file-name-as-directory elpaa--doc-subdirectory))))
    (when (and docfiles html-dir
	       ;; FIXME: This dir is shared, so it will always exist.
	       ;; Should we use (expand-file-name pkg html-dir) instead?
               (file-readable-p html-dir)) ;; html doc files were built
      (insert "<dt>Manual</dt> <dd>\n")
      (dolist (doc docfiles)
	(let ((html-file (concat html-dir (cdr doc))))
	  (insert "<a href=\"" html-file "\">"
	          (car doc)
	          "</a>\n")
	  ;; FIXME: get link text from info direntry?
	  ))
      (insert "</dd>\n"))))

(defun elpaa--html-make-pkg (pkg pkg-spec files srcdir plain-readme)
  (let* ((name (symbol-name (car pkg)))
         (latest (package-version-join (aref (cdr pkg) 0)))
         (mainsrcfile (expand-file-name (elpaa--main-file pkg-spec) srcdir))
         (desc (aref (cdr pkg) 2)))
    (cl-assert (equal name (car pkg-spec)))
    (elpaa--make-badge (concat name ".svg")
                       (format "%s ELPA" elpaa--name)
                       (format "%s %s" name latest))
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
      (let ((maints (elpaa--get-prop "Maintainer" name srcdir mainsrcfile)))
        (elpaa--message "maints=%S" maints)
        (insert
         "<dt>Maintainer</dt> <dd>"
         (mapconcat (lambda (maint)
                      (when (consp maint)
                        (setq maint (concat (if (car maint) (concat (car maint) " "))
                                            "<" (cdr maint) ">")))
                      (elpaa--html-quote maint))
                    (if (or (null maints) (consp (car-safe maints)))
                        maints
                      (list maints))
                    ", ")
         "</dd>\n"))
      (elpaa--insert-repolinks
       pkg-spec
       (or (cdr (assoc :url (aref (cdr pkg) 4)))
           (elpaa--get-prop "URL" name srcdir mainsrcfile)))
      (insert (format "<dt>Badge</dt><dd><img src=\"%s.svg\"/></dd>\n" (elpaa--html-quote name)))
      (elpaa--html-insert-docs pkg-spec)
      (insert "</dl>")
      (insert (format "<p>To install this package, run in Emacs:</p>
                       <pre>M-x <span class=\"kw\">package-install</span> RET <span class=\"kw\">%s</span> RET</pre>"
                      name))
      (let* ((readme-content (elpaa--get-README pkg-spec srcdir))
             (readme-text plain-readme)
             (readme-html (elpaa--section-to-html readme-content))
             (readme-output-filename (concat name "-readme.txt")))
        (write-region readme-text nil readme-output-filename)
        (insert "<h2>Full description</h2>\n"
                "<div class=\"splice fulldescription\">\n"
                readme-html
                "\n</div>\n"))

      ;; (message "latest=%S; files=%S" latest files)
      (unless (< (length files) (if (zerop (length latest)) 1 2))
        (insert (format "<h2>Old versions</h2><table>\n"))
        (dolist (file
                 (sort files (lambda (f1 f2) (version<= (car f2) (car f1)))))
          (unless (equal (pop file) latest)
            (let ((attrs (file-attributes file)))
              (insert (format "<tr><td><a href=%S>%s</a></td><td>%s</td><td>%s</td>\n"
                              file (elpaa--html-quote file)
                              (format-time-string "%Y-%b-%d" (nth 5 attrs))
                              (elpaa--html-bytes-format (nth 7 attrs)))))))
        (insert "</table>\n</div>\n"))
      (let ((news (elpaa--get-NEWS pkg-spec srcdir)))
        (when news
          (insert "<h2>News</h2>\n"
                  "<div class=\"splice news\">\n"
                  (elpaa--section-to-html news)
                  "\n</div>\n")))
      (insert (elpaa--html-footer))
      (write-region (point-min) (point-max) (concat name ".html")))))

(defun elpaa--html-make-index (pkgs)
  (with-temp-buffer
    (insert (elpaa--html-header
             (concat elpaa--name " ELPA Packages")
             nil elpaa--index-javascript-headers))
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
        </div>")
    (insert (elpaa--html-footer))
    (write-region (point-min) (point-max) "index.html")))

(defun elpaa--pull (dirname)
  (let ((default-directory (elpaa--dirname dirname)))
    (with-temp-buffer
      ;; Undo any local changes to `<pkg>-pkg.el', in case it's under
      ;; version control.
      (let ((elpaa--debug nil))
        (elpaa--call t "git" "checkout" "--"
                     (concat (file-name-nondirectory dirname) "-pkg.el")))
      (erase-buffer)     ;Throw away the error message we usually get.
      (cond
       ((file-directory-p ".git")
        (message "Running git pull in %S" default-directory)
        (elpaa--call t "git" "pull"))
       ((file-exists-p ".git")          ;A worktree, presumably.
        (let ((status
               (with-temp-buffer
                 (let ((elpaa--debug nil))
                   (elpaa--call t "git" "status" "--branch" "--porcelain=2"))
                 (buffer-string))))
          (if (string-match (regexp-quote "\n# branch.ab +0 -0") status)
              (elpaa--message "%s up-to-date" dirname)
            (let* ((br (and (string-match
                             (concat "\n# branch.head \\("
                                     (regexp-quote elpaa--branch-prefix)
                                     ".*\\)")
                             status)
                            (match-string 1 status)))
                   (ortb (and br (concat "refs/remotes/origin/" br))))
              ;; Set upstream if applicable.
              (when (and
                     ;; Upstream not set yet!
                     (not (string-match "\n# branch.upstream" status))
                     ;; This is one of the "elpa-managed" branches.
                     br
                     ;; There is an upstream to set it to!
                     (elpaa--git-branch-p ortb))
                (elpaa--call t "git" "branch" "--set-upstream-to" ortb))
              (if (or (not ortb)        ;Not a worktree, presumably.
                      (elpaa--git-branch-p ortb))
		  (progn
		    (message "Updating worktree in %S" default-directory)
		    (elpaa--call t "git" "merge"))
	        (message "Not pushed to origin yet.  Not updating worktree"))))))
       (t (error "No .git in %S" default-directory)))
      (unless (and (eobp) (bobp))
        (message "Updated %s:%s%s" dirname
                 (if (and (eobp) (bolp)
                          (eq (line-beginning-position 0) (point-min)))
                     " " "\n")
                 (buffer-string))))))

;;; Maintain worktrees in the `packages' subdirectory

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
    (when (file-directory-p emacs-repo-root)
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

(defun elpaa--cleanup-packages (specs with-core)
  "Remove unknown subdirectories of `packages/'.
This is any subdirectory inside `packages/' that's not under
version control nor listed in SPECS.
If WITH-CORE is non-nil, it means we manage :core packages as well."
  (when (file-directory-p (expand-file-name "packages/"))
    (let ((default-directory (expand-file-name "packages/")))
      (dolist (dir (directory-files "."))
        (cond
         ((file-symlink-p dir)
          ;; There are normally no such thing, but the user may elect to
          ;; add symlinks to other projects.  If so, update them, as if they
          ;; were "our" worktrees.
          (when (file-directory-p (expand-file-name ".git" dir))
            (elpaa--pull dir)))
         ((not (file-directory-p dir))
          ;; We only add/remove plain directories in elpa/packages (not
          ;; symlinks).
          nil)
         ((member dir '("." "..")) nil)
         ((assoc dir specs) nil)        ;One of our packages.
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


(defun elpaa--worktree-sync (pkg-spec)
  "Sync worktree of PKG-SPEC."
  (let ((name (car pkg-spec))
        (default-directory (expand-file-name "packages/")))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (cond ((not (file-exists-p name))
	   (message "Cloning branch %s:" name)
           (let* ((branch (concat elpaa--branch-prefix name))
                  (add-branches
                   (lambda ()
                     (let ((pos (point)))
                       (elpaa--call t "git" "config"
                                    "--get-all" "remote.origin.fetch")
                       (unless (or (= (point) pos)
                                   (save-excursion
				     (re-search-backward "\\*$" pos t)))
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
	     (message "%s" output)))
          ((not (file-exists-p (concat name "/.git")))
           (message "%s is in the way of our worktree, please remove!" name))
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

(defun elpaa--core-package-sync (pkg-spec)
  "Sync core package from PKG-SPEC."
  (let*
      ((name (car pkg-spec))
       (file-patterns (elpaa--spec-get pkg-spec :core))
       (excludes (elpaa--spec-get pkg-spec :excludes))
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

(defun elpaa-add/remove/update-worktrees ()
  "Remove leftover worktrees and set worktrees for packages."
  (let ((command-line-args-left '("-")))
    (elpaa-batch-archive-update-worktrees)))

(defun elpaa-batch-archive-update-worktrees (&rest _)
  (let ((specs (elpaa--get-specs))
        (pkgs command-line-args-left)
        (with-core (elpaa--sync-emacs-repo))
        msg-done)
    (setq command-line-args-left nil)
    (if (equal pkgs '("-")) (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (assoc pkg specs))
             (kind (nth 1 pkg-spec)))
        (pcase kind
          (':url (elpaa--worktree-sync pkg-spec))
          (':core
           (if (not with-core)
               (unless msg-done
                 (setq msg-done t)
                 (message "No \"emacs\" subdir: skipping :core packages"))
             (elpaa--core-package-sync pkg-spec)))
          (_ (if pkg-spec
                 (message "Unknown package kind `%S' for %s" kind pkg)
               (message "Unknown package %s" pkg))))))))

;;; Check copyrights

(defun elpaa--copyright-files (pkg-spec)
  "Return the list of ELisp files in the package PKG-SPEC."
  (let* ((pkgname (car pkg-spec))
         (default-directory (elpaa--dirname pkgname "packages"))
         (ignores (elpaa--spec-get pkg-spec :ignored-files))
         (all-ignores '("." ".." ".git" ".dir-locals.el" ".mailmap"
                        ".github" ".travis.yml"
                        "test" "tests"))
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
  (let ((res '())
        (find-file-suppress-same-file-warnings t))
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
      (declare-function message-setup "message"
                        (headers &optional yank-action actions continue
                                 switch-function return-action))
      (declare-function message-send "message" (&optional arg))
      (let* ((version (nth 1 metadata))
             (pkgname (car pkg-spec))
             (name (capitalize pkgname))
             (maint (cdr (assq :maintainer (nth 4 metadata))))
             ;; `:maintainer' can hold a list or a single maintainer.
             (maints (if (consp (car maint)) maint (list maint)))
             (maint-emails
              (mapcar (lambda (x)
                        (let ((name  (car-safe x))
                              (email (cdr-safe x)))
                          (if (not (and (stringp email)
                                        (string-match "@" email)))
                              (progn
                                (message "Error, no email address: %S" x)
                                nil)
                            (while (and (stringp name)
                                        (string-match "[<@>,]" name))
                              (message "Error, weird char \"%s\" in name: %S"
                                       (match-string 0 name) name)
                              (setq name (replace-match " " t t name)))
                            (format "%s <%s>" name email))))
                      maints))
             (maintainers
              (mapconcat #'identity (delq nil maint-emails) ",")))
        (message-setup `((From    . ,elpaa--email-from)
                         (To      . ,elpaa--email-to)
                         (Subject . ,(format "[%s ELPA] %s version %s"
                                             elpaa--name name version))
                         ,@(unless (equal maintainers "")
                             `((Cc . ,maintainers)))
                         ,@(if elpaa--email-reply-to
                               `((Reply-To . ,elpaa--email-reply-to)))))
        (insert "Version " version
                " of package " name
                " has just been released in " elpaa--name " ELPA.
You can now find it in M-x list-packages RET.

" name " describes itself as:
  " (nth 2 metadata) "

More at " (elpaa--default-url pkgname))
        (let ((news (elpaa--get-NEWS pkg-spec dir)))
          (when news
            (insert "\n\nRecent NEWS:\n\n"
                    (elpaa--section-to-plain-text news))))
        ;; (pop-to-buffer (current-buffer)) (debug t)
        (message-send)
        ))))

;;; Build Info files from Texinfo

(defun elpaa--build-Info (pkg-spec dir tarball-dir)
  "Build info files for docs specified in :doc field of PKG-SPEC.
If `elpa--doc-subdirectory' is non-nil, also build html files.
DIR is the package directory.  TARBALL-DIR is an absolute
directory; one of archive, archive-devel."
  ;; default-directory is the GNUMakefile directory.
  (let ((docfiles (elpaa--get-docfiles pkg-spec))
	(html-dir
	 (when elpaa--doc-subdirectory
	   (elpaa--dirname
	    (car pkg-spec)
	    (expand-file-name elpaa--doc-subdirectory tarball-dir)))))
    (when html-dir
      (when (not (file-readable-p html-dir)) ;FIXME: Why bother testing?
	(make-directory html-dir t)))

    (plist-put (cdr pkg-spec) :internal--html-docs nil)
    (dolist (f docfiles)
      (elpaa--build-Info-1 pkg-spec f dir html-dir))))

(defun elpaa--makeinfo (input output &optional extraargs)
  (let* ((input-dir (file-name-directory input))
         (input-name (file-name-nondirectory input))
         (output-ext (file-name-extension output))
	 ;; The sandbox may not allow write access to the output,
         ;; so we first create the file inside the sandbox and then
         ;; move it to its intended destination.
	 (tmpfile
	  (concat (make-temp-name (expand-file-name "doc" input-dir))
	          (if output-ext (concat "." output-ext)))))
    (elpaa--temp-file tmpfile)
    (with-temp-buffer
      ;; We change directory to that of the input file, because
      ;; `@include' searches for the files relative to PWD rather than
      ;; relative to the includer-file's location (this apparently
      ;; only applies to files whose name starts with `.' or `..'), so
      ;; we make the two dirs the same, to reduce the risk of problems.
      (let ((default-directory
             (if input-dir (expand-file-name input-dir)
               default-directory)))
        (apply #'elpaa--call-sandboxed
               t "makeinfo" "--no-split" input-name "-o" tmpfile extraargs))
      (message "%s" (buffer-string)))
    (elpaa--message "Renaming %S => %S" tmpfile output)
    (rename-file tmpfile output t)))

(defun elpaa--html-build-doc (pkg-spec docfile html-dir)
  (setq html-dir (directory-file-name html-dir))
  (let* ((destname (elpaa--doc-html-file docfile))
	 (html-file (expand-file-name destname html-dir))
	 (html-xref-file
	  (expand-file-name destname (file-name-directory html-dir))))
    (elpaa--makeinfo docfile html-file '("--html"))
    ;; FIXME: Use `push' in Emacs≥28
    (plist-put (cdr pkg-spec)
               :internal--html-docs
               (cons (cons (file-name-base html-file)
                           (file-name-nondirectory html-file))
                     (plist-get (cdr pkg-spec) :internal--html-docs)))

    ;; Create a symlink from elpa/archive[-devel]/doc/* to
    ;; the actual file, so html references work.
    (with-demoted-errors "%S" ;; 'make-symbolic-link' doesn't work on Windows
      (make-symbolic-link
       (concat (file-name-nondirectory html-dir) "/" destname)
       html-xref-file t))))

(defun elpaa--build-Info-1 (pkg-spec docfile dir html-dir)
  "Build an info file from DOCFILE (a texinfo source file).
DIR must be the package source directory.  If HTML-DIR is
non-nil, also build html files, store them there.  HTML-DIR is
relative to elpa root."
  (let* ((elpaa--sandbox-ro-binds
          (cons default-directory elpaa--sandbox-ro-binds))
         (default-directory (elpaa--dirname dir))
         (tmpfiles '()))

    (when (and docfile (string-match "\\.org\\'" docfile))
      (unless (file-readable-p docfile) (error "Can't read file: %s" docfile))
      (with-temp-buffer
        (elpaa--call-sandboxed
         t "emacs" "--batch" "-l" "ox-texinfo"
         ;; When building :core packages, don't follow the symlink,
         ;; otherwise Org will want to export into the Emacs tree!
         "--eval" "(setq vc-follow-symlinks nil)"
         docfile
         "--eval" "(setq org-babel-confirm-evaluate-answer-no t)"
         "--eval" "(message \"ELPATEXI=%s\" (org-texinfo-export-to-texinfo))")
        (message "%s" (buffer-string))
        (goto-char (point-max))
        (when (re-search-backward "ELPATEXI=\\(.*\\)\n?" nil t)
          (setq docfile (concat (file-name-directory docfile)
                                (match-string 1)))
          (push docfile tmpfiles)
          (elpaa--temp-file docfile)))

      ;; FIXME: also build html from org source.
      )

    (when (and docfile (string-match "\\.texi\\(nfo\\)?\\'" docfile))
      (let ((info-file (concat
                        (file-name-sans-extension
                         (file-name-nondirectory docfile))
                        ".info")))
        (elpaa--temp-file info-file)
        (elpaa--makeinfo docfile info-file)

	(when html-dir (elpaa--html-build-doc pkg-spec docfile html-dir))

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

    (mapc #'delete-file tmpfiles)     ;Avoid intermediate files in the tarball.

    (when (and docfile (file-readable-p docfile))
      (let ((dir-file (expand-file-name "dir")))
        (elpaa--temp-file dir-file)
        (with-temp-buffer
          (elpaa--call-sandboxed
           t "install-info" (concat "--dir=" dir-file) docfile)
          (message "%s" (buffer-string)))))))

(defun elpaa--make (pkg-spec dir)
  (let ((target (elpaa--spec-get pkg-spec :make))
        (cmd (elpaa--spec-get pkg-spec :shell-command)))
    (when (or cmd target)
      (with-temp-buffer
        (let ((elpaa--sandbox-ro-binds
               (cons default-directory elpaa--sandbox-ro-binds))
              (default-directory (elpaa--dirname dir)))
          (when cmd
            (elpaa--call-sandboxed t shell-file-name
                                   shell-command-switch
                                   cmd))
          (when target
            (apply #'elpaa--call-sandboxed t "make"
                   (if (consp target) target (list target))))
          (elpaa--message "%s" (buffer-string)))))))

;;; Fetch updates from upstream

(defun elpaa--branch (pkg-spec)
  (elpaa--spec-get pkg-spec :branch))

(defun elpaa--urtb (pkg-spec &optional branch)
  "Return our upstream remote tracking branch for PKG-SPEC."
  (format "refs/remotes/upstream/%s/%s" (car pkg-spec) (or branch "main")))

(defun elpaa--ortb (pkg-spec)
  "Return our origin remote tracking branch for PKG-SPEC."
  ;; We can't use the shorthand "origin/%s%s" when we pass it to
  ;; `git-show-ref'.
  (format "refs/remotes/origin/%s%s" elpaa--branch-prefix (car pkg-spec)))

(defun elpaa--git-branch-p (branch)
  "Return non-nil iff BRANCH is an existing branch."
  (equal 0 (elpaa--call t "git" "show-ref" "--verify" "--quiet" branch)))

(defun elpaa--is-ancestor (candidate rev)
  "Return non-nil if CANDIDATE is ancestor of REV."
  (zerop (elpaa--call t "git" "merge-base" "--is-ancestor"
                      candidate rev)))

(defun elpaa--fetch (pkg-spec &optional k show-diverged)
  (let* ((pkg (car pkg-spec))
         (url (elpaa--spec-get pkg-spec :url))
         (branch (elpaa--branch pkg-spec))
         (release-branch (elpaa--spec-get pkg-spec :release-branch))
         (ortb (elpaa--ortb pkg-spec))
         (urtb (elpaa--urtb pkg-spec))
         (refspec (if branch (format "+refs/heads/%s:%s" branch urtb)
                    (format "+HEAD:%s" urtb)))
         (release-refspec (if release-branch
                              (format "+refs/heads/%s:%s"
                                      release-branch
                                      (elpaa--urtb pkg-spec "release")))))
    (if (not url)
        (unless (elpaa--spec-member pkg-spec :url)
          (message "No upstream URL in %s for %s" elpaa--specs-file pkg))
      (message "Fetching updates for %s..." pkg)
      (with-temp-buffer
        (cond
         ((not (equal 0 (apply #'elpaa--call
                               t "git" "fetch" "--no-tags"
                               url refspec
                               (if release-refspec
                                   (list release-refspec)))))
          (message "Fetch error for %s:\n%s" pkg (buffer-string)))
	 ((not (elpaa--git-branch-p ortb))
	  (message "New package %s hasn't been pushed to origin yet" pkg)
	  (when k (funcall k pkg-spec)))
         ((elpaa--is-ancestor urtb ortb)
          (message "Nothing new upstream for %s" pkg))
         ((not (or (elpaa--is-ancestor ortb urtb)
                   (elpaa--spec-get pkg-spec :merge)))
          (message "Upstream of %s has DIVERGED!\n" pkg)
          (when show-diverged
            (elpaa--call t "git" "log"
                         "--format=%h  %<(16,trunc)%ae  %s"
                         (format "%s..%s" urtb ortb))
            (message "  Local changes:\n%s" (buffer-string))
            (erase-buffer)
            (elpaa--call t "git" "log"
                         "--format=%h  %<(16,trunc)%ae  %s"
                         (format "%s..%s" ortb urtb))
            (message "  Upstream changes:\n%s" (buffer-string))))
         ((not (zerop (elpaa--call t "git" "log"
                                   "--format=%h  %<(16,trunc)%ae  %s"
                                   (format "%s..%s" ortb urtb))))
          (message "Log error for %s:\n%s" pkg (buffer-string)))
         ((eq (point-min) (point-max))
          (message "No pending upstream changes for %s" pkg)
          (error "Empty log but there is something upstream!?\n%S\n%S"
                 pkg-spec (buffer-string)))
         (t (message "%s" (buffer-string))
            (when k (funcall k pkg-spec))))))))

(defun elpaa--merge (pkg-spec urtb ortb)
  "Return the merge branch, or nil upon failure."
  (if (not (file-directory-p "packages"))
      (progn
        (message "Can't find the 'packages' directory in: %S"
                 default-directory)
        nil)
    (let* ((pkg (car pkg-spec))
           (wt (expand-file-name pkg "packages"))
           (merge-branch (concat "elpa--merge/" pkg))
           last-release)
      ;; When the upstream changes includes changes to `Version:'), try to
      ;; merge only up to the first (new) revision that made such a change,
      ;; so that we hopefully get a merge commit from which to make the release.
      ;; The rest will be merged (hopefully) next time around.
      (while (and (setq last-release
                        (elpaa--get-last-release-commit pkg-spec
                                                        (concat urtb "~")))
                  (not (elpaa--is-ancestor last-release ortb)))
        (message "NOTE: merging from %s only up to release %s!!"
                 urtb last-release)
        (setq urtb last-release))
      (if (file-directory-p wt)
          (progn (message "Worktree exists already for merge of %S" pkg)
                 nil)
        (when (elpaa--git-branch-p (concat "refs/heads/" merge-branch))
          (elpaa--call t "git" "branch" "-D" merge-branch))
        (unwind-protect
            (progn
              (elpaa--call t "git" "worktree" "add" "-b" merge-branch wt ortb)
              (let ((default-directory (file-name-as-directory wt)))
                (when (zerop (elpaa--call t "git" "merge" urtb))
                  merge-branch)))
          (elpaa--call t "git" "worktree" "remove" "--force" wt))))))

(defun elpaa--push (pkg-spec)
  (let* ((pkg (car pkg-spec))
         (release-branch (elpaa--spec-get pkg-spec :release-branch))
         (ortb (elpaa--ortb pkg-spec))
         (ortb-p (elpaa--git-branch-p ortb))
         (urtb (elpaa--urtb pkg-spec))
         (merge (elpaa--spec-get pkg-spec :merge)))
    ;; FIXME: Arrange to merge if it's not a fast-forward.
    (with-temp-buffer
      (cond
       ((and ortb-p (elpaa--is-ancestor urtb ortb))
        (message "Nothing to push for %s" pkg))
       ((xor (and ortb-p (not (elpaa--is-ancestor ortb urtb)))
             merge)
        (if merge
            (message "Error: ':merge' used when not needed: %S\n%S"
                     pkg (buffer-string))
          (message "Can't push %s: not a fast-forward" pkg)))
       ((when merge
          ;; FIXME: This only handles merges on the devel branch.
          (not (setq urtb (elpaa--merge pkg-spec urtb ortb))))
        (message "Merge failure for %S:\n%S" pkg
                 (buffer-string)))
       ((equal 0 (elpaa--call
                  t "git" "push" "--set-upstream"
                  "origin"
                  (format "%s:refs/heads/%s%s"
                          urtb elpaa--branch-prefix pkg)
                  (when release-branch
                    (format "%s:refs/heads/%s%s"
                            (elpaa--urtb pkg-spec "release")
                            elpaa--release-branch-prefix pkg))))
        (message "Pushed %s successfully:\n%s" pkg (buffer-string))
        (when (file-directory-p (expand-file-name pkg "packages"))
          (elpaa--worktree-sync pkg-spec)))
       (t
        (message "Push error for %s:\n%s" pkg (buffer-string)))))))

(defun elpaa--batch-fetch-and (k)
  (let* ((specs (elpaa--get-specs))
         (pkgs command-line-args-left)
         (show-diverged (not (cdr pkgs)))
         (condition ':))
    (setq command-line-args-left nil)
    (when (and (null (cdr pkgs)) (string-match "\\`:" (car pkgs)))
      (setq show-diverged nil)
      (setq condition (intern (car pkgs)))
      (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (assoc pkg specs)))
        (cond
         ((not pkg-spec) (message "Unknown package: %s" pkg))
         ((or (eq condition ':)
              (elpaa--spec-get pkg-spec condition))
          ;; (unless (file-directory-p (expand-file-name pkg "packages"))
          ;;   (elpaa--worktree-sync pkg-spec))
          (elpaa--fetch pkg-spec k show-diverged)))))))

(defun elpaa-batch-fetch-and-show (&rest _)
  (elpaa--batch-fetch-and #'ignore))

(defun elpaa-batch-fetch-and-push (&rest _)
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

;;; Make dependencies

(defun elpaa-batch-pkg-spec-make-dependencies ()
  (let ((dst (pop command-line-args-left)))
    (with-temp-buffer
      (dolist (pkg-spec (elpaa--get-specs))
        (let* ((pkgname (car pkg-spec))
               (dir (concat "packages/" pkgname)))
          (when (file-directory-p dir)
            (insert
             (format "%s/%s-pkg.el: %s/%s\n"
                     dir pkgname dir (elpaa--main-file pkg-spec)))
            (let ((make-targets (elpaa--spec-get pkg-spec :make)))
              (when (consp make-targets)
                (dolist (target make-targets)
                  (insert (format "%s: %s/%s\n" dir dir target))
                  (insert (format "%s/%s:\n\tcd %s; $(MAKE) %s\n"
                                  dir target dir target)))
                (insert (format "clean-submake/%s:\n\t$(RM) %s\n"
                                pkgname
                                (mapconcat (lambda (f) (concat dir "/" f))
                                           make-targets
                                           " ")))
                (insert (format "clean clean/%s: clean-submake/%s\n"
                                pkgname pkgname)))))))
      (write-region (point-min) (point-max) dst nil 'silent))))

;; Generate autoloads for in-place use

(defun elpaa-batch-generate-autoloads (&rest _)
  (let* ((alf (pop command-line-args-left))
         (dir (file-name-directory alf))
         (pkgname (file-name-nondirectory (directory-file-name dir)))
         (pkg-spec (elpaa--get-package-spec pkgname 'noerror))
         (lisp-dir (elpaa--spec-get pkg-spec :lisp-dir)))
    (require 'package)
    (if (null lisp-dir)
        (progn
          (cl-assert (equal alf (concat dir pkgname "-autoloads.el")))
          (package-generate-autoloads pkgname dir))
      (package-generate-autoloads pkgname (concat dir lisp-dir))
      (write-region
       (format "(load (concat (file-name-directory #$) %S) nil 'nomsg)\n"
               (concat lisp-dir "/" pkgname "-autoloads.el"))
       nil alf nil 'silent))))

;;; Main files

(defun elpaa-batch-main-files ()
  (let ((dstfile (pop command-line-args-left)))
    (with-temp-buffer
      (dolist (pkg-spec (elpaa--get-specs))
        (let* ((pkgname (car pkg-spec))
               (defmf (concat pkgname ".el"))
               (mf (elpaa--main-file pkg-spec)))
          (unless (equal mf defmf)
            (insert (format "%s:%s\n" pkgname mf)))))
      (write-region (point-min) (point-max)
                    dstfile nil 'silent))))

;;; Edit support for `elpa-packages'

(defun elpaa--sort-packages ()
  "Sort packages by alphabetical order."
  (interactive)
  (goto-char (point-min))
  (down-list 1)
  (sort-subr nil
             (lambda ()
               (unless (save-excursion
                         (forward-comment (point-max))
                         (looking-at "("))
                 (goto-char (point-max))))
             (lambda ()
               (forward-sexp 1)
               (skip-chars-forward " \t")
               (when (or (eolp) (looking-at ";"))
                 ;; A comment was found between the two entries.
                 ;; Since it's right after the end (on the same line),
                 ;; it belongs to this record, otherwise it belongs
                 ;; to the next.
                 (forward-line 1))
               (skip-chars-forward " \t\n")
               (skip-chars-backward " \t"))
             (lambda () (forward-comment (point-max)))))

(provide 'elpa-admin)

;; Local Variables:
;; nameless-current-name: "elpaa"
;; End:

;;; elpa-admin.el ends here
