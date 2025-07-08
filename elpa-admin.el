;;; elpa-admin.el --- Auto-generate an Emacs Lisp package archive  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2025  Free Software Foundation, Inc

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

;; - bug#73425: Allow images in readme and manual
;;   + Support for Org and Markdown readmes.
;;   + Support for HTML manual.
;;   - Doesn't work for Kubed's HTML manual.
;;   - Doesn't work for Info manuals if the Texinfo is not in root directory,
;;     such as Hyperbole's.
;; - bug#73425: Need a convenient way to specify parts of the pkg spec
;;   directly in the package, such as in the main file.
;;   Currently this can be done for `:ignored-files' via `.elpaignore',
;;   but it should be made more general.  E.g. for `:doc-files', `:doc',
;;   `:readme', ...

;; - bug#45345: [elpa-archive] "make build/<package>" should not pull
;;   unconditionally
;; - bug#45346: make it easier to ignore all the files in <directory>
;;   except for a few exceptions.
;; - support for conveniently rebuilding individual files like
;;   index.html, archive-contents, or <pkg>.html
;; - render the README and News in the HTML rather than as <pre> block!

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'map))
(require 'xml)
(require 'lisp-mnt)
(require 'package)


(defvar elpaa--release-subdir "archive/"
  "Subdirectory where the ELPA release files (tarballs, ...) will be placed.")
(defvar elpaa--devel-subdir "archive-devel/"
  "Subdirectory where the ELPA bleeding edge files (tarballs, ...) will be placed.")

(defvar elpaa--wsl-stats-file "wsl-stats.eld"
  "File where web-server access stats are kept.")

(defvar elpaa--name "NonGNU")
(defvar elpaa--gitrepo "emacs/nongnu.git")
(defvar elpaa--url "https://elpa.gnu.org/nongnu/")
(defvar elpaa--devel-url "https://elpa.gnu.org/nongnu-devel/")
(defvar elpaa--css-url "https://www.gnu.org/software/emacs/manual.css")

(defvar elpaa--branch-prefix "elpa/")
(defvar elpaa--release-branch-prefix "elpa-release/")

(defvar elpaa--specs-file "elpa-packages")
(defvar elpaa--copyright-file "copyright_exceptions")
(defvar elpaa--email-to nil) ;;"gnu-emacs-sources@gnu.org"
(defvar elpaa--email-from nil) ;;"ELPA update <do.not.reply@elpa.gnu.org>"
(defvar elpaa--email-reply-to nil)
(defvar elpaa--notification-email-bcc "elpasync@gnu.org")

(defvar elpaa--dependencies-archive-contents nil
  "List of `archive-contents' files.")

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

(defvar elpaa--sync-failures-dir "sync-failures/")

(defvar elpaa--aggregated-feed-filename ".aggregated-feed.xml")

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
    ;; In other words, point is already at bob.
    ;;- (goto-char (point-min))
    (read (current-buffer))))

(defun elpaa-read-config (file)
  (let ((config (elpaa--form-from-file-contents file)))
    (pcase-dolist (`(,var ,val) config)
      (cl-assert (or (stringp val) (booleanp val)
                     (and (consp val) (seq-every-p #'stringp val)))
                 t)
      (setf (pcase-exhaustive var
              ('doc-dir                 elpaa--doc-subdirectory)
              ((guard (boundp (intern (format "elpaa--%s" var))))
               (symbol-value (intern (format "elpaa--%s" var)))))
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

(defun elpaa--pkg-root (pkg-spec)
  (or (elpaa--spec-get pkg-spec :internal--pkg-root)
      (let ((dir (elpaa--dirname (format "%s" (car pkg-spec)) "packages")))
        (plist-put (cdr pkg-spec) :internal--pkg-root dir)
        dir)))

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

(defun elpaa--write-archive-contents (ac dir)
  "Write archive contents AC into directory DIR."
  (with-temp-buffer
    (pp ac (current-buffer))
    (write-region nil nil (expand-file-name "archive-contents" dir))))

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
    (elpaa--write-archive-contents ac dir)))

(defun elpaa--get-specs (&optional no-follow-links)
  (let ((specs (elpaa--form-from-file-contents elpaa--specs-file)))
    (unless no-follow-links
      (dolist (spec specs)
        (when (eq :url (nth 1 spec))
          (let ((url (nth 2 spec)))
            (when (and url (symbolp url) url)
              (let ((real-url (elpaa--spec-get (assq url specs) :url)))
                (if (not (stringp real-url))  ;No subpackages for `:url nil'.
                    (user-error "Invalid :url redirection: %S" spec)
                  (setf (nth 2 spec) real-url)
                  (push url (nthcdr 3 spec))
                  (push :parent--package (nthcdr 3 spec)))))))))
    specs))

(defun elpaa--spec-get (pkg-spec prop &optional default)
  (or (plist-get (cdr pkg-spec) prop) default))

(defun elpaa--spec-member (pkg-spec prop)
  (plist-member (cdr pkg-spec) prop))

(defun elpaa--main-file (pkg-spec)
  (or ;; (elpaa--spec-get pkg-spec :main-file)
      (let ((ldir (elpaa--spec-get pkg-spec :lisp-dir)))
        (format "%s%s.el"
                (if ldir (file-name-as-directory ldir) "")
                (car pkg-spec)))))

(defun elpaa--get-last-release-commit (pkg-spec &optional from)
  "Return the commit that last changed `Version:' for package PKG-SPEC.
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
    (let* ((default-directory (elpaa--pkg-root pkg-spec))
           (release-branch (elpaa--spec-get pkg-spec :release-branch))
           (L-spec (concat "/^;;* *\\(Package-\\)\\?Version:/,+1:"
                           (elpaa--main-file pkg-spec)))
           (search-start-rev
            (or (if release-branch
                    (format "refs/remotes/origin/%s"
                            (elpaa--local-branch-name pkg-spec t)))
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
      (let* ((oldbranch
              (with-temp-buffer
                (and (zerop (elpaa--call t "git" "branch" "--show-current"))
                     (> (buffer-size) 0)
                     (buffer-substring (point-min) (1- (point-max))))))
             (oldrev
              (unless oldbranch
                (with-temp-buffer
                  (when (zerop (elpaa--call t "git" "rev-parse" "HEAD"))
                    (buffer-substring (point-min) (1- (point-max))))))))
        (with-temp-buffer
          ;; Run it within the true-filename directory holding the mainfile,
          ;; so that for :core packages we properly affect the Emacs tree.
          (elpaa--call t "git" "checkout" "--detach" rev)
          (elpaa--message "Selected release revision %s\n%s"
                          rev (buffer-string)))
        (elpaa--temp-file
         (lambda ()
           (let ((default-directory (file-name-directory ftn)))
             (with-temp-buffer
               ;; Re-select the original branch/commit.
               (elpaa--call t "git" "clean" "-x" "-d" "-f")
               (elpaa--call t "git" "reset" "--hard" oldrev)
               (when oldbranch
                 (elpaa--call t "git" "checkout" oldbranch))
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
               (elpaa--call t "git" "reset" "--hard"))
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
              (when (cddr bucket) ;There's more than 1 entry in this bucket.
                (dolist (oldtarball (cdr bucket))
                  (let ((tvers (car oldtarball)))
                    (push oldtarball
                          (alist-get (substring tvers 0
                                                (min (length tvers) (1+ len)))
                                     newbuckets nil nil #'equal))))
                (when (< (+ (length newbuckets) (length (cdr buckets)))
                         (- n (length kept)))
                  ;; (message "Spreading one bucket into: %S" newbuckets)
                  (setq buckets (nconc (cdr buckets)
                                       (mapcar (lambda (b)
                                                 (cons (length (car b)) (cdr b)))
                                               newbuckets)))
                  t))))
        ;; Finally, evenly select elements from every bucket.
        (setq buckets (sort buckets (lambda (b1 b2) (<= (length b1) (length b2)))))
        (while buckets
          (let ((bucket-size (/ (- n (length kept)) (length buckets)))
                (bucket (cdr (pop buckets))))
            (setq kept (nconc (elpaa--keep-old bucket
                                               bucket-size)
                              kept))))
        kept))))))

(defun elpaa--dependencies-archive-contents ()
  (let ((ac nil))
    (dolist (file elpaa--dependencies-archive-contents)
      (let ((form (elpaa--form-from-file-contents file)))
        (if ac
            (nconc ac (cdr form))
          (setq ac form))))
    ac))

(defun elpaa--check-dependencies (metadata &optional ac)
  ;; We have various sources of data we could use here to find which
  ;; packages are available and at which version:
  ;;
  ;; - `archive-contents'.
  ;; - `pkg-specs'.
  ;; - main files's headers (requires `pkg-specs').
  ;;
  ;; For use by the package maintainers (who would likely only have
  ;; a partial clone of the repositories), we can at best count on
  ;; `pkg-specs' and that only for the current repository.  So we
  ;; should probably support fetching&updating a local copy of the
  ;; official (Non)GNU(-devel) ELPA's `archive-contents'.
  ;;
  ;; For use on elpa.gnu.org, we already have a local copy of
  ;; `archive-contents'.
  (when elpaa--dependencies-archive-contents
    (let ((reqs (nth 3 metadata)))
      (elpaa--message "Checking dependencies: %S" reqs)
      (pcase-dolist (`(,pkg ,vers) reqs)
        (if (assq pkg package--builtin-versions)
            ;; Don't bother checking versions for builtin packages, since
            ;; the package can legitimately depend on versions more recent than
            ;; the currently running Emacs.
            nil
          (unless ac (setq ac (elpaa--dependencies-archive-contents)))
          (let* ((ac-data (assq pkg ac))
                 (pkg-vers (if ac-data (aref (cdr ac-data) 0))))
            (cond
             ((and pkg-vers (version-list-<= vers pkg-vers)) nil)
             ((not ac-data)
              (error "Unknown required package: %S" pkg))
             (t (error "Required %S-%s > than available (%s)"
                       pkg (package-version-join vers)
                       (package-version-join pkg-vers))))))))))

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

(defun elpaa--report-failure ( pkg-spec metadata txt basename destdir
                               title-format msg)
  (let* ((pkg (car pkg-spec))
         (file (expand-file-name basename destdir)))
    (if (not txt)
        (delete-file file)
      (let ((prev-size (or (file-attribute-size (file-attributes file)) 0)))
        (write-region txt nil file nil 'silent)
        (when (and elpaa--email-to
                   (> (file-attribute-size (file-attributes file))
                      ;; Arbitrarily require a "8 chars increase" minimum
                      ;; so we don't resend a notification when the timestamp
                      ;; in the version got a bit longer.
                      (+ prev-size 8)))
          (let ((maintainers (elpaa--maintainers pkg-spec metadata)))
            (elpaa--send-email
             `((From	  . ,elpaa--email-from)
               (To	  . ,(or maintainers
                                 elpaa--notification-email-bcc))
               (Bcc	  . ,(when maintainers
                               elpaa--notification-email-bcc))
               (Subject . ,(concat (format "[%s ELPA] "  elpaa--name)
                                   (format title-format pkg))))
             (concat msg
                     "\n\n## The current error output was the following:\n\n"
                     txt))))))))

(defun elpaa--parent-package (pkg-spec)
  (or (elpaa--spec-get pkg-spec  :parent--package)
      (let ((url (elpaa--spec-get pkg-spec :url)))
        (and (symbolp url) url))))

(defun elpaa--local-branch-name (pkg-spec &optional releasep)
  "Return the name of the branch in which the package is kept.
This is the name of the branch as used in the (Non)GNU ELPA repository
as well as in the local clone, not upstream."
  (format "%s%s"
          (if (and releasep (elpaa--spec-get pkg-spec :release-branch))
              elpaa--release-branch-prefix
            elpaa--branch-prefix)
          (or (elpaa--parent-package pkg-spec)
              (car pkg-spec))))

(defun elpaa--check-sync-failures (pkg-spec metadata)
  (let* ((pkg (car pkg-spec))
         (basename (format "%s-sync-failure.txt" pkg))
         (syncfail-file (expand-file-name basename elpaa--sync-failures-dir)))
    ;; FIXME: Add a link from <PKG>.html to this status report?
    (elpaa--report-failure
     pkg-spec metadata
     (when (file-exists-p syncfail-file)
       (with-temp-buffer
         (insert-file-contents syncfail-file)
         (buffer-string)))
     basename elpaa--release-subdir
     "Sync failure: %s has diverged!"
     (format "The scripts failed to synchronize with the upstream version
because the two have diverged.  This is usually the result of an
overly-optimistic force-push.  Please refrain from using force-push
on such public branches.

The archive will not be able to track your code until you resolve this
problem by (re?)merging the code that's in %S.  You can do that
with the following commands:

    git fetch https://git.sv.gnu.org/git/%s %s
    git merge FETCH_HEAD

Of course, feel free to undo the changes it may introduce in the file
contents: we only need the metadata to indicate that this commit was merged.

You can consult the latest error output in
[the sync-failure file](%s%s)."
             elpaa--gitrepo elpaa--gitrepo
             (elpaa--local-branch-name pkg-spec)
             elpaa--url basename))))

(defun elpaa--report-build-failure (pkg-spec version destdir txt)
  (let* ((pkg (car pkg-spec))
         (basename (format "%s-build-failure.txt" pkg)))
    ;; FIXME: Add a link from <PKG>.html to this status report?
    (elpaa--report-failure
     pkg-spec nil txt basename destdir
     "Tarball build failure for %s"
     ;; FIXME: Compute the actual URL.  We currently can't
     ;; do that for the devel site (sadly, the most important
     ;; case) because we don't know its URL.
     (format
      "The build scripts failed to build the tarball
for version %s of the package %s.
You can consult the latest error output in the file
%S in the %s ELPA archive web site.

You can also try and reproduce the error locally as follows:

    git clone --single-branch https://git.sv.gnu.org/git/%s
    cd %s
    make %s           # Setup the infrastructure
    make packages/%s  # Create a worktree of the package
    make build/%s     # Build the tarballs into archive(-devel)/"
      version pkg basename elpaa--name
      elpaa--gitrepo
      (file-name-sans-extension (file-name-nondirectory elpaa--gitrepo))
      (make-string (string-width (format "%s" pkg)) ?\s)
      pkg pkg))))

(defun elpaa--make-one-tarball ( tarball dir pkg-spec metadata-or-version
                                 &optional revision-function tarball-only)
  "Create file TARBALL for PKG-SPEC if not done yet.
Return non-nil if a new tarball was created.  Also create some
auxiliary files unless TARBALL-ONLY is non-nil ."
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
		 ;; FIXME: We shouldn't rebuild the tarball but only the
		 ;; particular missing file(s)!!!
		 (and (file-readable-p (format "%s-readme.txt" pkgname))
                      (file-readable-p (format "%s.xml" pkgname))
		      (file-readable-p (format "%s.html" pkgname))
		      (file-readable-p (format "%s.svg" pkgname))))))
      (progn
        (elpaa--message "Tarball %s already built!" tarball)
        nil)
    (when (file-readable-p elpaa--copyright-file)
      ;; Eagerly load this file, so that any spurious "Followed link to" is
      ;; emitted here rather than being included in the build failure report.
      (find-file-noselect elpaa--copyright-file))
    (let ((msg-start (with-current-buffer "*Messages*" (point-marker)))
          (res nil))
      (message "======== Building tarball %s..." tarball)
      (unwind-protect
          (condition-case-unless-debug err
              (setq res (elpaa--make-one-tarball-1
                         tarball dir pkg-spec metadata-or-version
                         revision-function tarball-only))
            (error (message "Build error for %s: %S" tarball err)
                   nil))
        (message (if res "######## Built new package %s!"
                   "######## Build of package %s FAILED!!")
                 tarball)

        (let ((msg (unless res
                     (with-current-buffer (marker-buffer msg-start)
                       (buffer-substring msg-start (point-max)))))
              (version (if (consp metadata-or-version)
                           (nth 1 metadata-or-version)
                         metadata-or-version)))
          (elpaa--report-build-failure pkg-spec version
                                       (file-name-directory tarball)
                                       msg))))))


(defun elpa-admin--tar-command ()
  (if (and (memq system-type '(berkeley-unix darwin))
           (not (string-match-p (rx "tar (GNU tar)")
                                (shell-command-to-string "tar --version"))))
      (or (executable-find "gtar")
          (error "Please install GNU tar"))
    "tar"))

(defun elpaa--make-one-tarball-1 ( tarball dir pkg-spec metadata-or-version
                              &optional revision-function tarball-only)
  (elpaa--with-temp-files
   dir
   (let* ((destdir (file-name-directory tarball))
          (pkg (car pkg-spec))
          (pkgname (format "%s" pkg))
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
     (elpaa--check-dependencies metadata)
     (let ((process-environment (elpaa--makeenv vers revision)))
       ;; Run `make' before building the Info file, so that the `make'
       ;; rule can be used to build the Info/Texinfo file.
       (elpaa--make pkg-spec dir)
       (elpaa--build-Info pkg-spec dir destdir))
     (elpaa--write-pkg-file dir pkg metadata revision)
     (setq rendered (elpaa--write-plain-readme dir pkg-spec))
     ;; FIXME: Allow renaming files or selecting a subset of the files!
     (cl-assert (not (string-match "[][*\\|?]" pkgname)))
     (cl-assert (not (string-match "[][*\\|?]" vers)))
     (apply #'elpaa--call
            nil (elpa-admin--tar-command)
            `("--exclude-vcs"
              ,@(mapcar (lambda (i) (format "--exclude=packages/%s/%s" pkg i))
                        ignores)
              ,@(when (file-readable-p elpaignore) `("-X" ,elpaignore))
              ,@(mapcar (lambda (r) (elpaa--make-tar-transform pkgname r))
                        renames)
              "--transform"
              ,(format "s|^packages/%s|%s-%s|" pkg pkg vers)
              "-chf" ,tarball
              ,(format "packages/%s" pkg)))
     (cl-assert (file-readable-p tarball))
     (unless tarball-only
       (let* ((pkgdesc
               ;; FIXME: `elpaa--write-pkg-file' wrote the metadata to
               ;; <pkg>-pkg.el and then `elpaa--process-multi-file-package'
               ;; reads it back.  We could/should skip the middle man.
               (elpaa--process-multi-file-package
                dir pkg 'dont-rename)))
         (elpaa--message "%s: %S" pkg pkgdesc)
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
                                  elpaa--name pkg vers))))
         (let ((link (expand-file-name (format "%s.tar" pkg) destdir)))
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
           ;; This also creates <pkg>.xml (atom feed), <pkg>-readme.txt and <pkg>.svg.
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

(defun elpaa--core-files (pkg-spec)
  "Get a list of core files (and only files) for PKG-SPEC.
Core folders are recursively searched, excluded files are ignored."
  (let* ((file-patterns (ensure-list (elpaa--spec-get pkg-spec :core)))
         (excludes (elpaa--spec-get pkg-spec :excludes))
         (default-directory (expand-file-name "emacs/"))
         (core-files nil))

    ;; Ensure we look at files from a core package.
    (cl-assert file-patterns)

    ;; We look at each file or files in folder and add them
    ;; to core-files.
    (dolist (item file-patterns)
      (if (file-directory-p item)
          (setq core-files (nconc (directory-files-recursively item ".*")
                                  core-files))
        (push item core-files)))

    ;; Remove all files which match a wildcard in the excludes.
    (if (null excludes)
        core-files
      (let ((re (concat "\\(?: "
                        (mapconcat #'wildcard-to-regexp excludes "\\)\\|\\(?:")
                        "\\)")))
        (seq-remove
         (lambda (file-name)
           (string-match-p re file-name))
         core-files)))))

(defun elpaa--get-devel-version (dir pkg-spec)
  "Compute the date-based pseudo-version used for devel builds."
  (let* ((gitdate
          (with-temp-buffer
            (if (plist-get (cdr pkg-spec) :core)
                (let ((core-files (elpaa--core-files pkg-spec))
                      (default-directory (expand-file-name "emacs/")))
                  ;; For core packages, don't use the date of the last
                  ;; commit to the branch, but that of the last commit
                  ;; to the core files.
                  (apply #'elpaa--call t "git" "log" "--pretty=format:%cI" "--no-patch"
                         "-1" "--" core-files))
              ;; FIXME: Why follow symlinks?  I have the nagging feeling that
              ;; this used to be needed for the :core case only, so not needed
              ;; here any more.
              (let* ((ftn (file-truename      ;; Follow symlinks!
                           (expand-file-name (elpaa--main-file pkg-spec) dir)))
                     (default-directory (file-name-directory ftn)))
                (elpaa--call t "git" "show" "--pretty=format:%cI" "--no-patch")))
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

(defun elpaa--get-package-spec (pkg &optional pkg-specs noerror)
  "Retrieve the property list for PKG from `elpaa--specs-file'.
If `noerror' is non-nil return nil upon error, unless it's `guess' in
which case it returns a trivial pkg-spec,"
  (if (stringp pkg) (setq pkg (intern pkg)))
  (or (assoc-string pkg (or pkg-specs (elpaa--get-specs)) t)
      (pcase noerror
        ('nil (error "Unknown package: %S" pkg))
        ('guess
	 (message "Unknown package: %S" pkg)
	 (list pkg)))))

(defun elpaa--scrub-archive-contents (dir specs)
  "Remove dead packages from archive contents in DIR.
SPECS is the list of package specifications."
  (let* ((filename (expand-file-name "archive-contents" dir))
         (ac (if (file-exists-p filename)
                 (elpaa--form-from-file-contents filename)
               '(1))))
    (elpaa--write-archive-contents
     (cons (car ac)
           (mapcan
            (lambda (pkg)
              (when (elpaa--get-package-spec (car pkg) specs 'noerror)
                (list pkg)))
            (cdr ac)))
     dir)))

(defconst elpaa--supported-keywords
  '(:auto-sync :branch :core :doc :excludes :ignored-files
    :lisp-dir :maintainer :make :manual-sync :merge :news ;;  :main-file
    :readme :release :release-branch :renames :rolling-release
    :shell-command :url :version-map
    ;; Internal use only.
    :parent--package)
  "List of keywords that can appear in a spec.")

(defun elpaa--publish-package-spec (spec)
  (let ((extra-keys
         (seq-difference (map-keys (cdr spec)) elpaa--supported-keywords)))
    (when extra-keys
      (message "Error: unknown keys in %S: %S"
               (car spec) extra-keys)))
  (condition-case err
      (pcase-exhaustive spec
        (`(,name :url ,url . ,rest)
         (if (stringp name) (setq name (intern name)))
         (unless url
           (setq url (concat "https://git.sv.gnu.org/git/"
                             elpaa--gitrepo))
           (setq rest
                 (plist-put rest :branch
                            (elpaa--local-branch-name spec)))
           (when (plist-get :release-branch rest)
             (setq rest (plist-put rest :release-branch
                                   (elpaa--local-branch-name spec t)))))
         `(,name :url ,url ,@rest))
        (`(,_ :core ,_ . ,_) nil)) ;Not supported in the published specs.
    (error (message "Error: %S" err)
           nil)))

(defun elpaa--publish-package-specs (specs)
  "Process and publish SPECS in elpa-packages.eld files."
  (with-temp-buffer
    ;; Remove :core packages, handle :url nil and and compress the
    ;; contents of the "elpa-packages".  Note that elpa-packages.eld
    ;; does not use the same format as "elpa-packages" in
    ;; {nongnu,elpa}.git.  The file is intended to be used by
    ;; package-vc.el.
    (prin1
     (list (delq nil (mapcar #'elpaa--publish-package-spec specs))
           :version 1 :default-vc 'Git)
     (current-buffer))
    (write-region nil nil
                  (expand-file-name "elpa-packages.eld" elpaa--release-subdir))
    (write-region nil nil
                  (expand-file-name "elpa-packages.eld" elpaa--devel-subdir))))

(defun elpaa-batch-make-all-packages (&rest _)
  "Check all the packages and build the relevant new tarballs."
  (let ((specs (elpaa--get-specs))
        (fuel most-positive-fixnum))
    (elpaa--scrub-archive-contents elpaa--release-subdir specs)
    (elpaa--scrub-archive-contents elpaa--devel-subdir specs)
    (elpaa--publish-package-specs specs)
    (while specs
      (let ((spec (pop specs)))
        ;; The topological-ordering algorithm used in
        ;; `elpaa--make-one-package' is very naive and doesn't detect cycles,
        ;; so break cycles by counting steps and disabling the
        ;; topological sort if the count is higher than normal.
        (setq fuel (min (1- fuel) (expt (length specs) 2)))
        (condition-case err
            (elpaa--make-one-package spec nil (if (> fuel 0) specs))
          (error (message "Build error for %s: %S" (car spec) err)))))))

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
    ;; ImageMagick 7.1.0 or later requires using the "magick" driver,
    ;; rather than "convert" directly, but Debian doesn't provide it
    ;; yet (2021).
    (let ((args
           (append (list (current-buffer))
                   (if (executable-find "magick") '("magick" "convert") '("convert"))
                   (list "-debug" "annotate" "xc:" "-font" "DejaVu-Sans"
                         "-pointsize" "110" "-annotate" "0" str "null:"))))
      (apply #'elpaa--call args)
      (goto-char (point-min))
      (if (not (re-search-forward "Metrics:.*?width: \\([0-9]+\\)"))
          (error "Could not determine string width")
        (let ((width (string-to-number (match-string 1))))
          ;; This test aims to catch the case where the font is missing,
          ;; but it seems it only works in some cases :-(
          (if (and (> (string-width str) 0) (not (> width 0)))
              (progn (message "convert:\n%s" (buffer-string))
                     (error "Could not determine string width"))
            width))))))

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

(defun elpaa--depends-on-pending-p (metadata pending-specs)
  "Return non-nil if one of the required packages is in `pending-specs'."
  (let ((reqs (nth 3 metadata))
        (depends nil))
    (pcase-dolist (`(,pkg ,_vers) reqs)
      (if (assq pkg pending-specs)
          (setq depends t)))
    depends))

(defun elpaa--make-one-package (pkg-spec &optional tarball-only pending-specs)
  "Build the new tarballs (if needed) for PKG-SPEC.
If TARBALL-ONLY is non-nil, don't try and select some other revision and
place the resulting tarball into the file named TARBALL-ONLY."
  (elpaa--message "Checking package %s for updates..." (car pkg-spec))
  (let* ((pkgname (car pkg-spec))
         (dir (elpaa--pkg-root pkg-spec))
         (_ (cond
             (tarball-only nil)
             ((eq (nth 1 pkg-spec) :core) (elpaa--core-package-sync pkg-spec))
             (t (elpaa--worktree-sync pkg-spec))))
         (_ (elpaa--message "pkg-spec for %s: %S" pkgname pkg-spec))
         (metadata (with-demoted-errors "elpaa--metadata error: %S"
                     (elpaa--metadata dir pkg-spec))))
    (elpaa--message "metadata = %S" metadata)
    (elpaa--check-sync-failures pkg-spec metadata)
    (cond
     ((null metadata)
      (error "No metadata found for package: %s" pkgname))
     ((and pending-specs (elpaa--depends-on-pending-p metadata pending-specs))
      ;; Try and build packages in dependency order, so as to avoid
      ;; signaling spurious "missing dependencies".
      (setq pending-specs (nconc pending-specs (list pkg-spec))))
     (t
      ;; Disregard the simple/multi distinction.  This might have been useful
      ;; in a distant past, but nowadays it's just unneeded extra complexity.
      (setf (car metadata) nil)
      ;; First, try and build the devel tarball
      ;; Do it before building the release tarball, because building
      ;; the release tarball may revert to some older commit.
      (let* ((vers (nth 1 metadata))
             (date-version (elpaa--get-devel-version dir pkg-spec))
             ;; Add a ".0." so that when the version number goes from
             ;; NN.MM to NN.MM.1 we don't end up with the devel build
             ;; of NN.MM comparing as more recent than NN.MM.1.
             ;; But be careful to turn "2.3" into "2.3.0.DATE"
             ;; and "2.3b" into "2.3b0.DATE".
             (devel-vers
              (concat vers (if (string-match "[0-9]\\'" vers) ".")
                      "0." date-version))
             (tarball (or tarball-only
                          (format "%s%s-%s.tar"
                                  elpaa--devel-subdir
                                  pkgname devel-vers)))
             (new
              (let ((elpaa--name (concat elpaa--name "-devel"))
                    (elpaa--url elpaa--devel-url))
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
         ((or (< (apply #'min (version-to-list vers)) 0)
              (elpaa--spec-get pkg-spec :release-branch))
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
                   (tarball (format "%s%s-%s.tar"
                                    elpaa--release-subdir
                                    pkgname (car last-rel)))
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
          (let ((tarball (format "%s%s-%s.tar"
                                 elpaa--release-subdir pkgname vers)))
            (when (elpaa--make-one-tarball
                   tarball dir pkg-spec vers
                   (lambda ()
                     (elpaa--get-release-revision
                      dir pkg-spec vers
                      (plist-get (cdr pkg-spec) :version-map))))
              (elpaa--release-email pkg-spec metadata dir))))))))))

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
Signal an error if the command did not finish with exit code 0.
PROGRAM, DESTINATION, ARGS is like in `elpaa--call'."
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

(defun elpaa--default-url (pkg) (format "%s%s.html" elpaa--url pkg))
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

(defun elpaa--lm-maintainers (orig-fun header &rest args)
  (if (not (equal header "maintainer"))
      (apply orig-fun header args)
    (or (apply orig-fun "package-maintainer" args)
        (apply orig-fun header args))))

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
    (setq files (delete (format "%s-pkg.el" pkg) files))
    (setq files (delete (format "%s-autoloads.el" pkg) files))
    (cond
     ((file-exists-p mainfile)
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
        (let* ((lmheader-advice
                (when (or (plist-get (cdr pkg-spec) :version-map)
                          (plist-get (cdr pkg-spec) :dont-release))
                  (apply-partially
                   #'elpaa--override-version
                   pkg-spec)))
               (pkg-desc
                (unwind-protect
                    (progn
                      (when lmheader-advice
                        (advice-add 'lm-header :around lmheader-advice))
                      (advice-add 'lm-header-multiline
                                  :around #'elpaa--lm-maintainers)
                      (package-buffer-info))
                  (advice-remove 'lm-header-multiline #'elpaa--lm-maintainers)
                  (advice-remove 'lm-header lmheader-advice)))
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
    (unless (string-equal (nth 1 exp) pkg)
      (error (format "Package name %S doesn't match file name %S"
		     (nth 1 exp) pkg)))
    (unless dont-rename (rename-file dir (format "%s-%s" pkg vers)))
    (if (stringp pkg) (setq pkg (intern pkg)))
    (cons pkg (vector (elpaa--version-to-list vers)
                      req (nth 3 exp) 'tar extras))))

(defun elpaa--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (format "%s-pkg.el" pkg) dir)))
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
  (let ((pkg-file (expand-file-name (format "%s-pkg.el" name) pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (elpaa--temp-file pkg-file)
    (pcase-let ((`(,version ,desc ,requires ,extras)
                 (cdr metadata)))
      (write-region
       (concat (format ";; Generated package description from %s.el  -*- %sno-byte-compile: t -*-\n"
		       name
		       (let* ((emacs-req (assq 'emacs requires))
		              (emacs-vers (car (cadr emacs-req))))
		         (if (not (and emacs-vers (>= emacs-vers 28)))
		             ""     ;Need compatibility with Emacs<28.
		           "mode: lisp-data; ")))
	       (prin1-to-string
                (nconc
                 (list 'define-package
                       (format "%s" name) ;It's been a string, historically :-(
                       version
                       desc
                       (list 'quote
                             ;; Turn version lists into string form.
                             (mapcar
                              (lambda (elt)
                                (list (car elt)
                                      (package-version-join (cadr elt))))
                              requires)))
                 (elpaa--alist-to-plist-args extras)))
	       "\n")
       nil
       pkg-file))))

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
              '("README-elpa.md"
                "README-elpa"
                "README"
                "README.org"
                "README.rst"
                "README.md")))
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
           (pkg-spec (elpaa--get-package-spec pkg nil 'guess)))
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

            <header class=\"small\">
                <div class=\"container\">
                    <h1>%s</h1>
                </div>
            </header>

            <main class=\"container\">\n"
          title (or head-extra "") (or header title)))

(defvar elpaa--index-javascript-headers "
        <script src=\"../javascript/package-search.js\" type=\"text/javascript\"></script>")

(defun elpaa--html-footer ()
  (format "\n
        <footer>
            <div class=\"container\">
                <p>Last refreshed on %s</p>
                <p>Copyright 2016-%s <a href=\"https://fsf.org\">Free Software Foundation</a>, Inc.</p>
                <p>Design provided by <a href=\"https://nicolas.petton.fr\">Nicolas Petton</a></p>
                <p>
                  This website is licensed under the
                  <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>
                  International License.
                </p>
                <p><a href=\"/jslicense.html\" data-jslicense=\"1\">JavaScript Licenses</a></p>
            </div>
        </footer>

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

(cl-defgeneric elpaa--section-to-html (section &optional _pkg-spec)
  "Return SECTION as HTML.
SECTION should be a cons as returned by `elpaa--get-section',
which see."
  (concat "<pre>\n"
          (elpaa--html-quote (cdr section))
          "\n</pre>\n"))

(cl-defmethod elpaa--section-to-html ((section (head text/x-org)) &optional pkg-spec)
  (let ((html (elpaa--export-org (cdr section) 'html
                                 :body-only t
                                 :ext-plist (append '(:html-toplevel-hlevel 3)
                                                    elpaa--org-export-options))))
    (with-temp-buffer
      (insert html)
      (elpaa--doc-html-adjust-auxfiles
       pkg-spec nil (current-buffer)
       (concat "doc/" (symbol-name (car pkg-spec)) "/"))
      (buffer-string))))

(defvar elpaa-markdown-command
  (if (executable-find "markdown2")
      ;; Presumably https://github.com/trentm/python-markdown2.
      ;; Stay conservative in the set of extensions we support.
      '("markdown2" "-x" "code-friendly,tables,fenced-code-blocks,nofollow")
    '("markdown")))

(cl-defmethod elpaa--section-to-html ((section (head text/markdown))
                                      &optional pkg-spec)
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
    ;; Adjust refs to local resources.
    (elpaa--doc-html-adjust-auxfiles
     pkg-spec nil (current-buffer)
     (concat "doc/" (symbol-name (car pkg-spec)) "/"))

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
             "--eval" "(setq org-export-with-toc nil)"
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


(defalias 'elpaa--html-quote #'url-insert-entities-in-string)

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
                              (elpaa--local-branch-name pkg-spec)))
                    '("cgit/%s/?h=%s"
                      "gitweb/?p=%s;a=shortlog;h=refs/heads/%s")))))
    (insert (format
             (concat (format "<dt>Browse %srepository</dt> <dd>"
                             (if url "ELPA's " ""))
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
    (cl-assert (string= name (car pkg-spec)))
    (elpaa--make-badge (concat name ".svg")
                       (format "%s ELPA" elpaa--name)
                       (format "%s %s" name latest))
    (elpaa--make-atom-feed pkg pkg-spec srcdir files)
    (with-temp-buffer
      (insert (elpaa--html-header
               (format "%s ELPA - %s" elpaa--name name)
               (format "<a href=\"index.html\">%s ELPA</a> - %s"
                       elpaa--name name)
               (format "<link href=\"%s.xml\" type=\"application/atom+xml\" rel=\"alternate\" />"
                       name)))
      (insert (format "<h2 class=\"package\">%s" name))
      (insert " <a class=\"badge\" href=\"" name ".xml\"><img src=\"/images/rss.svg\" alt=\"Atom Feed\"></a>")
      (insert "</h2>")
      (insert "<dl>")
      (insert (format "<dt>Description</dt><dd>%s</dd>\n" (elpaa--html-quote desc)))
      (if (zerop (length latest))
          (insert "<dd>This package "
                  (if files (concat "is not in " elpaa--name " ELPA any more")
                    "has not been released yet")
                  ".</dd>\n")
        (let* ((file (cdr (assoc latest files)))
               (attrs (file-attributes file)))
          (insert (format "<dt>Latest</dt> <dd><a href=%S>%s</a> (<a href=%S>.sig</a>), %s, %s</dd>\n"
                          file (elpaa--html-quote file) (concat file ".sig")
                          (format-time-string "%Y-%b-%d" (nth 5 attrs))
                          (elpaa--html-bytes-format (nth 7 attrs))))))
      ;; FIXME: Use `elpaa--maintainers'?
      (let ((maints (elpaa--get-prop "Maintainer" name srcdir mainsrcfile)))
        (elpaa--message "maints=%S" maints)
        (insert
         "<dt>Maintainer</dt> <dd>"
         (mapconcat (lambda (maint)
                      (when (consp maint)
                        (setq maint (concat (if (car maint)
                                                (concat (car maint) " "))
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
      (insert (format "<p>To install this package from Emacs, use %s%s.</p>"
                      (if (elpaa--spec-get pkg-spec :core)
                          ;; Just `package-install' doesn't really work for
                          ;; :core packages :-(
                          "" "<code>package-install</code> or ")
                      "<code>list-packages</code>"))
      (let* ((readme-content (elpaa--get-README pkg-spec srcdir))
             (readme-text plain-readme)
             (readme-html (elpaa--section-to-html readme-content pkg-spec))
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
        (insert "</table>\n"))
      (let ((news (elpaa--get-NEWS pkg-spec srcdir)))
        (when news
          (insert "<h2>News</h2>\n"
                  "<div class=\"splice news\">\n"
                  (elpaa--section-to-html news)
                  "\n</div>\n")))
      (insert "</main>\n")
      (insert (elpaa--html-footer))
      (write-region (point-min) (point-max) (concat name ".html")))))

(defun elpaa--html-make-index (pkgs)
  (with-temp-buffer
    (let ((scores (and elpaa--wsl-stats-file
                       (file-readable-p elpaa--wsl-stats-file)
                       (nth 3 (elpaa--form-from-file-contents
                               elpaa--wsl-stats-file)))))
      (insert (elpaa--html-header
               (concat elpaa--name " ELPA Packages") nil
               (concat
                elpaa--index-javascript-headers
                (format "<link href=\"%s\" type=\"application/atom+xml\" rel=\"alternate\" />"
                        elpaa--aggregated-feed-filename))))
      (insert "<table id=\"packages\">\n")
      (insert "<thead><tr><th>Package</th><th>Version</th><th>Description</th><th>Rank</th></tr></thead>\n")
      (insert "<tbody>")
      (dolist (pkg pkgs)
        (insert (format "<tr><td><a href=\"%s.html\">%s</a></td><td>%s</td><td>%s</td><td>%s</td></tr>\n"
                        (car pkg) (car pkg)
                        (package-version-join (aref (cdr pkg) 0))
                        (aref (cdr pkg) 2)
                        ;; Average rank over all the weeks' ranks.
                        ;; FIXME: Only use the more recent weeks?
                        (let* ((ranks (and (hash-table-p scores)
                                           (gethash (symbol-name (car pkg))
                                                    scores)))
                               (total (apply #'+ (mapcar #'cdr ranks))))
                          (if (null ranks) "?"
                            (format "%d%%" (/ total (length ranks))))))))
      (insert "</tbody></table>
            <div class=\"push\"></div>
        </main>")
      (insert (elpaa--html-footer))
      (write-region (point-min) (point-max) "index.html"))))

(defun elpaa-batch-html-make-index ()
  (let* ((ac-file (pop command-line-args-left))
         (devel (string-match "devel" (pop command-line-args-left)))
         (elpaa--name (concat elpaa--name (if devel "-devel" "")))
         (elpaa--url (if devel elpaa--devel-url elpaa--url))
         (ac (elpaa--form-from-file-contents ac-file))
         (default-directory (file-name-directory (expand-file-name ac-file))))
    (elpaa--html-make-index (cdr ac))
    (elpaa--make-aggregated-atom-feed elpaa--aggregated-feed-filename)))

;;; Statistics from the web server log

(defconst elpaa--wsl-time-re
  (rx (group (repeat 2 digit))          ;Day
      "/" (group (repeat 3 alpha))      ;Month
      "/" (group (repeat 4 digit))      ;Year
      ":" (group                        ;Time
           (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit)
           " " (or "+" "-") (repeat 4 digit))))

(defconst elpaa--wsl-line-re
  (rx bol
      (\? (+ (not " ")) " ")            ; VHost
      (+ (or xdigit "." ":"))           ; IP of client
      " - - "
      "[" (group (+ (not "]"))) "]"                    ; Date/time
      " \"" (group (* (or (not (any "\"" "\\"))
                          (seq "\\" anychar))))
      "\""
      " " (group (+ digit))                        ; Status code
      " " (or (+ digit) "-")                       ; Size
      " \"" (* (or (not (any "\"")) "\\\"")) "\" " ; Referrer
      "\"" (* (or (not (any "\"")) "\\\"")) "\""   ; User-Agent
      eol))

(defconst elpaa--wsl-request-re
  (rx (seq (+ (or alpha "_"))                ; Method
           " " (group (+ (not (any blank)))) ; Path
           " " "HTTP/" (+ (or alnum "."))))) ; Protocol

(defun elpaa--wsl-read (logfile fn)
  (with-temp-buffer
    (insert-file-contents logfile)
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (looking-at elpaa--wsl-line-re))
          (message "Unrecognized log line: %s"
                   (buffer-substring (point) (line-end-position)))
        (let* ((line (match-string 0))
               (timestr (match-string 1))
               (request (match-string 2))
               (status (match-string 3))
               (timestr
                (if (string-match "/\\([^/]*\\)/\\([^/:]*\\):" timestr)
                    (replace-match " \\1 \\2 " t nil timestr)
                  (message "Unrecognized timestamp: %s" timestr)
                  timestr))
               (time (encode-time (parse-time-string timestr))))
          (when (not (member status '("404" "400" "408")))
            (if (not (string-match elpaa--wsl-request-re request))
                (message "Unrecognized request (status=%s): %s" status request)
              (let* ((file (match-string 1 request))
                     (pkg (if (string-match
                               (rx bos "/"
                                   (or "packages" "devel"
                                       "nongnu" "nongnu-devel")
                                   (+ "/")
                                   (group (+? any))
                                   (\?
                                    "-" (or
                                         (seq
                                          (+ (or digit "."))
                                          (* (or "pre" "beta" "alpha"
                                                 "snapshot")
                                             (* (or digit "."))))
                                         "readme"
                                         "sync-failure"
                                         "build-failure"))
                                   "."
                                   (or "tar" "txt" "el" "html"))
                               file)
                              (match-string 1 file))))
                ;; It would make sense to include accesses to "doc/<NAME>" in
                ;; the counts, except that <NAME> is not always the name of the
                ;; corresponding package.
                (when (and pkg (not (string-match-p "/" pkg)))
                  (funcall fn time pkg file line)))))))
      (forward-line 1))))

(defun elpaa--wsl-one-file (logfile stats)
  (elpaa--wsl-read
   logfile
   ;; Keep a counter of accesses indexed by package and week.
   (lambda (time pkg file line)
     (let* ((secs (time-convert time 'integer))
            (week (/ secs 3600 24 7))
            (old (gethash pkg stats)))
       (unless old
         (message "New package: %S %S %S %S" time pkg file line))
       (cl-incf (alist-get week (gethash pkg stats) 0))))))

(defvar elpaa--wsl-directory "/var/log/apache2/")

(defun elpaa--wsl-scores (table)
  (let ((scores-by-week ()))
    (maphash (lambda (pkg data)
               (when (and pkg (not (string-match "/" pkg)))
                 (pcase-dolist (`(,week . ,count) data)
                   (push (cons count pkg) (alist-get week scores-by-week)))))
             table)
    ;; For each week, we sort packages by number of downloads, to
    ;; compute their percentile ranking.
    ;; FIXME: We don't take into account that several (many?) packages can
    ;; have the same number of downloads, in which case their relative ranking
    ;; (within the equiv class) is a lie.
    (dolist (scores scores-by-week)
      (setf (cdr scores)
            (nreverse (mapcar #'cdr (sort (cdr scores)
                                          #'car-less-than-car)))))
    (let ((score-table (make-hash-table :test 'equal)))
      (pcase-dolist (`(,week . ,pkgs) scores-by-week)
        (let* ((total (length pkgs))
               (rest total))
          (dolist (pkg pkgs)
            (setq rest (1- rest))
            (let ((percentile (/ (* 100 rest) total)))
              (push (cons week percentile) (gethash pkg score-table))))))
      score-table)))

(defun elpaa--wsl-collect ()
  (let* ((stats (elpaa--form-from-file-contents elpaa--wsl-stats-file))
         (seen (nth 1 stats))
         (table (nth 2 stats))
         (changed nil)
         (newseen ()))
    (cl-assert (eq :web-server-log-stats (nth 0 stats)))
    (unless table (setq table (make-hash-table :test 'equal)))
    ;; Only consider the compressed files, because we don't want to process
    ;; files that may still be modified.
    (dolist (logfile (directory-files elpaa--wsl-directory t "\\.[lgx]z\\'"))
      (let ((attrs (file-attributes logfile)))
        ;; The log files get renamed, which changes their `ctime', so let's
        ;; throw out this information.
        (setf (nth 6 attrs) nil)
        (cond
         ((string-match "error.log" logfile) nil) ;Ignore the error log files.
         ((member attrs seen) (push attrs newseen)) ;Already processed.
         (t
          (push attrs newseen)
          (setq changed t)
          (elpaa--wsl-one-file logfile table)))))
    (when changed
      (with-temp-buffer
        (funcall (if (fboundp 'pp-28) #'pp-28 #'pp)
                 `(:web-server-log-stats ,newseen ,table
                   ;; Rebuild the scoreboard "by week".
                   ,(elpaa--wsl-scores table))
                 (current-buffer))
        (princ "\n" (current-buffer))
        (write-region nil nil elpaa--wsl-stats-file)))))

;; (defun elpaa--wsl-foo ()
;;   (let ((diff (time-convert (time-subtract curtime time) 'integer))
;;         (diff-weeks (/ diff 3600 24 7))
;;         (timelog (/ (logb (1+ diff-weeks)) 2))
;;         (vec (gethash pkg stats)))
;;     (unless vec
;;       (setf (gethash pkg stats) (setq vec (make-vector 4 0))))
;;     (if (> timelog (length vec))
;;         (message "Entry too old: %s" timestr)
;;       (cl-incf (aref vec timelog)))))
;;       stats)))

;;; Maintain worktrees in the `packages' subdirectory

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

(defun elpaa--cleanup-packages (specs _with-core)
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
         ((elpaa--get-package-spec dir specs 'noerror) nil) ;One of our packages.
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
         ;; ;; Check if `dir' is under version control.
         ;; ((and with-core
         ;;       (not (zerop (elpaa--call nil "git" "ls-files"
         ;;                                 "--error-unmatch" dir))))
         ;;  ;; Not under version control.  Check if it only contains
         ;;  ;; symlinks and generated files, in which case it is probably
         ;;  ;; a leftover :core package that can safely be deleted.
         ;;  (let ((file (elpaa--find-non-trivial-file dir)))
         ;;    (if file
         ;;        (message "Keeping %s for non-trivial file \"%s\"" dir file)
         ;;      (progn
         ;;        (message "Deleted untracked package %s" dir)
         ;;        (delete-directory dir 'recursive t)))))
         )))))

(defun elpaa--worktree-sync (pkg-spec)
  "Sync worktree of PKG-SPEC."
  (let* ((pkg (car pkg-spec))
         (name (format "%s" pkg))
         (parent (elpaa--parent-package pkg-spec))
         (default-directory (expand-file-name "packages/")))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (cond (parent
           (unless (file-exists-p name)
             (message "Symlinking %s to %S" name parent)
             (make-symbolic-link (symbol-name parent) name)))
          ((not (file-exists-p name))
	   (message "Cloning branch %s:" pkg)
           (let* ((branch (elpaa--local-branch-name pkg-spec))
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
                                        (elpaa--local-branch-name pkg-spec t)))
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
                       (message "No branch %s for the worktree of %s:\n%s"
                                branch name (buffer-string))))
                     (buffer-string))))
	     (message "%s" output)))
          ((not (file-exists-p (format "%s/.git" name)))
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
       (package-root (elpaa--pkg-root pkg-spec))
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
  (let ((specs (elpaa--get-specs 'no-follow))
        (pkgs command-line-args-left)
        (with-core (elpaa--sync-emacs-repo))
        msg-done)
    (setq command-line-args-left nil)
    (if (equal pkgs '("-")) (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (elpaa--get-package-spec pkg specs))
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
  (let* ((pkg (car pkg-spec))
         (default-directory (elpaa--pkg-root pkg-spec))
         (ignores (elpaa--spec-get pkg-spec :ignored-files))
         (all-ignores '("." ".." ".git" ".dir-locals.el" ".mailmap"
                        ".github" ".travis.yml"
                        "test" "tests"))
         (dir-files (lambda (d)
                      (seq-difference (directory-files d)
                                      all-ignores #'equal)))
         (pending (seq-difference
                   (funcall dir-files ".")
                   (list (format "%s-pkg.el" pkg)
                         (format "%s-autoloads.el" pkg))
                   #'equal))
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
    (let* ((pkg (car pkg-spec))
           (elpaa--debug nil)
           (files (mapcar (lambda (f) (format "%s/%s" pkg f))
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

(defun elpaa-batch-check (&rest _)
  (let ((specs (elpaa--get-specs))
        (pkgs command-line-args-left)
        (ac (elpaa--dependencies-archive-contents)))
    (setq command-line-args-left nil)
    (when (equal pkgs '("-"))
      (setq pkgs (delq nil (mapcar (lambda (spec)
                                     (when (file-directory-p
                                            (elpaa--pkg-root spec))
                                       (car spec)))
                                   specs))))
    (dolist (pkg pkgs)
      (let ((pkg-spec (elpaa--get-package-spec pkg specs)))
        (ignore-error error
          (elpaa--copyright-check pkg-spec))
        (condition-case err
            ;; FIXME: elpaa--metadata should receive a single arg maybe?
            (let* ((metadata (elpaa--metadata (elpaa--pkg-root pkg-spec)
                                              pkg-spec)))
              (elpaa--check-dependencies metadata ac))
          (error (message "Dependency error in %S:\n%S" pkg err)))))))

;;; Announcement emails

(defun elpaa--pkg-name (pkg-spec)
  (let ((name (format "%s" (car pkg-spec))))
    (if (equal (downcase name) name)
        (capitalize name) name)))

(defun elpaa--send-email (headers body)
  (with-temp-buffer
    (message-mode)
    (declare-function message-setup "message"
                      (headers &optional yank-action actions continue
                               switch-function return-action))
    (declare-function message-send "message" (&optional arg))
    (message-setup headers)
    (insert body)
    ;; (pop-to-buffer (current-buffer)) (debug t)
    (message-send)
    ))

(defun elpaa--maintainers (pkg-spec metadata)
  (let* ((metadata (or metadata
                       (with-demoted-errors "elpaa--maintainers: %S"
                         (elpaa--metadata (elpaa--pkg-root pkg-spec)
                                          pkg-spec))))
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
         (emails (delq nil maint-emails)))
    (if emails
        (mapconcat #'identity emails ",")
      (elpaa--spec-get pkg-spec :maintainer))))

(defun elpaa--release-email (pkg-spec metadata dir)
  (when elpaa--email-to
    (with-temp-buffer
      (let* ((version (nth 1 metadata))
             (pkg (car pkg-spec))
             (name (elpaa--pkg-name pkg-spec))
             (desc (nth 2 metadata))
             (maintainers
              (elpaa--maintainers pkg-spec metadata)))
        (insert "Version " version
                " of package " name
                " has just been released in " elpaa--name " ELPA.\n"
                "You can now find it in M-x list-packages RET.\n\n"
                name " describes itself as:\n\n  "
                (make-string (string-width desc) ?=) "\n  "
                desc "\n  "
                (make-string (string-width desc) ?=) "\n\nMore at "
                (elpaa--default-url pkg))
        (let ((readme (elpaa--get-README pkg-spec dir)))
          (unless (bolp) (insert "\n"))
          (insert "\n## Summary:\n\n")
          (let ((beg (point)))
            (insert (if (not readme)
                        "[Not provided 🙁]"
                      (elpaa--section-to-plain-text readme)))
            ;; It's import to terminate lines properly so we can detect
            ;; truncated lines below to throw away the leftovers.
            (unless (bolp) (insert "\n"))
            ;; Keep a max of about 10 lines of full-length text.
            (delete-region (min (+ beg 800) (point)) (point))
            (let ((end (point)))
              (delete-region
               ;; Truncate at the end of the nearest paragraph.
               (or (re-search-backward "\n[ \t]*$" beg t)
                   ;; Throw away leftovers from truncated lines.
                   (re-search-backward "\n" beg t)
                   (point))
               end))
            (indent-rigidly beg (point) 2)))
        (let ((news (elpaa--get-NEWS pkg-spec dir)))
          (unless (bolp) (insert "\n"))
          (insert "\n## Recent NEWS:\n\n"
                  (if (not news)
                      "[Not provided 🙁]"
                    (elpaa--section-to-plain-text news))))
        (elpaa--send-email
         `((From    . ,elpaa--email-from)
           (To      . ,elpaa--email-to)
           (Subject . ,(format "[%s ELPA] %s version %s"
                               elpaa--name name version))
           ,@(when maintainers
               `((Cc . ,maintainers)))
           ,@(if elpaa--email-reply-to
                 `((Reply-To . ,elpaa--email-reply-to))))
         (buffer-string))))))

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
	    (format "%s" (car pkg-spec))
	    (expand-file-name elpaa--doc-subdirectory tarball-dir)))))
    (when html-dir
      (when (file-directory-p html-dir)
        (delete-directory html-dir 'recursive))
      (make-directory html-dir t)
      ;; (elpaa--doc-copy-auxfiles pkg-spec dir html-dir)
      )

    (plist-put (cdr pkg-spec) :internal--html-dir html-dir)
    (plist-put (cdr pkg-spec) :internal--html-docs nil)
    (plist-put (cdr pkg-spec) :internal--html-resources nil)
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
        ;; FIXME: The name of the output file is splattered all over the output
        ;; file, so it ends up wrong after renaming.  Maybe it's harmless,
        ;; I don't know, but it's not satisfactory.
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
    (elpaa--makeinfo docfile html-file
                     (list "--html" (format "--css-ref=%s" elpaa--css-url)))
    (elpaa--doc-html-adjust-auxfiles pkg-spec docfile html-file
                                     (concat (symbol-name (car pkg-spec)) "/"))
    (push (cons (file-name-base html-file)
                (file-name-nondirectory html-file))
          (plist-get (cdr pkg-spec) :internal--html-docs))

    ;; Create a symlink from elpa/archive[-devel]/doc/* to
    ;; the actual file, so html references work.
    (let ((target (file-name-concat (file-name-nondirectory html-dir)
                                    destname))
          (current-target (file-attribute-type
                           (file-attributes html-xref-file))))
      (cond
       ((not (stringp current-target))
        (with-demoted-errors "%S" ;; 'make-symbolic-link' fails on Windows.
          (make-symbolic-link target html-xref-file)))
       ((equal target current-target) nil) ;Nothing to do.
       (t (error "Manual name %S conflicts with %S"
                 destname current-target))))))

(defun elpaa--doc-html-adjust-auxfiles (pkg-spec docfile html-file offset)
  ;; (let* ((auxfiles (elpaa--spec-get pkg-spec :doc-files)))
  ;;  (when auxfiles
  (let* ((docdir (if (stringp docfile) (file-name-directory docfile)))
         (regexp " \\(?:src\\)=\"\\([^#/.\"][^:\"#]+\\)\""))
    (with-current-buffer (if (stringp html-file)
                             (find-file-noselect html-file)
                           html-file)
      (let ((default-directory (elpaa--pkg-root pkg-spec)))
        (elpaa--message "regexp=%S" regexp)
        ;; (elpaa--message "buffer-size=%S" (buffer-size))
        (elpaa--message "default-directory=%S" default-directory)
        (goto-char (point-min))
        (let ((case-fold-search t))
          ;; FIXME: Skip false positives found outside of tags!
          (while (re-search-forward regexp nil t)
            (message "found match for: %S" (match-string 1))
            (let* ((file (match-string 1))
                   (rootedfile (file-name-concat docdir file))
                   (idr (elpaa--spec-get pkg-spec :internal--html-resources)))
              (when (or (member rootedfile idr)
                        (cond
                         ((string-match "\\.\\." file)
                          (message "Suspicious file name: %S" file)
                          nil)
                         ((not (file-readable-p rootedfile))
                          (message "False positive?  Skipping: %S" file)
                          nil)
                         (t
                          (let* ((html-dir (elpaa--spec-get
                                            pkg-spec :internal--html-dir))
                                 (destfile
                                  (expand-file-name rootedfile html-dir))
                                 (destdir (file-name-directory destfile)))
                            (plist-put (cdr pkg-spec) :internal--html-resources
                                       (cons rootedfile idr))
                            (when destdir (make-directory destdir t))
                            (copy-file rootedfile destfile)
                            t))))
                (goto-char (match-beginning 1))
                (insert (concat offset docdir)))))))
      (when (stringp html-file)
        (let ((make-backup-files nil))
          (save-buffer))))))

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

(defvar elpaa--manual-sync-re
  regexp-unmatchable ;; "git\\.sr\\.ht/"
  "Regexp matching URLs from which we shouldn't poll.")

(defun elpaa--manual-sync-p (pkg-spec)
  (or (elpaa--spec-get pkg-spec :manual-sync)
      (let ((url (elpaa--spec-get pkg-spec :url)))
        (and url (string-match elpaa--manual-sync-re url)))))

(defun elpaa--branch (pkg-spec)
  (elpaa--spec-get pkg-spec :branch))

(defun elpaa--urtb (pkg-spec &optional branch)
  "Return our upstream remote tracking branch for PKG-SPEC."
  (format "refs/remotes/upstream/%s/%s" (car pkg-spec) (or branch "main")))

(defun elpaa--ortb (pkg-spec)
  "Return our origin remote tracking branch for PKG-SPEC."
  ;; We can't use the shorthand "origin/%s%s" when we pass it to
  ;; `git-show-ref'.
  (format "refs/remotes/origin/%s"
          (elpaa--local-branch-name pkg-spec)))

(defun elpaa--git-branch-p (branch)
  "Return non-nil iff BRANCH is an existing branch."
  (equal 0 (elpaa--call t "git" "show-ref" "--verify" "--quiet" branch)))

(defun elpaa--is-ancestor-p (candidate rev)
  "Return non-nil if CANDIDATE is ancestor of REV."
  (zerop (elpaa--call t "git" "merge-base" "--is-ancestor"
                      candidate rev)))

(defun elpaa--record-sync-failure (pkg-spec msg)
  (when (file-directory-p elpaa--sync-failures-dir)
    (let* ((pkg (car pkg-spec))
           (logfile (expand-file-name (format "%s-sync-failure.txt" pkg)
                                      elpaa--sync-failures-dir)))
      (if (null msg)
          (delete-file logfile)
        (write-region msg nil logfile nil 'silent)))))

(defun elpaa--git-short-log (from to)
  (elpaa--call t "git" "log"
               "--date=format:%Y-%m"
               "--format=%h %cd  %<(16,trunc)%ae  %s"
               (format "%s..%s" from to)))

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
        (unless (or (elpaa--spec-member pkg-spec :url)
                    (elpaa--spec-member pkg-spec :core))
          (message "No upstream URL in %s for %s" elpaa--specs-file pkg))
      (message "Fetching updates for %s..." pkg)
      (with-temp-buffer
        (cond
         ((not (equal 0 (apply #'elpaa--call
                               t "git" "fetch" "--no-tags"
                               "--negotiation-tip"
                               (elpaa--urtb pkg-spec "*")
                               url refspec
                               (if release-refspec
                                   (list release-refspec)))))
          (message "Fetch error for %s:\n%s" pkg (buffer-string)))
	 ((not (elpaa--git-branch-p ortb))
	  (message "New package %s hasn't been pushed to origin yet" pkg)
	  (when k (funcall k pkg-spec)))
         ((elpaa--is-ancestor-p urtb ortb)
          (message "Nothing new upstream for %s" pkg))
         ((not (or (elpaa--is-ancestor-p ortb urtb)
                   (elpaa--spec-get pkg-spec :merge)))
          (let ((output (delete-and-extract-region (point-min) (point-max))))
            (if (> (length output) 0) (message "%s" output)))
          (let* ((msg (format "Upstream of %s has DIVERGED!" pkg)))
            (when (or show-diverged (eq k #'elpaa--push))
              (let ((msgs (list "\n\n" msg)))
                (elpaa--git-short-log urtb ortb)
                (push "  Local changes:\n" msgs)
                (push (delete-and-extract-region (point-min) (point-max)) msgs)
                (elpaa--git-short-log ortb urtb)
                (push "\n  Upstream changes:\n" msgs)
                (push (delete-and-extract-region (point-min) (point-max)) msgs)
                (let ((total-msg
                       (mapconcat #'identity (nreverse msgs) "")))
                  (when show-diverged (setq msg total-msg))
                  (when (eq k #'elpaa--push)
                    (elpaa--record-sync-failure pkg-spec total-msg)))))
            (message "%s" msg)))
         ((not (zerop (elpaa--git-short-log ortb urtb)))
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
           (wt (expand-file-name (format "packages/%s" pkg)))
           (merge-branch (format "elpa--merge/%s" pkg))
           last-release)
      ;; When the upstream changes includes changes to `Version:'), try to
      ;; merge only up to the first (new) revision that made such a change,
      ;; so that we hopefully get a merge commit from which to make the release.
      ;; The rest will be merged (hopefully) next time around.
      (while (and (setq last-release
                        (elpaa--get-last-release-commit pkg-spec
                                                        (concat urtb "~")))
                  (not (elpaa--is-ancestor-p last-release ortb)))
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
    (elpaa--record-sync-failure pkg-spec nil)
    (with-temp-buffer
      (cond
       ((and ortb-p (elpaa--is-ancestor-p urtb ortb))
        (message "Nothing to push for %s" pkg))
       ((xor (and ortb-p (not (elpaa--is-ancestor-p ortb urtb)))
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
                  (format "%s:refs/heads/%s"
                          urtb (elpaa--local-branch-name pkg-spec))
                  (when release-branch
                    (format "%s:refs/heads/%s"
                            (elpaa--urtb pkg-spec "release")
                            (elpaa--local-branch-name pkg-spec t)))))
        (message "Pushed %s successfully:\n%s" pkg (buffer-string))
        (when (file-directory-p (elpaa--pkg-root pkg-spec))
          (elpaa--worktree-sync pkg-spec)))
       (t
        (message "Push error for %s:\n%s" pkg (buffer-string)))))))

(defun elpaa--batch-fetch-and (k)
  (let* ((specs (elpaa--get-specs 'no-follow))
         (pkgs (mapcar #'intern command-line-args-left))
         (show-diverged (not (cdr pkgs)))
         (condition ':)
         (all nil))
    (setq command-line-args-left nil)
    (when (and (null (cdr pkgs)) (keywordp (car pkgs)))
      (setq all t)
      (setq show-diverged nil)
      (setq condition (car pkgs))
      (setq pkgs (mapcar #'car specs)))
    (dolist (pkg pkgs)
      (let* ((pkg-spec (elpaa--get-package-spec pkg specs)))
        (cond
         ((let ((url (elpaa--spec-get pkg-spec :url)))
            (and url (symbolp url)))
          nil) ;; Skip "subpackages".
         ((and all (elpaa--manual-sync-p pkg-spec)) nil) ;Skip.
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
       (format "%s-autoloads.el" package) package-directory))
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
   (expand-file-name (format "packages/%s" package) top-directory)
   package)

  (ert-run-tests-batch-and-exit t))

;;; Make dependencies

(defun elpaa-batch-pkg-spec-make-dependencies ()
  (let ((dst (pop command-line-args-left)))
    (with-temp-buffer
      (dolist (pkg-spec (elpaa--get-specs))
        (let* ((pkgname (car pkg-spec))
               (dir (format "packages/%s" pkgname)))
          (when (file-directory-p dir)
            (insert
             (format "%s/%s-pkg.el: %s/%s\n"
                     dir pkgname dir (elpaa--main-file pkg-spec)))
            (let ((make-targets (ensure-list (elpaa--spec-get pkg-spec :make))))
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
         (pkg-spec (elpaa--get-package-spec pkgname nil 'guess))
         (lisp-dir (elpaa--spec-get pkg-spec :lisp-dir)))
    (require 'package)
    (if (null lisp-dir)
        (progn
          (cl-assert (equal alf (format "%s%s-autoloads.el"
                                        (or dir "") pkgname)))
          (package-generate-autoloads pkgname dir))
      (package-generate-autoloads pkgname (concat dir lisp-dir))
      (write-region
       (format ";; -*- lexical-binding:t -*-
(load (concat (file-name-directory load-file-name) %S) nil 'nomsg)\n"
               (format "%s/%s-autoloads.el" lisp-dir pkgname ))
       nil alf nil 'silent))))

;;; Main files

(defun elpaa-batch-main-files ()
  (let ((dstfile (pop command-line-args-left)))
    (with-temp-buffer
      (dolist (pkg-spec (elpaa--get-specs))
        (let* ((pkgname (car pkg-spec))
               (defmf (format "%s.el" pkgname))
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

(when (file-readable-p "elpa-config") (elpaa-read-config "elpa-config"))

;;; Atom feed generation

(defun elpaa--rfc3339 (time)
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time))

(defun elpaa--rfc4151 (url time)
  (unless (string-match "\\`\\(?:[^:/]*:\\)?/*\\([^/]+\\)/?" url)
    (error "Can't find the \"domain\" of this URL: %S" url))
  (let ((domain (match-string 1 url))
        (specific (substring url (match-end 0))))
    (concat "tag:" domain "," (format-time-string "%F" time)
            ":" specific)))

(defun elpaa--make-atom-feed (pkg pkg-spec srcdir files)
  (let* ((name (symbol-name (car pkg)))
         (metadata (elpaa--metadata srcdir pkg-spec))
         (filename (concat name ".xml"))
         (desc (nth 2 metadata))
         ;; RFC4287 says "This specification assigns no significance
         ;; to the order of atom:entry elements within the feed", but we
         ;; sort them from oldest to newest.
         (sorted (sort (mapcar
                        (lambda (file)
                          (cons (file-attribute-modification-time
                                 (file-attributes (cdr file)))
                                file))
                        files)
                       (lambda (a1 a2) (time-less-p (car a1) (car a2)))))
         (title (format "Update feed for %s" name))
         (self (concat elpaa--url filename)))
    (with-temp-buffer
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
      (xml-print
       ;; See https://validator.w3.org/feed/docs/rfc4287.html
       `((feed
          ((xmlns . "http://www.w3.org/2005/Atom"))
          (title nil ,title)
          (link ((href . ,self) (rel . "self")))
          (id nil ,self)
          (updated nil ,(elpaa--rfc3339 (current-time)))
          ,@(mapcar
             (pcase-lambda (`(,time ,version . ,_file))
               (let ((self (concat elpaa--url
                                   (format "%s.xml#v%s" name version)))
                     (content
                      `((p nil
                           ,(concat "Version " version " of package ")
                           (a ((href . ,(elpaa--default-url name))) ,name)
                           ,(concat " has just been released in " elpaa--name
                                    " ELPA."))
                        (p nil "You can now find it in "
                           (kbd nil "M-x list-packages RET") ".")
                        (p nil ,(concat name " describes itself as:"))
                        (blockquote nil ,desc))))
                 `(entry
                   nil
                   (title nil
                          ,(format "%s ELPA: Release of \"%s\", Version %s"
                                   elpaa--name name version))
                   (updated nil ,(elpaa--rfc3339 time))
                   (author
                    nil
                    (name nil "elpa-admin")
                    (email nil "emacs-devel@gnu.org"))
                   (id nil ,(elpaa--rfc4151 self time))
                   (link ((href . ,self) (rel . "self")))
                   (content
                    ((type . "html") (base . ,elpaa--url))
                    ,(with-temp-buffer
                       (xml-print content)
                       (buffer-string))))))
             sorted))))
      (write-region (point-min) (point-max) filename))))

(defun elpaa--package-oldfiles (pkgname dir)
  ;; FIXME: Use it in `elpaa--make-one-tarball-1'.
  (let ((re (concat "\\`" (regexp-quote pkgname)
                      "-\\([0-9].*\\)\\.\\(tar\\|el\\)\\(\\.[a-z]*z\\)?\\'")))
    (mapcar
     (lambda (file)
       (string-match re file)
       (cons (match-string 1 file) file))
     (directory-files dir nil re))))

(defun elpaa--batch-make-atom-feed ()
  (let* ((filename (pop command-line-args-left))
         (devel (string-match "devel" (file-name-directory filename)))
         (elpaa--url (if devel elpaa--devel-url elpaa--url))
         (pkgname (file-name-sans-extension
                       (file-name-nondirectory filename)))
         (pkg (intern pkgname))
         (pkg-spec (assoc-string pkg (elpaa--get-specs) t))
         (srcdir (format "packages/%s" pkg))
         (files 
          (elpaa--package-oldfiles
           pkgname
           (file-name-directory (expand-file-name filename)))))
    (elpaa--make-atom-feed pkg pkg-spec srcdir files)))
            
(defun elpaa--make-aggregated-atom-feed (filename)
  (let* ((files (sort
                 (directory-files "." nil "\\.xml\\'" 'nosort)
                 (lambda (f1 f2)
                   (time-less-p
                    (file-attribute-modification-time (file-attributes f2))
                    (file-attribute-modification-time (file-attributes f1))))))
         (tail (nthcdr 100 files))
         (entries '()))
    (when tail (setcdr tail nil))
    (setq files (delete filename files))
    ;; Fetch the last entry (which seems to be where the most recent
    ;; entry is placed) of each feed.
    (with-temp-buffer
      (dolist (file files)
        (erase-buffer)
        (insert-file-contents file)
        (let* ((xml (with-demoted-errors "%S" (libxml-parse-xml-region
                                               (point-min) (point-max))))
               (lastentry (assq 'entry (nreverse xml))))
          (when lastentry
            (push lastentry entries))))
      ;; Wrap the list into an actual Atom feed.
      ;; We don't bother to sort the entries because we assume that the
      ;; time of the last entry of each input feed is about the same as the
      ;; modification time of the file, so they should already be ordered.
      (erase-buffer)
      (let* ((self (concat elpaa--url filename)))
        (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
        (xml-print
         ;; See https://validator.w3.org/feed/docs/rfc4287.html
         `((feed
            ((xmlns . "http://www.w3.org/2005/Atom"))
            (title nil ,(concat elpaa--name " ELPA News"))
            (link ((href . ,self) (rel . "self")))
            (id nil ,self)
            (updated nil ,(elpaa--rfc3339 (current-time)))
            ,@entries)))
        (write-region (point-min) (point-max) filename)))))

(provide 'elpa-admin)

;; Local Variables:
;; nameless-current-name: "elpaa"
;; End:

;;; elpa-admin.el ends here
