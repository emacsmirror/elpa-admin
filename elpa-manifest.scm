;; GNU Guix manifest for (Non)GNU ELPA
;;
;; This file specifies all the packages that are required for the ELPA
;; build system to function correctly.  You can either use the "guix
;; shell" command to create an environment with everything prepared.

(specifications->manifest
 (list "bubblewrap"
       "coreutils"
       "emacs-minimal"
       "git"
       "grep"
       "imagemagick"
       "lzip"
       "make"
       "markdown"
       "tar"
       "texinfo"))

