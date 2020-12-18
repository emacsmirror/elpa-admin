# Makefile for GNU Emacs Lisp Package Archive.
#

EMACS=emacs --batch

.PHONY: all
all: all-in-place

.PHONY: check/% check-all
check-all: check/-
check/%:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-copyright-check $*

.PHONY: build/% build-all
build/%:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-make-one-package $*

build-all:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-make-all-packages

.PHONY: clean
clean:
#	rm -rf archive $(ARCHIVE_TMP)
	rm -f packages/*/*-autoloads.el
	find packages -name '*.elc' -print0 | xargs -0 rm -f

.PHONY: readme
readme:
	$(EMACS) --eval "(progn (require 'org) \
			  (find-file \"README\")\
			  (org-export-to-file 'html \"html/readme.html\"))"

########## Rules for in-place installation ####################################
pkgs := $(foreach pkg, $(wildcard packages/*), \
          $(if $(shell [ -d "$(pkg)" ] && echo true), $(pkg)))

define SET-diff
$(shell $(file > .tmp.setdiff, $(1))  \
        $(file >> .tmp.setdiff, $(2)) \
        $(file >> .tmp.setdiff, $(2)) \
        tr ' ' '\n' < .tmp.setdiff | sort | uniq -u ; rm .tmp.setdiff)
endef

define FILTER-nonsrc
$(filter-out %-autoloads.el %-pkg.el %/.dir-locals.el, $(1))
endef

define RULE-srcdeps
$(1): $$(call FILTER-nonsrc, $$(wildcard $$(dir $(1))/*.el))
endef

# Compute the set of autolods files and their dependencies.
autoloads := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-autoloads.el)

# FIXME: In 99% of the cases, autoloads can be generated in any order.
# But the `names' package is an exception because it sets up an advice that
# changes the way autload.el operates, and that advice is needed when creating
# the autoloads file of packages that use `names'.
# The right solution is to check the Package-Requires and create the autoloads
# files in topological order, but for now we can just do it the ad-hoc way and
# add hand-made dependencies between autoloads files, and explicitly
# load the names-autoloads file when building autoloads files. An example entry
# is commented below, this is what should be done if a package depends on Names.

# packages/aggressive-indent/aggressive-indent-autoloads.el: \
#     packages/names/names-autoloads.el

packages/%-autoloads.el:
	@#echo 'Generating autoloads for $@'
	@cd $(dir $@) && 						   \
	  $(EMACS) -l $(CURDIR)/admin/elpa-admin.el 		   \
	      --eval "(require 'package)" 				   \
	      --eval "(load (expand-file-name \"../names/names-autoloads.el\") t t)" \
	      --eval "(package-generate-autoloads \"$$(basename $$(pwd))\" \
	                                          \"$$(pwd)\")"

# Put into elcs the set of elc files we need to keep up-to-date.
# I.e. one for each .el file in each package root, except for the -pkg.el,
# the -autoloads.el, the .el files that are marked "no-byte-compile", and
# files matching patterns in packages' .elpaignore files.
# included_els := $(shell tar -cvhf /dev/null --exclude-ignore=.elpaignore \
#                             --exclude-vcs packages 2>&1 | grep '\.el$$')

# included_els := $(wildcard packages/*/*.el)

# els := $(call FILTER-nonsrc, $(wildcard packages/*/*.el     \
# 					packages/*/*/*.el   \
# 					packages/*/*/*/*.el \
# 	                                packages/*/*/*/*/*.el))
# els := $(call FILTER-nonsrc, $(included_els))
# naive_elcs := $(patsubst %.el, %.elc, $(els))
current_elcs := $(shell find packages -name '*.elc' -print)

extra_els := $(call SET-diff, $(els), $(patsubst %.elc, %.el, $(current_elcs)))
nbc_els := $(foreach el, $(extra_els), \
             $(if $(shell grep '^;.*no-byte-compile: *t' "$(el)"), $(el)))
elcs := $(call SET-diff, $(naive_elcs), $(patsubst %.el, %.elc, $(nbc_els)))

# '(dolist (al (quote ($(patsubst %, "%", $(autoloads))))) (load (expand-file-name al) nil t))'
.PRECIOUS: packages/%.elc
packages/%.elc: packages/%.el
	@echo 'Byte compiling $<'
	@$(EMACS) 		  		       	     	     \
	    --eval "(setq package-directory-list nil   	     	     \
			  load-prefer-newer t			     \
	                  package-user-dir \"$(abspath packages)\")" \
	    -f package-initialize 		       	     	     \
	    -L $(dir $@) -f batch-byte-compile $<

# .PHONY: elcs
# elcs: $(elcs)

# Remove .elc files that don't have a corresponding .el file any more.
# FIXME
# extra_elcs := $(call SET-diff, $(current_elcs), $(naive_elcs))
.PHONY: $(extra_elcs)
# $(extra_elcs):; rm $@

# # Put into single_pkgs the set of -pkg.el files we need to keep up-to-date.
# # I.e. all the -pkg.el files for the single-file packages.
pkg_descs:=$(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)
#$(foreach al, $(single_pkgs), $(eval $(call RULE-srcdeps, $(al))))
packages/%-pkg.el: packages/%.el
	@echo 'Generating description file $@'
	@$(EMACS) -l admin/elpa-admin.el \
	          -f elpaa-batch-generate-description-file "$@"

.PHONY: all-in-place
# Use order-only prerequisites, so that autoloads are done first.
all-in-place: | $(extra_elcs) $(autoloads) $(pkg_descs) elcs

define FILE-deps
$(if $(findstring /, $(1)), 			     \
     $(if $(patsubst %.elc,,$(1)),		     \
          $(patsubst %.elc, %.el, $(1))),	     \
     $(shell [ -d packages/$(1) ] && { 	  	     \
               echo packages/$(1)/$(1)-pkg.el; 	     \
               echo packages/$(1)/$(1)-autoloads.el; \
               tar -cvhf /dev/null 		     \
                   --exclude-ignore=.elpaignore      \
		   --exclude='*-pkg.el'		     \
		   --exclude='*-autoloads.el'	     \
                   --exclude-vcs packages/$(1) 2>&1  \
               | sed -ne 's/\.el$$/.elc/p';}))
endef

# define FILE-cmd
# $(if $(findstring /, $(1)), 			  		       \
#      $(if $(patsubst %.elc,,$(1)),		  		       \
# 	  $(EMACS) 		  		       	       	       \
# 	      --eval "(setq package-directory-list nil   	       \
# 			    load-prefer-newer t			       \
#                             package-user-dir \"$(abspath packages)\")" \
# 	      -f package-initialize 		       	       	       \
# 	      -L $(dir $@) -f batch-byte-compile $<,		       \
#           echo YUP: $(1)),					       \
# 	[ -d packages/$(1) ] || 				       \
#             $(EMACS) -l admin/elpa-admin.el       		       \
# 	             -f elpaa-batch-archive-update-worktrees "$(@F)")
# endef

# define EMACS-update-tree-cmd
# $(EMACS) -l admin/elpa-admin.el \
#          -f elpaa-batch-archive-update-worktrees "$(@F)"
# endef
# define EMACS-update-tree-cmd
# $(shell echo $(call EMACS-update-tree-cmd,$(1))) \
# $(call EMACS-update-tree-cmd,$(1))
# endef

.PHONY: dummy
dummy:
#	# echo Making dummies

.SECONDEXPANSION:
packages/% : dummy $$(call FILE-deps,$$*)
	[ -d packages/$* ] || 		    \
            $(EMACS) -l admin/elpa-admin.el \
	             -f elpaa-batch-archive-update-worktrees "$(@F)"

#### Fetching updates from upstream                                        ####

.PHONY: fetch/%
fetch/%:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-show "$*"

.PHONY: fetch-all
fetch-all:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-show "-"

.PHONY: sync/%
sync/%:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-push "$*"

.PHONY: sync-all
sync-all:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-push "-"



############### Rules to prepare the externals ################################

.PHONY: externals worktrees
externals worktrees:	# "externals" is the old name we used to use.
	$(EMACS) -l admin/elpa-admin.el \
	    -f elpaa-add/remove/update-worktrees


################### Testing ###############

PACKAGE_DIRS = $(shell find packages -maxdepth 1 -type d)
PACKAGES=$(subst /,,$(subst packages,,$(PACKAGE_DIRS)))

define test_template
$(1)-test:
	cd packages/$(1);				       	      \
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el.el 		      \
		--eval "(elpaa-ert-test-package \"$(CURDIR)\" '$(1))" \

$(1)-test-log:
	$(MAKE) $(1)-test > packages/$(1)/$(1).log 2>&1 || { stat=ERROR; }
endef

$(foreach package,$(PACKAGES),$(eval $(call test_template,$(package))))

PACKAGES_TESTS=$(addsuffix -test-log,$(PACKAGES))
PACKAGES_LOG=$(foreach package,$(PACKAGES),packages/$(package)/$(package).log)

check: $(PACKAGES_TESTS)
	$(EMACS) -l ert -f ert-summarize-tests-batch-and-exit $(PACKAGES_LOG)
