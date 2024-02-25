# Makefile for GNU Emacs Lisp Package Archive.
#

EMACSBIN=emacs
EMACS=$(EMACSBIN) --batch
RM=rm -f

PKG_DESCS_MK=.pkg-descs.mk

.PHONY: all
all: all-in-place

.PHONY: check/% check-all
check-all: check/-
check/%:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-check $*

.PHONY: build/% build-all %.tar
build/%:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-make-one-package $*

build-all:
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-make-all-packages

%.tar: dummy
	$(EMACS) -l $(CURDIR)/admin/elpa-admin.el	\
	         -f elpaa-batch-make-one-tarball $@

.PHONY: clean
clean:
#	rm -rf archive $(ARCHIVE_TMP)
	$(RM) $(PKG_DESCS_MK)
	$(RM) packages/*/*-autoloads.el
	$(RM) packages/*/*-pkg.el
	find packages -name '*.elc' -print0 | xargs -0 $(RM)

.PHONY: clean/%
clean/%:
	$(RM) packages/$*/*-autoloads.el
	$(RM) packages/$*/*-pkg.el
	find packages/$* -name '*.elc' -print0 | xargs -0 $(RM)

.PHONY: readme
readme:
	$(EMACS) --eval "(progn (require 'org) \
			  (find-file \"README\")\
			  (org-export-to-file 'html \"html/readme.html\"))"

########## Updating specific files ############################################

# Apparently `%` can't match the empty string!
archiv%/index.html: archiv%/archive-contents
	$(EMACS) -l admin/elpa-admin.el \
	         -f elpaa-batch-html-make-index $< $*

########## Rules for in-place installation ####################################
pkgs := $(wildcard packages/*)

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
descs := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)

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

# .PRECIOUS: packages/%-autoloads.el
packages/%-autoloads.el: # elpa-packages
	@#echo 'Generating autoloads for $@'
	$(EMACS) -l admin/elpa-admin.el 		   	   	   \
	         -f elpaa-batch-generate-autoloads $@

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
	    --eval "(setq package-directory-list 		     \
                          (list \"$(abspath other-packages)\")       \
			  load-prefer-newer t			     \
	                  package-user-dir \"$(abspath packages)\")" \
	    -f package-activate-all 		       	     	     \
	    -L $(dir $@) -f batch-byte-compile $<

# .PHONY: elcs
# elcs: $(elcs)

# Remove .elc files that don't have a corresponding .el file any more.
# FIXME
# extra_elcs := $(call SET-diff, $(current_elcs), $(naive_elcs))
.PHONY: $(extra_elcs)
# $(extra_elcs):; rm $@

packages:
	mkdir $@

include $(PKG_DESCS_MK)
$(PKG_DESCS_MK): elpa-packages packages
	$(EMACS) -Q -l admin/elpa-admin.el \
	         -f elpaa-batch-pkg-spec-make-dependencies $@

packages/%-pkg.el:
	@echo 'Generating description file $@'
	@$(EMACS) -l admin/elpa-admin.el \
	          -f elpaa-batch-generate-description-file "$@"

.PHONY: all-in-place autoloads descs install-in-place
# Use order-only prerequisites, so that autoloads are done first.
all-in-place: | install-in-place $(pkgs) #$(extra_elcs)
autoloads: $(autoloads)
descs: $(descs)
install-in-place: autoloads descs

define FILE-els
$(filter %.el, $(1))
endef

define FILE-elcs
$(filter %.elc, $(1))
endef

# Takes two args: arg1 is a set of .el and arg2 a set of .elc
# Return the set of .el files that don't yet have a .elc.
define FILE-notyetcompiled
$(call SET-diff, $(1), $(patsubst %.elc, %.el, $(2)))
endef

# Takes a set of .el files and returns those that can't be byte-compiled.
# FIXME: Take `dir-locals.el` settings into account!
define FILE-nobytecompile
$(foreach el, $(1), \
          $(if $(shell grep '^;.*no-byte-compile: *t' "$(el)"), $(el)))
endef

# Takes a set of .el in arg1 and .elc files in arg2
# and generates the set of .elc files that need to be generated.
define FILE-computeddeps2
$(patsubst %.el, %.elc, 		 	\
    $(call SET-diff, $(1), 		 	\
                     $(call FILE-nobytecompile, \
                            $(call FILE-notyetcompiled, $(1), $(2)))))
endef

# Takes a pkgname in arg1 and a set of .el and .elc files in arg2, and
# generates the set of .elc files that need to be generated.
define FILE-computeddeps1
$(call FILE-computeddeps2, $(call FILE-els, $(1)), $(call FILE-elcs, $(1)))
endef

define TRACE
$(info TRACE($(1)): $(2))$(2)
endef

# Compute the dependencies for a file packages/%.
# The main case is for the `packages/[PKGNAME]` directory.
# FIXME: Remove outdated .elc files with no matching .el file!
define FILE-deps
$(if $(findstring /, $(1)), 	       		       \
     $(if $(patsubst %.elc,,$(1)),		       \
          $(patsubst %.elc, %.el, $(1))),	       \
     $(if $(wildcard packages/$(1)/*),		       \
          packages/$(1)/$(1)-pkg.el      	       \
          packages/$(1)/$(1)-autoloads.el)             \
     $(call FILE-computeddeps1,			       \
        $(shell [ -d packages/$(1) ] && { 	       \
                  tar -cvhf /dev/null 		       \
                      --exclude-ignore=.elpaignore     \
                      --exclude='*-pkg.el'	       \
                      --exclude='*-autoloads.el'       \
                      --exclude='.dir-locals.el'       \
                      --exclude-vcs packages/$(1) 2>&1 \
                  | sed -ne 's/\(\.elc*\)$$/\1/p';})))
endef

.PHONY: dummy
dummy:
#	# echo Making dummies

.SECONDEXPANSION:
packages/% : dummy $$(call FILE-deps,$$*)
	@[ -d packages/$* ] || {		       	 	      \
            echo $(EMACS) -l admin/elpa-admin.el 		      \
	             -f elpaa-batch-archive-update-worktrees "$(@F)"; \
            $(EMACS) -l admin/elpa-admin.el 			      \
	             -f elpaa-batch-archive-update-worktrees "$(@F)"; }

#### Fetching updates from upstream                                        ####

.PHONY: fetch/%
fetch/%:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-show "$*"

.PHONY: fetch-all
fetch-all:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-show :

.PHONY: sync/%
sync/%:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-push "$*"

.PHONY: sync-all
sync-all:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-push :

# Only sync those packages which enable the `:auto-sync` property.
.PHONY: sync-some
sync-some:
	$(EMACS) -l admin/elpa-admin.el -f elpaa-batch-fetch-and-push :auto-sync



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
