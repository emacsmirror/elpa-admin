# Makefile for GNU Emacs Lisp Package Archive.
#

EMACS=emacs --batch

ARCHIVE_TMP=archive-tmp
SITE_DIR=site

.PHONY: archive-tmp changelogs process-archive archive-full org-fetch clean all do-it

all: all-in-place

CR_EXCEPTIONS=copyright_exceptions
.PHONY: check_copyrights
check_copyrights:
	@echo "Compute exceptions >$(CR_EXCEPTIONS)~"
	@export LC_ALL=C;					    \
	(cd packages &&						    \
	find . -name '.git' -prune -o				    \
	       -name 'test' -prune -o				    \
	       -name '*.el' -print0 |				    \
	    xargs -0 grep -L 'Free Software Foundation, Inc' |	    \
	    grep -v '\(\.dir-locals\|.-\(pkg\|autoloads\)\)\.el$$'; \
	find . -name '.git' -prune -o -name '*.el' -type f -print | \
	    while read f; do					    \
	        fquoted="$$(echo $$f|tr '|' '_')";		    \
	        sed -n -e '/[Cc]opyright.*, *[1-9][-0-9]*,\?$$/N'   \
	            -e '/Free Software Foundation/d'		    \
	            -e "s|^\\(.*;.*[Cc]opyright\\)|$$fquoted:\\1|p" \
	           "$$f";					    \
	    done) | sort >$(CR_EXCEPTIONS)~
	diff -u "$(CR_EXCEPTIONS)" "$(CR_EXCEPTIONS)~"

## Deploy the package archive to archive/, with packages in
## archive/packages/:
archive: archive-tmp
	$(MAKE) $(MFLAGS) process-archive

archive-tmp: packages
	-rm -r $(ARCHIVE_TMP)
	mkdir -p $(ARCHIVE_TMP)
	cp -a packages/. $(ARCHIVE_TMP)/packages

# Use && after the cd commands, not ;, to ensure the build fails
# immediately if the directory $(ARCHIVE_TMP)/packages does not exist.
# For process-archive this is crucial; otherwise batch-make-archive in
# archive-contents.el will interpret directories in the current
# directory as unreleased packages, and recursively delete them,
# including .git.  Prior to using &&, running "make process-archive"
# could silently delete all local git history!
process-archive:
	# FIXME, we could probably speed this up significantly with
	# rules like "%.tar: ../%/ChangeLog" so we only rebuild the packages
	# that have indeed changed.
	cd $(ARCHIVE_TMP)/packages &&				\
	  $(EMACS) -l $(CURDIR)/admin/archive-contents.el	\
	           -f batch-make-archive
	@cd $(ARCHIVE_TMP)/packages &&					\
	  for pt in *; do						\
	      if [ -f "$${pt}/.elpaignore" ]; then			\
		  ignore="$${pt}/.elpaignore";				\
	      else							\
		  ignore="/dev/null";					\
	      fi;							\
	      if [ -d $$pt ]; then					\
		  echo "Creating tarball $${pt}.tar" &&			\
		  tar --exclude-vcs -X "$$ignore" -chf $${pt}.tar $$pt;	\
		  rm -rf $${pt}; 					\
	      fi;							\
	  done
	mkdir -p archive/packages
	mv archive/packages archive/packages-old
	mv $(ARCHIVE_TMP)/packages archive/packages
	chmod -R a+rX archive/packages
	rm -rf archive/packages-old
	rm -rf $(ARCHIVE_TMP)

## Deploy the package archive to archive/ including the Org daily:
archive-full: archive-tmp org-fetch
	$(MAKE) $(MFLAGS) process-archive
	#mkdir -p archive/admin
	#cp admin/* archive/admin/

# FIXME: Turn it into an `external', which will require adding the notion of
# "snapshot" packages.
org-fetch: archive-tmp
	-cd $(ARCHIVE_TMP)/packages && 													       	\
	pkgname=`wget -q -O- https://orgmode.org/elpa/|perl -ne 'push @f, $$1 if m/(org-\d{8})\.tar/; END { @f = sort @f; print "$$f[-1]\n"}'`; \
	wget -q https://orgmode.org/elpa/$${pkgname}.tar -O $${pkgname}.tar; 								       	\
	if [ -f $${pkgname}.tar ]; then 												       	\
		tar xf $${pkgname}.tar; 												       	\
		rm -f $${pkgname}.tar; 													       	\
		mv $${pkgname} org; 													       	\
	fi

clean:
	rm -rf archive $(ARCHIVE_TMP) $(SITE_DIR)

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

$(foreach al, $(autoloads), $(eval $(call RULE-srcdeps, $(al))))
%-autoloads.el:
	@#echo 'Generating autoloads for $@'
	@cd $(dir $@) && 						   \
	  $(EMACS) -l $(CURDIR)/admin/archive-contents.el 		   \
	      --eval "(require 'package)" 				   \
	      --eval "(load (expand-file-name \"../names/names-autoloads.el\") t t)" \
	      --eval "(package-generate-autoloads \"$$(basename $$(pwd))\" \
	                                          \"$$(pwd)\")"

# Put into elcs the set of elc files we need to keep up-to-date.
# I.e. one for each .el file in each package root, except for the -pkg.el,
# the -autoloads.el, the .el files that are marked "no-byte-compile", and
# files matching patterns in packages' .elpaignore files.
included_els := $(shell tar -cvhf /dev/null --exclude-ignore=.elpaignore \
                            --exclude-vcs packages 2>&1 | grep '\.el$$')

# included_els := $(wildcard packages/*/*.el)

# els := $(call FILTER-nonsrc, $(wildcard packages/*/*.el     \
# 					packages/*/*/*.el   \
# 					packages/*/*/*/*.el \
# 	                                packages/*/*/*/*/*.el))
els := $(call FILTER-nonsrc, $(included_els))
naive_elcs := $(patsubst %.el, %.elc, $(els))
current_elcs := $(shell find packages -name '*.elc' -print)

extra_els := $(call SET-diff, $(els), $(patsubst %.elc, %.el, $(current_elcs)))
nbc_els := $(foreach el, $(extra_els), \
             $(if $(shell grep '^;.*no-byte-compile: *t' "$(el)"), $(el)))
elcs := $(call SET-diff, $(naive_elcs), $(patsubst %.el, %.elc, $(nbc_els)))

# '(dolist (al (quote ($(patsubst %, "%", $(autoloads))))) (load (expand-file-name al) nil t))'
%.elc: %.el
	@echo 'Byte compiling $<'
	@$(EMACS) 		  		       	       	     \
	    --eval "(setq package-directory-list nil   	       	     \
			  load-prefer-newer t			     \
                          package-user-dir \"$(abspath packages)\")" \
	    -f package-initialize 		       	       	     \
	    -L $(dir $@) -f batch-byte-compile $<

.PHONY: elcs
elcs: $(elcs)

# Remove .elc files that don't have a corresponding .el file any more.
extra_elcs := $(call SET-diff, $(current_elcs), $(naive_elcs))
.PHONY: $(extra_elcs)
$(extra_elcs):; rm $@

# # Put into single_pkgs the set of -pkg.el files we need to keep up-to-date.
# # I.e. all the -pkg.el files for the single-file packages.
pkg_descs:=$(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)
#$(foreach al, $(single_pkgs), $(eval $(call RULE-srcdeps, $(al))))
%-pkg.el: %.el
	@echo 'Generating description file $@'
	@$(EMACS) \
	    --eval '(require (quote package))' \
	    --eval '(setq b (find-file-noselect "$<"))' \
	    --eval '(setq d (with-current-buffer b (package-buffer-info)))' \
	    --eval '(package-generate-description-file d "$@")'

.PHONY: all-in-place
# Use order-only prerequisites, so that autoloads are done first.
all-in-place: | $(extra_elcs) $(autoloads) $(pkg_descs) elcs

##### Compiling the files of just a single package

# FIXME: This should be tuned so as to "git worktree add" the branch
# if the $(1) directory doesn't exist yet!
define RULE-singlepkg
$(filter $(1)/%, $(elcs)): $1/$(notdir $(1))-pkg.el \
                           $1/$(notdir $(1))-autoloads.el
$(1): $(filter $(1)/%, $(elcs))
endef
$(foreach pkg, $(pkgs), $(eval $(call RULE-singlepkg, $(pkg))))



############### Rules to prepare the externals ################################

.PHONY:
externals:
	$(EMACS) -l admin/archive-contents.el \
	    -f archive-add/remove/update-externals




################### Testing ###############

PACKAGE_DIRS = $(shell find packages -maxdepth 1 -type d)
PACKAGES=$(subst /,,$(subst packages,,$(PACKAGE_DIRS)))

TOP =$(shell pwd)

define test_template
$(1)-test:
	cd packages/$(1);\
	$(EMACS) -l $(TOP)/admin/ert-support.el \
		--eval "(ert-support-test-package \"$(TOP)\" '$(1))" \

$(1)-test-log:
	$(MAKE) $(1)-test > packages/$(1)/$(1).log 2>&1 || { stat=ERROR; }
endef

$(foreach package,$(PACKAGES),$(eval $(call test_template,$(package))))

PACKAGES_TESTS=$(addsuffix -test-log,$(PACKAGES))
PACKAGES_LOG=$(foreach package,$(PACKAGES),packages/$(package)/$(package).log)

check: $(PACKAGES_TESTS)
	$(EMACS) -l ert -f ert-summarize-tests-batch-and-exit $(PACKAGES_LOG)
