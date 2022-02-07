.PHONY: all update build dist

PATH := /local/usr/bin:$(PATH)

TEMPDIR := $(shell mktemp -d)
BASENAME := $(shell basename $(shell pwd))
GITHUB_REPO := git@github.com-$(BASENAME):legenepi/$(BASENAME).git

LOG_ROOT := /local/var/log
LOG := $(LOG_ROOT)/$(BASENAME).log

DIST_DIR := $(shell pwd)/dist
ARCHIVE_SUFFIX := .tar.gz

CONDA_ACTIVATE := source /local/miniconda3/etc/profile.d/conda.sh; conda activate

TAG_LIST := $(shell git fetch; git tag --list | sed -e "s/^v\(.*\)$$/\1/" | sort --version-sort)

define get_version
	$(lastword $(subst _, ,$(patsubst %$(ARCHIVE_SUFFIX),%,$(notdir $(1)))))
endef

define make_archive_name
	$(addsuffix $(ARCHIVE_SUFFIX),$(addprefix $(DIST_DIR)/$(subst -,.,$(BASENAME))_,$(1)))
endef

all:

$(DIST_DIR):
	mkdir -p $@

$(call make_archive_name,$(TAG_LIST)):
	echo $(shell date +"%d-%m-%Y %H:%M:%S") $(MAKE) -$(MAKEFLAGS) $@ 2>&1 | tee -a $(LOG)
	$(eval VERSION=$(call get_version,$@))
	$(eval RELEASE_DIR=$(TEMPDIR)/$(VERSION))
	git -c advice.detachedHead=false clone --depth 1 --branch v$(VERSION) $(GITHUB_REPO) $(RELEASE_DIR) 2>&1 | tee -a $(LOG)
	cd $(DIST_DIR); $(CONDA_ACTIVATE) $(shell pwd)/.conda; R CMD build $(RELEASE_DIR) 2>&1 | tee -a $(LOG)

dist: $(DIST_DIR) $(call make_archive_name,$(TAG_LIST))
	echo $(shell date +"%d-%m-%Y %H:%M:%S") $(MAKE) -$(MAKEFLAGS) $@ 2>&1 | tee -a $(LOG)
	cd $(DIST_DIR); ln -sf $(notdir $(call make_archive_name, $(lastword $(TAG_LIST)))) $(subst -,.,$(BASENAME))$(ARCHIVE_SUFFIX);
	$(RM) -r $(TEMPDIR)

update:
	echo $(shell date +"%d-%m-%Y %H:%M:%S") $(MAKE) -$(MAKEFLAGS) $@ 2>&1 | tee -a $(LOG)
	git checkout main
	git pull 2>&1 | tee -a $(LOG)

build:
	echo $(shell date +"%d-%m-%Y %H:%M:%S") $(MAKE) -$(MAKEFLAGS) $@ 2>&1 | tee -a $(LOG)
	$(CONDA_ACTIVATE) $(shell pwd)/.conda; R -e "pkgdown::build_site(preview = FALSE)" 2>&1 | tee -a $(LOG)

