.PHONY: setup
setup:
	$(MAKE) -C setup setup

# Ctrl + A ; X to close
.PHONY: start-vm
start-vm:
	$(MAKE) -C setup start

IGNORED_DIRS := claude/

stowed:
	stow -v -t $(HOME)/ --ignore=Makefile $(filter-out $(IGNORED_DIRS),$(wildcard */)) --no-folding --no

.PHONY: $(IGNORED_DIRS)
$(IGNORED_DIRS):
	$(MAKE) -C $@

.PHONY: custom
custom: $(IGNORED_DIRS)


.PHONY: deploy
deploy: stowed custom
