.PHONY: setup
setup:
	$(MAKE) -C setup setup

# Ctrl + A ; X to close
.PHONY: start-vm
start-vm:
	$(MAKE) -C setup start

.PHONY: deploy
deploy:
	stow -t ~ --ignore=setup */
