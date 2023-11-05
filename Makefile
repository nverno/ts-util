SHELL        = /bin/bash

BINDIR       = $(CURDIR)/bin

NEOVIM_REPO  = https://github.com/neovim/neovim
NVIM_TS_REPO = https://github.com/nvim-treesitter/nvim-treesitter
NEOVIM       = $(notdir $(NEOVIM_REPO))
NVIM         = $(notdir $(NVIM_TS_REPO))
SRCDIRS      = $(NEOVIM) $(NVIM)

NEOVIM_PATH  = $(CURDIR)/$(NEOVIM)/runtime/lua
NVIM_PATH    = $(CURDIR)/$(NVIM)/lua
LUA_PATH     = $(shell luarocks path --lr-path);$(NEOVIM_PATH)/?.lua;$(NVIM_PATH)/?.lua;;
VIM_PRELOAD  = $(NEOVIM_PATH)/vim/shared.lua
# Exports for $(BINDIR)/sources.lua
export LUA_PATH
export VIM_PRELOAD

CLONE        = @git clone --depth=1 $(1) $(2)

.PHONY: sources all install
all: install        ## Download repos

install: $(SRCDIRS) ## Clone neovim, nvim-treesitter repos
$(NEOVIM):
	$(call CLONE,$(NEOVIM_REPO),$@)
$(NVIM):
	$(call CLONE,$(NVIM_TS_REPO),$@)

sources:            ## Print neovim parsers and their source repos
	$(BINDIR)/sources.lua


.PHONY: clean distclean help
clean:
	$(RM) -r *~ *.o *.out *.exe

distclean: clean
	$(RM) -rf $$(git ls-files --others --ignored --exclude-standard)

help:               ## Show help for targets
	@grep -E '^[/.%0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	sort | awk \
	'BEGIN {FS = ":[^:#]*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
