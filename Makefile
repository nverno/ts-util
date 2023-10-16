SHELL = /bin/bash

BINDIR = $(CURDIR)/bin

NEOVIM_REPO  = https://github.com/neovim/neovim
NVIM_TS_REPO = https://github.com/nvim-treesitter/nvim-treesitter
SRCDIRS      = $(notdir $(REPOS))
NEOVIM       = $(notdir $(NEOVIM_REPO))
NVIM         = $(notdir $(NVIM_TS_REPO))

NEOVIM_PATH = $(CURDIR)/$(NEOVIM)/runtime/lua
NVIM_PATH   = $(CURDIR)/$(NVIM)/lua
LUA_PATH    = $(NEOVIM_PATH)/?.lua;$(NVIM_PATH)/?.lua;;
VIM_PRELOAD = $(NEOVIM_PATH)/vim/shared.lua

export LUA_PATH
export VIM_PRELOAD

CLONE = @git clone --depth=1 $(1) $(2)

all:
	@echo $(LUA_PATH)

install: $(SRCDIRS)
$(NEOVIM):
	$(call CLONE,$(NEOVIM_REPO),$@)
$(NVIM):
	$(call CLONE,$(NVIM_TS_REPO),$@)


.PHONY: sources
sources:
	$(BINDIR)/sources.lua
