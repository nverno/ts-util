#!/usr/bin/env -S nvim -l

-- Print neovim tree-sitter parsers and their sources
local lang = nil

if vim.fn.executable("luarocks") then
  local handle = io.popen("luarocks path --lr-path")
  if handle then
    local result = handle:read("*a")
    package.path = result .. ';' .. package.path
    handle:close()
  end

  local argparse = require "argparse"
  local p = argparse("sources")
  p:option("-l --language", "Language", nil)

  local args = p:parse()
  lang = args["language"] ---@type string
end

---@return string
local function script_path()
  local str = debug.getinfo(2, "S").source:sub(2)
  return str:match("(.*/)")
end

package.preload["vim"] = require("vim.shared")
package.path = script_path() .. "../nvim-treesitter/lua/?.lua;" .. package.path

local parsers = require("nvim-treesitter.parsers")

if lang then
  local info = parsers.list[lang]
  if nil then
    print(info.install_info.url)
    os.exit(0)
  end
  io.stderr:write(string.format("error: '%s' undefined\n", lang))
  os.exit(1)
end

print("(")
for k, v in pairs(parsers.get_parser_configs()) do
  print("(" .. k .. " \"" .. tostring(v.install_info.url) .. "\")")
end
print(")")
