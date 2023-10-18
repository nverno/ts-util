#!/usr/bin/env -S nvim -l

package.preload["vim"] = os.getenv("VIM_PRELOAD")

-- Equivalent to print(), but this will ensure consistent output regardless of
-- operating system.
local function io_print(text)
  if not text then
    text = ""
  end
  io.write(text, "\n")
end

local parsers = require("nvim-treesitter.parsers")

io_print("Available Parsers:")
for _, v in pairs(parsers.available_parsers()) do
  io_print(tostring(v))
end
io_print("")

io_print("Configs:")
for k, v in pairs(require("nvim-treesitter.parsers").get_parser_configs()) do
  print(k .. ": " .. tostring(v.install_info.url))
end
