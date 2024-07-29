#!/usr/bin/env ruby
# frozen_string_literal: true

##
# Extract code sections from corpii
##

content = []
ARGF.each_line do |line| 
  content << line
end

sections = content.join.split(/==+\n.*\n==+/)
res = []
sections.each do |section| 
  s = section.split(/\n*---+\n+/).first
  next unless s

  s.chomp!(' \t\n')
  res << s.strip

  c = ENV['TS_STATEMENT_TERM']
  res << c unless c.nil? || s.end_with?(c)
end

puts res.join("\n\n")
