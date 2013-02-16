#!/usr/bin/env ruby

require 'csv'

CSV.open(ARGV[0], 'r') do |row|
  puts row.join("\t")
end
