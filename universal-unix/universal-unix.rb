#!/usr/bin/env ruby

utilities_file = File.dirname(__FILE__) + '/universal-unix.txt'

puts
puts "SEARCH PATH:"
in_path = {}
ENV['PATH'].split(':').each do |path|
  if File.directory?(path)
    puts path
    Dir.open(path).each do |f|
      in_path[f.sub(/\.exe$/,'')] = true
    end
  end
end

utilities = File.open(utilities_file).readlines.map { |o| o.strip }
utilities.reject { |o| /^\s*$/.match(o) }.sort.uniq

puts
puts "NUMBER OF UNIVERSAL UNIX UTILITES:"
puts utilities.size

puts
puts "UNIVERSAL UNIX UTILITIES NOT IN SEARCH PATH:"
utilities.each do |utility|
  unless in_path[utility]
    puts utility
  end
end

