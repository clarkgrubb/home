#!/usr/bin/env ruby

require 'getoptlong'

opts = GetoptLong.new(
  ['--html-entity', '-e',
   GetoptLong::NO_ARGUMENT],
  ['--title', '-t',
   GetoptLong::REQUIRED_ARGUMENT]
)

title = nil
html_entity = false

def usage
  raise "USAGE: unicode-table.rb [--html-entity] [--title=TITLE] START_CODE_POINT END_CODE_POINT"
end

opts.each do |opt, arg|
  case opt
  when '--html-entity'
    html_entity = true
  when '--title'
    title = arg
  else
    usage
  end
end

unless /^1\.9/.match(RUBY_VERSION)
  raise "ruby 1.9 needed for unicode support"
end

usage if ARGV.size != 2

first_code_point = ARGV[0].to_i(16) / 16 * 16
last_code_point = ARGV[1].to_i(16) / 16 * 16

def print_row(a)
      puts '||' + a.join('||') + '||' unless a.empty?
end


puts '||' * 17 + '~ ' + title + '||' if title

header = [''] + ('0'..'9').to_a + ('A'..'F').to_a
header.map! { |s| '~ ' + s }
print_row(header)

row = []
(first_code_point...last_code_point).to_a.each do |i|

  if i % 16 == 0
    print_row(row)
    row = ['U+' + i.to_s(16)]
  end

  if html_entity
    row << '@<&#x' + i.to_s(16) + ';>@'
  else
    row << eval("\"\\u{#{i.to_s(16)}}\"")
  end
    
end
print_row(row)
