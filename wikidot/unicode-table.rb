#!/usr/bin/env ruby

if !/^1\.9/.match(RUBY_VERSION)
  raise "ruby 1.9 needed for unicode support"
end

HTML_ENTITY = ( ARGV[0] == '--html-entity' ? true : false )
ARGV.shift if HTML_ENTITY

if ARGV.size != 2
  raise "USAGE: unicode-table.rb [--html-entity] START_CODE_POINT END_CODE_POINT"
end

first_code_point = ARGV[0].to_i(16) / 16 * 16
last_code_point = ARGV[1].to_i(16) / 16 * 16

def print_row(a)
      puts '||' + a.join('||') + '||' unless a.empty?
end

header = [''] + ('0'..'9').to_a + ('A'..'F').to_a
header.map! { |s| '~ ' + s }
print_row(header)

row = []
(first_code_point...last_code_point).to_a.each do |i|

  if i % 16 == 0
    print_row(row)
    row = [i.to_s(16)]
  end

  if HTML_ENTITY
    row << '@<&#x' + i.to_s(16) + ';>@'
  else
    row << eval("\"\\u{#{i.to_s(16)}}\"")
  end
    
end
print_row(row)
