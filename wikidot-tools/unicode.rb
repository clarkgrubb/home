#!/usr/bin/env ruby

require 'getoptlong'
require 'pp'

class Formatter

  attr_accessor :html_entity, :title
  
  def initialize
    @title = ''
    @html_entity = false
    @format = 'tab'
    @formats = %w(wikidot tab)
    @ruby19 = /^1\.9/.match(RUBY_VERSION)
  end

  def format(fmt)
    unless @formats.include?(fmt)
      raise "Not a supported format: #{fmt} Choose from: #{@formats.join(' ')}"
    end
    @format = fmt
  end  

  def int(i)
    if @html_entity
      '@<&#x' + i.to_s(16) + ';>@'
    elsif @ruby19
      eval("\"\\u{#{i.to_s(16)}}\"")
    else
      raise "Ruby 1.9 required for U+10000 and higher"
    end
  end
  
  def print_row(f, row)
    case @format
    when 'tab'
      f.puts row.join("\t")
    when 'wikidot'
      f.puts '||' + row.join('||') + '||'
    else
      raise 'bad fmt: ' + @format
    end
  end

  def print_header(f, header)
    case @format
    when 'tab'
      print_row(f, header)
    when 'wikidot'
      print_row(f, header.map { |s| '~ ' + s })
    else
      raise 'bad fmt: ' + @format
    end
  end
  
  def print_title(f)
    case @format
    when 'tab'
      f.puts @title if @title
    when 'wikidot'
      f.puts '||' * 17 + '~ ' + @title + '||' if @title
    else
      raise 'bad fmt: ' + @format
    end
  end
    
end

def usage
  $stderr.puts <<EOF
USAGE: unicode-table.rb [--html-entity] \\
                        [--title=TITLE] \\
                        [--format=(tab|wikidot)] \\
                        CODE_POINT(-| )CODE_POINT
EOF
  exit 1
end

def process_args

  formatter = Formatter.new
  
  opts = GetoptLong.new(
                        ['--html-entity', '-e',
                         GetoptLong::NO_ARGUMENT],
                        ['--format', '-f',
                         GetoptLong::REQUIRED_ARGUMENT],
                        ['--title', '-t',
                         GetoptLong::REQUIRED_ARGUMENT]
                        )

  opts.each do |opt, arg|
    case opt
    when '--html-entity'
      formatter.html_entity = true
    when '--format'
      begin
        formatter.format(arg)
      rescue
        $stderr.puts $!
        usage
      end
    when '--title'
      formatter.title = arg
    else
      usage
    end
  end

  if ARGV.size == 0 or ARGV.size > 2
    $stderr.puts "Number of arguments provided: #{ARGV.size.to_s}"
    usage
  end
  if ARGV.size == 1
    args = ARGV[0].split('-')
  else
    args = ARGV
  end
  usage if args.size != 2
  
  first_code_point = args[0].to_i(16) / 16 * 16
  last_code_point = (args[1].to_i(16) + 1) / 16 * 16

  return formatter, first_code_point...last_code_point
end

def print_rows(formatter, code_point_range)
  
  formatter.print_title($stdout)
  
  header = [''] + ('0'..'9').to_a + ('A'..'F').to_a
  formatter.print_header($stdout, header)
  
  row = []
  code_point_range.to_a.each do |i|

    if i % 16 == 0
      formatter.print_row($stdout, row) unless row.empty?
      row = ['U+' + i.to_s(16)]
    end

    row << formatter.int(i)
    
  end
  formatter.print_row($stdout, row) unless row.empty?
end

formatter, code_point_range = process_args
print_rows(formatter, code_point_range)
