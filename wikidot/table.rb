#!/usr/bin/env ruby

require 'getoptlong'
require 'pp'

CONTINUATION_REGEX = / _$/

def bar_split(line)

  inside_at_quote = false
  a = []
  tokens = line.scan(/\|\||@@|.+?(?=\|\||@@)|.+$/m)

  s = ""

  loop do
    token = tokens.shift
    break if token.nil?
    case token
    when '||'
      if inside_at_quote
        s += token
      else
        a << s
        s = ""
      end
    when '@@'
      if inside_at_quote
        inside_at_quote = false
      elsif tokens.include?('@@')
        inside_at_quote = true
      else
        # unmatched starting @@
      end
      s += token
    else
      s += token
    end
  end

  a << s

  a
end

def header_row?(row)
  row.select { |col| not col.empty? }.size == 1
end

def header_column(row)
  row.select { |col| not col.empty? }.first
end

def parse(f)

  table = []
  columns = []

  f.each do |line|

    a = bar_split(line)

    if columns.empty?
      columns = a
    else
      columns[-1] += a.shift
      columns.concat(a)
    end

    if !CONTINUATION_REGEX.match(columns.last)
      columns[-1].chomp!
      table << columns
      columns = []
    end
  end

  unless columns.empty?
    columns[-1].chomp!
    table << columns
  end

  table
end

def generate(f, table)

  table.each do |columns|
    f.puts columns.join('||')
  end
end

def reorder(table, columns)

  columns.unshift(0)
  columns << 0

  table.map do |row|
    if header_row?(row)
      header = columns.map { |i| "" }
      header[-2] = header_column(row)
      header
    else
      columns.map { |i| (i > 0 and (row[i].nil? or row[i].empty?)) ? " " : row[i] }
    end
  end
end

def sort(table)
  table.sort { |o1,o2| o1[1] <=> o2[1] }
end

def usage
  $stderr.puts "table.rb --sort --columns=COL1,COL2,... < INPUT"
  exit -1
end

if $0 == __FILE__

  opts = GetoptLong.new(
                        [ '--columns', "-c", GetoptLong::REQUIRED_ARGUMENT ],
                        [ '--sort', "-s", GetoptLong::NO_ARGUMENT ]
                        )

  columns = []
  sort_table = false
  opts.each do |opt,arg|
    case opt
    when '--columns'
      columns = arg.split(',',-1).map { |s| s.to_i }
    when '--sort'
      sort_table = true
    end
  end

  usage if columns.empty?
  usage if columns.any? { |col| col.to_i < 1 }

  table = parse($stdin)

  if sort_table
    table = sort(table)
  end

  generate($stdout, reorder(table, columns))

end


