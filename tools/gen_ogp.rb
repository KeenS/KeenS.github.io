#!/usr/bin/env ruby
# coding: utf-8

require 'erb'
require 'yaml'
require 'date'
require 'fileutils'
require_relative 'inkscape'

def char_class_of(c)
  case c
  when /[a-zA-Zα-ωΑ-Ω]/
    :alphabetic
  when /[0-9０−９]/
    :numeral
  when /\s/
    :spaces
  when /[「（(\["'※『{〔〘〈《【〖]/
    :yaku_start
  when /[。、…)）」!?？！\]ー』}〕〙》〉】〗]/
    :yaku_end
  when /[\p{Hiragana}]/
    :hiragana
  when /[\p{Katakana}]/
    :katakana
  when /[\p{Han}]/
    :kanji
  else
    :other
  end
end

def all_char_classes
  [:alphabetic, :numeral, :spaces, :yaku_start, :yaku_end, :hiragana, :katakana, :kanji, :other]
end

def japanese_classes
  [:hiragana, :katakana, :kanji]
end

def split_point?(before, after)
  if before == nil
    return false
  end

  if after == :yaku_start
    return true
  end

  if before == :yaku_start
    return false
  end

  if after == :yaku_end
    return false
  end

  if japanese_classes.map{|j| [:kanji, j]}.include?([before, after])
    return false
  end

  if [[:alphabetic, :hiragana]].include?([before, after])
    return false
  end

  before != after
end

def tokenize(str)
  result = []
  current = ""
  last_char_class = nil

  str.each_char do |c|
    char_class = char_class_of(c)
    if split_point?(last_char_class, char_class)
      result << current
      last_char_class = nil
      current = c
    else
      current += c
    end
    last_char_class = char_class
  end
  result << current if current != ""
  result
end

def extract_metadata(file)

  input = IO.read(file)
  yaml= input.split("---")[1]

  data = YAML.load(yaml)

  titles = p(tokenize(data["title"]))
  date = data["date"].to_date
  tags = data["categories"]

  {titles: titles, date: date, tags: tags}
end


def generate_svg(inkscape, name, template, metadata)
  svg_name = name + ".svg"
  png_name = name + ".png"

  IO.write(svg_name, template.result_with_hash(metadata))
  inkscape.file_open(svg_name)

  inkscape.select_by_selector(".title-fragment")
  title_widths = inkscape.query_width
  inkscape.select_list.each do |item|
    inkscape.unselect_by_id(item[:id])
  end
  title_width = title_widths.sum

  inkscape.select_by_id("title")
  title_height = inkscape.query_height[0]
  inkscape.unselect_by_id("title")


  line_height = title_height
  if 1000 < title_width
    acc_width = 0
    title_widths.each_with_index do |width, i|
      acc_width += width
      if 1100 < acc_width
        metadata[:titles][i - 1] += "\n"
        acc_width = width
      end
    end

    inkscape.file_close()
    IO.write(svg_name, template.result_with_hash(metadata))
    inkscape.file_open(svg_name)

    inkscape.select_by_id("title")
    title_width = inkscape.query_width[0]
    title_height = inkscape.query_height[0]
    inkscape.unselect_by_id("title")
  end

  puts metadata[:titles].join

  if 1200 < title_width
    puts "Title too long!!!"
  end
  title_x = (1200 ) / 2
  title_y = (630 - 190 - title_height) / 2 + 190 + line_height


  inkscape.select_by_selector(".tag")
  widths = inkscape.query_width
  inkscape.select_list.each do |item|
    inkscape.unselect_by_id(item[:id])
  end
  # discard the last element because it will not be used
  widths.pop
  dx = 0
  i = 0

  inkscape.select_by_id("title")
  inkscape.transform_translate(title_x, title_y)
  inkscape.unselect_by_id("title")
  for width in widths do
    dx += width + 6
    i +=1
    inkscape.select_by_id("tag#{i}")
    inkscape.transform_translate(dx, 0)
    inkscape.unselect_by_id("tag#{i}")
  end

  inkscape.export_background("#ffffff")
  inkscape.export_area_page(true)
  inkscape.export_filename (png_name)
  inkscape.export_do

  inkscape.file_close()
  FileUtils.rm svg_name
end



script_dir = File.dirname(__FILE__)
outdir = File.join(script_dir, "../static/images/ogp/post")
template = File.join(script_dir, 'ogp_template.svg.erb')
template = ERB.new(IO.read(template))
inkscape = Inkscape.new

i = 0
all = ARGV.length
for input in ARGV
  i += 1
  puts "#{i}/#{all}: #{input}"
  metadata = extract_metadata(input)
  filename = File.join(outdir, File.basename(File.basename(input, ".md"), ".markdown"))
  generate_svg(inkscape, filename, template, metadata)
end
