#!/usr/bin/env ruby
# coding: utf-8

require 'erb'
require 'yaml'
require 'date'
require 'fileutils'

def extract_metadata(file)

  input = IO.read(file)
  yaml= input.split("---")[1]

  data = YAML.load(yaml)

  title = data["title"]
  date = data["date"].to_date
  tags = data["categories"]

  {title: title, date: date, tags: tags}
end


def generate_svg(name, template, metadata)
  svg_name = name + ".svg"
  png_name = name + ".png"

  IO.write(svg_name, template.result_with_hash(metadata))

  title_size =`inkscape #{svg_name} --actions='select-by-id:title;query-width;query-height' 2> /dev/null`
  title_width, title_height = title_size.split("\n").map(&:to_f)
  line_height = title_height
  if 1000 < title_width
    length = metadata[:title].length
    n_parts = ((title_width + 1200 - 1) / 1000).to_i
    (n_parts - 1).times do |i|
      metadata[:title] = metadata[:title].insert(length * (i + 1) / n_parts, "\n")
    end
    IO.write(svg_name, template.result_with_hash(metadata))
    title_size =`inkscape #{svg_name} --actions='select-by-id:title;query-width;query-height' 2> /dev/null`
    title_width, title_height = title_size.split("\n").map(&:to_f)

  end

  if 1200 < title_width
    puts "Title too long!!!"
  end
  title_x = (1200 ) / 2
  title_y = (630 - 190 - title_height) / 2 + 190 + line_height

  widths = `inkscape #{svg_name} --actions='select-by-selector:.tag;query-width' 2> /dev/null`
  widths = widths.split(",").map(&:to_f)
  # discard the last element because it will not be used
  widths.pop
  dx = 0
  i = 0
  actions = "select-by-id:title;transform-translate:#{title_x},#{title_y};unselect-by-id:title;"
  for width in widths do
    dx += width + 6
    i +=1
    actions += "select-by-id:tag#{i};transform-translate:#{dx},0;unselect-by-id:tag#{i};"
  end

  actions += "export-background:#ffffffff;export-area-page;export-filename:'#{png_name}';export-do;"

  system "inkscape #{svg_name} --actions='#{actions}' 2>/dev/null"
  FileUtils.rm svg_name
end



script_dir = File.dirname(__FILE__)
outdir = File.join(script_dir, "../static/images/ogp/post")
template = File.join(script_dir, 'ogp_template.svg.erb')
template = ERB.new(IO.read(template))

for input in ARGV
  puts input
#  Thread.new do
    metadata = extract_metadata(input)
    filename = File.join(outdir, File.basename(File.basename(input, ".md"), ".markdown"))
    generate_svg(filename, template, metadata)
#  end
end
