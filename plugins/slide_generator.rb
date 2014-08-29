# encoding: utf-8
#
# Jekyll category page generator.
# http://recursive-design.com/projects/jekyll-plugins/
#
# Version: 0.1.4 (201101061053)
#
# Copyright (c) 2010 Dave Perrett, http://recursive-design.com/
# Licensed under the MIT license (http://www.opensource.org/licenses/mit-license.php)
#
# A generator that creates category pages for jekyll sites.
#
# Included filters :
# - category_links:      Outputs the list of categories as comma-separated <a> links.
# - date_to_html_string: Outputs the post.date as formatted html, with hooks for CSS styling.
#
# Available _config.yml settings :
# - category_dir:          The subfolder to build category pages in (default is 'categories').
# - category_title_prefix: The string used before the category name in the page title (default is
#                          'Category: ').

require 'stringex'
require 'jekyll'

module Jekyll

  class Slide < Post
    attr_accessor :description, :format, :url_base, :nslides


    MATCHER = /(.*)(\.[^.]+)$/

    # Attributes for Liquid templates
    ATTRIBUTES_FOR_LIQUID = EXCERPT_ATTRIBUTES_FOR_LIQUID + %w[content excerpt nslides]
    def containing_dir(source, dir)
      return File.join(source, dir, '_slides')
    end

    def self.valid? file
      true
    end

    def next
      pos = site.slides.index {|slide| slide.equal?(self) }
      if pos && pos < site.slides.length - 1
        site.slides[pos + 1]
      else
        nil
      end
    end

    def process(name)
      slug, ext = *name.match(MATCHER)
      self.slug = slug
      self.ext = ext
    end

    def previous
      pos = site.slides.index {|slide| slide.equal?(self) }
      if pos && pos > 0
        site.slides[pos - 1]
      else
        nil
      end
    end


    def url
      @url ||= File.expand_path(File.join(url_base, @dir, File.basename(name, ".*")) + ".html", "/")
    end

    def destination(dest)
      # The url needs to be unescaped in order to preserve the correct filename
      path = File.join(dest,  URL.unescape_path(url))
      path = File.join(path, "index.html") if path[/\.html$/].nil?
      path
    end



    def transform
      self.nslides = content.scan(/\n\n|\n\n/).length + 1
      self.content = content
    end

  end

  def Page.valid? filename
    true
  end

  class Site

    attr_accessor :slides

    def each_site_file
      %w(slides posts pages static_files docs_to_write).each do |type|
        send(type).each do |item|
          yield item
        end
      end
    end

    def post_attr_hash(post_attr)
      # Build a hash map based on the specified post attribute ( post attr =>
      # array of posts ) then sort each array in reverse order.
      hash = Hash.new { |h, key| h[key] = [] }
      posts.each { |p| p.send(post_attr.to_sym).each { |t| hash[t] << p } }
      slides.each { |p| p.send(post_attr.to_sym).each { |t| hash[t] << p } }
      hash.values.each { |posts| posts.sort!.reverse! }
      hash
    end

    def read_slides(dir)
      slides = self.read_content("", "_slides", Slide)
      slides
    end

    def write_slide(slide, dir)
      slide.url_base = dir
      slide.render(self.layouts, site_payload)
      slide.write(self.dest)
      self.slides << slide
    end

    def write_slides
      if self.layouts.key? 'slide'
        @slides = []
        dir = self.config['slide_dir'] || 'slide'
        read_slides(dir).each do |slide|
          self.write_slide(slide, dir)
        end
        @slides.sort!
        @slides.reverse!
        self.read_content(dir, "", Page).each do |index|
          payload = site_payload
          payload["site"].merge!({ "slides"=> slides })
          index.render(self.layouts, payload)
          index.write(self.dest)
          self.pages << index
        end

      # Throw an exception if the layout couldn't be found.
      else
        raise <<-ERR


===============================================
 Error for slide_generator.rb plugin
-----------------------------------------------
 No 'slide.html' in source/_layouts/
 Perhaps you haven't installed a theme yet.
===============================================

ERR
      end
    end

  end


  # Jekyll hook - the generate method is called by jekyll, and generates all of the category pages.
  class GenerateSlides < Generator
    safe true
    priority :high

    def generate(site)
      site.write_slides
    end

  end

end

