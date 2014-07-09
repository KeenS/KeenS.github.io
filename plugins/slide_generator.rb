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


  class Slide < Page
    attr_accessor :url_base 

    def initialize(site, base, dir, name)
      @site = site
      @base = base
      @dir  = "/_slides/"
      @name = name

      process(name)
      read_yaml(File.join(base, @dir), name)

      data.default_proc = proc do |hash, key|
        site.frontmatter_defaults.find(File.join(dir, name), type, key)
      end
    end

    def self.valid? file
      true
    end

    def destination(dir)
      File.join(@site.dest, url)
    end

    def url_placeholders
      {
        :path => @url_base,
        :basename => basename,
        :output_ext => output_ext
      }
    end

    def transform
      self.content = content
    end
  end

  class Page

    def self.valid? file
      true
    end
  end

  class Site

    attr_accessor :slides

    def read_slides(dir)
      slides = self.read_content("", "_slides", Slide)
      slides
    end

    def write_slide(slide, dir)
      slide.url_base = dir
      slide.render(self.layouts, site_payload)
      slide.write(dir)
      self.slides << slide
      # Record the fact that this page has been added, otherwise Site::cleanup will remove it.
      self.pages << slide
    end

    def write_slides
      if self.layouts.key? 'slide'
        @slides = []
        dir = self.config['slide_dir'] || 'slide'
        read_slides(dir).each do |slide|
          self.write_slide(slide, dir)
        end
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
 No 'slide.hmtl' in source/_layouts/
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

