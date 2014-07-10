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
    attr_accessor :url_base, :categories, :tags, :date, :description, :format, :date_formatted

    def initialize(site, base, dir, name)
      @dir  = "/_slides/"
      @site = site
      @base = base
      @name = name
      @date_formatted = "aaa"
      process(name)
      read_yaml(File.join(base, @dir), name)

      data.default_proc = proc do |hash, key|
        site.frontmatter_defaults.find(File.join(dir, name), type, key)
      end

      if data.has_key?('date')
        self.date = Time.parse(data["date"].to_s)
      end

      populate_categories
      populate_tags
            format = self.site.config['date_format']

      unless self.data['date'].nil?

        date_f = DateFormat.format_date(self.data['date'], format) unless self.data['date'].nil?
        date_t = DateFormat.time_tag(self.data['date'], date_f, 'published')

        self.data['date_formatted'] = date_f
        self.data['time_tag'] = date_t
      end

      unless self.data['updated'].nil?
        updated = Time.parse(self.data['updated'].to_s)
        updated_f = DateFormat.format_date(self.data['updated'], format)
        updated_t = DateFormat.time_tag(self.date, date_f, 'updated')

        self.data['updated'] = updated
        self.data['updated_formatted'] = updated_f
        self.data['updated_time_tag'] = updated_t
      end
    end

    def self.valid? file
      true
    end

    def title
      data.fetch("title", titleized_slug)
    end

    def destination(dir)
      File.join(@site.dest, url)
    end

    def populate_categories
      categories_from_data = Utils.pluralized_array_from_hash(data, 'category', 'categories')
      self.categories = (
        Array(categories) + categories_from_data
      ).map {|c| c.to_s.downcase}.flatten.uniq
    end

    def populate_tags
      self.tags = Utils.pluralized_array_from_hash(data, "tag", "tags").flatten
    end

    def url_placeholders
      {
        :year => date.strftime("%Y"),
        :month => date.strftime("%m"),
        :day => date.strftime("%d"),
        # :title => title,
        :i_day => date.strftime("%d").to_i.to_s,
        :i_month => date.strftime("%m").to_i.to_s,
        :short_month => date.strftime("%b"),
        :short_year => date.strftime("%y"),
        :y_day => date.strftime("%j"),
        :path => @url_base,
        :basename => basename,
        :categories => (categories || []).map { |c| c.to_s }.join('/'),
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

    def categories
      hash = post_attr_hash('categories')
      self.pages.each do |page|
        if page.respond_to? :categories
          page.categories.each do |cat|
            hash[cat] = (hash[cat] || []).concat [page]
          end
        end
      end
      hash
    end

    def write_slide(slide, dir)
      slide.url_base = dir
      slide.render(self.layouts, site_payload)
      slide.write(dir)
      self.slides << slide
      # Record the fact that this page has been added, otherwise Site::cleanup will remove it.
      self.pages << slide
      self.categories
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

