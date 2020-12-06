class Inkscape
  private def discard_prompt
    @inkscape.read(2)
  end

  def initialize
    @inkscape = IO.popen("inkscape --shell", mode = "r+")
    # discard hello message
    4.times { @inkscape.gets }
    discard_prompt
  end

  attr_accessor :inkscape

  def run_action(action)
    @inkscape.puts(action)
    @inkscape.flush
  end

  def read_until_prompt
    output = ""
    state = :newline
    while c = @inkscape.getc do
      output << c
      case state
      when :char
        if c == "\n"
          state = :newline
          next
        end
        state = :char
      when :newline
        if c == ">"
          state = :gt
          next
        end
        state = :char
      when :gt
        if c == " "
          break
        end
        state = :char
      end
    end
    # discard the last "> "
    output[0..-3]
  end

  def execute(action)
    run_action(action)
    discard_prompt
  end

  def query_line(action)
    run_action(action)
    ret = @inkscape.gets.chomp
    discard_prompt
    ret
  end

  def query(action)
    run_action(action)
    read_until_prompt
  end

  def close
    @inkscape.close
  end

  ## actions
  # See https://gitlab.com/inkscape/inkscape/-/tree/master/src/actions for detailed behaviors

  # action-list         :  Print a list of actions and exit.
  def action_list
    output = query("action-list")
    output.lines(chomp: true).map do |line|
      line.split(":", 2).map(&:strip)
    end
  end

  # convert-dpi-method  :  Import DPI convert method.
  def convert_dpi_method(method = nil)
    execute("convert-dpi-method" + (method? ":#{method}":""))
  end

  # export-area         :  Export area.
  def export_area(x0, y0, x1, y1)
    execute("export-area:#{x0},#{y0},#{x1},#{y1}")
  end

  # export-area-drawing :  Export drawing area.
  def export_area_drawing(bool)
    execute("export-area-drawing:#{bool}")
  end

  # export-area-page    :  Export page area.
  def export_area_page(bool)
    execute("export-area-page:#{bool}")
  end

  # export-area-snap    :  Export snap area to integer values.
  def export_area_snap(bool)
    execute("export-area-snap:#{bool}")
  end

  # export-background   :  Export background color.
  def export_background(color)
    execute("export-background:#{color}")
  end

  # export-background-opacity:  Export background opacity.
  def export_background_opacity(opacity)
    execute("export-background-opacity:#{opacity}")
  end

  # export-do           :  Do export.
  def export_do
    execute("export-do")
  end

  # export-dpi          :  Export DPI.
  def export_dpi(dpi)
    execute("export-dpi:#{dpi}")
  end

  # export-filename     :  Export file name.
  def export_filename(filename)
    execute("export-filename:#{filename}")
  end

  # export-height       :  Export height.
  def export_height(height)
    execute("export-height:#{height}")
  end

  # export-id           :  Export id(s).
  def export_id(ids)
    execute("export-id:#{ids.join(";")}")
  end

  # export-id-only      :  Export id(s) only.
  def export_id_only(bool)
    execute("export-id-only:#{bool}")
  end

  # export-ignore-filters:  Export ignore filters.
  def export_ignore_filters(bool)
    execute("export-ignore-filters:#{bool}")
  end

  # export-latex        :  Export LaTeX.
  def export_ignore_latex(bool)
    execute("export-ignore-latex:#{bool}")
  end

  # export-margin       :  Export margin.
  def export_mergin(mergin)
    execute("export-mergin:#{mergin}")
  end

  # export-overwrite    :  Export over-write file.
  def export_overwrite(bool)
    execute("export-overwrite:#{bool}")
  end

  # export-pdf-version  :  Export PDF version.
  def export_pdf_version(level)
    execute("export-pdf-version:#{level}")
  end

  # export-plain-svg    :  Export as plain SVG.
  def export_plain_svg(bool)
    execute("export-plain-svg:#{bool}")
  end

  # export-ps-level     :  Export PostScript level.
  def export_ps_level(level)
    execute("export-ps-level:#{level}")
  end

  # export-text-to-path :  Export convert text to paths.
  def export_text_to_path(bool)
    execute("export_text_to_path:#{bool}")
  end

  # export-type         :  Export file type.
  def export_type(type)
    execute("export-type:#{type}")
  end

  # export-use-hints    :  Export using saved hints.
  def export_use_hints(bool)
    execute("export_use_hints:#{bool}")
  end

  # export-width        :  Export width.
  def export_width(width)
    execute("export-width:#{width}")
  end

  # file-close          :  Close active document.
  def file_close
    execute("file-close")
  end

  # file-new            :  Open new document using template.
  def file_new(filename)
    execute("file-new:#{filename}")
  end

  # file-open           :  Open file.
  def file_open(filename)
    execute("file-open:#{filename}")
  end

  # inkscape-version    :  Print Inkscape version and exit.
  def inkscape_version
    query("inkscape-version")
  end

  # no-convert-baseline :  Import convert text baselines.
  def no_convert_baseline
    execute("no-convert-baseline")
  end

  # object-set-attribute:  Set or update an attribute on selected objects. Usage: object-set-attribute:attribute name, attribute value;
  def object_set_attribute(name, value)
    # TODO handle selection empty error
    execute("object-set-attribute:#{name},#{value}")
  end

  # object-set-property :  Set or update a property on selected objects. Usage: object-set-property:property name, property value;
  def object_set_property(name, value)
    # TODO handle selection empty error
    execute("object-set-property:#{name},#{value}")
  end

  # object-to-path      :  Convert shapes to paths.
  def object_to_path
    execute("object-to-path")
  end

  # object-unlink-clones:  Unlink clones and symbols.
  def object_unlink_clones
    execute("object-unlink-clones")
  end

  # open-page           :  Import page number.
  def open_page(page=nil)
    # TODO: page might be an optional argumetn
    execute("open-page" + (page? ":#{page}": ""))
  end

  # query-all           :  Query 'x', 'y', 'width', and 'height'.
  def query_all
    # TODO: handle no_document
    query("query-all").lines(chomp: true).map do |line|
      data = line.split(",")
      [data[0], data[1..].map(&:to_f)]
    end
  end

  # query-height        :  Query 'height' value(s) of object(s).
  def query_height
    query_line("query-height").split(",").map(&:to_f)
  end

  # query-width         :  Query 'width' value(s) of object(s).
  def query_width
    query_line("query-width").split(",").map(&:to_f)
  end

  # query-x             :  Query 'x' value(s) of selected objects.
  def query_x
    query_line("query-x").split(",").map(&:to_f)
  end

  # query-y             :  Query 'y' value(s) of selected objects.
  def query_y
    query_line("query-y").split(",").map(&:to_f)
  end

  # quit-inkscape       :  Immediately quit Inkscape.
  def quit_inkscape
    execute("quit-inkscape")
  end

  # select              :  Select by ID (Deprecated)
  def select(*ids)
    # TODO: handle id not found error
    execute("select:#{id.join(",")}")
  end

  # select-all          :  Select all. Options: 'all' (every object including groups), 'layers', 'no-layers' (top level objects in layers), 'groups' (all groups including layers), 'no-groups' (all objects other than groups and layers, default).
  def select_all(condition)
    # TODO: handle non allowed options
    execute("select-all:#{condition}")
  end

  # select-by-class     :  Select by class
  def select_by_class(klass)
    execute("select-by-class:#{klass}")
  end

  # select-by-element   :  Select by SVG element (e.g. 'rect').
  def select_by_element(element)
    execute("select-by-element:#{element}")
  end

  # select-by-id        :  Select by ID
  def select_by_id(*ids)
    # TODO: handle id not found error
    execute("select-by-id:#{ids.join(",")}")
  end

  # select-by-selector  :  Select by CSS selector
  def select_by_selector(selector)
    execute("select-by-selector:#{selector}")
  end

  # select-clear        :  Selection clear
  def select_clear
    execute("select-clear")
  end

  # select-invert       :  Invert selection. Options: 'all', 'layers', 'no-layers', 'groups', 'no-groups' (default).
  def select_invert(condition)
    # TODO: handle non allowed options
    execute("select-invert:#{condition}")
  end

  # select-list         :  Print a list of objects in current selection.
  def select_list
    # TODO: parse output
    query("select-list").lines(chomp: true).map do |line|
      match_data =line.match(/(\w+|No ID) cloned: (true|false) ref: (\d+) href: (\d+) total href: (\d+)/)
      ret = {}
      ret[:id] = match_data[1] if match_data[1] != "No ID"
      ret[:cloned] = match_data[2] == "true"
      ret[:ref] = match_data[3].to_i
      ret[:href] = match_data[4].to_i
      ret[:total_href] = match_data[5].to_i
      ret
    end
  end

  # system-data-directory:  Print system data directory and exit.
  def system_data_directory
    query_line("system-data-dilectory")
  end

  # transform-remove    :  Remove any transforms from selected objects.
  def transform_remove
    execute("transform-remove")
  end

  # transform-rotate    :  Rotate selected objects by degrees.
  def transform_rotate(angle)
    # TODO: check if angle is a double
    execute("transform-rotate:#{angle}")
  end

  # transform-scale     :  Scale selected objects by scale factor.
  def transform_scale(scale)
    # TODO: check if scale is a double
    execute("transform-scale:#{scale}")
  end

  # transform-translate :  Translate selected objects (dx,dy).
  def transform_translate(x, y)
    execute("transform-translate:#{x},#{y}")
  end

  # unselect            :  Unselect by ID (Deprecated)
  def unselect(*ids)
    # TODO: handle id not found error
    execute("unselect:#{ids.join(",")}")
  end

  # unselect-by-id      :  Unselect by ID
  def unselect_by_id(*ids)
    # TODO: handle id not found error
    execute("unselect-by-id:#{ids.join(",")}")
  end

  # user-data-directory :  Print user data directory and exit.
  def user_data_directory
    query_line("user-data-directory")
  end

  # vacuum-defs         :  Remove unused definitions (gradients, etc.).
  def vacuum_defs
    execute("vacuum-defs")
  end

  # verb                :  Execute verb(s).
  # verb-list           :  Print a list of verbs and exit.

  # window-close        :  Close the active window.
  def window_close
    execute("window-close")
  end
  # window-open         :  Open a window for the active document. GUI only.
  def window_open
    execute("window-open")
  end
end
