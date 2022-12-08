class Arg
  attr_reader :name, :func, :idx

  def initialize(name)
    @name = name
  end

  def set_func(func, idx)
    @func = func
    @idx = idx
  end

  def write(str)
    puts "\t" + str
  end

  def parse_error()
    "\trv = enif_make_badarg2(env, \"#{@name}\", argv[#{@idx}]);\n" +
    "\t\tgoto out;"
  end

  def parse_error_rc()
    "\trv = make_errno(env, rc);\n" +
    "\t\tgoto out;"
  end

  def declare
    write "// no declarations for #{@name}"
  end

  def parse
    write "// no parse for #{@name}"
  end

  def precall
  end

  def arg_type
    nil
  end

  def call
    write "    #{arg_type}, #{@name},"
  end
end

class Void < Arg
  def arg_type; 'ARG_NONE'; end
  def call
    raise Exception.new('void args not ok')
  end
  def erl_type; ''; end
end

class StylePropVal < Arg
  def arg_type; 'ARG_STYLEVAL'; end
  def declare
    write "lv_style_value_t #{@name}_val;"
    write "lv_style_prop_t #{@name}_prop;"
  end
  def parse
    write "rc = parse_style_prop_val(env, argv[#{@idx}], argv[#{@idx + 1}],"
    write "    &#{@name}_prop, &#{@name}_val);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
  def call
    write "    ARG_UINT32, #{@name}_prop,"
    write "    ARG_STYLEVAL, &#{@name}_val,"
  end
  def erl_type
    "style_generic_prop()"
  end
end

class Dummy < Arg
  def declare
  end
  def parse
  end
  def call
  end
  def erl_type
    "style_generic_val()"
  end
end

class Handle < Arg
  def stem; nil; end
  def struct; stem.gsub('_', ''); end
  def arg_type; "ARG_PTR_#{stem.upcase}"; end
  def declare
    write "struct lvk#{struct} *#{@name};"
  end
  def parse
    write "rc = enter_#{struct}_hdl(env, argv[#{@idx}], &nls, &#{@name}, 0);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
end

class Obj < Handle
  def stem; 'obj'; end
  def erl_type; 'object()'; end
end

class Style < Handle
  def stem; 'style'; end
  def erl_type; 'style()'; end
end

class Group < Handle
  def stem; 'group'; end
  def erl_type; 'group()'; end
end

class ChartSeries < Handle
  def stem; 'chart_ser'; end
  def erl_type; 'lv_chart:series()'; end
end

class ChartCursor < Handle
  def stem; 'chart_cur'; end
  def erl_type; 'lv_chart:cursor()'; end
end

class MeterInd < Handle
  def stem; 'meter_ind'; end
  def erl_type; 'lv_meter:indicator()'; end
end

class MeterScale < Handle
  def stem; 'meter_scl'; end
  def erl_type; 'lv_meter:scale()'; end
end

class Buffer < Arg
  def arg_type; 'ARG_PTR_BUFFER'; end
  def declare
    write "struct lvkbuf *#{@name};"
  end
  def parse
    write "rc = unpack_buf_hdl(env, argv[#{@idx}], &nls, &#{@name});"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
  def erl_type; 'buffer()'; end
end

class Inst < Arg
  def arg_type; 'ARG_NONE'; end
  def declare
    write "struct lvkinst *#{@name};"
  end
  def parse
    write "rc = enter_inst_hdl(env, argv[#{@idx}], &nls, &#{@name}, 0);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
  def call
  end
  def erl_type; 'instance()'; end
end

class InstMember < Inst
  def arg_type; 'ARG_PTR'; end
  def initialize(name, member)
    super(name)
    @member = member
  end
  def call
    write "    ARG_PTR, #{@name}->#{@member},"
  end
end

class InlineStr < Arg
  def arg_type; 'ARG_INLINE_STR'; end
  def declare
    write "ErlNifBinary #{@name};"
  end
  def parse
    write "if (!enif_inspect_iolist_as_binary(env, argv[#{@idx}], &#{@name})) {"
    write parse_error
    write "}"
  end
  def call
    write "    ARG_INLINE_BUF, &#{@name},"
  end
  def erl_type; 'iolist()'; end
end

class InlineStrArray < Arg
  def arg_type; 'ARG_INL_BUF_ARR'; end
  def declare
    write "ErlNifBinary #{@name}[16];"
    write "size_t #{@name}_n = 0;"
    write "ERL_NIF_TERM #{@name}_list, #{@name}_hd;"
  end
  def parse
    write "#{@name}_list = argv[#{@idx}];"
    write "while (enif_get_list_cell(env, #{@name}_list, &#{@name}_hd, &#{@name}_list)) {"
    write "\tassert(#{@name}_n < 16);"
    write "\tif (!enif_inspect_iolist_as_binary(env, #{@name}_hd, &#{@name}[#{@name}_n])) {"
    write "\t" + parse_error
    write "\t}"
    write "\t++#{@name}_n;"
    write "}"
  end
  def call
    write "    ARG_INL_BUF_ARR, #{@name}, #{@name}_n,"
  end
  def erl_type; '[iolist()]'; end
end

class ImgSrc < Arg
  def arg_type; 'ARG_INLINE_STR'; end
  def declare
    write "ErlNifBinary #{@name}_bin;"
    write "enum arg_type #{@name}_type;"
    write "void *#{@name};"
  end
  def parse
    write "rc = parse_img_src(env, argv[#{@idx}], &#{@name}_bin, &#{@name}_type,"
    write "    &#{@name});"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
  def call
    write "    #{@name}_type, #{@name},"
  end
  def erl_type; 'lv_img:src()'; end
end

class Bool < Arg
  def declare
    @func.if_not_flag('atom_decl') do
      write "char atom[32];"
    end
    write "uint #{@name};"
  end
  def parse
    write "if (!enif_get_atom(env, argv[#{@idx}], atom, sizeof (atom), ERL_NIF_LATIN1)) {"
    write parse_error
    write "}"
    write "if (strcmp(atom, \"true\") == 0) {"
    write "\t#{@name} = 1;"
    write "} else if (strcmp(atom, \"false\") == 0) {"
    write "\t#{@name} = 0;"
    write "} else {"
    write parse_error
    write "}"
  end
  def erl_type; 'boolean()'; end
end
class Bool8 < Bool
  def arg_type; 'ARG_UINT8'; end
end
class Bool16 < Bool
  def arg_type; 'ARG_UINT16'; end
end
class Bool32 < Bool
  def arg_type; 'ARG_UINT32'; end
end

class Point < Arg
  def arg_type; 'ARG_POINT'; end
  def declare
    @func.if_not_flag('tuple_decl') do
      write "int tuplen;"
      write "const ERL_NIF_TERM *tup;"
    end
    write "lv_point_t #{@name};"
  end
  def parse
    write "if (!enif_get_tuple(env, argv[#{@idx}], &tuplen, &tup)) {"
    write parse_error
    write "}"
    write "if (tuplen != 2) {"
    write parse_error
    write "}"
    write "if ((rc = parse_coord(env, tup[0], &#{@name}.x))) {"
    write parse_error_rc
    write "}"
    write "if ((rc = parse_coord(env, tup[1], &#{@name}.y))) {"
    write parse_error_rc
    write "}"
  end
  def call
    write "    ARG_POINT, &#{@name},"
  end
  def erl_type; 'lv:point()'; end
end

class ChartValue < Arg
  def arg_type; 'ARG_UINT16'; end
  def declare
    @func.if_not_flag('atom_decl') do
      write "char atom[32];"
    end
    write "lv_coord_t #{@name};"
  end
  def parse
    write "if (enif_get_atom(env, argv[#{@idx}], atom, sizeof (atom), ERL_NIF_LATIN1) &&"
    write "    strcmp(atom, \"none\") == 0) {"
    write "\t#{@name} = LV_CHART_POINT_NONE;"
    write "} else if ((rc = parse_coord(env, argv[#{@idx}], &#{@name}))) {"
    write parse_error_rc
    write "}"
  end
  def erl_type; 'none | integer()'; end
end

class Coord < Arg
  def arg_type; 'ARG_UINT16'; end
  def declare
    write "lv_coord_t #{@name};"
  end
  def parse
    write "if ((rc = parse_coord(env, argv[#{@idx}], &#{@name}))) {"
    write parse_error_rc
    write "}"
  end
  def erl_type; 'lv:coord()'; end
end

class SplitPoint < Point
  def call
    write "    ARG_UINT32, #{@name}.x,"
    write "    ARG_UINT32, #{@name}.y,"
  end
end

class Color < Arg
  def declare
    write "lv_color_t #{@name};"
  end
  def parse
    write "if (!enif_get_color(env, argv[#{@idx}], &#{@name})) {"
    write parse_error
    write "}"
  end
  def call
    write "    ARG_COLOR, &#{@name},"
  end
  def erl_type; 'lv:color()'; end
end

class IntBase < Arg
  def declare
    write "int #{@name};"
  end
  def parse
    write "if (!enif_get_int(env, argv[#{@idx}], &#{@name})) {"
    write parse_error
    write "}"
  end
  def erl_type; 'integer()'; end
end

class UIntBase < Arg
  def declare
    write "uint #{@name};"
  end
  def parse
    write "if (!enif_get_uint(env, argv[#{@idx}], &#{@name})) {"
    write parse_error
    write "}"
  end
  def erl_type; 'integer()'; end
end

class UInt32 < UIntBase
  def arg_type; 'ARG_UINT32'; end
end

class Int32 < IntBase
  def arg_type; 'ARG_UINT32'; end
end

class UInt16 < UIntBase
  def arg_type; 'ARG_UINT16'; end
end

class Int16 < IntBase
  def arg_type; 'ARG_UINT16'; end
end

class UInt8 < UIntBase
  def arg_type; 'ARG_UINT8'; end
end

class AnimEnable < Int32
  def declare
    @func.if_not_flag('atom_decl') do
      write "char atom[32];"
    end
    write "int #{@name};"
  end
  def parse
    write "if (!enif_get_atom(env, argv[#{@idx}], atom, sizeof (atom), ERL_NIF_LATIN1)) {"
    write parse_error
    write "}"
    write "if (strcmp(atom, \"on\") == 0) {"
    write "\t#{@name} = LV_ANIM_ON;"
    write "} else if (strcmp(atom, \"off\") == 0) {"
    write "\t#{@name} = LV_ANIM_OFF;"
    write "} else {"
    write parse_error
    write "}"
  end
  def erl_type; 'lv_anim:enable()'; end
end

class EnumBase < IntBase
  def parse
    write "if ((rc = parse_enum(env, argv[#{@idx}], #{enum}, #{multi}, &#{@name}))) {"
    write parse_error_rc
    write "}"
  end
  def precall
    write "ncd->ncd_enum = #{enum};"
    write "ncd->ncd_multi = #{multi};"
  end
  def enum; nil; end
  def multi; false; end
  def erl_flag_type; 'atom()'; end
  def erl_type;
    if multi
      "lv:flags(#{erl_flag_type})"
    else
      erl_flag_type
    end
  end
end

class Enum16 < EnumBase
  def arg_type; 'ARG_UINT16'; end
end

class Enum8 < EnumBase
  def arg_type; 'ARG_UINT8'; end
end

class Enum32 < EnumBase
  def arg_type; 'ARG_UINT32'; end
end

class AlignSpec < Enum32
  def enum; 'align_specs'; end
  def multi; false; end
  def erl_flag_type; 'lv_obj:align_spec()'; end
end
class ObjFlags < Enum32
  def enum; 'obj_flags'; end
  def multi; true; end
  def erl_flag_type; 'lv_obj:flag()'; end
end
class ObjStates < Enum32
  def enum; 'obj_state_specs'; end
  def multi; true; end
  def erl_flag_type; 'lv_obj:state()'; end
end
class StyleSelector < Enum32
  def enum; 'style_selector_specs'; end
  def multi; true; end
  def erl_flag_type; 'lv_obj:selector()'; end
end
class FlexAlign < Enum32
  def enum; 'flex_align'; end
  def multi; false; end
  def erl_flag_type; 'lv_style:flex_align()'; end
end
class FlexFlow < Enum32
  def enum; 'flex_flow'; end
  def multi; false; end
  def erl_flag_type; 'lv_style:flex_flow()'; end
end
class ScrLoadAnim < Enum32
  def enum; 'scr_load_anims'; end
  def multi; false; end
  def erl_flag_type; 'lv_scr:load_anim()'; end
end
class BarMode < Enum8
  def enum; 'bar_mode'; end
  def multi; false; end
  def erl_flag_type; 'lv_bar:mode()'; end
end
class MenuModeRootBackBtn < Enum8
  def enum; 'menu_mode_root_back_btn'; end
  def multi; false; end
  def erl_flag_type; 'lv_menu:root_back_btn_mode()'; end
end
class MenuModeHeader < Enum8
  def enum; 'menu_mode_header'; end
  def multi; false; end
  def erl_flag_type; 'lv_menu:header_mode()'; end
end
class TableCellCtrl < Enum8
  def enum; 'table_cell_ctrls'; end
  def multi; false; end
  def erl_flag_type; 'lv_table:ctrl()'; end
end
class BtnMatrixControl < Enum16
  def enum; 'btnmatrix_ctrls'; end
  def multi; true; end
  def erl_flag_type; 'lv_btnmatrix:ctrl()'; end
end
class ChartType < Enum8
  def enum; 'chart_types'; end
  def multi; false; end
  def erl_flag_type; 'lv_chart:type()'; end
end
class ChartUpdateMode < Enum8
  def enum; 'chart_update_modes'; end
  def multi; false; end
  def erl_flag_type; 'lv_chart:update_mode()'; end
end
class ChartAxis < Enum8
  def enum; 'chart_axes'; end
  def multi; false; end
  def erl_flag_type; 'lv_chart:axis()'; end
end

class Func
  attr_reader :flags

  def initialize(name, call, rtype, *args)
    @name = name
    @call = call
    @rtype = rtype.new('rv')
    @rtype.set_func(self, -1)
    @args = args
    @flags = {}
    args.each_with_index { |a,i| a.set_func(self, i) }
    Func.register(self)
  end

  def self.register(inst)
    @funcs ||= []
    @funcs << inst
  end

  def self.print_all()
    @funcs.each { |f| f.print }
  end

  def self.print_all_nif_defs()
    defs = @funcs.map { |f| f.get_nif_def }
    puts defs.join(", \\\n")
  end

  def self.print_all_erl()
    @funcs.each { |f| f.print_erl }
  end

  def if_not_flag(flag, &blk)
    return if @flags[flag]
    blk.call()
    @flags[flag] = true
  end

  def declare
    puts "\tstruct nif_lock_state nls;"
    puts "\tstruct nif_call_data *ncd = NULL;"
    puts "\tERL_NIF_TERM msgref, rv;"
    puts "\tint rc;"
    @args.each { |a| a.declare }
  end

  def parse
    @args.each { |a| a.parse }
  end

  def precall
    @rtype.precall
  end

  def print
    puts "static ERL_NIF_TERM"
    puts "rlvgl_#{@name}#{@args.size}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])"
    puts "{"
    declare
    puts ""
    puts "\tbzero(&nls, sizeof (nls));"
    puts ""
    puts "\tif (argc != #{@args.size})"
    puts "\t\treturn (enif_make_badarg(env));"
    puts ""
    parse
    puts ""
    puts "\trc = make_ncd(env, &msgref, &ncd);"
    puts "\tif (rc != 0) {"
    puts "\t\trv = make_errno(env, rc);"
    puts "\t\tgoto out;"
    puts "\t}"
    puts ""
    precall
    puts ""
    puts "\trc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,"
    puts "\t    #{@rtype.arg_type}, #{@call},"
    @args.each { |a| a.call }
    puts "\t    ARG_NONE);"
    puts ""
    puts "\tif (rc != 0) {"
    puts "\t\trv = make_errno(env, rc);"
    puts "\t\tgoto out;"
    puts "\t}"
    puts ""
    puts "\tncd = NULL;"
    puts "\trv = enif_make_tuple2(env,"
    puts "\t    enif_make_atom(env, \"async\"),"
    puts "\t    msgref);"
    puts ""
    puts "out:"
    puts "\tleave_nif(&nls);"
    puts "\tfree_ncd(ncd);"
    puts "\treturn (rv);"
    puts "}"
    puts
  end

  def print_erl
    rtype = @rtype.erl_type
    args = @args.map { |a| a.erl_type }.join(', ')
    argnames = @args.map { |a| '_' + a.name[0].upcase + a.name[1..] }.join(', ')
    puts "-spec #{@name}(#{args}) -> async_return(#{rtype})."
    puts "#{@name}(#{argnames}) -> error(no_nif)."
  end

  def get_nif_def
    tabs = "\t" * (5 - ((@name.size + 5) / 8))
    "{ \"#{@name}\",#{tabs}#{@args.size}, rlvgl_#{@name}#{@args.size} }"
  end
end

class LvFunc < Func
  def initialize(name, rtype, *args)
    super(name, "lv_#{name}", rtype, *args)
  end
end

class WidgetCreateFunc < Func
  def initialize(widget, *args)
    args.unshift(Obj.new('parent'))
    super("#{widget}_create", "lv_#{widget}_create", Obj, *args)
  end
end

class ObjFunc < Func
  def initialize(name, rtype, *args)
    args.unshift(Obj.new('obj'))
    super("obj_#{name}", "lv_obj_#{name}", rtype, *args)
  end
end

class StyleFunc < Func
  def initialize(name, rtype, *args)
    args.unshift(Style.new('style'))
    super("style_#{name}", "lv_style_#{name}", rtype, *args)
  end
end

class WidgetFunc < Func
  def initialize(widget, name, rtype, *args)
    @widget = widget
    args.unshift(Obj.new('obj'))
    super("#{widget}_#{name}", "lv_#{widget}_#{name}", rtype, *args)
  end

  def parse
    super
    puts ""
    puts "\tif (!lv_obj_class_has_base(obj->lvko_class, &lv_#{@widget}_class)) {"
    puts "\t\trv = make_errno(env, EINVAL);"
    puts "\t\tgoto out;"
    puts "\t}"
  end
end

WidgetCreateFunc.new('bar')
WidgetFunc.new('bar', 'set_value', Void, Int32.new('value'), AnimEnable.new('anim'))
WidgetFunc.new('bar', 'set_start_value', Void, Int32.new('value'), AnimEnable.new('anim'))
WidgetFunc.new('bar', 'set_range', Void, Int32.new('min'), Int32.new('max'))
WidgetFunc.new('bar', 'set_mode', Void, BarMode.new('mode'))
WidgetFunc.new('bar', 'get_value', Int32)

WidgetCreateFunc.new('btn')

WidgetCreateFunc.new('checkbox')
WidgetFunc.new('checkbox', 'set_text', Void, InlineStr.new('text'))
WidgetFunc.new('checkbox', 'get_text', InlineStr)

WidgetCreateFunc.new('textarea')
WidgetFunc.new('textarea', 'set_text', Void, InlineStr.new('text'))
WidgetFunc.new('textarea', 'get_text', InlineStr)
WidgetFunc.new('textarea', 'set_placeholder_text', Void, InlineStr.new('text'))
WidgetFunc.new('textarea', 'set_text_selection', Void, Bool8.new('state'))
WidgetFunc.new('textarea', 'set_password_mode', Void, Bool8.new('state'))
WidgetFunc.new('textarea', 'set_one_line', Void, Bool8.new('state'))
WidgetFunc.new('textarea', 'set_accepted_chars', Void, Buffer.new('buf'))
WidgetFunc.new('textarea', 'get_label', Obj)
WidgetFunc.new('textarea', 'set_password_show_time', Void, UInt16.new('time'))

WidgetCreateFunc.new('img')
WidgetFunc.new('img', 'set_offset', Void, Point.new('pt'))
WidgetFunc.new('img', 'set_src', Void, ImgSrc.new('src'))
WidgetFunc.new('img', 'set_angle', Void, UInt16.new('angle'))
WidgetFunc.new('img', 'set_pivot', Void, SplitPoint.new('pivot'))
WidgetFunc.new('img', 'set_zoom', Void, UInt16.new('zoom'))
WidgetFunc.new('img', 'set_antialias', Void, Bool8.new('ena'))

WidgetCreateFunc.new('label')
WidgetFunc.new('label', 'get_text', InlineStr)
WidgetFunc.new('label', 'set_text', Void, InlineStr.new('text'))
WidgetFunc.new('label', 'set_text_sel_start', Void, UInt32.new('index'))
WidgetFunc.new('label', 'set_text_sel_end', Void, UInt32.new('index'))

WidgetCreateFunc.new('btnmatrix')
WidgetFunc.new('btnmatrix', 'set_map', Void, InlineStrArray.new('map'))
WidgetFunc.new('btnmatrix', 'set_btn_ctrl', Void, UInt16.new('idx'), BtnMatrixControl.new('ctrl'))
WidgetFunc.new('btnmatrix', 'clear_btn_ctrl', Void, UInt16.new('idx'), BtnMatrixControl.new('ctrl'))
WidgetFunc.new('btnmatrix', 'set_btn_ctrl_all', Void, BtnMatrixControl.new('ctrl'))
WidgetFunc.new('btnmatrix', 'clear_btn_ctrl_all', Void, BtnMatrixControl.new('ctrl'))
WidgetFunc.new('btnmatrix', 'set_btn_width', Void, UInt16.new('idx'), UInt8.new('width'))
WidgetFunc.new('btnmatrix', 'set_one_checked', Void, Bool8.new('checked'))
WidgetFunc.new('btnmatrix', 'get_selected_btn', UInt16)
WidgetFunc.new('btnmatrix', 'set_selected_btn', Void, UInt16.new('idx'))
WidgetFunc.new('btnmatrix', 'get_btn_text', InlineStr, UInt16.new('idx'))
WidgetFunc.new('btnmatrix', 'has_btn_ctrl', Bool8, UInt16.new('idx'), BtnMatrixControl.new('ctrl'))

WidgetCreateFunc.new('dropdown')
WidgetFunc.new('dropdown', 'set_options', Void, InlineStr.new('opts'))
WidgetFunc.new('dropdown', 'add_option', Void, InlineStr.new('text'), Int32.new('index'))
WidgetFunc.new('dropdown', 'get_selected', Int32)
WidgetFunc.new('dropdown', 'set_selected', Void, Int32.new('index'))
WidgetFunc.new('dropdown', 'clear_options', Void)
WidgetFunc.new('dropdown', 'get_selected_str', InlineStr)

WidgetCreateFunc.new('imgbtn')

WidgetCreateFunc.new('led')
WidgetFunc.new('led', 'set_color', Void, Color.new('color'))
WidgetFunc.new('led', 'set_brightness', Void, UInt8.new('bright'))
WidgetFunc.new('led', 'on', Void)
WidgetFunc.new('led', 'off', Void)
WidgetFunc.new('led', 'toggle', Void)
WidgetFunc.new('led', 'get_brightness', UInt8)

WidgetCreateFunc.new('list')
WidgetFunc.new('list', 'add_text', Obj, InlineStr.new('text'))
WidgetFunc.new('list', 'add_btn', Obj, ImgSrc.new('icon'), InlineStr.new('text'))
WidgetFunc.new('list', 'get_btn_text', InlineStr, Obj.new('btn'))

WidgetCreateFunc.new('menu')
WidgetFunc.new('menu', 'page_create', Obj, InlineStr.new('title'))
LvFunc.new('menu_cont_create', Obj, Obj.new('parent'))
LvFunc.new('menu_section_create', Obj, Obj.new('parent'))
LvFunc.new('menu_separator_create', Obj, Obj.new('parent'))
WidgetFunc.new('menu', 'set_page', Void, Obj.new('page'))
WidgetFunc.new('menu', 'set_sidebar_page', Void, Obj.new('page'))
WidgetFunc.new('menu', 'set_mode_root_back_btn', Void, MenuModeRootBackBtn.new('mode'))
WidgetFunc.new('menu', 'set_mode_header', Void, MenuModeHeader.new('mode'))
WidgetFunc.new('menu', 'set_load_page_event', Void, Obj.new('btn'), Obj.new('page'))

WidgetCreateFunc.new('roller')

WidgetCreateFunc.new('slider')

WidgetCreateFunc.new('switch')

WidgetCreateFunc.new('table')
WidgetFunc.new('table', 'set_row_cnt', Void, UInt16.new('rows'))
WidgetFunc.new('table', 'set_col_cnt', Void, UInt16.new('cols'))
WidgetFunc.new('table', 'set_col_width', Void, UInt16.new('col_idx'), Coord.new('width'))
WidgetFunc.new('table', 'set_cell_value', Void, UInt16.new('row'), UInt16.new('col'), InlineStr.new('text'))
WidgetFunc.new('table', 'add_cell_ctrl', Void, UInt16.new('row'), UInt16.new('col'), TableCellCtrl.new('ctrl'))
WidgetFunc.new('table', 'clear_cell_ctrl', Void, UInt16.new('row'), UInt16.new('col'), TableCellCtrl.new('ctrl'))
WidgetFunc.new('table', 'get_selected_cell_pt', Point)

WidgetCreateFunc.new('msgbox', InlineStr.new('title'), InlineStr.new('text'), InlineStrArray.new('btns'), Bool8.new('add_close'));
WidgetFunc.new('msgbox', 'get_active_btn', UInt16)
WidgetFunc.new('msgbox', 'get_active_btn_text', InlineStr)
WidgetFunc.new('msgbox', 'close', Void)
WidgetFunc.new('msgbox', 'get_btns', Obj)

WidgetCreateFunc.new('spinner', UInt32.new('time'), UInt32.new('arclen'))

WidgetCreateFunc.new('meter')
WidgetFunc.new('meter', 'add_scale', MeterScale)
WidgetFunc.new('meter', 'set_scale_ticks', Void, MeterScale.new('scale'), UInt16.new('cnt'), UInt16.new('width'), UInt16.new('len'), Color.new('color'))
WidgetFunc.new('meter', 'set_scale_major_ticks', Void, MeterScale.new('scale'), UInt16.new('nth'), UInt16.new('width'), UInt16.new('len'), Color.new('color'), Int16.new('label_gap'))
WidgetFunc.new('meter', 'set_scale_range', Void, MeterScale.new('scale'), Int32.new('min'), Int32.new('max'), UInt32.new('angle_range'), UInt32.new('rotation'))
WidgetFunc.new('meter', 'add_needle_line', MeterInd, MeterScale.new('scale'), UInt16.new('width'), Color.new('color'), Int16.new('r_mod'))
WidgetFunc.new('meter', 'add_arc', MeterInd, MeterScale.new('scale'), UInt16.new('width'), Color.new('color'), Int16.new('r_mod'))
WidgetFunc.new('meter', 'set_indicator_value', Void, MeterInd.new('ind'), Int32.new('value'))
WidgetFunc.new('meter', 'set_indicator_start_value', Void, MeterInd.new('ind'), Int32.new('value'))
WidgetFunc.new('meter', 'set_indicator_end_value', Void, MeterInd.new('ind'), Int32.new('value'))

WidgetCreateFunc.new('chart')
WidgetFunc.new('chart', 'set_type', Void, ChartType.new('type'))
WidgetFunc.new('chart', 'set_point_count', Void, UInt16.new('count'))
WidgetFunc.new('chart', 'set_range', Void, ChartAxis.new('axis'), ChartValue.new('min'), ChartValue.new('max'))
WidgetFunc.new('chart', 'set_update_mode', Void, ChartUpdateMode.new('mode'))
WidgetFunc.new('chart', 'set_div_line_count', Void, UInt8.new('hdiv'), UInt8.new('vdiv'))
WidgetFunc.new('chart', 'set_zoom_x', Void, UInt16.new('zoom'))
WidgetFunc.new('chart', 'set_zoom_y', Void, UInt16.new('zoom'))
WidgetFunc.new('chart', 'set_axis_tick', Void, ChartAxis.new('axis'), Coord.new('major_len'), Coord.new('minor_len'), Coord.new('major_cnt'), Coord.new('minor_cnt'), Bool8.new('label_en'), Coord.new('draw_size'))
WidgetFunc.new('chart', 'get_point_count', UInt16)
WidgetFunc.new('chart', 'add_series', ChartSeries, Color.new('color'), ChartAxis.new('axis'))
WidgetFunc.new('chart', 'remove_series', Void, ChartSeries.new('series'))
WidgetFunc.new('chart', 'hide_series', Void, ChartSeries.new('series'), Bool8.new('hide'))
WidgetFunc.new('chart', 'set_series_color', Void, ChartSeries.new('series'), Color.new('color'))
WidgetFunc.new('chart', 'get_series_next', ChartSeries, ChartSeries.new('series'))
WidgetFunc.new('chart', 'set_all_value', Void, ChartSeries.new('series'), ChartValue.new('y'))
WidgetFunc.new('chart', 'set_next_value', Void, ChartSeries.new('series'), ChartValue.new('y'))
WidgetFunc.new('chart', 'set_next_value2', Void, ChartSeries.new('series'), ChartValue.new('x'), ChartValue.new('y'))

Func.new('obj_create', 'lv_disp_obj_create', Obj, InstMember.new('inst', 'lvki_disp'), Obj.new('parent'))
ObjFunc.new('center', Void)
ObjFunc.new('add_flag', Void, ObjFlags.new('flags'))
ObjFunc.new('clear_flag', Void, ObjFlags.new('flags'))
ObjFunc.new('has_flag', Bool8, ObjFlags.new('flags'))
ObjFunc.new('has_flag_any', Bool8, ObjFlags.new('flags'))
ObjFunc.new('add_state', Void, ObjStates.new('states'))
ObjFunc.new('clear_state', Void, ObjStates.new('states'))
ObjFunc.new('get_state', ObjStates)
ObjFunc.new('add_style', Void, Style.new('style'), StyleSelector.new('sel'))
ObjFunc.new('align', Void, AlignSpec.new('align'), SplitPoint.new('offset'))
Func.new('obj_align', 'lv_obj_set_align', Void, Obj.new('obj'), AlignSpec.new('align'))
ObjFunc.new('align_to', Void, Obj.new('tobj'), AlignSpec.new('align'), SplitPoint.new('offset'))
ObjFunc.new('get_pos', Point)
ObjFunc.new('get_size', Point)
ObjFunc.new('set_size', Void, SplitPoint.new('size'))
ObjFunc.new('set_local_style_prop', Void, StylePropVal.new('sty'), Dummy.new('val'), StyleSelector.new('sel'))
ObjFunc.new('move_foreground', Void)
ObjFunc.new('move_background', Void)
ObjFunc.new('swap', Void, Obj.new('other'))
ObjFunc.new('set_parent', Void, Obj.new('parent'))
ObjFunc.new('move_to_index', Void, Int32.new('index'))
ObjFunc.new('get_index', Int32)
ObjFunc.new('get_child_cnt', UInt32)
ObjFunc.new('get_child', Obj, Int32.new('index'))
ObjFunc.new('get_parent', Obj)
ObjFunc.new('get_screen', Obj)
ObjFunc.new('clean', Void)
ObjFunc.new('del', Void)
ObjFunc.new('refresh_ext_draw_size', Void)

LvFunc.new('group_create', Group, Inst.new('inst'))
LvFunc.new('group_add_obj', Void, Group.new('group'), Obj.new('obj'))

Func.new('style_create', 'lv_style_alloc', Style, Inst.new('inst'))
StyleFunc.new('set_flex_align', Void, FlexAlign.new('main'), FlexAlign.new('cross'), FlexAlign.new('tracks'))
StyleFunc.new('set_flex_flow', Void, FlexFlow.new('flow'))
StyleFunc.new('set_prop', Void, StylePropVal.new('sty'), Dummy.new('val'))

LvFunc.new('disp_set_bg_color', Void, InstMember.new('inst', 'lvki_disp'), Color.new('color'))
LvFunc.new('disp_set_bg_image', Void, InstMember.new('inst', 'lvki_disp'), ImgSrc.new('src'))
LvFunc.new('disp_set_bg_opa', Void, InstMember.new('inst', 'lvki_disp'), UInt8.new('opacity'))
LvFunc.new('disp_get_inactive_time', UInt32, InstMember.new('inst', 'lvki_disp'))
LvFunc.new('disp_trig_activity', Void, InstMember.new('inst', 'lvki_disp'))
LvFunc.new('disp_get_layer_sys', Obj, InstMember.new('inst', 'lvki_disp'))
LvFunc.new('disp_get_layer_top', Obj, InstMember.new('inst', 'lvki_disp'))
LvFunc.new('disp_get_scr_act', Obj, InstMember.new('inst', 'lvki_disp'))
LvFunc.new('disp_get_scr_prev', Obj, InstMember.new('inst', 'lvki_disp'))
Func.new('set_kbd_group', 'lv_indev_set_group', Void, InstMember.new('inst', 'lvki_kbd'), Group.new('group'))
Func.new('scr_load', 'lv_disp_scr_load', Void, InstMember.new('inst', 'lvki_disp'), Obj.new('screen'))
Func.new('scr_load_anim', 'lv_disp_scr_load_anim', Void, InstMember.new('inst', 'lvki_disp'), Obj.new('screen'), ScrLoadAnim.new('anim'), UInt32.new('time'), UInt32.new('delay'), Bool8.new('autodel'))
Func.new('set_mouse_cursor', 'lv_indev_set_cursor', Void, InstMember.new('inst', 'lvki_mouse'), Obj.new('cursor'))

LvFunc.new('indev_get_focused', Obj, InstMember.new('inst', 'lvki_kbd'))

$stdout.reopen('c_src/nif_gen.h', 'w')
Func.print_all
puts "#define AUTOGEN_NIFS \\"
Func.print_all_nif_defs
$stdout.close

$stdout.reopen('src/nif_gen.hrl', 'w')
Func.print_all_erl
$stdout.close
