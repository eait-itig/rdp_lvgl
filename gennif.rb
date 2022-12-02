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
end

class LvObject < Arg
  def arg_type; 'ARG_OBJPTR'; end
  def declare
    write "struct lvkobj *#{@name};"
  end
  def parse
    write "rc = enter_obj_hdl(env, argv[#{@idx}], &nls, &#{@name}, 0);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
end

class Group < Arg
  def arg_type; 'ARG_GRPPTR'; end
  def declare
    write "struct lvkgroup *#{@name};"
  end
  def parse
    write "rc = enter_grp_hdl(env, argv[#{@idx}], &nls, &#{@name}, 0);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
end

class Buffer < Arg
  def arg_type; 'ARG_BUFPTR'; end
  def declare
    write "struct lvkbuf *#{@name};"
  end
  def parse
    write "rc = unpack_buf_hdl(env, argv[#{@idx}], &nls, &#{@name});"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
end

class Style < Arg
  def arg_type; 'ARG_STYPTR'; end
  def declare
    write "struct lvkstyle *#{@name};"
  end
  def parse
    write "rc = enter_sty_hdl(env, argv[#{@idx}], &nls, &#{@name}, 0);"
    write "if (rc != 0) {"
    write parse_error_rc
    write "}"
  end
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
    write "    ARG_INLINE_BUF, #{@name}.data, #{@name}.size,"
  end
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
end
class ObjFlags < Enum32
  def enum; 'obj_flags'; end
  def multi; true; end
end
class ObjStates < Enum32
  def enum; 'obj_state_specs'; end
  def multi; true; end
end
class StyleSelector < Enum32
  def enum; 'style_selector_specs'; end
  def multi; true; end
end
class FlexAlign < Enum32
  def enum; 'flex_align'; end
  def multi; false; end
end
class FlexFlow < Enum32
  def enum; 'flex_flow'; end
  def multi; false; end
end
class ScrLoadAnim < Enum32
  def enum; 'scr_load_anims'; end
  def multi; false; end
end
class BarMode < Enum8
  def enum; 'bar_mode'; end
  def multi; false; end
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
    args.unshift(LvObject.new('parent'))
    super("#{widget}_create", "lv_#{widget}_create", LvObject, *args)
  end
end

class ObjFunc < Func
  def initialize(name, rtype, *args)
    args.unshift(LvObject.new('obj'))
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
    args.unshift(LvObject.new('obj'))
    super("#{widget}_#{name}", "lv_#{widget}_#{name}", rtype, *args)
  end

  def parse
    super
    puts ""
    puts "\tif (obj->lvko_class != &lv_#{@widget}_class) {"
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

WidgetCreateFunc.new('img')
WidgetFunc.new('img', 'set_offset', Void, Point.new('pt'))

WidgetCreateFunc.new('label')
WidgetFunc.new('label', 'set_text', Void, InlineStr.new('text'))

WidgetCreateFunc.new('btnmatrix')

WidgetCreateFunc.new('dropdown')
WidgetFunc.new('dropdown', 'set_options', Void, InlineStr.new('opts'))
WidgetFunc.new('dropdown', 'add_option', Void, InlineStr.new('text'),
  Int32.new('index'))
WidgetFunc.new('dropdown', 'get_selected', Int32)
WidgetFunc.new('dropdown', 'set_selected', Void, Int32.new('index'))
WidgetFunc.new('dropdown', 'clear_options', Void)
WidgetFunc.new('dropdown', 'get_selected_str', InlineStr)

WidgetCreateFunc.new('imgbtn')

WidgetCreateFunc.new('led')

WidgetCreateFunc.new('list')

WidgetCreateFunc.new('menu')

WidgetCreateFunc.new('roller')

WidgetCreateFunc.new('slider')

WidgetCreateFunc.new('switch')

WidgetCreateFunc.new('table')

WidgetCreateFunc.new('spinner', UInt32.new('time'), UInt32.new('arclen'))

ObjFunc.new('center', Void)
ObjFunc.new('add_flag', Void, ObjFlags.new('flags'))
ObjFunc.new('clear_flag', Void, ObjFlags.new('flags'))
ObjFunc.new('add_state', Void, ObjStates.new('states'))
ObjFunc.new('clear_state', Void, ObjStates.new('states'))
ObjFunc.new('get_state', ObjStates)
ObjFunc.new('add_style', Void, Style.new('style'), StyleSelector.new('sel'))
ObjFunc.new('align', Void, AlignSpec.new('align'), SplitPoint.new('offset'))
Func.new('obj_align', 'lv_obj_set_align', Void, LvObject.new('obj'),
  AlignSpec.new('align'))
ObjFunc.new('align_to', Void, LvObject.new('tobj'), AlignSpec.new('align'),
  SplitPoint.new('offset'))
ObjFunc.new('get_pos', Point)
ObjFunc.new('get_size', Point)
ObjFunc.new('set_size', Void, SplitPoint.new('size'))

LvFunc.new('group_create', Group, Inst.new('inst'))
LvFunc.new('group_add_obj', Void, Group.new('group'),
  LvObject.new('obj'))

Func.new('style_create', 'lv_style_alloc', Style, Inst.new('inst'))
StyleFunc.new('set_flex_align', Void, FlexAlign.new('main'),
  FlexAlign.new('cross'), FlexAlign.new('tracks'))
StyleFunc.new('set_flex_flow', Void, FlexFlow.new('flow'))

LvFunc.new('disp_set_bg_color', Void, InstMember.new('inst', 'lvki_disp'),
  Color.new('color'))
LvFunc.new('disp_get_layer_sys', LvObject, InstMember.new('inst', 'lvki_disp'))
Func.new('set_kbd_group', 'lv_indev_set_group', Void,
  InstMember.new('inst', 'lvki_kbd'), Group.new('group'))
Func.new('scr_load', 'lv_disp_scr_load', Void,
  InstMember.new('inst', 'lvki_disp'), LvObject.new('screen'))
Func.new('scr_load_anim', 'lv_disp_scr_load_anim', Void,
  InstMember.new('inst', 'lvki_disp'),
  LvObject.new('screen'),
  ScrLoadAnim.new('anim'),
  UInt32.new('time'),
  UInt32.new('delay'),
  Bool8.new('autodel'))
Func.new('set_mouse_cursor', 'lv_indev_set_cursor', Void,
  InstMember.new('inst', 'lvki_mouse'),
  LvObject.new('cursor'))

Func.print_all

puts "#define AUTOGEN_NIFS \\"
Func.print_all_nif_defs
