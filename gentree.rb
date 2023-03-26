require 'open3'
require 'json'
require 'ostruct'

Types = {
  'v' => 'void',
  'p' => 'void *',
  'q' => 'uint64_t',
  'l' => 'uint32_t',
  'w' => 'uint16_t',
  'c' => 'uint8_t',
  'C' => 'lv_color_t',
  'S' => 'lv_style_value_t',
  'P' => 'lv_point_t'}
Cases = {
  'v' => %w{ARG_NONE},
  'p' => %w{ARG_PTR ARG_CSTRING ARG_BUFFER ARG_CSTRING_ARRAY ARG_BUFFER_ARRAY},
  'q' => %w{ARG_UINT64},
  'l' => %w{ARG_UINT32},
  'w' => %w{ARG_UINT16},
  'c' => %w{ARG_UINT8},
  'C' => %w{ARG_COLOR},
  'S' => %w{ARG_STYLEVAL},
  'P' => %w{ARG_POINT}}
MaxArgIdx = 8

TypeAliases = {
  'p' => %w{lv_tlsf_t lv_pool_t},
  'l' => %w{uint32_t int32_t lv_indev_type_t lv_gridnav_ctrl_t lv_lru_res_t lv_fs_whence_t int lv_scr_load_anim_t lv_disp_rot_t lv_event_code_t lv_draw_layer_flags_t lv_imgbtn_state_t lv_part_t lv_obj_flag_t lv_style_selector_t lv_grid_align_t lv_flex_align_t lv_anim_enable_t lv_flex_flow_t lv_style_prop_t lv_palette_t lv_indev_state_t},
  'w' => %w{uint16_t int16_t lv_coord_t lv_state_t lv_btnmatrix_ctrl_t},
  'c' => %w{bool char uint8_t int8_t lv_res_t lv_slider_mode_t lv_chart_update_mode_t lv_draw_mask_res_t lv_img_src_t lv_chart_type_t lv_img_size_mode_t lv_fs_res_t lv_style_res_t lv_table_cell_ctrl_t lv_text_flag_t lv_text_align_t lv_dir_t lv_opa_t lv_text_align_t lv_text_flag_t lv_align_t lv_arc_mode_t lv_blend_mode_t lv_border_side_t lv_base_dir_t lv_grad_dir_t lv_dither_mode_t lv_span_mode_t lv_span_overflow_t lv_roller_mode_t lv_scroll_snap_t lv_scrollbar_mode_t lv_img_cf_t lv_chart_axis_t lv_bar_mode_t lv_keyboard_mode_t lv_label_long_mode_t lv_fs_mode_t lv_draw_mask_line_side_t lv_text_decor_t lv_menu_mode_header_t lv_menu_mode_root_back_btn_t},
  'q' => %w{uint64_t int64_t size_t},
  'C' => %w{lv_color_t lv_grad_color_t},
  'S' => %w{lv_style_value_t},
  'P' => %w{lv_point_t}
}
TypeAliasLookup = {}
TypeAliases.each do |t,as|
  as.each do |a|
    TypeAliasLookup[a] = t
  end
end
WarningBlacklist = %w{_cb ... va_list _sdl_ _demo_ _ffmpeg_ _pinyin_}

stdin, stdout, wait_thr = Open3.popen2('ctags', '-x', '--c-kinds=f', '--output-format=json', '--fields=tNS', '-R', 'c_src')
stdin.close
combos = {}
until stdout.eof?
  obj = JSON.parse(stdout.readline, :object_class => OpenStruct)
  next unless obj.name =~ /^lv_/
  next unless obj.signature =~ /^[(](.*)[)]$/
  args = $1.split(",")
  args = [] if args == ["void"]
  args = args.map do |a|
    a = a.gsub(/const /,'')
    t = a.split.first
    if a.split.include?('*')
      'p'
    else
      TypeAliasLookup[t]
    end
  end
  if args.include?(nil) and not WarningBlacklist.find { |b| obj.signature.include?(b) }
    STDERR.puts "#{obj.name}: unmapped arg types: #{obj.signature}"
  end
  next if args.include?(nil)
  next unless obj.typeref =~ /^typename:([^:]*)/
  crt = $1
  crt = crt.gsub(/const /,'')
  crt = crt.gsub(/^LV_ATTRIBUTE_[A-Z0-9_]+ /, '')
  rt = nil
  if crt.include?('*')
    rt = 'p'
  elsif crt == 'void'
    rt = 'v'
  else
    rt = TypeAliasLookup[crt]
  end
  if rt.nil?
    STDERR.puts "#{obj.name}: unmapped return type '#{crt}'"
    next
  end
  if args.size > MaxArgIdx
    STDERR.puts "#{obj.name}: too many args (#{args.size})"
    next
  end
  combos[[rt] + args] = true
end
wait_thr.join

class RetLayer
  def initialize(typepath)
    @typepath = typepath
    @cases = []
  end

  def [](type)
    @cases << type unless @cases.include?(type)
  end

  def print(level = 0)
    ind = "\t"*level
    args = []
    @typepath.each_with_index { |t,i| args << "a#{i}#{t}" }
    puts "#{ind}switch (cc->cc_return_type) {"
    @cases.each do |type|
      ctype = Types[type]
      Cases[type].each do |c|
        puts "#{ind}case #{c}:"
      end
      path = [type] + @typepath
      cbtype = "lv_call_func#{@typepath.size}_#{path.join('')}_t"
      if type == 'v'
        puts "#{ind}\t(*(#{cbtype})cc_func)("
        puts "#{ind}\t    #{args.join(",\n#{ind}\t    ")});"
        puts "#{ind}\treturn (0);"
      else
        puts "#{ind}\tret#{type} = (*(#{cbtype})cc_func)("
        puts "#{ind}\t    #{args.join(",\n#{ind}\t    ")});"
        if type == 'C'
          puts "#{ind}\treturn ((uint64_t)ret#{type}.full);"
        elsif type.upcase == type
          puts "#{ind}\tbcopy(&ret#{type}, &retq, sizeof (ret#{type}));"
          puts "#{ind}\treturn (retq);"
        else
          puts "#{ind}\treturn ((uint64_t)ret#{type});"
        end
      end
    end
    puts "#{ind}}"
  end
end

class ArgLayer
  def initialize(idx, types = [])
    @idx = idx
    @types = types
    @cases = {}
  end

  def [](type)
    if @cases[type].nil?
      @cases[type] = ArgLayer.new(@idx + 1, @types + [type])
    end
    @cases[type]
  end

  def ret()
    if @cases['v'].nil?
      @cases['v'] = RetLayer.new(@types)
    end
    @cases['v']
  end

  def print(level = 0)
    ind = "\t"*level
    if @idx == MaxArgIdx
      layer = @cases['v']
      layer.print(level) if layer
      return
    end
    if @idx == 0
      puts "#{ind}cc_arg = SLIST_FIRST(&cc->cc_args);"
    else
      puts "#{ind}cc_arg = SLIST_NEXT(cc_arg, ca_entry);"
    end
    puts "#{ind}switch (cc_arg->ca_type) {"
    @cases.each do |type,layer|
      ctype = Types[type]
      Cases[type].each do |c|
        puts "#{ind}case #{c}:"
      end
      if type != 'v'
        if type == 'p'
          puts "#{ind}\ta#{@idx}#{type} = rref_deref(cc_arg->ca_ref._ref, REF_ANY);"
        elsif type == 'q'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_u64;"
        elsif type == 'l'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_u32;"
        elsif type == 'w'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_u16;"
        elsif type == 'c'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_u8;"
        elsif type == 'C'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_color;"
        elsif type == 'S'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_style_val;"
        elsif type == 'P'
          puts "#{ind}\ta#{@idx}#{type} = cc_arg->ca_point;"
        end
      end
      layer.print(level + 1)
      puts "#{ind}\tbreak;"
    end
    puts "#{ind}}"
  end
end

puts '/* Auto-generated file. */'
puts '#include "lvcall.h"'
puts '#include <dlfcn.h>'

combos.each do |ts,_|
  tr = ts.first
  args = ts.slice(1, ts.size)
  n = args.size
  tsig = args.join('')
  trc = Types[tr]
  argc = args.map { |a| Types[a] }
  argc = ["void"] if args.size == 0
  puts "typedef #{trc} (*lv_call_func#{n}_#{tr}#{tsig}_t)(#{argc.join(', ')});"
end

puts "uint64_t"
puts "lv_do_real_call(const struct cmd_call *cc)"
puts "{"

Types.each do |type,ctype|
  next if ctype == 'void'
  vars = []
  prefix = ''
  if ctype =~ /^([^*]+) [*]$/
    ctype = $1
    prefix = '*'
  end
  lvls = ['ret']
  MaxArgIdx.times do |i|
    next unless combos.keys.find { |k| k[i + 1] == type }
    lvls << "a#{i}"
  end
  lvls.each do |level|
    vars << "#{prefix}#{level}#{type}"
  end
  puts "\t#{ctype} #{vars.join(", ")};"
end

puts "\tvoid *cc_func = dlsym(RTLD_DEFAULT, cc->cc_func_name);"
puts "\tconst struct cmd_arg *cc_arg;"

root = ArgLayer.new(0)
combos.each do |ts,_|
  tr = ts.first
  args = ts.slice(1, ts.size)
  layer = root
  args.each do |atype|
    layer = layer[atype]
  end
  layer.ret[tr]
end
root.print(1)

puts "\tassert(0);"
puts "}"
