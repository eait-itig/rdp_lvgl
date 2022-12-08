-spec bar_create(object()) -> async_return(object()).
bar_create(_Parent) -> error(no_nif).
-spec bar_set_value(object(), integer(), lv_anim:enable()) -> async_return().
bar_set_value(_Obj, _Value, _Anim) -> error(no_nif).
-spec bar_set_start_value(object(), integer(), lv_anim:enable()) -> async_return().
bar_set_start_value(_Obj, _Value, _Anim) -> error(no_nif).
-spec bar_set_range(object(), integer(), integer()) -> async_return().
bar_set_range(_Obj, _Min, _Max) -> error(no_nif).
-spec bar_set_mode(object(), lv_bar:mode()) -> async_return().
bar_set_mode(_Obj, _Mode) -> error(no_nif).
-spec bar_get_value(object()) -> async_return(integer()).
bar_get_value(_Obj) -> error(no_nif).
-spec btn_create(object()) -> async_return(object()).
btn_create(_Parent) -> error(no_nif).
-spec checkbox_create(object()) -> async_return(object()).
checkbox_create(_Parent) -> error(no_nif).
-spec checkbox_set_text(object(), iolist()) -> async_return().
checkbox_set_text(_Obj, _Text) -> error(no_nif).
-spec checkbox_get_text(object()) -> async_return(iolist()).
checkbox_get_text(_Obj) -> error(no_nif).
-spec textarea_create(object()) -> async_return(object()).
textarea_create(_Parent) -> error(no_nif).
-spec textarea_set_text(object(), iolist()) -> async_return().
textarea_set_text(_Obj, _Text) -> error(no_nif).
-spec textarea_get_text(object()) -> async_return(iolist()).
textarea_get_text(_Obj) -> error(no_nif).
-spec textarea_set_placeholder_text(object(), iolist()) -> async_return().
textarea_set_placeholder_text(_Obj, _Text) -> error(no_nif).
-spec textarea_set_text_selection(object(), boolean()) -> async_return().
textarea_set_text_selection(_Obj, _State) -> error(no_nif).
-spec textarea_set_password_mode(object(), boolean()) -> async_return().
textarea_set_password_mode(_Obj, _State) -> error(no_nif).
-spec textarea_set_one_line(object(), boolean()) -> async_return().
textarea_set_one_line(_Obj, _State) -> error(no_nif).
-spec textarea_set_accepted_chars(object(), buffer()) -> async_return().
textarea_set_accepted_chars(_Obj, _Buf) -> error(no_nif).
-spec textarea_get_label(object()) -> async_return(object()).
textarea_get_label(_Obj) -> error(no_nif).
-spec textarea_set_password_show_time(object(), integer()) -> async_return().
textarea_set_password_show_time(_Obj, _Time) -> error(no_nif).
-spec img_create(object()) -> async_return(object()).
img_create(_Parent) -> error(no_nif).
-spec img_set_offset(object(), lv:point()) -> async_return().
img_set_offset(_Obj, _Pt) -> error(no_nif).
-spec img_set_src(object(), lv_img:src()) -> async_return().
img_set_src(_Obj, _Src) -> error(no_nif).
-spec img_set_angle(object(), integer()) -> async_return().
img_set_angle(_Obj, _Angle) -> error(no_nif).
-spec img_set_pivot(object(), lv:point()) -> async_return().
img_set_pivot(_Obj, _Pivot) -> error(no_nif).
-spec img_set_zoom(object(), integer()) -> async_return().
img_set_zoom(_Obj, _Zoom) -> error(no_nif).
-spec img_set_antialias(object(), boolean()) -> async_return().
img_set_antialias(_Obj, _Ena) -> error(no_nif).
-spec label_create(object()) -> async_return(object()).
label_create(_Parent) -> error(no_nif).
-spec label_get_text(object()) -> async_return(iolist()).
label_get_text(_Obj) -> error(no_nif).
-spec label_set_text(object(), iolist()) -> async_return().
label_set_text(_Obj, _Text) -> error(no_nif).
-spec label_set_text_sel_start(object(), integer()) -> async_return().
label_set_text_sel_start(_Obj, _Index) -> error(no_nif).
-spec label_set_text_sel_end(object(), integer()) -> async_return().
label_set_text_sel_end(_Obj, _Index) -> error(no_nif).
-spec btnmatrix_create(object()) -> async_return(object()).
btnmatrix_create(_Parent) -> error(no_nif).
-spec btnmatrix_set_map(object(), [iolist()]) -> async_return().
btnmatrix_set_map(_Obj, _Map) -> error(no_nif).
-spec btnmatrix_set_btn_ctrl(object(), integer(), lv:flags(lv_btnmatrix:ctrl())) -> async_return().
btnmatrix_set_btn_ctrl(_Obj, _Idx, _Ctrl) -> error(no_nif).
-spec btnmatrix_clear_btn_ctrl(object(), integer(), lv:flags(lv_btnmatrix:ctrl())) -> async_return().
btnmatrix_clear_btn_ctrl(_Obj, _Idx, _Ctrl) -> error(no_nif).
-spec btnmatrix_set_btn_ctrl_all(object(), lv:flags(lv_btnmatrix:ctrl())) -> async_return().
btnmatrix_set_btn_ctrl_all(_Obj, _Ctrl) -> error(no_nif).
-spec btnmatrix_clear_btn_ctrl_all(object(), lv:flags(lv_btnmatrix:ctrl())) -> async_return().
btnmatrix_clear_btn_ctrl_all(_Obj, _Ctrl) -> error(no_nif).
-spec btnmatrix_set_btn_width(object(), integer(), integer()) -> async_return().
btnmatrix_set_btn_width(_Obj, _Idx, _Width) -> error(no_nif).
-spec btnmatrix_set_one_checked(object(), boolean()) -> async_return().
btnmatrix_set_one_checked(_Obj, _Checked) -> error(no_nif).
-spec btnmatrix_get_selected_btn(object()) -> async_return(integer()).
btnmatrix_get_selected_btn(_Obj) -> error(no_nif).
-spec btnmatrix_set_selected_btn(object(), integer()) -> async_return().
btnmatrix_set_selected_btn(_Obj, _Idx) -> error(no_nif).
-spec btnmatrix_get_btn_text(object(), integer()) -> async_return(iolist()).
btnmatrix_get_btn_text(_Obj, _Idx) -> error(no_nif).
-spec btnmatrix_has_btn_ctrl(object(), integer(), lv:flags(lv_btnmatrix:ctrl())) -> async_return(boolean()).
btnmatrix_has_btn_ctrl(_Obj, _Idx, _Ctrl) -> error(no_nif).
-spec dropdown_create(object()) -> async_return(object()).
dropdown_create(_Parent) -> error(no_nif).
-spec dropdown_set_options(object(), iolist()) -> async_return().
dropdown_set_options(_Obj, _Opts) -> error(no_nif).
-spec dropdown_add_option(object(), iolist(), integer()) -> async_return().
dropdown_add_option(_Obj, _Text, _Index) -> error(no_nif).
-spec dropdown_get_selected(object()) -> async_return(integer()).
dropdown_get_selected(_Obj) -> error(no_nif).
-spec dropdown_set_selected(object(), integer()) -> async_return().
dropdown_set_selected(_Obj, _Index) -> error(no_nif).
-spec dropdown_clear_options(object()) -> async_return().
dropdown_clear_options(_Obj) -> error(no_nif).
-spec dropdown_get_selected_str(object()) -> async_return(iolist()).
dropdown_get_selected_str(_Obj) -> error(no_nif).
-spec imgbtn_create(object()) -> async_return(object()).
imgbtn_create(_Parent) -> error(no_nif).
-spec led_create(object()) -> async_return(object()).
led_create(_Parent) -> error(no_nif).
-spec led_set_color(object(), lv:color()) -> async_return().
led_set_color(_Obj, _Color) -> error(no_nif).
-spec led_set_brightness(object(), integer()) -> async_return().
led_set_brightness(_Obj, _Bright) -> error(no_nif).
-spec led_on(object()) -> async_return().
led_on(_Obj) -> error(no_nif).
-spec led_off(object()) -> async_return().
led_off(_Obj) -> error(no_nif).
-spec led_toggle(object()) -> async_return().
led_toggle(_Obj) -> error(no_nif).
-spec led_get_brightness(object()) -> async_return(integer()).
led_get_brightness(_Obj) -> error(no_nif).
-spec list_create(object()) -> async_return(object()).
list_create(_Parent) -> error(no_nif).
-spec list_add_text(object(), iolist()) -> async_return(object()).
list_add_text(_Obj, _Text) -> error(no_nif).
-spec list_add_btn(object(), lv_img:src(), iolist()) -> async_return(object()).
list_add_btn(_Obj, _Icon, _Text) -> error(no_nif).
-spec list_get_btn_text(object(), object()) -> async_return(iolist()).
list_get_btn_text(_Obj, _Btn) -> error(no_nif).
-spec menu_create(object()) -> async_return(object()).
menu_create(_Parent) -> error(no_nif).
-spec menu_page_create(object(), iolist()) -> async_return(object()).
menu_page_create(_Obj, _Title) -> error(no_nif).
-spec menu_cont_create(object()) -> async_return(object()).
menu_cont_create(_Parent) -> error(no_nif).
-spec menu_section_create(object()) -> async_return(object()).
menu_section_create(_Parent) -> error(no_nif).
-spec menu_separator_create(object()) -> async_return(object()).
menu_separator_create(_Parent) -> error(no_nif).
-spec menu_set_page(object(), object()) -> async_return().
menu_set_page(_Obj, _Page) -> error(no_nif).
-spec menu_set_sidebar_page(object(), object()) -> async_return().
menu_set_sidebar_page(_Obj, _Page) -> error(no_nif).
-spec menu_set_mode_root_back_btn(object(), lv_menu:root_back_btn_mode()) -> async_return().
menu_set_mode_root_back_btn(_Obj, _Mode) -> error(no_nif).
-spec menu_set_mode_header(object(), lv_menu:header_mode()) -> async_return().
menu_set_mode_header(_Obj, _Mode) -> error(no_nif).
-spec menu_set_load_page_event(object(), object(), object()) -> async_return().
menu_set_load_page_event(_Obj, _Btn, _Page) -> error(no_nif).
-spec roller_create(object()) -> async_return(object()).
roller_create(_Parent) -> error(no_nif).
-spec slider_create(object()) -> async_return(object()).
slider_create(_Parent) -> error(no_nif).
-spec switch_create(object()) -> async_return(object()).
switch_create(_Parent) -> error(no_nif).
-spec table_create(object()) -> async_return(object()).
table_create(_Parent) -> error(no_nif).
-spec table_set_row_cnt(object(), integer()) -> async_return().
table_set_row_cnt(_Obj, _Rows) -> error(no_nif).
-spec table_set_col_cnt(object(), integer()) -> async_return().
table_set_col_cnt(_Obj, _Cols) -> error(no_nif).
-spec table_set_col_width(object(), integer(), lv:coord()) -> async_return().
table_set_col_width(_Obj, _Col_idx, _Width) -> error(no_nif).
-spec table_set_cell_value(object(), integer(), integer(), iolist()) -> async_return().
table_set_cell_value(_Obj, _Row, _Col, _Text) -> error(no_nif).
-spec table_add_cell_ctrl(object(), integer(), integer(), lv_table:ctrl()) -> async_return().
table_add_cell_ctrl(_Obj, _Row, _Col, _Ctrl) -> error(no_nif).
-spec table_clear_cell_ctrl(object(), integer(), integer(), lv_table:ctrl()) -> async_return().
table_clear_cell_ctrl(_Obj, _Row, _Col, _Ctrl) -> error(no_nif).
-spec table_get_selected_cell_pt(object()) -> async_return(lv:point()).
table_get_selected_cell_pt(_Obj) -> error(no_nif).
-spec msgbox_create(object(), iolist(), iolist(), [iolist()], boolean()) -> async_return(object()).
msgbox_create(_Parent, _Title, _Text, _Btns, _Add_close) -> error(no_nif).
-spec msgbox_get_active_btn(object()) -> async_return(integer()).
msgbox_get_active_btn(_Obj) -> error(no_nif).
-spec msgbox_get_active_btn_text(object()) -> async_return(iolist()).
msgbox_get_active_btn_text(_Obj) -> error(no_nif).
-spec msgbox_close(object()) -> async_return().
msgbox_close(_Obj) -> error(no_nif).
-spec msgbox_get_btns(object()) -> async_return(object()).
msgbox_get_btns(_Obj) -> error(no_nif).
-spec spinner_create(object(), integer(), integer()) -> async_return(object()).
spinner_create(_Parent, _Time, _Arclen) -> error(no_nif).
-spec meter_create(object()) -> async_return(object()).
meter_create(_Parent) -> error(no_nif).
-spec meter_add_scale(object()) -> async_return(lv_meter:scale()).
meter_add_scale(_Obj) -> error(no_nif).
-spec meter_set_scale_ticks(object(), lv_meter:scale(), integer(), integer(), integer(), lv:color()) -> async_return().
meter_set_scale_ticks(_Obj, _Scale, _Cnt, _Width, _Len, _Color) -> error(no_nif).
-spec meter_set_scale_major_ticks(object(), lv_meter:scale(), integer(), integer(), integer(), lv:color(), integer()) -> async_return().
meter_set_scale_major_ticks(_Obj, _Scale, _Nth, _Width, _Len, _Color, _Label_gap) -> error(no_nif).
-spec meter_set_scale_range(object(), lv_meter:scale(), integer(), integer(), integer(), integer()) -> async_return().
meter_set_scale_range(_Obj, _Scale, _Min, _Max, _Angle_range, _Rotation) -> error(no_nif).
-spec meter_add_needle_line(object(), lv_meter:scale(), integer(), lv:color(), integer()) -> async_return(lv_meter:indicator()).
meter_add_needle_line(_Obj, _Scale, _Width, _Color, _R_mod) -> error(no_nif).
-spec meter_add_arc(object(), lv_meter:scale(), integer(), lv:color(), integer()) -> async_return(lv_meter:indicator()).
meter_add_arc(_Obj, _Scale, _Width, _Color, _R_mod) -> error(no_nif).
-spec meter_set_indicator_value(object(), lv_meter:indicator(), integer()) -> async_return().
meter_set_indicator_value(_Obj, _Ind, _Value) -> error(no_nif).
-spec meter_set_indicator_start_value(object(), lv_meter:indicator(), integer()) -> async_return().
meter_set_indicator_start_value(_Obj, _Ind, _Value) -> error(no_nif).
-spec meter_set_indicator_end_value(object(), lv_meter:indicator(), integer()) -> async_return().
meter_set_indicator_end_value(_Obj, _Ind, _Value) -> error(no_nif).
-spec chart_create(object()) -> async_return(object()).
chart_create(_Parent) -> error(no_nif).
-spec obj_create(instance(), object()) -> async_return(object()).
obj_create(_Inst, _Parent) -> error(no_nif).
-spec obj_center(object()) -> async_return().
obj_center(_Obj) -> error(no_nif).
-spec obj_add_flag(object(), lv:flags(lv_obj:flag())) -> async_return().
obj_add_flag(_Obj, _Flags) -> error(no_nif).
-spec obj_clear_flag(object(), lv:flags(lv_obj:flag())) -> async_return().
obj_clear_flag(_Obj, _Flags) -> error(no_nif).
-spec obj_has_flag(object(), lv:flags(lv_obj:flag())) -> async_return(boolean()).
obj_has_flag(_Obj, _Flags) -> error(no_nif).
-spec obj_has_flag_any(object(), lv:flags(lv_obj:flag())) -> async_return(boolean()).
obj_has_flag_any(_Obj, _Flags) -> error(no_nif).
-spec obj_add_state(object(), lv:flags(lv_obj:state())) -> async_return().
obj_add_state(_Obj, _States) -> error(no_nif).
-spec obj_clear_state(object(), lv:flags(lv_obj:state())) -> async_return().
obj_clear_state(_Obj, _States) -> error(no_nif).
-spec obj_get_state(object()) -> async_return(lv:flags(lv_obj:state())).
obj_get_state(_Obj) -> error(no_nif).
-spec obj_add_style(object(), style(), lv:flags(lv_obj:selector())) -> async_return().
obj_add_style(_Obj, _Style, _Sel) -> error(no_nif).
-spec obj_align(object(), lv_obj:align_spec(), lv:point()) -> async_return().
obj_align(_Obj, _Align, _Offset) -> error(no_nif).
-spec obj_align(object(), lv_obj:align_spec()) -> async_return().
obj_align(_Obj, _Align) -> error(no_nif).
-spec obj_align_to(object(), object(), lv_obj:align_spec(), lv:point()) -> async_return().
obj_align_to(_Obj, _Tobj, _Align, _Offset) -> error(no_nif).
-spec obj_get_pos(object()) -> async_return(lv:point()).
obj_get_pos(_Obj) -> error(no_nif).
-spec obj_get_size(object()) -> async_return(lv:point()).
obj_get_size(_Obj) -> error(no_nif).
-spec obj_set_size(object(), lv:point()) -> async_return().
obj_set_size(_Obj, _Size) -> error(no_nif).
-spec obj_set_local_style_prop(object(), style_generic_prop(), style_generic_val(), lv:flags(lv_obj:selector())) -> async_return().
obj_set_local_style_prop(_Obj, _Sty, _Val, _Sel) -> error(no_nif).
-spec obj_move_foreground(object()) -> async_return().
obj_move_foreground(_Obj) -> error(no_nif).
-spec obj_move_background(object()) -> async_return().
obj_move_background(_Obj) -> error(no_nif).
-spec obj_swap(object(), object()) -> async_return().
obj_swap(_Obj, _Other) -> error(no_nif).
-spec obj_set_parent(object(), object()) -> async_return().
obj_set_parent(_Obj, _Parent) -> error(no_nif).
-spec obj_move_to_index(object(), integer()) -> async_return().
obj_move_to_index(_Obj, _Index) -> error(no_nif).
-spec obj_get_index(object()) -> async_return(integer()).
obj_get_index(_Obj) -> error(no_nif).
-spec obj_get_child_cnt(object()) -> async_return(integer()).
obj_get_child_cnt(_Obj) -> error(no_nif).
-spec obj_get_child(object(), integer()) -> async_return(object()).
obj_get_child(_Obj, _Index) -> error(no_nif).
-spec obj_get_parent(object()) -> async_return(object()).
obj_get_parent(_Obj) -> error(no_nif).
-spec obj_get_screen(object()) -> async_return(object()).
obj_get_screen(_Obj) -> error(no_nif).
-spec obj_clean(object()) -> async_return().
obj_clean(_Obj) -> error(no_nif).
-spec obj_del(object()) -> async_return().
obj_del(_Obj) -> error(no_nif).
-spec group_create(instance()) -> async_return(group()).
group_create(_Inst) -> error(no_nif).
-spec group_add_obj(group(), object()) -> async_return().
group_add_obj(_Group, _Obj) -> error(no_nif).
-spec style_create(instance()) -> async_return(style()).
style_create(_Inst) -> error(no_nif).
-spec style_set_flex_align(style(), lv_style:flex_align(), lv_style:flex_align(), lv_style:flex_align()) -> async_return().
style_set_flex_align(_Style, _Main, _Cross, _Tracks) -> error(no_nif).
-spec style_set_flex_flow(style(), lv_style:flex_flow()) -> async_return().
style_set_flex_flow(_Style, _Flow) -> error(no_nif).
-spec style_set_prop(style(), style_generic_prop(), style_generic_val()) -> async_return().
style_set_prop(_Style, _Sty, _Val) -> error(no_nif).
-spec disp_set_bg_color(instance(), lv:color()) -> async_return().
disp_set_bg_color(_Inst, _Color) -> error(no_nif).
-spec disp_set_bg_image(instance(), lv_img:src()) -> async_return().
disp_set_bg_image(_Inst, _Src) -> error(no_nif).
-spec disp_set_bg_opa(instance(), integer()) -> async_return().
disp_set_bg_opa(_Inst, _Opacity) -> error(no_nif).
-spec disp_get_inactive_time(instance()) -> async_return(integer()).
disp_get_inactive_time(_Inst) -> error(no_nif).
-spec disp_trig_activity(instance()) -> async_return().
disp_trig_activity(_Inst) -> error(no_nif).
-spec disp_get_layer_sys(instance()) -> async_return(object()).
disp_get_layer_sys(_Inst) -> error(no_nif).
-spec disp_get_layer_top(instance()) -> async_return(object()).
disp_get_layer_top(_Inst) -> error(no_nif).
-spec disp_get_scr_act(instance()) -> async_return(object()).
disp_get_scr_act(_Inst) -> error(no_nif).
-spec disp_get_scr_prev(instance()) -> async_return(object()).
disp_get_scr_prev(_Inst) -> error(no_nif).
-spec set_kbd_group(instance(), group()) -> async_return().
set_kbd_group(_Inst, _Group) -> error(no_nif).
-spec scr_load(instance(), object()) -> async_return().
scr_load(_Inst, _Screen) -> error(no_nif).
-spec scr_load_anim(instance(), object(), lv_scr:load_anim(), integer(), integer(), boolean()) -> async_return().
scr_load_anim(_Inst, _Screen, _Anim, _Time, _Delay, _Autodel) -> error(no_nif).
-spec set_mouse_cursor(instance(), object()) -> async_return().
set_mouse_cursor(_Inst, _Cursor) -> error(no_nif).
-spec indev_get_focused(instance()) -> async_return(object()).
indev_get_focused(_Inst) -> error(no_nif).
