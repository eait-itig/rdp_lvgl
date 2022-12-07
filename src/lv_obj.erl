%%
%% RDP UI framework using LVGL
%%
%% Copyright 2022 Alex Wilson <alex@uq.edu.au>, The University of Queensland
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc The base LVGL object, parent of all widgets.
%%
%% The base widget can also be used as a generic container.
%%
%% See [https://docs.lvgl.io/master/widgets/obj.html]
-module(lv_obj).

-export([
    add_flag/2,
    clear_flag/2,
    has_flag/2,
    has_flag_any/2,
    add_state/2,
    clear_state/2,
    get_state/1,
    get_class/1,
    has_class/2,
    has_state/2,
    center/1,
    create/2,
    align/3,
    align/2,
    align_to/4,
    align_to/3,
    set_size/2,
    add_style/2,
    add_style/3,
    get_size/1,
    get_pos/1,
    move_foreground/1,
    move_background/1,
    swap/2,
    set_parent/2,
    move_to_index/2,
    get_index/1,
    get_child_cnt/1,
    get_child/2,
    get_parent/1,
    get_screen/1,
    clean/1,
    del/1,
    set_style_width/2,
    set_style_width/3,
    set_style_min_width/2,
    set_style_min_width/3,
    set_style_max_width/2,
    set_style_max_width/3,
    set_style_height/2,
    set_style_height/3,
    set_style_min_height/2,
    set_style_min_height/3,
    set_style_max_height/2,
    set_style_max_height/3,
    set_style_x/2,
    set_style_x/3,
    set_style_y/2,
    set_style_y/3,
    set_style_align/2,
    set_style_align/3,
    set_style_radius/2,
    set_style_radius/3,
    set_style_pad_top/2,
    set_style_pad_top/3,
    set_style_pad_bottom/2,
    set_style_pad_bottom/3,
    set_style_pad_left/2,
    set_style_pad_left/3,
    set_style_pad_right/2,
    set_style_pad_right/3,
    set_style_pad_row/2,
    set_style_pad_row/3,
    set_style_pad_column/2,
    set_style_pad_column/3,
    set_style_base_dir/2,
    set_style_base_dir/3,
    set_style_clip_corner/2,
    set_style_clip_corner/3,
    set_style_bg_color/2,
    set_style_bg_color/3,
    set_style_bg_opa/2,
    set_style_bg_opa/3,
    set_style_bg_grad_color/2,
    set_style_bg_grad_color/3,
    set_style_bg_grad_dir/2,
    set_style_bg_grad_dir/3,
    set_style_bg_main_stop/2,
    set_style_bg_main_stop/3,
    set_style_bg_grad_stop/2,
    set_style_bg_grad_stop/3,
    set_style_bg_img_opa/2,
    set_style_bg_img_opa/3,
    set_style_bg_img_recolor/2,
    set_style_bg_img_recolor/3,
    set_style_bg_img_recolor_opa/2,
    set_style_bg_img_recolor_opa/3,
    set_style_bg_img_tiled/2,
    set_style_bg_img_tiled/3,
    set_style_border_color/2,
    set_style_border_color/3,
    set_style_border_opa/2,
    set_style_border_opa/3,
    set_style_border_width/2,
    set_style_border_width/3,
    set_style_border_side/2,
    set_style_border_side/3,
    set_style_border_post/2,
    set_style_border_post/3,
    set_style_outline_width/2,
    set_style_outline_width/3,
    set_style_outline_color/2,
    set_style_outline_color/3,
    set_style_outline_opa/2,
    set_style_outline_opa/3,
    set_style_outline_pad/2,
    set_style_outline_pad/3,
    set_style_shadow_width/2,
    set_style_shadow_width/3,
    set_style_shadow_ofs_x/2,
    set_style_shadow_ofs_x/3,
    set_style_shadow_ofs_y/2,
    set_style_shadow_ofs_y/3,
    set_style_shadow_spread/2,
    set_style_shadow_spread/3,
    set_style_shadow_color/2,
    set_style_shadow_color/3,
    set_style_shadow_opa/2,
    set_style_shadow_opa/3,
    set_style_img_opa/2,
    set_style_img_opa/3,
    set_style_img_recolor/2,
    set_style_img_recolor/3,
    set_style_img_recolor_opa/2,
    set_style_img_recolor_opa/3,
    set_style_line_width/2,
    set_style_line_width/3,
    set_style_line_dash_width/2,
    set_style_line_dash_width/3,
    set_style_line_dash_gap/2,
    set_style_line_dash_gap/3,
    set_style_line_rounded/2,
    set_style_line_rounded/3,
    set_style_line_color/2,
    set_style_line_color/3,
    set_style_line_opa/2,
    set_style_line_opa/3,
    set_style_arc_width/2,
    set_style_arc_width/3,
    set_style_arc_rounded/2,
    set_style_arc_rounded/3,
    set_style_arc_color/2,
    set_style_arc_color/3,
    set_style_arc_opa/2,
    set_style_arc_opa/3,
    set_style_text_color/2,
    set_style_text_color/3,
    set_style_text_opa/2,
    set_style_text_opa/3,
    set_style_text_letter_space/2,
    set_style_text_letter_space/3,
    set_style_text_line_space/2,
    set_style_text_line_space/3,
    set_style_text_decor/2,
    set_style_text_decor/3,
    set_style_text_align/2,
    set_style_text_align/3,
    set_style_opa/2,
    set_style_opa/3,
    set_style_color_filter_opa/2,
    set_style_color_filter_opa/3,
    set_style_anim_time/2,
    set_style_anim_time/3,
    set_style_anim_speed/2,
    set_style_anim_speed/3,
    set_style_blend_mode/2,
    set_style_blend_mode/3,
    set_style_transform_width/2,
    set_style_transform_width/3,
    set_style_transform_height/2,
    set_style_transform_height/3,
    set_style_translate_x/2,
    set_style_translate_x/3,
    set_style_translate_y/2,
    set_style_translate_y/3,
    set_style_transform_zoom/2,
    set_style_transform_zoom/3,
    set_style_transform_angle/2,
    set_style_transform_angle/3,
    set_style_transform_pivot_x/2,
    set_style_transform_pivot_x/3,
    set_style_transform_pivot_y/2,
    set_style_transform_pivot_y/3,
    set_style_text_font/2,
    set_style_text_font/3
    ]).

-include("async_wrappers.hrl").

-export_type([
    flag/0, align_spec/0, dir_spec/0, state/0, part/0,
    selector/0
    ]).

-type flag() :: hidden | clickable | click_focusable | checkable |
    scrollable | scroll_elastic | scroll_momentum | scroll_one |
    scroll_chain_hor | scroll_chain_ver | scroll_on_focus | scroll_with_arrow |
    snappable | press_lock | event_bubble | gesture_bubble | adv_hittest |
    ignore_layout | floating | overflow_visible | flex_in_new_track.

-type state() :: checked | focused | focus_key | edited | hovered | pressed |
    scrolled | disabled | user_1 | user_2 | user_3 | user_4.

-type part() :: main | scrollbar | indicator | knob | selected | items |
    ticks | cursor.

-type selector() :: [state() | part() | any_state | any_part].
%% Determines what combination of states and parts of a widget a style should
%% be applied to.
%%
%% For example, the selector <code>[knob, focused]</code> can be used to apply
%% a style only to the "knob" part of the widget when the widget is in input
%% focus.
%%
%% The selector <code>[]</code> is equivalent to <code>[any_state, any_part]</code>.

-type align_spec() :: out_top_left | out_top_mid | out_top_right |
    out_right_top | out_right_mid | out_right_bottom | out_bottom_right |
    out_bottom_mid | out_bottom_left | out_left_bottom | out_left_mid |
    out_left_top | top_left | top_mid | top_right | left_mid | center |
    right_mid | bottom_left | bottom_mid | bottom_right.
%% Specifies where a widget should be placed with respect to its parent.
%%
%% See [https://docs.lvgl.io/master/widgets/obj.html#alignment]

-type dir_spec() :: top | bottom | left | right | horizontal | vertical | all.

%% @doc Sets a list of flags on an object.
%%
%% If a given flag is already set, then it will not be changed.
%%
%% @see flag()
-spec add_flag(lv:object(), [flag()]) -> ok | lv:error().
add_flag(Obj, Flags) ->
    ?async_void_wrapper(obj_add_flag, Obj, Flags).

%% @doc Clears a list of flags on an object.
%%
%% If a given flag is already unset, then it will not be changed.
%%
%% @see flag()
-spec clear_flag(lv:object(), [flag()]) -> ok | lv:error().
clear_flag(Obj, Flags) ->
    ?async_void_wrapper(obj_clear_flag, Obj, Flags).

%% @doc Tests whether all of the given flags are set on an object.
%%
%% @see flag()
-spec has_flag(lv:object(), [flag()]) -> ok | lv:error().
has_flag(Obj, Flags) ->
    ?async_wrapper(obj_has_flag, Obj, Flags).

%% @doc Tests whether any one of the given flags is set on an object.
%%
%% @see flag()
-spec has_flag_any(lv:object(), [flag()]) -> ok | lv:error().
has_flag_any(Obj, Flags) ->
    ?async_wrapper(obj_has_flag_any, Obj, Flags).

%% @doc Sets a state flag on an object.
%%
%% @see state()
-spec add_state(lv:object(), state() | [state()]) -> ok | lv:error().
add_state(Obj, State) ->
    ?async_void_wrapper(obj_add_state, Obj, State).

%% @doc Clears a state flag on an object.
%%
%% @see state()
-spec clear_state(lv:object(), state() | [state()]) -> ok | lv:error().
clear_state(Obj, State) ->
    ?async_void_wrapper(obj_clear_state, Obj, State).

-type class() :: lv_animimg | lv_arc | lv_bar | lv_btn | lv_btnmatrix |
    lv_calendar | lv_calendar_header_arrow | lv_calendar_header_dropdown |
    lv_canvas | lv_chart | lv_checkbox | lv_colorwheel | lv_dropdown |
    lv_dropdownlist | lv_ffmpeg_player | lv_gif | lv_ime_pinyin | lv_img |
    lv_imgbtn | lv_keyboard | lv_label | lv_led | lv_line | lv_list_btn |
    lv_list | lv_list_text | lv_menu | lv_menu_cont | lv_menu_main_cont |
    lv_menu_main_header_cont | lv_menu_page | lv_menu_section |
    lv_menu_separator | lv_menu_sidebar_cont | lv_menu_sidebar_header_cont |
    lv_meter | lv_msgbox_backdrop | lv_msgbox | lv_msgbox_content | lv_obj |
    lv_qrcode | lv_rlottie | lv_roller | lv_roller_label | lv_slider |
    lv_spangroup | lv_spinbox | lv_spinner | lv_switch | lv_table |
    lv_tabview | lv_templ | lv_textarea | lv_tileview | lv_tileview_tile|
    lv_win.

%% @doc Identifies the class of an object.
%%
%% @see class()
-spec get_class(lv:object()) -> class() | unknown.
get_class(Obj) ->
    rdp_lvgl_nif:obj_get_class(Obj).

%% @doc Detects whether an object is based on the given class or a subclass.
%%
%% @see class()
-spec has_class(lv:object(), class()) -> boolean().
has_class(Obj, Class) ->
    rdp_lvgl_nif:obj_has_class(Obj, Class).

%% @doc Retrieves the list of all state flags on an object.
%%
%% @see state()
-spec get_state(lv:object()) -> {ok, [state()]} | lv:error().
get_state(Obj) ->
    ?async_wrapper(obj_get_state, Obj).

%% @doc Tests whether an object has a particular state set.
%%
%% @see state()
-spec has_state(lv:object(), state()) -> {ok, boolean()} | lv:error().
has_state(Obj, State) ->
    case get_state(Obj) of
        {ok, States} ->
            {ok, lists:member(State, States)};
        Err ->
            Err
    end.

%% @doc Centers an object inside its parent.
%%
%% Equivalent to <code>lv_obj:align(Obj, center).</code>
%%
%% @see lv_obj:align/2
-spec center(lv:object()) -> ok | lv:error().
center(Obj) ->
    ?async_void_wrapper(obj_center, Obj).

%% @doc Creates a base lv_obj widget.
-spec create(lv:instance(), lv:object()) -> {ok, lv:object()} | lv:error().
create(Inst, Parent) ->
    ?async_wrapper(obj_create, Inst, Parent).

%% @doc Aligns a widget within its parent, with an offset.
%%
%% @see align_spec()
-spec align(lv:object(), align_spec(), lv:point()) -> ok | lv:error().
align(Obj, Spec, Offset) ->
    ?async_void_wrapper(obj_align, Obj, Spec, Offset).

%% @doc Aligns a widget within its parent.
%%
%% @see align_spec()
-spec align(lv:object(), align_spec()) -> ok | lv:error().
align(Obj, Spec) ->
    ?async_void_wrapper(obj_align, Obj, Spec).

%% @doc Aligns a widget using a sibling widget as a base, with an offset.
%%
%% @see align_spec()
%% @see lv_obj:align/3
-spec align_to(lv:object(), lv:object(), align_spec(), lv:point()) -> ok | lv:error().
align_to(Obj, RefObj, Spec, Offset) ->
    ?async_void_wrapper(obj_align_to, Obj, RefObj, Spec, Offset).

%% @doc Aligns a widget using a sibling widget as a base.
%%
%% @see align_spec()
%% @see lv_obj:align/2
-spec align_to(lv:object(), lv:object(), align_spec()) -> ok | lv:error().
align_to(Obj, RefObj, Spec) ->
    ?async_void_wrapper(obj_align_to, Obj, RefObj, Spec, {0, 0}).

%% @doc Resizes a widget.
%%
%% @see lv:size()
-spec set_size(lv:object(), lv:size()) -> ok | lv:error().
set_size(Obj, Size) ->
    ?async_void_wrapper(obj_set_size, Obj, Size).

%% @doc Adds a style to an object.
%%
%% @see lv:style()
%% @see lv_style:create/1
-spec add_style(lv:object(), lv:style()) -> ok | lv:error().
add_style(Obj, Style) ->
    ?async_void_wrapper(obj_add_style, Obj, Style, []).

%% @doc Adds a style to an object with a selector.
%%
%% @see lv:style()
%% @see lv_style:create/1
%% @see selector()
-spec add_style(lv:object(), lv:style(), selector()) -> ok | lv:error().
add_style(Obj, Style, Selector) ->
    ?async_void_wrapper(obj_add_style, Obj, Style, Selector).

%% @doc Retrieves the current size of a widget on the screen.
%%
%% @see lv:size()
-spec get_size(lv:object()) -> {ok, lv:size()} | lv:error().
get_size(Obj) ->
    ?async_wrapper(obj_get_size, Obj).

%% @doc Retrives the current position of a widget on the screen.
%%
%% @see lv:point()
-spec get_pos(lv:object()) -> {ok, lv:point()} | lv:error().
get_pos(Obj) ->
    ?async_wrapper(obj_get_pos, Obj).

%% @doc Move the object to the foreground.
%%
%% @see move_to_index/2
-spec move_foreground(lv:object()) -> ok | lv:error().
move_foreground(Obj) ->
    ?async_void_wrapper(obj_move_down, Obj).

%% @doc Move the object to the background.
%%
%% @see move_to_index/2
-spec move_background(lv:object()) -> ok | lv:error().
move_background(Obj) ->
    ?async_void_wrapper(obj_move_background, Obj).

-type index() :: integer().
%% Position in a list of children widgets. Can be specified as a negative
%% number to count backwards from the last child.

%% @doc Changes the relative position of an object within its parent.
%%
%% @see index()
%% @see get_index/1
-spec move_to_index(lv:object(), index()) -> ok | lv:error().
move_to_index(Obj, Index) ->
    ?async_void_wrapper(obj_move_to_index, Obj, Index).

%% @doc Returns an object's relative position amongst its siblings.
%%
%% @see index()
-spec get_index(lv:object()) -> {ok, index()} | lv:error().
get_index(Obj) ->
    ?async_wrapper(obj_get_index, Obj).

%% @doc Gets the number of children within a widget.
-spec get_child_cnt(lv:object()) -> {ok, integer()} | lv:error().
get_child_cnt(Obj) ->
    ?async_wrapper(obj_get_child_cnt, Obj).

%% @doc Retrieves a child of this widget at the specified index.
%%
%% @see index()
-spec get_child(lv:object(), index()) -> {ok, lv:object()} | lv:error().
get_child(Obj, Index) ->
    ?async_wrapper(obj_get_child, Obj, Index).

%% @doc Retrieves the parent of this widget.
-spec get_parent(lv:object()) -> {ok, lv:object() | null} | lv:error().
get_parent(Obj) ->
    ?async_wrapper(obj_get_parent, Obj).

%% @doc Retrieves the screen that this object is placed on.
%%
%% @see lv_scr
-spec get_screen(lv:object()) -> {ok, lv:scr()} | lv:error().
get_screen(Obj) ->
    ?async_wrapper(obj_get_screen, Obj).

%% @doc Deletes all children of a widget.
%%
%% Note that this operation can race against other attempts to use handles for
%% this object's children. When using this function, be sure that there can
%% be no concurrent or following attempt to use handles to any children which
%% are being deleted.
%%
%% @see del/1
-spec clean(lv:object()) -> ok | lv:error().
clean(Obj) ->
    ?async_void_wrapper(obj_clean, Obj).

%% @doc Deletes a widget.
%%
%% Note that this operation can race against other attempts to use the same
%% <code>lv:object()</code> or its children. When using this function, be sure
%% that there can be no concurrent or following attempt to use the handle.
-spec del(lv:object()) -> ok | lv:error().
del(Obj) ->
    ?async_void_wrapper(obj_del, Obj).

%% @doc Swaps the positions of two objects.
%%
%% When used in listboxes, it can be used to sort the listbox items.
-spec swap(lv:object(), lv:object()) -> ok | lv:error().
swap(Obj, OtherObj) ->
    ?async_void_wrapper(obj_swap, Obj, OtherObj).

%% @doc Move the parent of an object.
%%
%% The relative coordinates will be kept.
-spec set_parent(lv:object(), lv:object()) -> ok | lv:error().
set_parent(Obj, NewParent) ->
    ?async_void_wrapper(obj_set_parent, Obj, NewParent).

%% @doc Sets the 'width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, width, Value, []).
%% @doc Sets the 'width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, width, Value, Selector).
%% @doc Sets the 'min_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_min_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_min_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, min_width, Value, []).
%% @doc Sets the 'min_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_min_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_min_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, min_width, Value, Selector).
%% @doc Sets the 'max_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_max_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_max_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, max_width, Value, []).
%% @doc Sets the 'max_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_max_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_max_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, max_width, Value, Selector).
%% @doc Sets the 'height' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_height(lv:object(), lv:coord()) -> ok | lv:error().
set_style_height(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, height, Value, []).
%% @doc Sets the 'height' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_height(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_height(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, height, Value, Selector).
%% @doc Sets the 'min_height' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_min_height(lv:object(), lv:coord()) -> ok | lv:error().
set_style_min_height(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, min_height, Value, []).
%% @doc Sets the 'min_height' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_min_height(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_min_height(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, min_height, Value, Selector).
%% @doc Sets the 'max_height' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_max_height(lv:object(), lv:coord()) -> ok | lv:error().
set_style_max_height(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, max_height, Value, []).
%% @doc Sets the 'max_height' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_max_height(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_max_height(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, max_height, Value, Selector).
%% @doc Sets the 'x' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_x(lv:object(), lv:coord()) -> ok | lv:error().
set_style_x(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, x, Value, []).
%% @doc Sets the 'x' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_x(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_x(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, x, Value, Selector).
%% @doc Sets the 'y' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_y(lv:object(), lv:coord()) -> ok | lv:error().
set_style_y(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, y, Value, []).
%% @doc Sets the 'y' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_y(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_y(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, y, Value, Selector).
%% @doc Sets the 'align' style property as a local style on this object.
%% @see lv_obj:align_spec()
-spec set_style_align(lv:object(), lv_obj:align_spec()) -> ok | lv:error().
set_style_align(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, align, Value, []).
%% @doc Sets the 'align' style property as a local style on this object.
%% @see lv_obj:align_spec()
%% @see selector()
-spec set_style_align(lv:object(), lv_obj:align_spec(), selector()) -> ok | lv:error().
set_style_align(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, align, Value, Selector).
%% @doc Sets the 'radius' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_radius(lv:object(), lv:coord()) -> ok | lv:error().
set_style_radius(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, radius, Value, []).
%% @doc Sets the 'radius' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_radius(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_radius(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, radius, Value, Selector).
%% @doc Sets the 'pad_top' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_top(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_top(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_top, Value, []).
%% @doc Sets the 'pad_top' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_top(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_top(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_top, Value, Selector).
%% @doc Sets the 'pad_bottom' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_bottom(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_bottom(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_bottom, Value, []).
%% @doc Sets the 'pad_bottom' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_bottom(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_bottom(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_bottom, Value, Selector).
%% @doc Sets the 'pad_left' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_left(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_left(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_left, Value, []).
%% @doc Sets the 'pad_left' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_left(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_left(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_left, Value, Selector).
%% @doc Sets the 'pad_right' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_right(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_right(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_right, Value, []).
%% @doc Sets the 'pad_right' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_right(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_right(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_right, Value, Selector).
%% @doc Sets the 'pad_row' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_row(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_row(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_row, Value, []).
%% @doc Sets the 'pad_row' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_row(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_row(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_row, Value, Selector).
%% @doc Sets the 'pad_column' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_pad_column(lv:object(), lv:coord()) -> ok | lv:error().
set_style_pad_column(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_column, Value, []).
%% @doc Sets the 'pad_column' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_pad_column(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_pad_column(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, pad_column, Value, Selector).
%% @doc Sets the 'base_dir' style property as a local style on this object.
%% @see lv_obj:dir_spec()
-spec set_style_base_dir(lv:object(), lv_obj:dir_spec()) -> ok | lv:error().
set_style_base_dir(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, base_dir, Value, []).
%% @doc Sets the 'base_dir' style property as a local style on this object.
%% @see lv_obj:dir_spec()
%% @see selector()
-spec set_style_base_dir(lv:object(), lv_obj:dir_spec(), selector()) -> ok | lv:error().
set_style_base_dir(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, base_dir, Value, Selector).
%% @doc Sets the 'clip_corner' style property as a local style on this object.
-spec set_style_clip_corner(lv:object(), boolean()) -> ok | lv:error().
set_style_clip_corner(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, clip_corner, Value, []).
%% @doc Sets the 'clip_corner' style property as a local style on this object.
%% @see selector()
-spec set_style_clip_corner(lv:object(), boolean(), selector()) -> ok | lv:error().
set_style_clip_corner(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, clip_corner, Value, Selector).
%% @doc Sets the 'bg_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_bg_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_bg_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_color, Value, []).
%% @doc Sets the 'bg_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_bg_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_bg_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_color, Value, Selector).
%% @doc Sets the 'bg_opa' style property as a local style on this object.
-spec set_style_bg_opa(lv:object(), integer()) -> ok | lv:error().
set_style_bg_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_opa, Value, []).
%% @doc Sets the 'bg_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_bg_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_bg_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_opa, Value, Selector).
%% @doc Sets the 'bg_grad_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_bg_grad_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_bg_grad_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_color, Value, []).
%% @doc Sets the 'bg_grad_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_bg_grad_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_bg_grad_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_color, Value, Selector).
%% @doc Sets the 'bg_grad_dir' style property as a local style on this object.
%% @see lv_color:grad_dir()
-spec set_style_bg_grad_dir(lv:object(), lv_color:grad_dir()) -> ok | lv:error().
set_style_bg_grad_dir(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_dir, Value, []).
%% @doc Sets the 'bg_grad_dir' style property as a local style on this object.
%% @see lv_color:grad_dir()
%% @see selector()
-spec set_style_bg_grad_dir(lv:object(), lv_color:grad_dir(), selector()) -> ok | lv:error().
set_style_bg_grad_dir(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_dir, Value, Selector).
%% @doc Sets the 'bg_main_stop' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_bg_main_stop(lv:object(), lv:coord()) -> ok | lv:error().
set_style_bg_main_stop(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_main_stop, Value, []).
%% @doc Sets the 'bg_main_stop' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_bg_main_stop(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_bg_main_stop(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_main_stop, Value, Selector).
%% @doc Sets the 'bg_grad_stop' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_bg_grad_stop(lv:object(), lv:coord()) -> ok | lv:error().
set_style_bg_grad_stop(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_stop, Value, []).
%% @doc Sets the 'bg_grad_stop' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_bg_grad_stop(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_bg_grad_stop(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_grad_stop, Value, Selector).
%% @doc Sets the 'bg_img_opa' style property as a local style on this object.
-spec set_style_bg_img_opa(lv:object(), integer()) -> ok | lv:error().
set_style_bg_img_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_opa, Value, []).
%% @doc Sets the 'bg_img_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_bg_img_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_bg_img_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_opa, Value, Selector).
%% @doc Sets the 'bg_img_recolor' style property as a local style on this object.
%% @see lv:color()
-spec set_style_bg_img_recolor(lv:object(), lv:color()) -> ok | lv:error().
set_style_bg_img_recolor(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_recolor, Value, []).
%% @doc Sets the 'bg_img_recolor' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_bg_img_recolor(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_bg_img_recolor(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_recolor, Value, Selector).
%% @doc Sets the 'bg_img_recolor_opa' style property as a local style on this object.
-spec set_style_bg_img_recolor_opa(lv:object(), integer()) -> ok | lv:error().
set_style_bg_img_recolor_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_recolor_opa, Value, []).
%% @doc Sets the 'bg_img_recolor_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_bg_img_recolor_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_bg_img_recolor_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_recolor_opa, Value, Selector).
%% @doc Sets the 'bg_img_tiled' style property as a local style on this object.
-spec set_style_bg_img_tiled(lv:object(), boolean()) -> ok | lv:error().
set_style_bg_img_tiled(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_tiled, Value, []).
%% @doc Sets the 'bg_img_tiled' style property as a local style on this object.
%% @see selector()
-spec set_style_bg_img_tiled(lv:object(), boolean(), selector()) -> ok | lv:error().
set_style_bg_img_tiled(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, bg_img_tiled, Value, Selector).
%% @doc Sets the 'border_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_border_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_border_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_color, Value, []).
%% @doc Sets the 'border_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_border_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_border_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_color, Value, Selector).
%% @doc Sets the 'border_opa' style property as a local style on this object.
-spec set_style_border_opa(lv:object(), integer()) -> ok | lv:error().
set_style_border_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_opa, Value, []).
%% @doc Sets the 'border_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_border_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_border_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_opa, Value, Selector).
%% @doc Sets the 'border_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_border_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_border_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_width, Value, []).
%% @doc Sets the 'border_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_border_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_border_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_width, Value, Selector).
%% @doc Sets the 'border_side' style property as a local style on this object.
%% @see lv_style:border_side()
-spec set_style_border_side(lv:object(), lv:flags(lv_style:border_side())) -> ok | lv:error().
set_style_border_side(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_side, Value, []).
%% @doc Sets the 'border_side' style property as a local style on this object.
%% @see lv_style:border_side()
%% @see selector()
-spec set_style_border_side(lv:object(), lv:flags(lv_style:border_side()), selector()) -> ok | lv:error().
set_style_border_side(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_side, Value, Selector).
%% @doc Sets the 'border_post' style property as a local style on this object.
-spec set_style_border_post(lv:object(), boolean()) -> ok | lv:error().
set_style_border_post(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_post, Value, []).
%% @doc Sets the 'border_post' style property as a local style on this object.
%% @see selector()
-spec set_style_border_post(lv:object(), boolean(), selector()) -> ok | lv:error().
set_style_border_post(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, border_post, Value, Selector).
%% @doc Sets the 'outline_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_outline_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_outline_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_width, Value, []).
%% @doc Sets the 'outline_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_outline_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_outline_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_width, Value, Selector).
%% @doc Sets the 'outline_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_outline_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_outline_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_color, Value, []).
%% @doc Sets the 'outline_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_outline_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_outline_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_color, Value, Selector).
%% @doc Sets the 'outline_opa' style property as a local style on this object.
-spec set_style_outline_opa(lv:object(), integer()) -> ok | lv:error().
set_style_outline_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_opa, Value, []).
%% @doc Sets the 'outline_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_outline_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_outline_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_opa, Value, Selector).
%% @doc Sets the 'outline_pad' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_outline_pad(lv:object(), lv:coord()) -> ok | lv:error().
set_style_outline_pad(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_pad, Value, []).
%% @doc Sets the 'outline_pad' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_outline_pad(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_outline_pad(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, outline_pad, Value, Selector).
%% @doc Sets the 'shadow_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_shadow_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_shadow_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_width, Value, []).
%% @doc Sets the 'shadow_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_shadow_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_shadow_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_width, Value, Selector).
%% @doc Sets the 'shadow_ofs_x' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_shadow_ofs_x(lv:object(), lv:coord()) -> ok | lv:error().
set_style_shadow_ofs_x(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_ofs_x, Value, []).
%% @doc Sets the 'shadow_ofs_x' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_shadow_ofs_x(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_shadow_ofs_x(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_ofs_x, Value, Selector).
%% @doc Sets the 'shadow_ofs_y' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_shadow_ofs_y(lv:object(), lv:coord()) -> ok | lv:error().
set_style_shadow_ofs_y(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_ofs_y, Value, []).
%% @doc Sets the 'shadow_ofs_y' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_shadow_ofs_y(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_shadow_ofs_y(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_ofs_y, Value, Selector).
%% @doc Sets the 'shadow_spread' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_shadow_spread(lv:object(), lv:coord()) -> ok | lv:error().
set_style_shadow_spread(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_spread, Value, []).
%% @doc Sets the 'shadow_spread' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_shadow_spread(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_shadow_spread(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_spread, Value, Selector).
%% @doc Sets the 'shadow_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_shadow_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_shadow_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_color, Value, []).
%% @doc Sets the 'shadow_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_shadow_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_shadow_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_color, Value, Selector).
%% @doc Sets the 'shadow_opa' style property as a local style on this object.
-spec set_style_shadow_opa(lv:object(), integer()) -> ok | lv:error().
set_style_shadow_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_opa, Value, []).
%% @doc Sets the 'shadow_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_shadow_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_shadow_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, shadow_opa, Value, Selector).
%% @doc Sets the 'img_opa' style property as a local style on this object.
-spec set_style_img_opa(lv:object(), integer()) -> ok | lv:error().
set_style_img_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_opa, Value, []).
%% @doc Sets the 'img_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_img_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_img_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_opa, Value, Selector).
%% @doc Sets the 'img_recolor' style property as a local style on this object.
%% @see lv:color()
-spec set_style_img_recolor(lv:object(), lv:color()) -> ok | lv:error().
set_style_img_recolor(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_recolor, Value, []).
%% @doc Sets the 'img_recolor' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_img_recolor(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_img_recolor(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_recolor, Value, Selector).
%% @doc Sets the 'img_recolor_opa' style property as a local style on this object.
-spec set_style_img_recolor_opa(lv:object(), integer()) -> ok | lv:error().
set_style_img_recolor_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_recolor_opa, Value, []).
%% @doc Sets the 'img_recolor_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_img_recolor_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_img_recolor_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, img_recolor_opa, Value, Selector).
%% @doc Sets the 'line_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_line_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_line_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_width, Value, []).
%% @doc Sets the 'line_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_line_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_line_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_width, Value, Selector).
%% @doc Sets the 'line_dash_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_line_dash_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_line_dash_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_dash_width, Value, []).
%% @doc Sets the 'line_dash_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_line_dash_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_line_dash_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_dash_width, Value, Selector).
%% @doc Sets the 'line_dash_gap' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_line_dash_gap(lv:object(), lv:coord()) -> ok | lv:error().
set_style_line_dash_gap(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_dash_gap, Value, []).
%% @doc Sets the 'line_dash_gap' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_line_dash_gap(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_line_dash_gap(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_dash_gap, Value, Selector).
%% @doc Sets the 'line_rounded' style property as a local style on this object.
-spec set_style_line_rounded(lv:object(), boolean()) -> ok | lv:error().
set_style_line_rounded(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_rounded, Value, []).
%% @doc Sets the 'line_rounded' style property as a local style on this object.
%% @see selector()
-spec set_style_line_rounded(lv:object(), boolean(), selector()) -> ok | lv:error().
set_style_line_rounded(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_rounded, Value, Selector).
%% @doc Sets the 'line_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_line_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_line_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_color, Value, []).
%% @doc Sets the 'line_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_line_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_line_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_color, Value, Selector).
%% @doc Sets the 'line_opa' style property as a local style on this object.
-spec set_style_line_opa(lv:object(), integer()) -> ok | lv:error().
set_style_line_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_opa, Value, []).
%% @doc Sets the 'line_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_line_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_line_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, line_opa, Value, Selector).
%% @doc Sets the 'arc_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_arc_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_arc_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_width, Value, []).
%% @doc Sets the 'arc_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_arc_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_arc_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_width, Value, Selector).
%% @doc Sets the 'arc_rounded' style property as a local style on this object.
-spec set_style_arc_rounded(lv:object(), boolean()) -> ok | lv:error().
set_style_arc_rounded(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_rounded, Value, []).
%% @doc Sets the 'arc_rounded' style property as a local style on this object.
%% @see selector()
-spec set_style_arc_rounded(lv:object(), boolean(), selector()) -> ok | lv:error().
set_style_arc_rounded(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_rounded, Value, Selector).
%% @doc Sets the 'arc_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_arc_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_arc_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_color, Value, []).
%% @doc Sets the 'arc_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_arc_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_arc_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_color, Value, Selector).
%% @doc Sets the 'arc_opa' style property as a local style on this object.
-spec set_style_arc_opa(lv:object(), integer()) -> ok | lv:error().
set_style_arc_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_opa, Value, []).
%% @doc Sets the 'arc_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_arc_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_arc_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, arc_opa, Value, Selector).
%% @doc Sets the 'text_color' style property as a local style on this object.
%% @see lv:color()
-spec set_style_text_color(lv:object(), lv:color()) -> ok | lv:error().
set_style_text_color(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_color, Value, []).
%% @doc Sets the 'text_color' style property as a local style on this object.
%% @see lv:color()
%% @see selector()
-spec set_style_text_color(lv:object(), lv:color(), selector()) -> ok | lv:error().
set_style_text_color(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_color, Value, Selector).
%% @doc Sets the 'text_opa' style property as a local style on this object.
-spec set_style_text_opa(lv:object(), integer()) -> ok | lv:error().
set_style_text_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_opa, Value, []).
%% @doc Sets the 'text_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_text_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_text_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_opa, Value, Selector).
%% @doc Sets the 'text_letter_space' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_text_letter_space(lv:object(), lv:coord()) -> ok | lv:error().
set_style_text_letter_space(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_letter_space, Value, []).
%% @doc Sets the 'text_letter_space' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_text_letter_space(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_text_letter_space(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_letter_space, Value, Selector).
%% @doc Sets the 'text_line_space' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_text_line_space(lv:object(), lv:coord()) -> ok | lv:error().
set_style_text_line_space(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_line_space, Value, []).
%% @doc Sets the 'text_line_space' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_text_line_space(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_text_line_space(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_line_space, Value, Selector).
%% @doc Sets the 'text_decor' style property as a local style on this object.
%% @see lv_style:text_decor()
-spec set_style_text_decor(lv:object(), lv:flags(lv_style:text_decor())) -> ok | lv:error().
set_style_text_decor(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_decor, Value, []).
%% @doc Sets the 'text_decor' style property as a local style on this object.
%% @see lv_style:text_decor()
%% @see selector()
-spec set_style_text_decor(lv:object(), lv:flags(lv_style:text_decor()), selector()) -> ok | lv:error().
set_style_text_decor(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_decor, Value, Selector).
%% @doc Sets the 'text_align' style property as a local style on this object.
%% @see lv_style:text_align()
-spec set_style_text_align(lv:object(), lv_style:text_align()) -> ok | lv:error().
set_style_text_align(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_align, Value, []).
%% @doc Sets the 'text_align' style property as a local style on this object.
%% @see lv_style:text_align()
%% @see selector()
-spec set_style_text_align(lv:object(), lv_style:text_align(), selector()) -> ok | lv:error().
set_style_text_align(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_align, Value, Selector).
%% @doc Sets the 'opa' style property as a local style on this object.
-spec set_style_opa(lv:object(), integer()) -> ok | lv:error().
set_style_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, opa, Value, []).
%% @doc Sets the 'opa' style property as a local style on this object.
%% @see selector()
-spec set_style_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, opa, Value, Selector).
%% @doc Sets the 'color_filter_opa' style property as a local style on this object.
-spec set_style_color_filter_opa(lv:object(), integer()) -> ok | lv:error().
set_style_color_filter_opa(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, color_filter_opa, Value, []).
%% @doc Sets the 'color_filter_opa' style property as a local style on this object.
%% @see selector()
-spec set_style_color_filter_opa(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_color_filter_opa(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, color_filter_opa, Value, Selector).
%% @doc Sets the 'anim_time' style property as a local style on this object.
-spec set_style_anim_time(lv:object(), integer()) -> ok | lv:error().
set_style_anim_time(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, anim_time, Value, []).
%% @doc Sets the 'anim_time' style property as a local style on this object.
%% @see selector()
-spec set_style_anim_time(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_anim_time(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, anim_time, Value, Selector).
%% @doc Sets the 'anim_speed' style property as a local style on this object.
-spec set_style_anim_speed(lv:object(), integer()) -> ok | lv:error().
set_style_anim_speed(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, anim_speed, Value, []).
%% @doc Sets the 'anim_speed' style property as a local style on this object.
%% @see selector()
-spec set_style_anim_speed(lv:object(), integer(), selector()) -> ok | lv:error().
set_style_anim_speed(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, anim_speed, Value, Selector).
%% @doc Sets the 'blend_mode' style property as a local style on this object.
%% @see lv_style:blend_mode()
-spec set_style_blend_mode(lv:object(), lv_style:blend_mode()) -> ok | lv:error().
set_style_blend_mode(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, blend_mode, Value, []).
%% @doc Sets the 'blend_mode' style property as a local style on this object.
%% @see lv_style:blend_mode()
%% @see selector()
-spec set_style_blend_mode(lv:object(), lv_style:blend_mode(), selector()) -> ok | lv:error().
set_style_blend_mode(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, blend_mode, Value, Selector).
%% @doc Sets the 'transform_width' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_width(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_width(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_width, Value, []).
%% @doc Sets the 'transform_width' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_width(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_width(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_width, Value, Selector).
%% @doc Sets the 'transform_height' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_height(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_height(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_height, Value, []).
%% @doc Sets the 'transform_height' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_height(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_height(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_height, Value, Selector).
%% @doc Sets the 'translate_x' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_translate_x(lv:object(), lv:coord()) -> ok | lv:error().
set_style_translate_x(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, translate_x, Value, []).
%% @doc Sets the 'translate_x' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_translate_x(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_translate_x(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, translate_x, Value, Selector).
%% @doc Sets the 'translate_y' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_translate_y(lv:object(), lv:coord()) -> ok | lv:error().
set_style_translate_y(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, translate_y, Value, []).
%% @doc Sets the 'translate_y' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_translate_y(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_translate_y(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, translate_y, Value, Selector).
%% @doc Sets the 'transform_zoom' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_zoom(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_zoom(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_zoom, Value, []).
%% @doc Sets the 'transform_zoom' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_zoom(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_zoom(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_zoom, Value, Selector).
%% @doc Sets the 'transform_angle' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_angle(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_angle(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_angle, Value, []).
%% @doc Sets the 'transform_angle' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_angle(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_angle(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_angle, Value, Selector).
%% @doc Sets the 'transform_pivot_x' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_pivot_x(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_pivot_x(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_pivot_x, Value, []).
%% @doc Sets the 'transform_pivot_x' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_pivot_x(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_pivot_x(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_pivot_x, Value, Selector).
%% @doc Sets the 'transform_pivot_y' style property as a local style on this object.
%% @see lv:coord()
-spec set_style_transform_pivot_y(lv:object(), lv:coord()) -> ok | lv:error().
set_style_transform_pivot_y(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_pivot_y, Value, []).
%% @doc Sets the 'transform_pivot_y' style property as a local style on this object.
%% @see lv:coord()
%% @see selector()
-spec set_style_transform_pivot_y(lv:object(), lv:coord(), selector()) -> ok | lv:error().
set_style_transform_pivot_y(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, transform_pivot_y, Value, Selector).
%% @doc Sets the 'text_font' style property as a local style on this object.
%% @see lv:font()
-spec set_style_text_font(lv:object(), lv:font()) -> ok | lv:error().
set_style_text_font(Obj, Value) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_font, Value, []).
%% @doc Sets the 'text_font' style property as a local style on this object.
%% @see lv:font()
%% @see selector()
-spec set_style_text_font(lv:object(), lv:font(), selector()) -> ok | lv:error().
set_style_text_font(Obj, Value, Selector) -> ?async_void_wrapper(obj_set_local_style_prop, Obj, text_font, Value, Selector).
