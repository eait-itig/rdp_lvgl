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

%% @private
-module(rdp_lvgl_nif).

-compile(export_all).
-compile(nowarn_export_all).

-on_load(init/0).

-export_type([
    instance/0, buffer/0, object/0, event/0, msgref/0, group/0,
    instance_msg/0, event_msg/0, style/0
    ]).

try_paths([Last], BaseName) ->
    filename:join([Last, BaseName]);
try_paths([Path | Next], BaseName) ->
    case filelib:is_dir(Path) of
        true ->
            WCard = filename:join([Path, "{lib,}" ++ BaseName ++ ".*"]),
            case filelib:wildcard(WCard) of
                [] -> try_paths(Next, BaseName);
                _ -> filename:join([Path, BaseName])
            end;
        false -> try_paths(Next, BaseName)
    end.

init() ->
    Paths0 = [
        filename:join(["..", lib, pcsc, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(pcsc) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    SoName = try_paths(Paths1, "rdp_lvgl_nif"),
    erlang:load_nif(SoName, 0).

-type instance() :: reference().
-type buffer() :: reference().
-type object() :: reference().
-type event() :: reference().
-type style() :: reference().
-type group() :: reference().

-type msgref() :: reference().

-type pixeldata() :: iolist().

-type msec() :: integer().
-type degrees() :: integer().

-type color() :: {R :: integer(), G :: integer(), B :: integer()}.

-spec setup_instance(lv:size()) ->
    {ok, instance(), msgref()} | lv:error().
setup_instance(_Size) -> error(no_nif).

-type async_return_of(_MsgType) :: {async, msgref()} | lv:error().

-type async_return() :: async_return_of(
    {msgref(), error, integer(), string()} |
    {msgref(), ok}).

-type async_return(T) :: async_return_of(
    {msgref(), error, integer(), string()} |
    {msgref(), ok, T}).

-type event_msg() ::
    {msgref(), event, lv_event:type(), Target :: lv:object(), CurTarget :: lv:object()}.

-spec setup_event(object(), lv_event:type()) -> {async, event(), msgref()} | lv:error().
setup_event(_Obj, _Type) -> error(no_nif).

-spec set_kbd_group(instance(), group()) -> async_return().
set_kbd_group(_Inst, _Group) -> error(no_nif).

-type instance_msg() ::
    {msgref(), setup_done} |
    {msgref(), flush, lv:rect(), pixeldata()} |
    {msgref(), flush_sync}.

-spec prefork(integer()) -> ok.
prefork(_N) -> error(no_nif).

-spec flush_done(instance()) -> ok | {error, term()}.
flush_done(_Inst) -> error(no_nif).

-spec make_buffer(binary()) -> {ok, buffer()}.
make_buffer(_Data) -> error(no_nif).

-spec read_framebuffer(instance(), lv:rect()) -> {ok, pixeldata()}.
read_framebuffer(_Inst, _Rect) -> error(no_nif).

-spec send_pointer_event(instance(), lv:point(), lv_indev:state()) -> ok | {error, term()}.
send_pointer_event(_Inst, _Pt, _Btn) -> error(no_nif).

-spec send_key_event(instance(), lv_indev:key(), lv_indev:state()) -> ok | {error, term()}.
send_key_event(_Inst, _Key, _Btn) -> error(no_nif).

-spec send_text_event(instance(), buffer()) -> ok | {error, term()}.
send_text_event(_Inst, _Buf) -> error(no_nif).

-spec obj_create(instance(), Parent :: object() | none) ->
    async_return(object()).
obj_create(_Inst, _Parent) -> error(no_nif).

-spec obj_align(object(), lv_obj:align_spec(), lv:point()) ->
    async_return().
obj_align(_Obj, _AlignSpec, _Offset) -> error(no_nif).

-spec obj_align(object(), lv_obj:align_spec()) -> async_return().
obj_align(_Obj, _AlignSpec) -> error(no_nif).

-spec obj_align_to(object(), object(), lv_obj:align_spec(), lv:point()) ->
    async_return().
obj_align_to(_Obj, _RefObj, _AlignSpec, _Offset) -> error(no_nif).

-spec obj_align_to(object(), object(), lv_obj:align_spec()) ->
    async_return().
obj_align_to(_Obj, _RefObj, _AlignSpec) -> error(no_nif).

-spec scr_load(instance(), object()) -> async_return().
scr_load(_Inst, _Screen) -> error(no_nif).

-spec disp_set_bg_color(instance(), color()) -> async_return().
disp_set_bg_color(_Inst, _Color) -> error(no_nif).

-spec spinner_create(object(), Time :: msec(), ArcLen :: degrees()) ->
    async_return(object()).
spinner_create(_Obj, _Time, _ArcLen) -> error(no_nif).

-spec obj_set_size(object(), lv:size()) -> async_return().
obj_set_size(_Obj, _Size) -> error(no_nif).

-spec obj_center(object()) -> async_return().
obj_center(_Obj) -> error(no_nif).

-spec img_create(object()) -> async_return(object()).
img_create(_Parent) -> error(no_nif).

-spec img_set_src(object(), lv_img:src()) -> async_return().
img_set_src(_Img, _Src) -> error(no_nif).

-spec img_set_offset(object(), lv:point()) -> async_return().
img_set_offset(_Img, _Offset) -> error(no_nif).

-spec disp_get_layer_sys(instance()) -> async_return(object()).
disp_get_layer_sys(_Inst) -> error(no_nif).

-spec set_mouse_cursor(instance(), object()) -> async_return().
set_mouse_cursor(_Inst, _Img) -> error(no_nif).

-spec btn_create(object()) -> async_return(object()).
btn_create(_Parent) -> error(no_nif).

-spec label_create(object()) -> async_return(object()).
label_create(_Parent) -> error(no_nif).

-spec label_set_text(object(), string()) -> async_return().
label_set_text(_Label, _Text) -> error(no_nif).

-spec style_create(instance()) -> async_return(style()).
style_create(_Inst) -> error(no_nif).

-spec style_set_flex_flow(style(), lv_style:flex_flow()) -> async_return().
style_set_flex_flow(_Style, _Flow) -> error(no_nif).

-spec style_set_flex_align(style(), lv_style:flex_align(), lv_style:flex_align(), lv_style:flex_align()) -> async_return().
style_set_flex_align(_Style, _Main, _Cross, _Tracks) -> error(no_nif).

-type layout() :: flex | grid.
-spec style_set_layout(style(), lv_style:layout()) -> async_return().
style_set_layout(_Style, _Layout) -> error(no_nif).

-spec obj_add_style(object(), style()) -> async_return().
obj_add_style(_Obj, _Style) -> error(no_nif).

-spec obj_add_flags(object(), [lv_obj:flag()]) -> async_return().
obj_add_flags(_Obj, _Flags) -> error(no_nif).

-spec obj_clear_flags(object(), [lv_obj:flag()]) -> async_return().
obj_clear_flags(_Obj, _Flags) -> error(no_nif).

-spec obj_has_all_flags(object(), [lv_obj:flag()]) -> async_return(boolean()).
obj_has_all_flags(_Obj, _Flags) -> error(no_nif).

-spec obj_has_any_flags(object(), [lv_obj:flag()]) -> async_return(boolean()).
obj_has_any_flags(_Obj, _Flags) -> error(no_nif).

-spec style_set_bg_opa(style(), integer()) -> async_return().
style_set_bg_opa(_Style, _Opacity) -> error(no_nif).

-spec textarea_create(object()) -> async_return(object()).
textarea_create(_Parent) -> error(no_nif).

-spec textarea_set_text(object(), string()) -> async_return().
textarea_set_text(_Obj, _Text) -> error(no_nif).

-spec textarea_set_password_mode(object(), boolean()) -> async_return().
textarea_set_password_mode(_Obj, _State) -> error(no_nif).

-spec textarea_set_one_line(object(), boolean()) -> async_return().
textarea_set_one_line(_Obj, _State) -> error(no_nif).

-spec textarea_set_cursor_hidden(object(), boolean()) -> async_return().
textarea_set_cursor_hidden(_Obj, _State) -> error(no_nif).

-spec textarea_set_placeholder_text(object(), string()) -> async_return().
textarea_set_placeholder_text(_Obj, _Text) -> error(no_nif).

-spec textarea_add_text(object(), string()) -> async_return().
textarea_add_text(_Obj, _Text) -> error(no_nif).

-spec textarea_set_accepted_chars(object(), string()) -> async_return().
textarea_set_accepted_chars(_Obj, _Chars) -> error(no_nif).

-spec textarea_set_max_length(object(), integer()) -> async_return().
textarea_set_max_length(_Obj, _MaxLength) -> error(no_nif).

-spec textarea_set_text_sel(object(), boolean()) -> async_return().
textarea_set_text_sel(_Obj, _State) -> error(no_nif).

-spec textarea_get_text(object()) -> async_return(binary()).
textarea_get_text(_Obj) -> error(no_nif).

-spec textarea_clear_selection(object()) -> async_return().
textarea_clear_selection(_Obj) -> error(no_nif).

-spec textarea_set_cursor_click_pos(object(), boolean()) -> async_return().
textarea_set_cursor_click_pos(_Obj, _State) -> error(no_nif).

-spec textarea_set_cursor_pos(object(), integer()) -> async_return().
textarea_set_cursor_pos(_Obj, _MaxLength) -> error(no_nif).

-spec group_create(instance()) -> async_return(group()).
group_create(_Inst) -> error(no_nif).

-spec group_add_obj(group(), object()) -> async_return().
group_add_obj(_Group, _Obj) -> error(no_nif).

-spec group_remove_obj(group(), object()) -> async_return().
group_remove_obj(_Group, _Obj) -> error(no_nif).

-spec group_remove_all_objs(group()) -> async_return().
group_remove_all_objs(_Group) -> error(no_nif).

-spec group_get_focused(group()) -> async_return(object()).
group_get_focused(_Group) -> error(no_nif).

-spec group_set_wrap(group(), boolean()) -> async_return().
group_set_wrap(_Group, _State) -> error(no_nif).

-spec group_focus_obj(object()) -> async_return().
group_focus_obj(_Obj) -> error(no_nif).

-spec group_focus_freeze(group(), boolean()) -> async_return().
group_focus_freeze(_Group, _State) -> error(no_nif).
