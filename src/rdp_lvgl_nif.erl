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
        filename:join(["..", lib, rdp_lvgl, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(rdp_lvgl) of
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

-type style_generic_prop() :: width | min_width | max_width | height |
    min_height | max_height | x | y | align | radius | pad_top | pad_bottom |
    pad_left | pad_right | pad_row | pad_column | base_dir | clip_corner |
    bg_color | bg_opa | bg_grad_color | bg_grad_dir | bg_main_stop |
    bg_grad_stop | bg_img_opa | bg_img_recolor | bg_img_recolor_opa |
    bg_img_tiled | border_color | border_opa | border_width | border_side |
    border_post | outline_width | outline_color | outline_opa | outline_pad |
    shadow_width | shadow_ofs_x | shadow_ofs_y | shadow_spread | shadow_color |
    shadow_opa | img_opa | img_recolor | img_recolor_opa | line_width |
    line_dash_width | line_dash_gap | line_rounded | line_color | line_opa |
    arc_width | arc_rounded | arc_color | arc_opa | text_color | text_opa |
    text_letter_space | text_line_space | text_decor | text_align | opa |
    color_filter_opa | anim_time | anim_speed | blend_mode | transform_width |
    transform_height | translate_x | translate_y | transform_zoom |
    transform_angle | transform_pivot_x | transform_pivot_y.
-type style_generic_val() :: integer() | lv:coord() |
    atom() | boolean() | lv:color().

-type config_prop() :: fbufs_per_child | ring_size | fb_max_res | max_lvkids.
-spec configure(config_prop(), integer() | {integer(), integer()}) -> ok.
configure(_Prop, _Value) -> error(no_nif).

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

-spec take_log_ownership() -> ok.
take_log_ownership() -> error(no_nif).

-spec setup_event(object(), lv_event:type()) -> {async, event(), msgref()} | lv:error().
setup_event(_Obj, _Type) -> error(no_nif).

-spec setup_event(object(), lv_event:type(), term()) -> {async, event(), msgref()} | lv:error().
setup_event(_Obj, _Type, _CustomMsg) -> error(no_nif).

-spec send_text(instance(), string()) -> async_return().
send_text(_Inst, _Text) -> error(no_nif).

-spec send_wheel_event(instance(), integer()) -> ok | {error, term()}.
send_wheel_event(_Inst, _Dy) -> error(no_nif).

-type instance_msg() ::
    {msgref(), setup_done} |
    {msgref(), flush, lv:rect(), pixeldata()} |
    {msgref(), flush_sync}.

-spec prefork(integer()) -> ok.
prefork(_N) -> error(no_nif).

-spec flush_done(instance()) -> ok | {error, term()}.
flush_done(_Inst) -> error(no_nif).

-spec flush_state(instance()) -> idle | flushing | teardown.
flush_state(_Inst) -> error(no_nif).

-spec make_buffer(instance(), binary()) -> async_return(buffer()).
make_buffer(_Inst, _Data) -> error(no_nif).

-type tile() :: {lv:rect(), pixeldata()}.

-spec read_framebuffer(instance(), lv:rect()) -> {ok, [tile()]}.
read_framebuffer(_Inst, _Rect) -> error(no_nif).

-spec send_pointer_event(instance(), lv:point(), lv_indev:state()) -> ok | {error, term()}.
send_pointer_event(_Inst, _Pt, _Btn) -> error(no_nif).

-spec send_key_event(instance(), lv_indev:key(), lv_indev:state()) -> ok | {error, term()}.
send_key_event(_Inst, _Key, _Btn) -> error(no_nif).

-spec send_text_event(instance(), buffer()) -> ok | {error, term()}.
send_text_event(_Inst, _Buf) -> error(no_nif).

-spec obj_get_class(object()) -> atom().
obj_get_class(_Obj) -> error(no_nif).

-spec obj_has_class(object(), atom()) -> boolean().
obj_has_class(_Obj, _Class) -> error(no_nif).

-include("nif_gen.hrl").
