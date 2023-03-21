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

-module(lv_img).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    src/0
    ]).

-include("async_wrappers.hrl").

-type file_path() :: string().
-type symbol() :: bullet | audio | video | list | ok | close | power |
    settings | home | download | drive | refresh | mute | volume_mid |
    volume_max | image | tint | prev | play | pause | stop | next | eject |
    left | right | plus | minus | eye_open | eye_close | warning | shuffle |
    up | down | loop | directory | upload | call | cut | copy | save | bars |
    envelope | charge | paste | bell | keyboard | gps | file | wifi |
    battery_full | battery_3 | battery_2 | battery_1 | battery_empty |
    usb | bluetooth | trash | edit | backspace | sd_card | new_line | dummy.
-type src() :: file_path() | symbol() | none.

-type degrees() :: integer().
%% An integer number of degrees in the range 0 - 360.

-type zoom() :: float().
%% Zoom factor. 1.0 is 1:1 scaled against source image (no zoom). &lt;1.0 shrinks
%% the image, while &gt;1.0 enlarges it.

-spec create(lv:object()) -> {ok, lv:img()} | lv:error().
create(Parent) ->
    ?async_wrapper(img_create, Parent).

-spec set_offset(lv:img(), lv:point()) -> ok | lv:error().
set_offset(Img, Offset) ->
    ?async_void_wrapper(img_set_offset, Img, Offset).

-spec set_src(lv:img(), src()) -> ok | lv:error().
set_src(Img, Src) ->
    ?async_void_wrapper(img_set_src, Img, Src).

-spec set_angle(lv:img(), degrees()) -> ok | lv:error().
set_angle(Img, Angle) ->
    ?async_void_wrapper(img_set_angle, Img, Angle).

-spec set_pivot(lv:img(), lv:point()) -> ok | lv:error().
set_pivot(Img, Pivot) ->
    ?async_void_wrapper(img_set_pivot, Img, Pivot).

-spec set_zoom(lv:img(), zoom()) -> ok | lv:error().
set_zoom(Img, ZoomFactor) ->
    Zoom0 = round(ZoomFactor * 256),
    Zoom1 = if
        (Zoom0 < 1) -> 1;
        (Zoom0 > (1 bsl 15)) -> (1 bsl 15);
        true -> Zoom0
    end,
    ?async_void_wrapper(img_set_zoom, Img, Zoom1).

-spec set_antialias(lv:img(), boolean()) -> ok | lv:error().
set_antialias(Img, Ena) ->
    ?async_void_wrapper(img_set_antialias, Img, Ena).
