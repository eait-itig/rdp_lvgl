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

%% @doc Functions related to display management.
%%
%% See [https://docs.lvgl.io/master/overview/display.html]
-module(lv_disp).

-export([
    get_layer_sys/1,
    get_layer_top/1,
    get_scr_act/1,
    get_scr_prev/1,
    set_bg_color/2,
    set_bg_image/2,
    set_bg_opa/2,
    get_inactive_time/1,
    trig_activity/1
    ]).

-include("async_wrappers.hrl").

%% @doc Retrieves a reference to the system layer of an instance's display.
-spec get_layer_sys(lv:instance()) -> {ok, lv:object()} | lv:error().
get_layer_sys(Inst) ->
    ?async_wrapper(disp_get_layer_sys, Inst).

%% @doc Retrieves a reference to the top layer of an instance's display.
-spec get_layer_top(lv:instance()) -> {ok, lv:object()} | lv:error().
get_layer_top(Inst) ->
    ?async_wrapper(disp_get_layer_top, Inst).

%% @doc Retrieves the current active screen.
%%
%% @see lv_scr
-spec get_scr_act(lv:instance()) -> {ok, lv:scr()} | lv:error().
get_scr_act(Inst) ->
    ?async_wrapper(disp_get_scr_act, Inst).

%% @doc Retrieves the previously active screen.
%%
%% @see lv_scr
-spec get_scr_prev(lv:instance()) -> {ok, lv:scr()} | lv:error().
get_scr_prev(Inst) ->
    ?async_wrapper(disp_get_scr_prev, Inst).

%% @doc Sets the display's background color.
%%
%% @see lv_color
-spec set_bg_color(lv:instance(), lv:color()) -> ok | lv:error().
set_bg_color(Inst, Color) ->
    ?async_void_wrapper(disp_set_bg_color, Inst, Color).

%% @doc Sets a background image for the display.
%%
%% @see lv_img:src()
-spec set_bg_image(lv:instance(), lv_img:src()) -> ok | lv:error().
set_bg_image(Inst, ImgSrc) ->
    ?async_void_wrapper(disp_set_bg_image, Inst, ImgSrc).

%% @doc Sets the opacity of the display background.
%%
%% @see lv_color:opacity()
-spec set_bg_opa(lv:instance(), lv_color:opacity()) -> ok | lv:error().
set_bg_opa(Inst, Opacity) ->
    ?async_void_wrapper(disp_set_bg_opa, Inst, Opacity).

-type msec() :: integer().
%% Number of milliseconds.

%% @doc Get elapsed time since last user activity on a display (e.g. click)
%%
%% @see msec()
-spec get_inactive_time(lv:instance()) -> {ok, msec()} | lv:error().
get_inactive_time(Inst) ->
    ?async_wrapper(disp_get_inactive_time, Inst).

%% @doc Manually trigger an activity on a display
%%
%% @see get_inactive_time/1
-spec trig_activity(lv:instance()) -> ok | lv:error().
trig_activity(Inst) ->
    ?async_void_wrapper(disp_trig_activity, Inst).
