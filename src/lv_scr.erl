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

-module(lv_scr).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([load_anim/0]).

-include("async_wrappers.hrl").

-spec create(lv:instance()) -> {ok, lv:screen()} | lv:error().
create(Inst) ->
    ?async_wrapper(obj_create, Inst, none).

-spec load(lv:instance(), lv:screen()) -> ok | lv:error().
load(Inst, Screen) ->
    ?async_void_wrapper(scr_load, Inst, Screen).

-type load_anim() :: none | over_left | over_right | over_top |
    over_bottom | move_left | move_right | move_top | move_bottom |
    fade_in | fade_out | out_left | out_right | out_top | out_bottom.
-type msec() :: integer().

-spec load_anim(lv:instance(), lv:screen(), load_anim(), msec(),
    msec(), boolean()) -> ok | lv:error().
load_anim(Inst, Screen, Anim, Time, Delay, AutoDel) ->
    ?async_void_wrapper(scr_load_anim, Inst, Screen, Anim, Time, Delay,
        AutoDel).
