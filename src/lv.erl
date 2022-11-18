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

-module(lv).

-export_type([
    object/0, instance/0, event/0, style/0, error/0, rect/0, point/0, size/0,
    color/0,
    btn/0, label/0, scr/0, img/0, spinner/0, textarea/0
	]).

-opaque object() :: rdp_lvgl_nif:object().
-opaque instance() :: rdp_lvgl_nif:instance().
-opaque event() :: rdp_lvgl_nif:event().
-opaque style() :: rdp_lvgl_nif:style().
-opaque group() :: rdp_lvgl_nif:group().

-type error() :: {error, integer(), string()} | {error, term()}.

-type color() :: lv_color:color().

-type px() :: integer().
-type rect() :: {X1 :: px(), Y1 :: px(), X2 :: px(), Y2 :: px()}.
-type point() :: {X :: px(), Y :: px()}.
-type size() :: {Width :: px(), Height :: px()}.

-type btn() :: object().
-type label() :: object().
-type scr() :: object().
-type img() :: object().
-type spinner() :: object().
-type textarea() :: object().
