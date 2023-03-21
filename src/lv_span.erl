%%
%% RDP UI framework using LVGL
%%
%% Copyright 2023 Alex Wilson <alex@uq.edu.au>, The University of Queensland
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

%% @doc The <code>lv_span</code> widget, for rich text display.
%%
%% See [https://docs.lvgl.io/master/widgets/span.html]
-module(lv_span).

-export([
    create/1,
    new_span/1,
    del_span/2,
    set_text/2,
    set_style/2,
    set_align/2,
    get_child_cnt/1,
    get_child/2,
    set_mode/2,
    set_overflow/2,
    refr_mode/1
    ]).

-export_type([
    span/0, mode/0, overflow_mode/0
    ]).

-include("async_wrappers.hrl").

-opaque span() :: reference().

-type mode() :: fixed | expand | break.
-type overflow_mode() :: clip | ellipsis.

%% @doc Create a new span group widget to contain spans.
-spec create(lv:object()) -> {ok, lv:spangroup()} | lv:error().
create(Parent) ->
    ?async_wrapper(spangroup_create, Parent).

%% @doc Create a new span within a spangroup.
-spec new_span(lv:spangroup()) -> {ok, span()} | lv:error().
new_span(SpanGroup) ->
    ?async_wrapper(spangroup_new_span, SpanGroup).

%% @doc Delete a span.
-spec del_span(lv:spangroup(), span()) -> ok | lv:error().
del_span(SpanGroup, Span) ->
    ?async_void_wrapper(spangroup_del_span, SpanGroup, Span).

%% @doc Set the text content of a span.
-spec set_text(span(), string()) -> ok | lv:error().
set_text(Span, Text) ->
    ?async_void_wrapper(span_set_text, Span, Text).

%% @doc Set style for a span.
-spec set_style(span(), lv:style()) -> ok | lv:error().
set_style(Span, Style) ->
    ?async_void_wrapper(span_set_style, Span, Style).

%% @doc Set overall text alignment for a spangroup.
%%
%% @see lv_style:text_align()
-spec set_align(lv:spangroup(), lv_style:text_align()) -> ok | lv:error().
set_align(SpanGroup, Align) ->
    ?async_void_wrapper(spangroup_set_align, SpanGroup, Align).

%% @doc Count number of children spans within a spangroup.
-spec get_child_cnt(lv:spangroup()) -> {ok, integer()} | lv:error().
get_child_cnt(SpanGroup) ->
    ?async_wrapper(spangroup_get_child_cnt, SpanGroup).

-type index() :: integer().

%% @doc Gets a child span within a spangroup.
%%
%% @see index()
-spec get_child(lv:spangroup(), index()) -> {ok, span()} | lv:error().
get_child(SpanGroup, Index) ->
    ?async_wrapper(spangroup_get_child, SpanGroup, Index).

%% @doc Change the spangroup resizing mode.
%%
%% @see mode()
-spec set_mode(lv:spangroup(), mode()) -> ok | lv:error().
set_mode(SpanGroup, Mode) ->
    ?async_void_wrapper(spangroup_set_mode, SpanGroup, Mode).

%% @doc Change the spangroup overflow mode.
%%
%% @see overflow_mode()
-spec set_overflow(lv:spangroup(), overflow_mode()) -> ok | lv:error().
set_overflow(SpanGroup, Mode) ->
    ?async_void_wrapper(spangroup_set_overflow, SpanGroup, Mode).

%% @doc Refresh mode and text content of a spangroup.
-spec refr_mode(lv:spangroup()) -> ok | lv:error().
refr_mode(SpanGroup) ->
    ?async_void_wrapper(spangroup_refr_mode, SpanGroup).
