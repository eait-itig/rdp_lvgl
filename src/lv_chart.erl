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

%% @doc The <code>lv_chart</code> widget, a data visualisation tool.
%%
%% See [https://docs.lvgl.io/master/widgets/chart.html]
-module(lv_chart).

-export([
    create/1,
    set_type/2,
    set_point_count/2,
    set_range/4,
    set_update_mode/2,
    set_div_line_count/3,
    set_zoom_x/2,
    set_zoom_y/2,
    add_series/3,
    remove_series/2,
    hide_series/3,
    set_series_color/3,
    get_series_next/2,
    set_all_value/3,
    set_next_value/3,
    set_next_value2/4,
    set_axis_tick/8
    ]).

-export_type([
    type/0, axis/0, update_mode/0, series/0, cursor/0
    ]).

-include("async_wrappers.hrl").

-type type() :: none | line | bar | scatter.
-type axis() :: primary_y | secondary_y | primary_x | secondary_x.
-type update_mode() :: shift | circular.
-type value() :: none | integer().

-type zoom() :: float().
%% Zoom factor. 1.0 is 1:1 scaled against the data (no zoom). &lt;1.0 shrinks
%% the data, while &gt;1.0 enlarges it.

-opaque series() :: reference().
-opaque cursor() :: reference().

%% @doc Creates a new lv_chart widget.
-spec create(lv:object()) -> {ok, lv:chart()} | lv:error().
create(Parent) ->
    ?async_wrapper(chart_create, Parent).

-spec set_type(lv:chart(), type()) -> ok | lv:error().
set_type(Chart, Type) ->
    ?async_void_wrapper(chart_set_type, Chart, Type).

-spec set_point_count(lv:chart(), integer()) -> ok | lv:error().
set_point_count(Chart, Count) ->
    ?async_void_wrapper(chart_set_point_count, Chart, Count).

-spec set_range(lv:chart(), axis(), value(), value()) -> ok | lv:error().
set_range(Chart, Axis, Min, Max) ->
    ?async_void_wrapper(chart_set_range, Chart, Axis, Min, Max).

-spec set_update_mode(lv:chart(), update_mode()) -> ok | lv:error().
set_update_mode(Chart, Mode) ->
    ?async_void_wrapper(chart_set_update_mode, Chart, Mode).

-spec set_div_line_count(lv:chart(), integer(), integer()) -> ok | lv:error().
set_div_line_count(Chart, HorizonalDivs, VerticalDivs) ->
    ?async_void_wrapper(chart_set_div_line_count, Chart, HorizonalDivs,
        VerticalDivs).

-spec set_axis_tick(lv:chart(), axis(), lv:coord(), lv:coord(), integer(),
    integer(), boolean(), lv:coord()) -> ok | lv:error().
set_axis_tick(Chart, Axis, MajorLen, MinorLen, MajorCnt, MinorCnt, LabelEn, DrawSize) ->
    ?async_void_wrapper(chart_set_axis_tick, Chart, Axis, MajorLen, MinorLen,
        MajorCnt, MinorCnt, LabelEn, DrawSize).

-spec set_zoom_x(lv:chart(), zoom()) -> ok | lv:error().
set_zoom_x(Chart, ZoomFactor) ->
    Zoom0 = round(ZoomFactor * 256),
    Zoom1 = if
        (Zoom0 < 1) -> 1;
        (Zoom0 > (1 bsl 15)) -> (1 bsl 15);
        true -> Zoom0
    end,
    ?async_void_wrapper(chart_set_zoom_x, Chart, Zoom1).

-spec set_zoom_y(lv:chart(), zoom()) -> ok | lv:error().
set_zoom_y(Chart, ZoomFactor) ->
    Zoom0 = round(ZoomFactor * 256),
    Zoom1 = if
        (Zoom0 < 1) -> 1;
        (Zoom0 > (1 bsl 15)) -> (1 bsl 15);
        true -> Zoom0
    end,
    ?async_void_wrapper(chart_set_zoom_y, Chart, Zoom1).

-spec add_series(lv:chart(), lv:color(), axis()) -> {ok, series()} | lv:error().
add_series(Chart, Color, Axis) ->
    ?async_wrapper(chart_add_series, Chart, Color, Axis).

-spec remove_series(lv:chart(), series()) -> ok | lv:error().
remove_series(Chart, Series) ->
    ?async_void_wrapper(chart_remove_series, Chart, Series).

-spec hide_series(lv:chart(), series(), boolean()) -> ok | lv:error().
hide_series(Chart, Series, Hide) ->
    ?async_void_wrapper(chart_hide_series, Chart, Series, Hide).

-spec set_series_color(lv:chart(), series(), lv:color()) -> ok | lv:error().
set_series_color(Chart, Series, Color) ->
    ?async_void_wrapper(chart_set_series_color, Chart, Series, Color).

-spec get_series_next(lv:chart(), series()) -> {ok, series() | null} | lv:error().
get_series_next(Chart, Series) ->
    ?async_wrapper(chart_get_series_next, Chart, Series).

-spec set_all_value(lv:chart(), series(), value()) -> ok | lv:error().
set_all_value(Chart, Series, Y) ->
    ?async_void_wrapper(chart_set_all_value, Chart, Series, Y).

-spec set_next_value(lv:chart(), series(), value()) -> ok | lv:error().
set_next_value(Chart, Series, Y) ->
    ?async_void_wrapper(chart_set_next_value, Chart, Series, Y).

-spec set_next_value2(lv:chart(), series(), value(), value()) -> ok | lv:error().
set_next_value2(Chart, Series, X, Y) ->
    ?async_void_wrapper(chart_set_next_value2, Chart, Series, X, Y).
