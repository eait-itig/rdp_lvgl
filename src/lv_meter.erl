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

%% @doc The <code>lv_meter</code> widget, a data visualisation tool.
%%
%% See [https://docs.lvgl.io/master/widgets/meter.html]
-module(lv_meter).

-export([
    create/1,
    set_scale_ticks/6,
    set_scale_major_ticks/7,
    add_needle_line/5,
    add_arc/5,
    set_indicator_value/3,
    set_indicator_start_value/3,
    set_indicator_end_value/3,
    set_scale_range/6,
    add_scale/1
    ]).

-export_type([
    indicator/0,
    scale/0
    ]).

-opaque indicator() :: reference().
-opaque scale() :: reference().

-type px() :: integer().

-type value() :: integer().

-type degrees() :: integer().

-include("async_wrappers.hrl").

%% @doc Creates a new lv_chart widget.
-spec create(lv:object()) -> {ok, lv:meter()} | lv:error().
create(Parent) ->
    ?async_wrapper(meter_create, Parent).

-spec add_scale(lv:meter()) -> {ok, scale()} | lv:error().
add_scale(Meter) ->
    ?async_wrapper(meter_add_scale, Meter).

-spec set_scale_ticks(lv:meter(), scale(), integer(), px(), px(), lv:color())
    -> ok | lv:error().
set_scale_ticks(Meter, Scale, Count, Width, Length, Color) ->
    ?async_void_wrapper(meter_set_scale_ticks, Meter, Scale, Count, Width,
        Length, Color).

-spec set_scale_major_ticks(lv:meter(), scale(), integer(), px(), px(),
    lv:color(), px()) -> ok | lv:error().
set_scale_major_ticks(Meter, Scale, Nth, Width, Length, Color, LabelGap) ->
    ?async_void_wrapper(meter_set_scale_major_ticks, Meter, Scale, Nth, Width,
        Length, Color, LabelGap).

-spec set_scale_range(lv:meter(), scale(), value(), value(), degrees(),
    degrees()) -> ok | lv:error().
set_scale_range(Meter, Scale, Min, Max, AngleRange, Rotation) ->
    ?async_void_wrapper(meter_set_scale_range, Meter, Scale, Min, Max,
        AngleRange, Rotation).

-spec add_needle_line(lv:meter(), scale(), px(), lv:color(), px())
    -> {ok, indicator()} | lv:error().
add_needle_line(Meter, Scale, Width, Color, RadiusMod) ->
    ?async_wrapper(meter_add_needle_line, Meter, Scale, Width, Color, RadiusMod).

-spec add_arc(lv:meter(), scale(), px(), lv:color(), px())
    -> {ok, indicator()} | lv:error().
add_arc(Meter, Scale, Width, Color, RadiusMod) ->
    ?async_wrapper(meter_add_arc, Meter, Scale, Width, Color, RadiusMod).

-spec set_indicator_value(lv:meter(), indicator(), value()) -> ok | lv:error().
set_indicator_value(Meter, Indicator, Value) ->
    ?async_void_wrapper(meter_set_indicator_value, Meter, Indicator, Value).

-spec set_indicator_start_value(lv:meter(), indicator(), value()) -> ok | lv:error().
set_indicator_start_value(Meter, Indicator, Value) ->
    ?async_void_wrapper(meter_set_indicator_start_value, Meter, Indicator, Value).

-spec set_indicator_end_value(lv:meter(), indicator(), value()) -> ok | lv:error().
set_indicator_end_value(Meter, Indicator, Value) ->
    ?async_void_wrapper(meter_set_indicator_end_value, Meter, Indicator, Value).
