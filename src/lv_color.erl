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

%% @doc Types and utility functions related to colors.
-module(lv_color).

-export([
    make/1,
    make/3,
    palette/1,
    lighten/2,
    darken/2
    ]).

-export_type([
    opacity/0, color/0, grad_dir/0
    ]).

-include("async_wrappers.hrl").

-type opacity() :: float().
%% Specifies the opacity of a widget. 0 means fully transparent, while 1.0
%% means fully opaque.

-type color() :: {Red :: uint8(), Green :: uint8(), Blue :: uint8()}.
%% Specifies a color.
%%
%% <b>See also:</b> {@link lv_color:make/1}, {@link lv_color:palette/1}

-type grad_dir() :: none | vertical | horizontal.
%% The direction of a gradient.

-type palette() :: red | pink | purple | deep_purple | indigo | blue |
    light_blue | cyan | teal | green | light_green | lime | yellow | amber |
    orange | deep_orange | brown | blue_grey | grey | black | white.
%% A named color.

-type lighten_level() :: integer().
%% An integer in the range 1 - 5.
%%
%% <b>See also:</b> {@link lv_color:lighten/2}

-type darken_level() :: integer().
%% An integer in the range 1 - 4.
%%
%% <b>See also:</b> {@link lv_color:darken/2}

-type uint24() :: integer().
%% An integer in the range 0 - 2^24-1 (0 - 16777215).

-type uint8() :: integer().
%% An integer in the range 0 - 2^8-1 (0 - 255).

%% @doc Creates a {@link lv:color()} from a single 24-bit integer.
-spec make(uint24()) -> color().
make(Hex) ->
    {(Hex band 16#FF0000) bsr 16,
     (Hex band 16#00FF00) bsr 8,
     (Hex band 16#0000FF)}.

%% @doc Creates a {@link lv:color()} from 3 8-bit integers.
-spec make(uint8(), uint8(), uint8()) -> color().
make(R, G, B) -> {R, G, B}.

%% @doc Creates a {@link lv:color()} based on a palette color name.
%%
%% @see palette()
%% @see color()
-spec palette(palette()) -> color().
palette(red) -> { 244, 67, 54 };
palette(pink) -> { 233, 30, 99 };
palette(purple) -> { 156, 39, 176 };
palette(deep_purple) -> { 103, 58, 183 };
palette(indigo) -> { 63, 81, 181 };
palette(blue) -> { 33, 150, 243 };
palette(light_blue) -> { 3, 169, 244 };
palette(cyan) -> { 0, 188, 212 };
palette(teal) -> { 0, 150, 136 };
palette(green) -> { 76, 175, 80 };
palette(light_green) -> { 139, 195, 74 };
palette(lime) -> { 205, 220, 57 };
palette(yellow) -> { 255, 235, 59 };
palette(amber) -> { 255, 193, 7 };
palette(orange) -> { 255, 152, 0 };
palette(deep_orange) -> { 255, 87, 34 };
palette(brown) -> { 121, 85, 72 };
palette(blue_grey) -> { 96, 125, 139 };
palette(grey) -> { 158, 158, 158 };
palette(black) -> { 0, 0, 0 };
palette(white) -> { 255, 255, 255 };
palette(X) -> error({invalid_color, X}).

%% @doc Darkens a base palette color by a number of steps.
%%
%% @see palette()
%% @see darken_level()
%% @see color()
-spec darken(palette(), darken_level()) -> color().
darken(red, 1) -> { 229, 57, 53 };
darken(red, 2) -> { 211, 47, 47 };
darken(red, 3) -> { 198, 40, 40 };
darken(red, 4) -> { 183, 28, 28 };
darken(pink, 1) -> { 216, 27, 96 };
darken(pink, 2) -> { 194, 24, 91 };
darken(pink, 3) -> { 173, 20, 87 };
darken(pink, 4) -> { 136, 14, 79 };
darken(purple, 1) -> { 142, 36, 170 };
darken(purple, 2) -> { 123, 31, 162 };
darken(purple, 3) -> { 106, 27, 154 };
darken(purple, 4) -> { 74, 20, 140 };
darken(deep_purple, 1) -> { 94, 53, 177 };
darken(deep_purple, 2) -> { 81, 45, 168 };
darken(deep_purple, 3) -> { 69, 39, 160 };
darken(deep_purple, 4) -> { 49, 27, 146 };
darken(indigo, 1) -> { 57, 73, 171 };
darken(indigo, 2) -> { 48, 63, 159 };
darken(indigo, 3) -> { 40, 53, 147 };
darken(indigo, 4) -> { 26, 35, 126 };
darken(blue, 1) -> { 30, 136, 229 };
darken(blue, 2) -> { 25, 118, 210 };
darken(blue, 3) -> { 21, 101, 192 };
darken(blue, 4) -> { 13, 71, 161 };
darken(light_blue, 1) -> { 3, 155, 229 };
darken(light_blue, 2) -> { 2, 136, 209 };
darken(light_blue, 3) -> { 2, 119, 189 };
darken(light_blue, 4) -> { 1, 87, 155 };
darken(cyan, 1) -> { 0, 172, 193 };
darken(cyan, 2) -> { 0, 151, 167 };
darken(cyan, 3) -> { 0, 131, 143 };
darken(cyan, 4) -> { 0, 96, 100 };
darken(teal, 1) -> { 0, 137, 123 };
darken(teal, 2) -> { 0, 121, 107 };
darken(teal, 3) -> { 0, 105, 92 };
darken(teal, 4) -> { 0, 77, 64 };
darken(green, 1) -> { 67, 160, 71 };
darken(green, 2) -> { 56, 142, 60 };
darken(green, 3) -> { 46, 125, 50 };
darken(green, 4) -> { 27, 94, 32 };
darken(light_green, 1) -> { 124, 179, 66 };
darken(light_green, 2) -> { 104, 159, 56 };
darken(light_green, 3) -> { 85, 139, 47 };
darken(light_green, 4) -> { 51, 105, 30 };
darken(lime, 1) -> { 192, 202, 51 };
darken(lime, 2) -> { 175, 180, 43 };
darken(lime, 3) -> { 158, 157, 36 };
darken(lime, 4) -> { 130, 119, 23 };
darken(yellow, 1) -> { 253, 216, 53 };
darken(yellow, 2) -> { 251, 192, 45 };
darken(yellow, 3) -> { 249, 168, 37 };
darken(yellow, 4) -> { 245, 127, 23 };
darken(amber, 1) -> { 255, 179, 0 };
darken(amber, 2) -> { 255, 160, 0 };
darken(amber, 3) -> { 255, 143, 0 };
darken(amber, 4) -> { 255, 111, 0 };
darken(orange, 1) -> { 251, 140, 0 };
darken(orange, 2) -> { 245, 124, 0 };
darken(orange, 3) -> { 239, 108, 0 };
darken(orange, 4) -> { 230, 81, 0 };
darken(deep_orange, 1) -> { 244, 81, 30 };
darken(deep_orange, 2) -> { 230, 74, 25 };
darken(deep_orange, 3) -> { 216, 67, 21 };
darken(deep_orange, 4) -> { 191, 54, 12 };
darken(brown, 1) -> { 109, 76, 65 };
darken(brown, 2) -> { 93, 64, 55 };
darken(brown, 3) -> { 78, 52, 46 };
darken(brown, 4) -> { 62, 39, 35 };
darken(blue_grey, 1) -> { 84, 110, 122 };
darken(blue_grey, 2) -> { 69, 90, 100 };
darken(blue_grey, 3) -> { 55, 71, 79 };
darken(blue_grey, 4) -> { 38, 50, 56 };
darken(grey, 1) -> { 117, 117, 117 };
darken(grey, 2) -> { 97, 97, 97 };
darken(grey, 3) -> { 66, 66, 66 };
darken(grey, 4) -> { 33, 33, 33 };
darken(X, _) -> error({invalid_color, X}).

%% @doc Lightens a base palette color by a number of steps.
%%
%% @see palette()
%% @see lighten_level()
%% @see color()
-spec lighten(palette(), lighten_level()) -> color().
lighten(red, 1) -> { 239, 83, 80 };
lighten(red, 2) -> { 229, 115, 115 };
lighten(red, 3) -> { 239, 154, 154 };
lighten(red, 4) -> { 255, 205, 210 };
lighten(red, 5) -> { 255, 235, 238 };
lighten(pink, 1) -> { 236, 64, 122 };
lighten(pink, 2) -> { 240, 98, 146 };
lighten(pink, 3) -> { 244, 143, 177 };
lighten(pink, 4) -> { 248, 187, 208 };
lighten(pink, 5) -> { 252, 228, 236 };
lighten(purple, 1) -> { 171, 71, 188 };
lighten(purple, 2) -> { 186, 104, 200 };
lighten(purple, 3) -> { 206, 147, 216 };
lighten(purple, 4) -> { 225, 190, 231 };
lighten(purple, 5) -> { 243, 229, 245 };
lighten(deep_purple, 1) -> { 126, 87, 194 };
lighten(deep_purple, 2) -> { 149, 117, 205 };
lighten(deep_purple, 3) -> { 179, 157, 219 };
lighten(deep_purple, 4) -> { 209, 196, 233 };
lighten(deep_purple, 5) -> { 237, 231, 246 };
lighten(indigo, 1) -> { 92, 107, 192 };
lighten(indigo, 2) -> { 121, 134, 203 };
lighten(indigo, 3) -> { 159, 168, 218 };
lighten(indigo, 4) -> { 197, 202, 233 };
lighten(indigo, 5) -> { 232, 234, 246 };
lighten(blue, 1) -> { 66, 165, 245 };
lighten(blue, 2) -> { 100, 181, 246 };
lighten(blue, 3) -> { 144, 202, 249 };
lighten(blue, 4) -> { 187, 222, 251 };
lighten(blue, 5) -> { 227, 242, 253 };
lighten(light_blue, 1) -> { 41, 182, 246 };
lighten(light_blue, 2) -> { 79, 195, 247 };
lighten(light_blue, 3) -> { 129, 212, 250 };
lighten(light_blue, 4) -> { 179, 229, 252 };
lighten(light_blue, 5) -> { 225, 245, 254 };
lighten(cyan, 1) -> { 38, 198, 218 };
lighten(cyan, 2) -> { 77, 208, 225 };
lighten(cyan, 3) -> { 128, 222, 234 };
lighten(cyan, 4) -> { 178, 235, 242 };
lighten(cyan, 5) -> { 224, 247, 250 };
lighten(teal, 1) -> { 38, 166, 154 };
lighten(teal, 2) -> { 77, 182, 172 };
lighten(teal, 3) -> { 128, 203, 196 };
lighten(teal, 4) -> { 178, 223, 219 };
lighten(teal, 5) -> { 224, 242, 241 };
lighten(green, 1) -> { 102, 187, 106 };
lighten(green, 2) -> { 129, 199, 132 };
lighten(green, 3) -> { 165, 214, 167 };
lighten(green, 4) -> { 200, 230, 201 };
lighten(green, 5) -> { 232, 245, 233 };
lighten(light_green, 1) -> { 156, 204, 101 };
lighten(light_green, 2) -> { 174, 213, 129 };
lighten(light_green, 3) -> { 197, 225, 165 };
lighten(light_green, 4) -> { 220, 237, 200 };
lighten(light_green, 5) -> { 241, 248, 233 };
lighten(lime, 1) -> { 212, 225, 87 };
lighten(lime, 2) -> { 220, 231, 117 };
lighten(lime, 3) -> { 230, 238, 156 };
lighten(lime, 4) -> { 240, 244, 195 };
lighten(lime, 5) -> { 249, 251, 231 };
lighten(yellow, 1) -> { 255, 238, 88 };
lighten(yellow, 2) -> { 255, 241, 118 };
lighten(yellow, 3) -> { 255, 245, 157 };
lighten(yellow, 4) -> { 255, 249, 196 };
lighten(yellow, 5) -> { 255, 253, 231 };
lighten(amber, 1) -> { 255, 202, 40 };
lighten(amber, 2) -> { 255, 213, 79 };
lighten(amber, 3) -> { 255, 224, 130 };
lighten(amber, 4) -> { 255, 236, 179 };
lighten(amber, 5) -> { 255, 248, 225 };
lighten(orange, 1) -> { 255, 167, 38 };
lighten(orange, 2) -> { 255, 183, 77 };
lighten(orange, 3) -> { 255, 204, 128 };
lighten(orange, 4) -> { 255, 224, 178 };
lighten(orange, 5) -> { 255, 243, 224 };
lighten(deep_orange, 1) -> { 255, 112, 67 };
lighten(deep_orange, 2) -> { 255, 138, 101 };
lighten(deep_orange, 3) -> { 255, 171, 145 };
lighten(deep_orange, 4) -> { 255, 204, 188 };
lighten(deep_orange, 5) -> { 251, 233, 231 };
lighten(brown, 1) -> { 141, 110, 99 };
lighten(brown, 2) -> { 161, 136, 127 };
lighten(brown, 3) -> { 188, 170, 164 };
lighten(brown, 4) -> { 215, 204, 200 };
lighten(brown, 5) -> { 239, 235, 233 };
lighten(blue_grey, 1) -> { 120, 144, 156 };
lighten(blue_grey, 2) -> { 144, 164, 174 };
lighten(blue_grey, 3) -> { 176, 190, 197 };
lighten(blue_grey, 4) -> { 207, 216, 220 };
lighten(blue_grey, 5) -> { 236, 239, 241 };
lighten(grey, 1) -> { 189, 189, 189 };
lighten(grey, 2) -> { 224, 224, 224 };
lighten(grey, 3) -> { 238, 238, 238 };
lighten(grey, 4) -> { 245, 245, 245 };
lighten(grey, 5) -> { 250, 250, 250 };
lighten(X, _) -> error({invalid_color, X}).
