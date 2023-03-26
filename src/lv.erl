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

%% @doc Top-level types and functions for controlling an LVGL system.
%%
%%
-module(lv).

-export([
    make_buffer/2,
    setup/1,
    flush_done/1,
    read_framebuffer/2
    ]).

-export_type([
    rref/0, object/0, instance/0, event/0, style/0, error/0, rect/0, point/0,
    size/0, color/0, buffer/0,
    btn/0, label/0, scr/0, img/0, spinner/0, textarea/0, tabview/0, btnmatrix/0,
    checkbox/0, dropdown/0, imgbtn/0, led/0, listview/0, menu/0, msgbox/0,
    roller/0, slider/0, switch/0, table/0, bar/0,
    flags/1, font/0, group/0, coord/0, flush_msg/0,
    chart/0, calendar/0, meter/0, spangroup/0, msgref/0
	]).

-record(lv_instance, {
    cmd_fsm :: pid(),
    flush_fsm :: pid(),
    evmux :: pid(),
    gen :: integer(),
    idx :: integer()
    }).

-record(lv_rref, {
    cmd_fsm :: pid(),
    evmux :: pid(),
    gen :: integer(),
    idx :: integer()
    }).

-opaque rref() :: #lv_rref{}.

-opaque object() :: rref().
%% An lv_obj instance.
%%
%% <b>See also:</b> {@link lv_obj}

-opaque instance() :: #lv_instance{}.
%% An instance of LVGL, consisting of display, input devices, and all of the
%% objects related to them.
%%
%% <b>See also:</b> {@link rdp_lvgl_server}, {@link lv:setup/1}

-opaque event() :: rref().
%% A handle for an LVGL event handler. Messages will be sent to the process
%% which created it when matching LVGL events occur.
%%
%% <b>See also:</b> {@link lv_event}

-opaque style() :: rref().
%% A handle to a <code>lv_style_t</code> instance. Styles are sets of cosmetic
%% properties which can be created and then applied to multiple widgets.
%%
%% <b>See also:</b> {@link lv_style}

-opaque group() :: rref().
%% A handle to a <code>lv_group_t</code> instance. Groups are used to map
%% input devices to widgets which can accept input.
%%
%% <b>See also:</b> {@link lv_group}, {@link lv_indev:set_group/3}

-opaque buffer() :: rref().
%% A handle to a buffer of bytes with a lifetime equal to the lifetime of the
%% entire LVGL instance. Used in place of <code>static</code> data in C when
%% calling certain functions.
%%
%% <b>See also:</b> {@link lv:make_buffer/2}

-type error() :: {error, integer(), string()} | {error, term()}.
%% The type of an error returned from LVGL.

-type color() :: lv_color:color().
%% See {@link lv_color:color()}

-type px() :: integer().
%% A positive integer number of pixels.

-type rect() :: {X1 :: px(), Y1 :: px(), X2 :: px(), Y2 :: px()}.
%% A rectangle in pixel coordinates.

-type point() :: {X :: coord(), Y :: coord()}.
%% Specifies a point within a widget or on the screen.
%%
%% <b>See also:</b> {@link coord()}

-type size() :: {Width :: coord(), Height :: coord()}.
%% A size specification, used with e.g. {@link lv_obj:set_size/2}.
%%
%% <b>See also:</b> {@link coord()}

-type coord() :: px() | content | {percent, integer()}.
%% An LVGL coordinate or distance specification.
%%
%% This can be a number of pixels, a percentage of the size of a parent or
%% container, or the atom <code>content</code>, which specifies the size of
%% all children of the object in question.
%%
%% <b>See also:</b> {@link point()}
%% <b>See also:</b> {@link size()}

-type btn() :: object().
%% A widget of type {@link lv_btn}
-type label() :: object().
%% A widget of type {@link lv_label}
-type scr() :: object().
%% A widget of type {@link lv_scr}
-type img() :: object().
%% A widget of type {@link lv_img}
-type spinner() :: object().
%% A widget of type {@link lv_spinner}
-type textarea() :: object().
%% A widget of type {@link lv_textarea}
-type tabview() :: object().
%% A widget of type {@link lv_tabview}
-type btnmatrix() :: object().
%% A widget of type {@link lv_btnmatrix}
-type checkbox() :: object().
%% A widget of type {@link lv_checkbox}
-type dropdown() :: object().
%% A widget of type {@link lv_dropdown}
-type imgbtn() :: object().
%% A widget of type {@link lv_imgbtn}
-type led() :: object().
%% A widget of type {@link lv_led}
-type listview() :: object().
%% A widget of type {@link lv_listview}
-type menu() :: object().
%% A widget of type {@link lv_menu}
-type msgbox() :: object().
%% A widget of type {@link lv_msgbox}
-type roller() :: object().
%% A widget of type {@link lv_roller}
-type slider() :: object().
%% A widget of type {@link lv_slider}
-type switch() :: object().
%% A widget of type {@link lv_switch}
-type table() :: object().
%% A widget of type {@link lv_table}
-type bar() :: object().
%% A widget of type {@link lv_bar}
-type chart() :: object().
%% A widget of type {@link lv_chart}
-type calendar() :: object().
%% A widget of type {@link lv_calendar}
-type meter() :: object().
%% A widget of type {@link lv_meter}
-type spangroup() :: object().
%% A widget of type {@link lv_spangroup}

-type flags(T) :: T | [T].

-type points() :: integer().
%% Font size, in units of one "point" (a la Postscript).

-type font_family() :: string().
%% The name of a font family (e.g. "Montserrat").

-type font_variant() :: bold | italic | regular.
%% Font face variant.

-type font() :: points() | font_variant() | font_family() |
    {font_variant(), points()} | {font_family(), points()} |
    {font_family(), font_variant(), points()}.
%% Specifies a font family, variant and size used for text.
%%
%% <b>See also:</b> {@link lv_style:set_text_font/2}, {@link lv_obj:set_style_text_font/2}

-type msgref() :: reference().
%% An opaque reference returned by {@link lv:setup/1} which then can be used
%% to match the relevant {@link flush_msg()} messages received by the same
%% process.

-type flush_rect() :: {msgref(), flush, rect(), iolist()}.
%% Sent for each invalidated rectangle in a given frame. The
%% <code>iolist()</code> contains pixel data, 16bpp (5-6-5 RGB).

-type flush_sync() :: {msgref(), flush_sync}.
%% Sent at the end of a frame.

-type flush_msg() :: flush_rect() | flush_sync().
%% Messages sent by an LVGL instance to its owner process to handle drawing
%% on the display.
%%
%% Each frame of rendering consists of multiple {@link flush_rect()} messages
%% followed by a single {@link flush_sync()}.
%%
%% Once the {@link flush_sync()} message has been received, the owner must check
%% that all the {@link flush_rect()} messages for that frame have been handled
%% completely (with no live references to the <code>iolist()</code> remaining)
%% before calling {@link lv:flush_done/1}.

%% @doc Creates an instance of the LVGL system.
%%
%% The process which calls this function becomes the owner of this LVGL
%% instance and must immediately begin handling flush messages
%% (see {@link lv:flush_msg()}). These messages will contain the
%% same <code>msgref()</code> returned by this function.
%%
%% The LVGL instance lasts until the owner process exits.
-spec setup(size()) -> {ok, instance(), msgref()} | lv:error().
setup(Size) ->
    case rdp_lvgl_nif:setup_instance(Size) of
        {ok, Inst, MsgRef} ->
            receive {MsgRef, setup_done} -> ok end,
            {ok, Inst, MsgRef};
        Err ->
            Err
    end.

%% @doc Informs LVGL that the current frame has been flushed out to display.
%%
%% At entry to this function, there must be no live references remaining to the
%% <code>iolist()</code> values contained in {@link flush_rect()} messages for
%% the current frame.
%%
%% If any live references remain, the <code>{error, busy}</code> error will be
%% returned, and the <code>flush_done()</code> will not be processed. You will
%% need to call it again once the references are gone.
-spec flush_done(instance()) -> ok | {error, busy} | {error, teardown}.
flush_done(#lv_instance{flush_fsm = Fsm}) ->
    lv_flush_fsm:flush_done(Fsm).

%% @doc Creates a byte buffer whose liftime is tied to an LVGL instance.
%%
%% This is useful to replace the use of <code>static</code> or global data in
%% C when calling LVGL functions which require such.
%%
%% @see instance()
%% @see buffer()
-spec make_buffer(instance(), iolist()) -> {ok, buffer()} | error().
make_buffer(Inst, Data) ->
    case rdp_lvgl_nif:make_buffer(Inst, Data) of
        {async, Buf, MsgRef} ->
            receive
                {MsgRef, ok} -> {ok, Buf};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end.

-type tile() :: {rect(), iolist()}.
%% One tile of pixel data returned in the result of {@link read_framebuffer/2}.

%% @doc Reads from the current framebuffer of an LVGL instance.
%%
%% Returns all of the pixels for the specified rectangle as a series of
%% {@link tile()} tuples.
-spec read_framebuffer(instance(), rect()) -> {ok, [tile()]} | error().
read_framebuffer(Inst, Rect) ->
    rdp_lvgl_nif:read_framebuffer(Inst, Rect).
