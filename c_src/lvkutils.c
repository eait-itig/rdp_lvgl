/*
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
*/

#include "lvkutils.h"

#define	TILE_XSIZE	228
#define TILE_YSIZE	128

uint
lvk_next_tile(const lv_area_t *area, lv_area_t *tile)
{
	if (tile->x1 == 0 && tile->y1 == 0 && tile->x2 == 0 && tile->y2 == 0) {
		tile->x1 = area->x1;
		tile->y1 = area->y1;
		tile->x2 = area->x1 + TILE_XSIZE;
		tile->y2 = area->y1 + TILE_YSIZE;
		if (tile->y2 > area->y2)
			tile->y2 = area->y2;
		if (tile->x2 > area->x2)
			tile->x2 = area->x2;
		return (1);
	}
	if (tile->x2 >= area->x2) {
		if (tile->y2 >= area->y2) {
			return (0);
		}
		tile->x1 = area->x1;
		tile->y1 += TILE_YSIZE;
		tile->y2 = tile->y1 + TILE_YSIZE;
		if (tile->y2 > area->y2)
			tile->y2 = area->y2;
	} else {
		tile->x1 += TILE_XSIZE;
	}
	tile->x2 = tile->x1 + TILE_XSIZE;
	if (tile->x2 > area->x2)
		tile->x2 = area->x2;
	return (1);
}

ERL_NIF_TERM
lvk_tile_to_iolist(ErlNifEnv *env, struct fbuf *fb, lv_color_t *buf,
    void *rsrc, const lv_area_t *tile)
{
	ERL_NIF_TERM list, *row;
	lv_color_t *p;
	uint nrow, ncol, i, y;
	nrow = tile->y2 - tile->y1;
	ncol = tile->x2 - tile->x1;
	row = alloca(nrow * sizeof (ERL_NIF_TERM));
	for (i = 0, y = tile->y1; i < nrow; ++i, ++y) {
		p = &buf[fb->fb_w * y + tile->x1];
		row[i] = enif_make_resource_binary(env, rsrc, p,
		    ncol * sizeof (lv_color_t));
	}
	list = enif_make_list_from_array(env, row, nrow);
	return (list);
}
