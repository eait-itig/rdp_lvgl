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

#if !defined(_RBUF_H)
#define _RBUF_H

#include <sys/types.h>
#include <stdint.h>

#include <lvgl.h>

enum rbuf_err {
	RBUF_SUCCESS	= 0,
	RBUF_NO_MEMORY,
	RBUF_NOT_ENOUGH_DATA,
	RBUF_INVALID_ARG,
	RBUF_INVALID_DATA
};

struct rrefctx;
typedef struct rrefctx rrefctx_t;

struct rref;
typedef struct rref rref_t;

rrefctx_t *rrefctx_alloc(void);
void rrefctx_free(rrefctx_t *ctx);

void rrefctx_clear(rrefctx_t *ctx);

typedef enum ref_type {
	REF_BUFFER	= 0x0001,
	REF_DISP	= 0x0002,
	REF_DISP_DRV	= 0x0003,
	REF_KBD		= 0x0004,
	REF_KBD_DRV	= 0x0005,
	REF_MOUSE	= 0x0006,
	REF_MOUSE_DRV	= 0x0007,
	REF_STYLE	= 0x0010,
	REF_GROUP	= 0x0011,
	REF_OBJ		= 0x0100,
	REF_CHART_SER	= 0x0101,
	REF_CHART_CUR	= 0x0102,
	REF_METER_SCL	= 0x0103,
	REF_METER_IND	= 0x0104,
	REF_SPAN	= 0x0105,
	REF_ANY		= 0xFFFF,
} ref_type_t;

rref_t *rref_make(rrefctx_t *ctx, ref_type_t type, void *ptr);
void rref_invalidate(rrefctx_t *ctx, ref_type_t type, void *ptr);

void *rref_deref(rref_t *ref, ref_type_t type);
const void *rref_const_deref(const rref_t *ref, ref_type_t type);
int rref_is_foreign(const rref_t *ref);

struct rbuf;
typedef struct rbuf rbuf_t;

rbuf_t *rbuf_from(void *data, size_t len);
rbuf_t *rbuf_alloc(void);
void rbuf_free(rbuf_t *buf);

void rbuf_shift(rbuf_t *buf);
void rbuf_reset(rbuf_t *buf);

int rbuf_recv(rbuf_t *buf, int fd);
int rbuf_send(rbuf_t *buf, int fd);

size_t rbuf_len(const rbuf_t *buf);
const uint8_t *rbuf_ptr(const rbuf_t *buf);

int rbuf_read_u64(rbuf_t *buf, uint64_t *v);
int rbuf_write_u64(rbuf_t *buf, uint64_t v);

int rbuf_read_u32(rbuf_t *buf, uint32_t *v);
int rbuf_write_u32(rbuf_t *buf, uint32_t v);

int rbuf_read_u16(rbuf_t *buf, uint16_t *v);
int rbuf_write_u16(rbuf_t *buf, uint16_t v);

int rbuf_read_u8(rbuf_t *buf, uint8_t *v);
int rbuf_write_u8(rbuf_t *buf, uint8_t v);

int rbuf_read_cstring(rbuf_t *buf, char **outp);
int rbuf_write_cstring(rbuf_t *buf, const char *str);

int rbuf_read_rref(rbuf_t *buf, rrefctx_t *ctx, rref_t **refp);
int rbuf_write_rref(rbuf_t *rbuf, const rrefctx_t *ctx, const rref_t *ref);

int rbuf_readb(rbuf_t *buf, rbuf_t **outb);
int rbuf_writeb(rbuf_t *buf, const rbuf_t *inb);

int rbuf_read_point(rbuf_t *buf, lv_point_t *pp);
int rbuf_write_point(rbuf_t *buf, lv_point_t p);

int rbuf_read_color(rbuf_t *buf, lv_color_t *cp);
int rbuf_write_color(rbuf_t *buf, lv_color_t c);

int rbuf_read_style_val(rbuf_t *buf, lv_style_value *vp);
int rbuf_write_style_val(rbuf_t *buf, lv_style_value v);

#endif
