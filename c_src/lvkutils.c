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

#include "log.h"
#include "lvkutils.h"

#include <unistd.h>
#include <sys/mman.h>

#define	TILE_XSIZE	120
#define TILE_YSIZE	64

uint
lvk_next_tile(const lv_area_t *area, lv_area_t *tile)
{
	if (tile->x1 == 0 && tile->y1 == 0 && tile->x2 == 0 && tile->y2 == 0) {
		tile->x1 = area->x1;
		tile->y1 = area->y1;
		tile->x2 = area->x1 + TILE_XSIZE - 1;
		tile->y2 = area->y1 + TILE_YSIZE - 1;
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
		tile->y2 = tile->y1 + TILE_YSIZE - 1;
		if (tile->y2 > area->y2)
			tile->y2 = area->y2;
	} else {
		tile->x1 += TILE_XSIZE;
	}
	tile->x2 = tile->x1 + TILE_XSIZE - 1;
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
	assert(buf != NULL);
	assert(rsrc != NULL);
	nrow = tile->y2 - tile->y1 + 1;
	ncol = tile->x2 - tile->x1 + 1;
	row = alloca(nrow * sizeof (ERL_NIF_TERM));
	for (i = 0, y = tile->y1; i < nrow; ++i, ++y) {
		p = &buf[fb->fb_w * y + tile->x1];
		row[i] = enif_make_resource_binary(env, rsrc, p,
		    ncol * sizeof (lv_color_t));
	}
	list = enif_make_list_from_array(env, row, nrow);
	return (list);
}


void
lv_group_send_text(lv_group_t *group, const char *text)
{
	lv_obj_t *act;
	act = lv_group_get_focused(group);
	if (act == NULL)
		return;
	if (lv_obj_has_state(act, LV_STATE_DISABLED))
		return;
	if (!lv_obj_has_class(act, &lv_textarea_class))
		return;
	lv_textarea_add_text(act, text);
}

void
lv_indev_send_text(lv_indev_t *indev, const char *text)
{
	lv_group_t *group;
	group = indev->group;
	if (group == NULL)
		return;
	lv_group_send_text(group, text);
}

lv_obj_t *
lv_indev_get_focused(lv_indev_t *indev)
{
	lv_group_t *group;
	lv_obj_t *act;
	group = indev->group;
	if (group == NULL)
		return (NULL);
	act = lv_group_get_focused(group);
	if (act == NULL)
		return (NULL);
	if (lv_obj_has_state(act, LV_STATE_DISABLED))
		return (NULL);
	return (act);
}

void
lv_img_set_offset(lv_obj_t *obj, lv_point_t pt)
{
	lv_img_set_offset_x(obj, pt.x);
	lv_img_set_offset_y(obj, pt.y);
}

void
lv_disp_scr_load(lv_disp_t *disp, lv_obj_t *scr)
{
	lv_disp_set_default(disp);
	lv_scr_load(scr);
}

void
lv_disp_scr_load_anim(lv_disp_t *disp, lv_obj_t *scr,
    lv_scr_load_anim_t anim_type, uint32_t time, uint32_t delay,
    bool auto_del)
{
	lv_disp_set_default(disp);
	lv_scr_load_anim(scr, anim_type, time, delay, auto_del);
}

lv_obj_t *
lv_disp_obj_create(lv_disp_t *disp, lv_obj_t *parent)
{
	lv_disp_set_default(disp);
	return (lv_obj_create(parent));
}

lv_style_t *
lv_style_alloc(void)
{
	lv_style_t *sty = calloc(1, sizeof (*sty));
	assert(sty != NULL);
	lv_style_init(sty);
	return (sty);
}

void
lv_style_free(lv_style_t *style)
{
	lv_style_reset(style);
	free(style);
}

void
lv_style_set_flex_align(lv_style_t *style, lv_flex_align_t main_place,
    lv_flex_align_t cross_place, lv_flex_align_t track_cross_place)
{
	lv_style_set_flex_main_place(style, main_place);
	lv_style_set_flex_cross_place(style, cross_place);
	lv_style_set_flex_track_place(style, track_cross_place);
	lv_style_set_layout(style, LV_LAYOUT_FLEX);
}

lv_point_t
lv_obj_get_size(lv_obj_t *obj)
{
	lv_point_t pt;
	pt = (lv_point_t){
		.x = lv_obj_get_self_width(obj),
		.y = lv_obj_get_self_height(obj)
	};
	return (pt);
}

lv_point_t
lv_obj_get_pos(lv_obj_t *obj)
{
	lv_point_t pt;
	pt = (lv_point_t){
		.x = lv_obj_get_x(obj),
		.y = lv_obj_get_y(obj)
	};
	return (pt);
}

void
lv_wheel_scroll_by(lv_disp_t *disp, lv_indev_t *mouse, int dy,
    lv_anim_enable_t anim)
{
	lv_point_t pt;
	lv_obj_t *target;
	int height, scaled;

	lv_indev_get_point(mouse, &pt);

	target = lv_indev_search_obj(disp->act_scr, &pt);

	while (target != NULL &&
	    !lv_obj_has_flag(target, LV_OBJ_FLAG_SCROLLABLE)) {
		target = lv_obj_get_parent(target);
	}

	if (target == NULL)
		return;

	height = lv_obj_get_height(target);
	/* We scroll 40% of the widget height for every 1 windows WHEEL_DELTA */
	scaled = (height * (dy * 100 / 120)) / 250;

	if (target != NULL)
		lv_obj_scroll_by_bounded(target, 0, scaled, anim);
}

bool
lv_obj_class_has_base(const lv_obj_class_t *class, const lv_obj_class_t *base)
{
	while (class) {
		if (class == base)
			return (true);
		class = class->base_class;
	}
	return (false);
}

lv_point_t
lv_table_get_selected_cell_pt(lv_obj_t *obj)
{
	lv_point_t pt;
	uint16_t row, col;

	lv_table_get_selected_cell(obj, &row, &col);
	pt.x = col;
	pt.y = row;

	return (pt);
}

int
lv_style_prop_iter(lv_style_t *style, style_prop_iter_cb cb, void *cookie)
{
	uint32_t i;
	lv_style_prop_t prop_id;
	lv_style_value_t value;
	int res;

	if (style->prop1 == LV_STYLE_PROP_ANY) {
		const lv_style_const_prop_t *const_prop;

		for (i = 0; i < style->prop_cnt; i++) {
			const_prop = style->v_p.const_props + i;
			prop_id = LV_STYLE_PROP_ID_MASK(const_prop->prop);
			if (const_prop->prop & LV_STYLE_PROP_META_INHERIT)
				continue;
			value =
			    (const_prop->prop & LV_STYLE_PROP_META_INITIAL) ?
			    lv_style_prop_get_default(prop_id) :
			    const_prop->value;
			res = (*cb)(prop_id, value, cookie);
			if (res != 0)
				return (res);
		}
		return (0);
	}
	if (style->prop_cnt > 1) {
		uint8_t * tmp = style->v_p.values_and_props +
		    style->prop_cnt * sizeof(lv_style_value_t);
		uint16_t * props = (uint16_t *)tmp;
		for (i = 0; i < style->prop_cnt; i++) {
			prop_id = LV_STYLE_PROP_ID_MASK(props[i]);
			if (props[i] & LV_STYLE_PROP_META_INHERIT)
				continue;
			if (props[i] & LV_STYLE_PROP_META_INITIAL) {
				value = lv_style_prop_get_default(prop_id);
			} else {
				lv_style_value_t * values = (lv_style_value_t *)
				    style->v_p.values_and_props;
				value = values[i];
			}
			res = (*cb)(prop_id, value, cookie);
			if (res != 0)
				return (res);
		}
	} else if (style->prop_cnt == 1) {
		prop_id = LV_STYLE_PROP_ID_MASK(style->prop1);
		if (style->prop1 & LV_STYLE_PROP_META_INHERIT)
			return (0);
		value = (style->prop1 & LV_STYLE_PROP_META_INITIAL) ?
		    lv_style_prop_get_default(prop_id) :
		    style->v_p.value1;
		res = (*cb)(prop_id, value, cookie);
		if (res != 0)
			return (res);
	}
	return (0);
}

static int
lv_style_copy_iter_cb(lv_style_prop_t prop, lv_style_value_t value,
    void *arg)
{
	lv_style_t *target = arg;
	lv_style_set_prop(target, prop, value);
	return (0);
}

void
lv_style_copy(lv_style_t *from, lv_style_t *to)
{
	lv_style_reset(to);
	lv_style_prop_iter(from, lv_style_copy_iter_cb, to);
}

void
lv_span_set_style(lv_span_t *span, lv_style_t *sty)
{
	lv_style_copy(sty, &span->style);
}

static size_t PAGE = 0;

void *
lvk_secure_alloc(size_t len)
{
	int rc;
	void *base;
	void *ptr;
	size_t npages;
	if (PAGE == 0)
		PAGE = sysconf(_SC_PAGESIZE);
	npages = 2 + ((len / PAGE) + 1);
	base = mmap(NULL, npages * PAGE, PROT_NONE, MAP_PRIVATE | MAP_ANON,
	    -1, 0);
	if (base == MAP_FAILED)
		return (NULL);
	ptr = (char *)base + PAGE;
	rc = mprotect(ptr, (npages - 2) * PAGE, PROT_READ | PROT_WRITE);
	if (rc < 0) {
		munmap(base, npages * PAGE);
		return (NULL);
	}
	madvise(base, PAGE, MADV_DONTNEED);
#if defined(MADV_DONTDUMP)
	madvise(ptr, (npages - 2) * PAGE, MADV_DONTDUMP);
#endif
#if defined(MADV_WIPEONFORK)
	madvise(ptr, (npages - 2) * PAGE, MADV_WIPEONFORK);
#endif
	madvise((char *)base + (npages - 1) * PAGE, PAGE, MADV_DONTNEED);
	return (ptr);
}

void
lvk_secure_free(void *ptr, size_t len)
{
	size_t npages;
	void *base;
	if (ptr == NULL)
		return;
	if (len == 0)
		return;
	npages = 2 + ((len / PAGE) + 1);
	base = (char *)ptr - PAGE;
	explicit_bzero(ptr, len);
	munmap(base, npages * PAGE);
}

struct dbuf {
	void	*d_b;
	void	*d_p;
	size_t	 d_len;
	size_t	 d_size;
	uint8_t	 d_free;
};

struct dbuf *
dbuf_new(void)
{
	struct dbuf *d;
	d = calloc(1, sizeof (*d));
	assert(d != NULL);
	d->d_size = 512;
	d->d_b = (d->d_p = malloc(d->d_size));
	assert(d->d_p != NULL);
	d->d_free = 1;
	return (d);
}

struct dbuf *
dbuf_from(const void *p, size_t len)
{
	struct dbuf *d;
	d = calloc(1, sizeof (*d));
	assert(d != NULL);
	d->d_b = (d->d_p = (void *)p);
	d->d_size = len;
	return (d);
}

void
dbuf_free(struct dbuf *d)
{
	if (d->d_free) {
		explicit_bzero(d->d_b, d->d_size);
		free(d->d_b);
	}
	free(d);
}

void
dbuf_put(struct dbuf *d, const void *p, size_t s)
{
	while (d->d_len + s >= d->d_size) {
		void *newb;
		d->d_size *= 2;
		newb = malloc(d->d_size);
		assert(newb != NULL);
		memcpy(newb, d->d_b, d->d_len);
		free(d->d_b);
		d->d_b = newb;
		d->d_p = newb + d->d_len;
	}
	d->d_len += s;
	memcpy(d->d_p, p, s);
	d->d_p += s;
}

void *
dbuf_get(struct dbuf *d, size_t s)
{
	uint8_t *p;
	assert(d->d_len + s <= d->d_size);

	p = malloc(s + 1);
	assert(p != NULL);
	memcpy(p, d->d_p, s);
	p[s] = 0;

	d->d_p += s;
	d->d_len += s;

	return (p);
}

const void *
dbuf_data(const struct dbuf *d)
{
	return (d->d_b);
}

size_t
dbuf_len(const struct dbuf *d)
{
	return (d->d_len);
}
