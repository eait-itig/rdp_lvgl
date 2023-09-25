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
#include "lvcall.h"
#include "lvkutils.h"

#include <string.h>
#include <sys/errno.h>
#include <queue.h>

struct ibuf {
	uint8_t	 ib_buf[4096];
	uint8_t *ib_ptrs[4];
	size_t	 ib_len;
	uint	 ib_idx;
};

static struct ibuf ibuf[MAX_ARGS];

#define	LVCALL_DEBUG	0

#if LVCALL_DEBUG == 1
# define debug(fmt...)	log_debug(fmt)
#else
# define debug(fmt...)	(void)0
#endif

LIST_HEAD(ptrentries, ptrentry);
struct ptrentry {
	LIST_ENTRY(ptrentry)	 pe_ht_ent;
	LIST_ENTRY(ptrentry)	 pe_dep_ent;
	struct ptrentries	 pe_dependents;
	struct ptrentry		*pe_parent;
	uintptr_t		 pe_ptr;
};
static struct ptrentries ptrht[256] = { LIST_HEAD_INITIALIZER(ptrentries) };

int
lvptr_check(void *pptr)
{
	uintptr_t ptr = (uintptr_t)pptr;
	uint htidx;
	struct ptrentries *list;
	struct ptrentry *pe, *npe;

	htidx = (ptr >> 4) & 0xFF;
	htidx ^= (ptr >> 12) & 0xFF;
	list = &ptrht[htidx];

	LIST_FOREACH_SAFE(pe, list, pe_ht_ent, npe) {
		if (pe->pe_ptr == ptr) {
			LIST_REMOVE(pe, pe_ht_ent);
			LIST_INSERT_HEAD(list, pe, pe_ht_ent);
			return (1);
		}
	}

	return (0);
}

void
lvptr_validate(void *pptr)
{
	uintptr_t ptr = (uintptr_t)pptr;
	uint htidx;
	struct ptrentries *list;
	struct ptrentry *pe;

	htidx = (ptr >> 4) & 0xFF;
	htidx ^= (ptr >> 12) & 0xFF;
	list = &ptrht[htidx];

	LIST_FOREACH(pe, list, pe_ht_ent) {
		if (pe->pe_ptr == ptr)
			return;
	}

	pe = calloc(1, sizeof (*pe));
	LIST_INIT(&pe->pe_dependents);
	pe->pe_ptr = ptr;
	LIST_INSERT_HEAD(list, pe, pe_ht_ent);
}

void
lvptr_validate_with_dep(void *pptr, void *pparent)
{
	uintptr_t ptr = (uintptr_t)pptr;
	uintptr_t parent = (uintptr_t)pparent;
	uint htidx;
	struct ptrentries *list, *plist;
	struct ptrentry *pe = NULL, *tpe, *ppe = NULL;

	htidx = (ptr >> 4) & 0xFF;
	htidx ^= (ptr >> 12) & 0xFF;
	list = &ptrht[htidx];

	htidx = (parent >> 4) & 0xFF;
	htidx ^= (parent >> 12) & 0xFF;
	plist = &ptrht[htidx];

	LIST_FOREACH(tpe, list, pe_ht_ent) {
		if (tpe->pe_ptr == ptr) {
			pe = tpe;
			break;
		}
	}

	LIST_FOREACH(tpe, plist, pe_ht_ent) {
		if (tpe->pe_ptr == parent) {
			ppe = tpe;
			break;
		}
	}

	if (ppe == NULL) {
		ppe = calloc(1, sizeof (*ppe));
		LIST_INIT(&ppe->pe_dependents);
		ppe->pe_ptr = parent;
		LIST_INSERT_HEAD(plist, ppe, pe_ht_ent);
	}

	if (pe == NULL) {
		pe = calloc(1, sizeof (*pe));
		LIST_INIT(&pe->pe_dependents);
		pe->pe_ptr = ptr;
		pe->pe_parent = ppe;
		LIST_INSERT_HEAD(list, pe, pe_ht_ent);
	}

	LIST_INSERT_HEAD(&ppe->pe_dependents, pe, pe_dep_ent);
}


static void lvptr_ent_delete(struct ptrentry *pe);

static void
lvptr_ent_delete(struct ptrentry *pe)
{
	struct ptrentry *tpe, *npe;

	if (pe->pe_parent != NULL) {
		LIST_REMOVE(pe, pe_dep_ent);
		pe->pe_parent = NULL;
	}

	LIST_FOREACH_SAFE(tpe, &pe->pe_dependents, pe_dep_ent, npe) {
		lvptr_ent_delete(tpe);
	}

	LIST_REMOVE(pe, pe_ht_ent);
	pe->pe_ptr = 0;

	free(pe);
}

void
lvptr_invalidate(void *pptr)
{
	uintptr_t ptr = (uintptr_t)pptr;
	uint htidx;
	struct ptrentries *list;
	struct ptrentry *pe = NULL, *tpe;

	htidx = (ptr >> 4) & 0xFF;
	htidx ^= (ptr >> 12) & 0xFF;
	list = &ptrht[htidx];

	LIST_FOREACH(tpe, list, pe_ht_ent) {
		if (tpe->pe_ptr == ptr) {
			pe = tpe;
			break;
		}
	}

	if (pe == NULL)
		return;

	lvptr_ent_delete(pe);
}

void
lv_do_call(struct shmintf *shm, struct cdesc **cd, uint ncd)
{
	struct cdesc_call *cdc = &cd[0]->cd_call;
	struct rdesc rd[RING_MAX_CHAIN];
	uint64_t rv;
	lv_obj_t *obj;
	lv_group_t *grp;
	lv_style_t *sty;
	lv_chart_cursor_t *chartcur;
	lv_chart_series_t *chartser;
	lv_meter_indicator_t *meterind;
	lv_meter_scale_t *meterscl;
	lv_span_t *span;
	struct cdinline *inl;
	uint ib = 0, i;
	size_t rem, off, take;
	uint8_t *data;
	enum arg_type rt = cdc->cdc_rettype;

	debug("call to %p", (void *)cdc->cdc_func);
	debug("rtype = %u", cdc->cdc_rettype);

	inl = cdi_init(cd, ncd, FOFFSET_CALL);
	assert(inl != NULL);

	for (i = 0; i < MAX_ARGS; ++i) {
		switch (cdc->cdc_argtype[i]) {
		case ARG_PTR:
		case ARG_PTR_BUFFER:
		case ARG_PTR_OBJ:
		case ARG_PTR_STYLE:
		case ARG_PTR_GROUP:
		case ARG_PTR_CHART_SER:
		case ARG_PTR_CHART_CUR:
		case ARG_PTR_METER_SCL:
		case ARG_PTR_METER_IND:
		case ARG_PTR_SPAN:
			if (cdc->cdc_arg[i] == 0)
				continue;
			if (!lvptr_check((void *)cdc->cdc_arg[i])) {
				debug("arg %u invalid ptr (%p)", i,
				    cdc->cdc_arg[i]);
				rd[0] = (struct rdesc){
					.rd_error = EFAULT,
					.rd_cookie = cd[0]->cd_cookie,
				};
				shm_produce_rsp(shm, rd, 1);
				cdi_free(inl);
				return;
			}
			break;
		default:
			/* nothing */
		}
	}

	for (i = 0; i < MAX_ARGS; ++i) {
		if (cdc->cdc_argtype[i] != ARG_INLINE_BUF &&
		    cdc->cdc_argtype[i] != ARG_INLINE_STR &&
		    cdc->cdc_argtype[i] != ARG_INL_BUF_ARR)
			continue;

		if (cdc->cdc_arg[i] == 0) {
			cdc->cdc_argtype[i] = ARG_PTR;
			continue;
		}

		if (cdc->cdc_argtype[i] == ARG_INL_BUF_ARR) {
			uint64_t v = cdc->cdc_arg[i];
			uint8_t len;
			uint aidx = 0;

			ibuf[ib].ib_len = 0;
			ibuf[ib].ib_idx = i;
			bzero(ibuf[ib].ib_buf, sizeof (ibuf[ib].ib_buf));

			cdc->cdc_argtype[i] = ARG_PTR;
			cdc->cdc_arg[i] = (uint64_t)ibuf[ib].ib_ptrs;

			while ((v & 0xFF) != 0) {
				len = v & 0xFF;
				v >>= 8;

				debug("inline arr buf in arg%d/%d: "
				    "%d bytes", i, aidx, len);

				ibuf[ib].ib_ptrs[aidx++] =
				    &ibuf[ib].ib_buf[ibuf[ib].ib_len];
				cdi_get(inl, &ibuf[ib].ib_buf[ibuf[ib].ib_len],
				    len);
				ibuf[ib].ib_len += len + 1;

				debug("=> inlined as ib %d/%d", ib, aidx);
			}

			++ib;
			continue;
		}

		debug("inline buf in arg%d: %lu bytes", i, cdc->cdc_arg[i]);

		ibuf[ib].ib_len = cdc->cdc_arg[i];
		ibuf[ib].ib_idx = i;
		bzero(ibuf[ib].ib_buf, sizeof (ibuf[ib].ib_buf));

		cdi_get(inl, ibuf[ib].ib_buf, ibuf[ib].ib_len);

		cdc->cdc_argtype[i] = ARG_PTR;
		cdc->cdc_arg[i] = (uint64_t)ibuf[ib].ib_buf;

		debug("=> inlined as ib %d", ib);

		++ib;
	}

	cdi_free(inl);

	if (rt == ARG_INLINE_BUF || rt == ARG_INLINE_STR)
		cdc->cdc_rettype = ARG_PTR_BUFFER;

#if LVCALL_DEBUG == 1
	for (i = 0; i < MAX_ARGS; ++i) {
		debug("arg%u type = %u, val = %lx",
		    i, cdc->cdc_argtype[i], cdc->cdc_arg[i]);
		if (cdc->cdc_argtype[i] == ARG_NONE)
			break;
	}
#endif

	rv = lv_do_real_call(cdc);

	debug("rv = %lx", rv);

	switch (rt) {
	case ARG_INLINE_STR:
		rem = strlen((const char *)rv);
		cdc->cdc_rbuflen = rem;
		/* FALL THROUGH */
	case ARG_INLINE_BUF:
		data = (uint8_t *)rv;

		rem = cdc->cdc_rbuflen;
		off = 0;

		if (rem > RDESC_MAX_INLINE) {
			log_warn("call returned buffer of len %u > max "
			    "inline which is %u. returning ENOSPC",
			    rem, RDESC_MAX_INLINE);
			rd[0] = (struct rdesc){
				.rd_error = ENOSPC,
				.rd_cookie = cd[0]->cd_cookie,
			};
			shm_produce_rsp(shm, rd, 1);
			break;
		}

		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return_buf = (struct rdesc_retbuf){
				.rdrb_len = rem,
			}
		};

		take = sizeof (rd[0].rd_return_buf.rdrb_data);
		if (take > rem)
			take = rem;
		if (take > 0)
			bcopy(data, rd[0].rd_return_buf.rdrb_data, take);
		rem -= take;
		off += take;

		i = 1;
		while (i < RING_MAX_CHAIN && rem > 0) {
			rd[i - 1].rd_chain = 1;
			rd[i] = (struct rdesc){
				.rd_chain = 0,
				.rd_cookie = cd[0]->cd_cookie,
			};
			take = sizeof (rd[i].rd_data);
			if (take > rem)
				take = rem;
			bcopy(&data[off], rd[i].rd_data, take);
			rem -= take;
			off += take;
			++i;
		}
		rd[0].rd_return_buf.rdrb_len = off;

		shm_produce_rsp(shm, rd, i);
		break;
	case ARG_PTR_OBJ:
		obj = (lv_obj_t *)rv;
		if (obj != NULL)
			lvptr_validate(obj);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_class = (obj == NULL) ? 0 : (uint64_t)lv_obj_get_class(obj),
				.rdr_udata = (obj == NULL) ? 0 : (uint64_t)lv_obj_get_user_data(obj),
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_GROUP:
		grp = (lv_group_t *)rv;
		if (grp != NULL)
			lvptr_validate(grp);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (grp == NULL) ? 0 : (uint64_t)grp->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_STYLE:
		sty = (lv_style_t *)rv;
		if (sty != NULL)
			lvptr_validate(sty);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (sty == NULL) ? 0 : (uint64_t)sty->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_CHART_CUR:
		chartcur = (lv_chart_cursor_t *)rv;
		if (chartcur != NULL)
			lvptr_validate_with_dep(chartcur, chartcur->ser);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (chartcur == NULL) ? 0 : (uint64_t)chartcur->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_CHART_SER:
		chartser = (lv_chart_series_t *)rv;
		if (chartser != NULL)
			lvptr_validate(chartser);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (chartser == NULL) ? 0 : (uint64_t)chartser->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_METER_IND:
		meterind = (lv_meter_indicator_t *)rv;
		if (meterind != NULL)
			lvptr_validate_with_dep(meterind, meterind->scale);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (meterind == NULL) ? 0 : (uint64_t)meterind->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_METER_SCL:
		meterscl = (lv_meter_scale_t *)rv;
		if (meterscl != NULL)
			lvptr_validate(meterscl);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (meterscl == NULL) ? 0 : (uint64_t)meterscl->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_PTR_SPAN:
		span = (lv_span_t *)rv;
		if (span != NULL)
			lvptr_validate_with_dep(span, span->spangroup);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (span == NULL) ? 0 : (uint64_t)span->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	default:
		if (rt == ARG_PTR)
			lvptr_validate((void *)rv);
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
			}
		};
		shm_produce_rsp(shm, rd, 1);
	}
}
