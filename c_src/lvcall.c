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

#include "lvcall.h"

void
lv_do_call(struct shmintf *shm, struct cdesc **cd, uint ncd)
{
	struct cdesc_call *cdc = &cd[0]->cd_call;
	struct rdesc rd[8];
	uint64_t rv;
	lv_obj_t *obj;
	lv_group_t *grp;
	char ibuf[256];
	uint i;
	size_t rem, off, take;
	uint8_t *data;
	enum arg_type rt = cdc->cdc_rettype;

	if (cdc->cdc_ibuf_len > 0) {
		bzero(ibuf, sizeof (ibuf));
		rem = cdc->cdc_ibuf_len;
		off = 0;

		take = sizeof (cdc->cdc_ibuf);
		if (take > rem)
			take = rem;

		bcopy(cdc->cdc_ibuf, ibuf, take);
		off += take;
		rem -= take;

		for (i = 1; i < ncd; ++i) {
			take = sizeof (cd[i]->cd_data);
			if (take > rem)
				take = rem;
			bcopy(cd[i]->cd_data, &ibuf[off], take);
			off += take;
			rem -= take;
		}
		assert(rem == 0);

		assert(cdc->cdc_argtype[cdc->cdc_ibuf_idx] == ARG_INLINE_BUF ||
		    cdc->cdc_argtype[cdc->cdc_ibuf_idx] == ARG_INLINE_STR);
		cdc->cdc_argtype[cdc->cdc_ibuf_idx] = ARG_PTR;
		cdc->cdc_arg[cdc->cdc_ibuf_idx] = (uint64_t)ibuf;
	}

	if (rt == ARG_INLINE_BUF || rt == ARG_INLINE_STR)
		cdc->cdc_rettype = ARG_BUFPTR;

	/*fprintf(stderr, "call to %p\r\n", (void *)cdc->cdc_func);
	fprintf(stderr, "rtype = %u\r\n", cdc->cdc_rettype);
	for (i = 0; i < 8; ++i) {
		fprintf(stderr, "arg%u type = %u, val = %lx\r\n",
		    i, cdc->cdc_argtype[i], cdc->cdc_arg[i]);
		if (cdc->cdc_argtype[i] == ARG_NONE)
			break;
	}*/

	rv = lv_do_real_call(cdc);

	/*fprintf(stderr, "rv = %lx\r\n", rv);*/

	switch (rt) {
	case ARG_INLINE_STR:
		rem = strlen((const char *)rv);
		assert(rem < UINT8_MAX);
		cdc->cdc_rbuflen = rem;
		/* FALL THROUGH */
	case ARG_INLINE_BUF:
		assert(cdc->cdc_rbuflen > 0);

		data = (uint8_t *)rv;

		rem = cdc->cdc_rbuflen;
		off = 0;

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
		bcopy(data, rd[0].rd_return_buf.rdrb_data, take);
		rem -= take;
		off += take;

		i = 1;
		while (i < 8 && rem > 0) {
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

		shm_produce_rsp(shm, rd, i);
		break;
	case ARG_OBJPTR:
		obj = (lv_obj_t *)rv;
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_class = (uint64_t)lv_obj_get_class(obj),
				.rdr_udata = (uint64_t)lv_obj_get_user_data(obj),
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	case ARG_GRPPTR:
		grp = (lv_group_t *)rv;
		rd[0] = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd[0]->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_udata = (uint64_t)grp->user_data,
			}
		};
		shm_produce_rsp(shm, rd, 1);
		break;
	default:
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
