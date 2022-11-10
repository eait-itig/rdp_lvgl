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
lv_do_call(struct shmintf *shm, struct cdesc *cd)
{
	struct cdesc_call *cdc = &cd->cd_call;
	struct rdesc rd;
	uint64_t rv;
	lv_obj_t *obj;
	/*uint i;*/

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

	switch (cdc->cdc_rettype) {
	case ARG_OBJPTR:
		obj = (lv_obj_t *)rv;
		rd = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
				.rdr_class = (uint64_t)lv_obj_get_class(obj),
				.rdr_udata = (uint64_t)lv_obj_get_user_data(obj),
			}
		};
		shm_produce_rsp(shm, &rd, 1);
		break;
	default:
		rd = (struct rdesc){
			.rd_error = 0,
			.rd_cookie = cd->cd_cookie,
			.rd_return = (struct rdesc_return){
				.rdr_val = rv,
			}
		};
		shm_produce_rsp(shm, &rd, 1);
	}
}
