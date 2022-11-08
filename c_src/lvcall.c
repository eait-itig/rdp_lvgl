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
#include "lvcall_gen.h"

static uint64_t lv_do_real_call(const struct cdesc_call *cdc);

void
lv_do_call(struct shmintf *shm, struct cdesc *cd)
{
	struct cdesc_call *cdc = &cd->cd_call;
	struct rdesc rd;
	uint64_t rv;
	lv_obj_t *obj;

	rv = lv_do_real_call(cdc);

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
static uint64_t
lv_do_real_call(const struct cdesc_call *cdc)
{
	void *retp, *a0p, *a1p, *a2p, *a3p;
	uint64_t retq, a0q, a1q, a2q, a3q;
	uint32_t retl, a0l, a1l, a2l, a3l;
	uint16_t retw, a0w, a1w, a2w, a3w;
	uint8_t retc, a0c, a1c, a2c, a3c;
	switch (cdc->cdc_argtype[0]) {
	case ARG_NONE:
		switch (cdc->cdc_rettype) {
		case ARG_NONE:
			(*(lv_call_func0_v_t)cdc->cdc_func)(
			    );
			return (0);
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			retp = (*(lv_call_func0_p_t)cdc->cdc_func)(
			    );
			return ((uint64_t)retp);
		case ARG_UINT64:
			retq = (*(lv_call_func0_q_t)cdc->cdc_func)(
			    );
			return ((uint64_t)retq);
		case ARG_UINT32:
			retl = (*(lv_call_func0_l_t)cdc->cdc_func)(
			    );
			return ((uint64_t)retl);
		case ARG_UINT16:
			retw = (*(lv_call_func0_w_t)cdc->cdc_func)(
			    );
			return ((uint64_t)retw);
		case ARG_UINT8:
			retc = (*(lv_call_func0_c_t)cdc->cdc_func)(
			    );
			return ((uint64_t)retc);
		}
		break;
	case ARG_BUFPTR:
	case ARG_OBJPTR:
	case ARG_PTR:
		a0p = (void *)cdc->cdc_arg[0];
		switch (cdc->cdc_argtype[1]) {
		case ARG_NONE:
			switch (cdc->cdc_rettype) {
			case ARG_NONE:
				(*(lv_call_func1_vp_t)cdc->cdc_func)(
				    a0p);
				return (0);
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				retp = (*(lv_call_func1_pp_t)cdc->cdc_func)(
				    a0p);
				return ((uint64_t)retp);
			case ARG_UINT64:
				retq = (*(lv_call_func1_qp_t)cdc->cdc_func)(
				    a0p);
				return ((uint64_t)retq);
			case ARG_UINT32:
				retl = (*(lv_call_func1_lp_t)cdc->cdc_func)(
				    a0p);
				return ((uint64_t)retl);
			case ARG_UINT16:
				retw = (*(lv_call_func1_wp_t)cdc->cdc_func)(
				    a0p);
				return ((uint64_t)retw);
			case ARG_UINT8:
				retc = (*(lv_call_func1_cp_t)cdc->cdc_func)(
				    a0p);
				return ((uint64_t)retc);
			}
			break;
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			a1p = (void *)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vpp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_ppp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qpp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lpp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wpp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cpp_t)cdc->cdc_func)(
					    a0p,
					    a1p);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpppp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cppq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppqp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppqq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppql_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppqw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppqc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cppl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpplp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpplq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppll_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpplw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpplc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cppw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppwp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppwq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppwl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppww_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppwc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cppc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppcp_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppcq_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppcl_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppcw_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cppcc_t)cdc->cdc_func)(
						    a0p,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT64:
			a1q = (uint64_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vpq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_ppq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qpq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lpq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wpq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cpq_t)cdc->cdc_func)(
					    a0p,
					    a1q);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqpp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqpq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqpl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqpw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqpc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqqp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqqq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpql_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqlp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqlq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqll_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqlw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqlc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpqw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqwp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqwq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqwl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqww_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqwc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpqc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqcp_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqcq_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqcl_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqcw_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpqcc_t)cdc->cdc_func)(
						    a0p,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT32:
			a1l = (uint32_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vpl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_ppl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qpl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lpl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wpl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cpl_t)cdc->cdc_func)(
					    a0p,
					    a1l);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cplp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplpp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplpq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplpl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplpw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplpc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cplq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplqp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplqq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplql_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplqw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplqc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpllp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpllq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplll_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpllw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpllc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cplw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplwp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplwq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplwl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplww_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplwc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cplc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplcp_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplcq_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplcl_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplcw_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cplcc_t)cdc->cdc_func)(
						    a0p,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT16:
			a1w = (uint16_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vpw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_ppw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qpw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lpw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wpw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cpw_t)cdc->cdc_func)(
					    a0p,
					    a1w);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwpp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwpq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwpl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwpw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwpc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwqp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwqq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwql_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwqw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwqc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwlp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwlq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwll_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwlw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwlc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwwp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwwq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwwl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwww_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpwc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwcp_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwcq_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwcl_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwcw_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpwcc_t)cdc->cdc_func)(
						    a0p,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT8:
			a1c = (uint8_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vpc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_ppc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qpc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lpc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wpc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cpc_t)cdc->cdc_func)(
					    a0p,
					    a1c);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpcp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcpp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcpq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcpl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcpw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcpc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpcq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcqp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcqq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcql_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcqw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcqc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpcl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpclp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpclq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcll_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpclw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpclc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpcw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcwp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcwq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcwl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcww_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpcwc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vpcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_ppcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qpcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lpcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wpcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cpcc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpccp_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpccq_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpccl_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpccw_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vpccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_ppccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qpccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lpccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wpccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cpccc_t)cdc->cdc_func)(
						    a0p,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		}
		break;
	case ARG_UINT64:
		a0q = (uint64_t)cdc->cdc_arg[0];
		switch (cdc->cdc_argtype[1]) {
		case ARG_NONE:
			switch (cdc->cdc_rettype) {
			case ARG_NONE:
				(*(lv_call_func1_vq_t)cdc->cdc_func)(
				    a0q);
				return (0);
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				retp = (*(lv_call_func1_pq_t)cdc->cdc_func)(
				    a0q);
				return ((uint64_t)retp);
			case ARG_UINT64:
				retq = (*(lv_call_func1_qq_t)cdc->cdc_func)(
				    a0q);
				return ((uint64_t)retq);
			case ARG_UINT32:
				retl = (*(lv_call_func1_lq_t)cdc->cdc_func)(
				    a0q);
				return ((uint64_t)retl);
			case ARG_UINT16:
				retw = (*(lv_call_func1_wq_t)cdc->cdc_func)(
				    a0q);
				return ((uint64_t)retw);
			case ARG_UINT8:
				retc = (*(lv_call_func1_cq_t)cdc->cdc_func)(
				    a0q);
				return ((uint64_t)retc);
			}
			break;
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			a1p = (void *)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cqp_t)cdc->cdc_func)(
					    a0q,
					    a1p);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqpp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqppp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqppq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqppl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqppw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqppc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqpq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpqp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpqq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpql_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpqw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpqc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqpl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqplp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqplq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpll_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqplw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqplc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqpw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpwp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpwq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpwl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpww_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpwc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqpc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpcp_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpcq_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpcl_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpcw_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqpcc_t)cdc->cdc_func)(
						    a0q,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT64:
			a1q = (uint64_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cqq_t)cdc->cdc_func)(
					    a0q,
					    a1q);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqpp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqpq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqpl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqpw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqpc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqqp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqqq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqql_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqlp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqlq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqll_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqlw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqlc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqqw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqwp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqwq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqwl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqww_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqwc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqqc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqcp_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqcq_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqcl_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqcw_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqqcc_t)cdc->cdc_func)(
						    a0q,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT32:
			a1l = (uint32_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cql_t)cdc->cdc_func)(
					    a0q,
					    a1l);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqlp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlpp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlpq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlpl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlpw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlpc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqlq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlqp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlqq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlql_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlqw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlqc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqllp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqllq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlll_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqllw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqllc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqlw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlwp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlwq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlwl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlww_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlwc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqlc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlcp_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlcq_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlcl_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlcw_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqlcc_t)cdc->cdc_func)(
						    a0q,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT16:
			a1w = (uint16_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cqw_t)cdc->cdc_func)(
					    a0q,
					    a1w);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwpp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwpq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwpl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwpw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwpc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwqp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwqq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwql_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwqw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwqc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwlp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwlq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwll_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwlw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwlc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwwp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwwq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwwl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwww_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqwc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwcp_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwcq_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwcl_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwcw_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqwcc_t)cdc->cdc_func)(
						    a0q,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT8:
			a1c = (uint8_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cqc_t)cdc->cdc_func)(
					    a0q,
					    a1c);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqcp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcpp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcpq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcpl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcpw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcpc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqcq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcqp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcqq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcql_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcqw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcqc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqcl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqclp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqclq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcll_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqclw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqclc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqcw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcwp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcwq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcwl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcww_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqcwc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cqcc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqccp_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqccq_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqccl_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqccw_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cqccc_t)cdc->cdc_func)(
						    a0q,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		}
		break;
	case ARG_UINT32:
		a0l = (uint32_t)cdc->cdc_arg[0];
		switch (cdc->cdc_argtype[1]) {
		case ARG_NONE:
			switch (cdc->cdc_rettype) {
			case ARG_NONE:
				(*(lv_call_func1_vl_t)cdc->cdc_func)(
				    a0l);
				return (0);
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				retp = (*(lv_call_func1_pl_t)cdc->cdc_func)(
				    a0l);
				return ((uint64_t)retp);
			case ARG_UINT64:
				retq = (*(lv_call_func1_ql_t)cdc->cdc_func)(
				    a0l);
				return ((uint64_t)retq);
			case ARG_UINT32:
				retl = (*(lv_call_func1_ll_t)cdc->cdc_func)(
				    a0l);
				return ((uint64_t)retl);
			case ARG_UINT16:
				retw = (*(lv_call_func1_wl_t)cdc->cdc_func)(
				    a0l);
				return ((uint64_t)retw);
			case ARG_UINT8:
				retc = (*(lv_call_func1_cl_t)cdc->cdc_func)(
				    a0l);
				return ((uint64_t)retc);
			}
			break;
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			a1p = (void *)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vlp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_plp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qlp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_llp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wlp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_clp_t)cdc->cdc_func)(
					    a0l,
					    a1p);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clpp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clppp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clppq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clppl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clppw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clppc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clpq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpqp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpqq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpql_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpqw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpqc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clpl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clplp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clplq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpll_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clplw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clplc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clpw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpwp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpwq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpwl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpww_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpwc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clpc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpcp_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpcq_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpcl_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpcw_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clpcc_t)cdc->cdc_func)(
						    a0l,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT64:
			a1q = (uint64_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vlq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_plq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qlq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_llq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wlq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_clq_t)cdc->cdc_func)(
					    a0l,
					    a1q);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqpp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqpq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqpl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqpw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqpc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqqp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqqq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clql_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqlp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqlq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqll_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqlw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqlc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clqw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqwp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqwq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqwl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqww_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqwc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clqc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqcp_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqcq_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqcl_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqcw_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clqcc_t)cdc->cdc_func)(
						    a0l,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT32:
			a1l = (uint32_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cll_t)cdc->cdc_func)(
					    a0l,
					    a1l);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllpp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllpq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllpl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllpw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllpc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllqp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllqq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllql_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllqw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllqc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clllp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clllq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllll_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cllw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllwp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllwq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllwl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllww_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllwc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cllc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllcp_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllcq_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllcl_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllcw_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cllcc_t)cdc->cdc_func)(
						    a0l,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT16:
			a1w = (uint16_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vlw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_plw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qlw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_llw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wlw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_clw_t)cdc->cdc_func)(
					    a0l,
					    a1w);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwpp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwpq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwpl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwpw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwpc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwqp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwqq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwql_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwqw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwqc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwlp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwlq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwll_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwlw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwlc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwwp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwwq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwwl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwww_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clwc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwcp_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwcq_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwcl_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwcw_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clwcc_t)cdc->cdc_func)(
						    a0l,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT8:
			a1c = (uint8_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vlc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_plc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qlc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_llc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wlc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_clc_t)cdc->cdc_func)(
					    a0l,
					    a1c);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clcp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcpp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcpq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcpl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcpw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcpc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clcq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcqp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcqq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcql_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcqw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcqc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clcl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clclp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clclq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcll_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clclw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clclc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clcw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcwp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcwq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcwl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcww_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clcwc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vlcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_plcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qlcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_llcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wlcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_clcc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clccp_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clccq_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clccl_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clccw_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vlccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_plccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qlccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_llccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wlccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_clccc_t)cdc->cdc_func)(
						    a0l,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		}
		break;
	case ARG_UINT16:
		a0w = (uint16_t)cdc->cdc_arg[0];
		switch (cdc->cdc_argtype[1]) {
		case ARG_NONE:
			switch (cdc->cdc_rettype) {
			case ARG_NONE:
				(*(lv_call_func1_vw_t)cdc->cdc_func)(
				    a0w);
				return (0);
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				retp = (*(lv_call_func1_pw_t)cdc->cdc_func)(
				    a0w);
				return ((uint64_t)retp);
			case ARG_UINT64:
				retq = (*(lv_call_func1_qw_t)cdc->cdc_func)(
				    a0w);
				return ((uint64_t)retq);
			case ARG_UINT32:
				retl = (*(lv_call_func1_lw_t)cdc->cdc_func)(
				    a0w);
				return ((uint64_t)retl);
			case ARG_UINT16:
				retw = (*(lv_call_func1_ww_t)cdc->cdc_func)(
				    a0w);
				return ((uint64_t)retw);
			case ARG_UINT8:
				retc = (*(lv_call_func1_cw_t)cdc->cdc_func)(
				    a0w);
				return ((uint64_t)retc);
			}
			break;
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			a1p = (void *)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cwp_t)cdc->cdc_func)(
					    a0w,
					    a1p);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwpp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwppp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwppq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwppl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwppw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwppc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwpq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpqp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpqq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpql_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpqw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpqc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwpl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwplp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwplq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpll_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwplw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwplc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwpw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpwp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpwq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpwl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpww_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpwc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwpc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpcp_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpcq_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpcl_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpcw_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwpcc_t)cdc->cdc_func)(
						    a0w,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT64:
			a1q = (uint64_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cwq_t)cdc->cdc_func)(
					    a0w,
					    a1q);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqpp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqpq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqpl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqpw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqpc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqqp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqqq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwql_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqlp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqlq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqll_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqlw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqlc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwqw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqwp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqwq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqwl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqww_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqwc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwqc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqcp_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqcq_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqcl_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqcw_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwqcc_t)cdc->cdc_func)(
						    a0w,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT32:
			a1l = (uint32_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cwl_t)cdc->cdc_func)(
					    a0w,
					    a1l);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwlp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlpp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlpq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlpl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlpw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlpc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwlq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlqp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlqq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlql_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlqw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlqc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwllp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwllq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlll_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwllw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwllc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwlw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlwp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlwq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlwl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlww_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlwc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwlc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlcp_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlcq_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlcl_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlcw_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwlcc_t)cdc->cdc_func)(
						    a0w,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT16:
			a1w = (uint16_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vww_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pww_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qww_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lww_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_www_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cww_t)cdc->cdc_func)(
					    a0w,
					    a1w);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwpp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwpq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwpl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwpw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwpc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwqp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwqq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwql_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwqw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwqc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwlp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwlq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwll_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwlw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwlc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwwp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwwq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwwl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwww_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwwc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwcp_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwcq_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwcl_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwcw_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwwcc_t)cdc->cdc_func)(
						    a0w,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT8:
			a1c = (uint8_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_cwc_t)cdc->cdc_func)(
					    a0w,
					    a1c);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwcp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcpp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcpq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcpl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcpw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcpc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwcq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcqp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcqq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcql_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcqw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcqc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwcl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwclp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwclq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcll_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwclw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwclc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwcw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcwp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcwq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcwl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcww_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwcwc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cwcc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwccp_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwccq_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwccl_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwccw_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cwccc_t)cdc->cdc_func)(
						    a0w,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		}
		break;
	case ARG_UINT8:
		a0c = (uint8_t)cdc->cdc_arg[0];
		switch (cdc->cdc_argtype[1]) {
		case ARG_NONE:
			switch (cdc->cdc_rettype) {
			case ARG_NONE:
				(*(lv_call_func1_vc_t)cdc->cdc_func)(
				    a0c);
				return (0);
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				retp = (*(lv_call_func1_pc_t)cdc->cdc_func)(
				    a0c);
				return ((uint64_t)retp);
			case ARG_UINT64:
				retq = (*(lv_call_func1_qc_t)cdc->cdc_func)(
				    a0c);
				return ((uint64_t)retq);
			case ARG_UINT32:
				retl = (*(lv_call_func1_lc_t)cdc->cdc_func)(
				    a0c);
				return ((uint64_t)retl);
			case ARG_UINT16:
				retw = (*(lv_call_func1_wc_t)cdc->cdc_func)(
				    a0c);
				return ((uint64_t)retw);
			case ARG_UINT8:
				retc = (*(lv_call_func1_cc_t)cdc->cdc_func)(
				    a0c);
				return ((uint64_t)retc);
			}
			break;
		case ARG_BUFPTR:
		case ARG_OBJPTR:
		case ARG_PTR:
			a1p = (void *)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vcp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pcp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qcp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lcp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wcp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_ccp_t)cdc->cdc_func)(
					    a0c,
					    a1p);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccpp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccppp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccppq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccppl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccppw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccppc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccpq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpqp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpqq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpql_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpqw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpqc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccpl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccplp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccplq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpll_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccplw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccplc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccpw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpwp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpwq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpwl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpww_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpwc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccpc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpcp_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpcq_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpcl_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpcw_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccpcc_t)cdc->cdc_func)(
						    a0c,
						    a1p,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT64:
			a1q = (uint64_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vcq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pcq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qcq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lcq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wcq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_ccq_t)cdc->cdc_func)(
					    a0c,
					    a1q);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqpp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqpq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqpl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqpw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqpc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqqp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqqq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccql_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqlp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqlq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqll_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqlw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqlc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccqw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqwp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqwq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqwl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqww_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqwc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccqc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqcp_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqcq_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqcl_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqcw_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccqcc_t)cdc->cdc_func)(
						    a0c,
						    a1q,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT32:
			a1l = (uint32_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vcl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pcl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qcl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lcl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wcl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_ccl_t)cdc->cdc_func)(
					    a0c,
					    a1l);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cclp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclpp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclpq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclpl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclpw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclpc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cclq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclqp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclqq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclql_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclqw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclqc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccllp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccllq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclll_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccllw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccllc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cclw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclwp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclwq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclwl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclww_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclwc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cclc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclcp_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclcq_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclcl_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclcw_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cclcc_t)cdc->cdc_func)(
						    a0c,
						    a1l,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT16:
			a1w = (uint16_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vcw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pcw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qcw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lcw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wcw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_ccw_t)cdc->cdc_func)(
					    a0c,
					    a1w);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwpp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwpq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwpl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwpw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwpc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwqp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwqq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwql_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwqw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwqc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwlp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwlq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwll_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwlw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwlc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwwp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwwq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwwl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwww_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vcwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pcwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qcwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lcwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wcwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_ccwc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwcp_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwcq_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwcl_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwcw_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccwcc_t)cdc->cdc_func)(
						    a0c,
						    a1w,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		case ARG_UINT8:
			a1c = (uint8_t)cdc->cdc_arg[1];
			switch (cdc->cdc_argtype[2]) {
			case ARG_NONE:
				switch (cdc->cdc_rettype) {
				case ARG_NONE:
					(*(lv_call_func2_vcc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return (0);
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					retp = (*(lv_call_func2_pcc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return ((uint64_t)retp);
				case ARG_UINT64:
					retq = (*(lv_call_func2_qcc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return ((uint64_t)retq);
				case ARG_UINT32:
					retl = (*(lv_call_func2_lcc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return ((uint64_t)retl);
				case ARG_UINT16:
					retw = (*(lv_call_func2_wcc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return ((uint64_t)retw);
				case ARG_UINT8:
					retc = (*(lv_call_func2_ccc_t)cdc->cdc_func)(
					    a0c,
					    a1c);
					return ((uint64_t)retc);
				}
				break;
			case ARG_BUFPTR:
			case ARG_OBJPTR:
			case ARG_PTR:
				a2p = (void *)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccpp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccpq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccpl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccpw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccpc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2p,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT64:
				a2q = (uint64_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccqp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccqq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccql_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccqw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccqc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2q,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT32:
				a2l = (uint32_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccclp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccclq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccll_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccclw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccclc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2l,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT16:
				a2w = (uint16_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccwp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccwq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccwl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccww_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_cccwc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2w,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			case ARG_UINT8:
				a2c = (uint8_t)cdc->cdc_arg[2];
				switch (cdc->cdc_argtype[3]) {
				case ARG_NONE:
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func3_vccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func3_pccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func3_qccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func3_lccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func3_wccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func3_cccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c);
						return ((uint64_t)retc);
					}
					break;
				case ARG_BUFPTR:
				case ARG_OBJPTR:
				case ARG_PTR:
					a3p = (void *)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccccp_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3p);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT64:
					a3q = (uint64_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccccq_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3q);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT32:
					a3l = (uint32_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccccl_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3l);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT16:
					a3w = (uint16_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccccw_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3w);
						return ((uint64_t)retc);
					}
					break;
				case ARG_UINT8:
					a3c = (uint8_t)cdc->cdc_arg[3];
					switch (cdc->cdc_rettype) {
					case ARG_NONE:
						(*(lv_call_func4_vcccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return (0);
					case ARG_BUFPTR:
					case ARG_OBJPTR:
					case ARG_PTR:
						retp = (*(lv_call_func4_pcccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retp);
					case ARG_UINT64:
						retq = (*(lv_call_func4_qcccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retq);
					case ARG_UINT32:
						retl = (*(lv_call_func4_lcccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retl);
					case ARG_UINT16:
						retw = (*(lv_call_func4_wcccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retw);
					case ARG_UINT8:
						retc = (*(lv_call_func4_ccccc_t)cdc->cdc_func)(
						    a0c,
						    a1c,
						    a2c,
						    a3c);
						return ((uint64_t)retc);
					}
					break;
				}
				break;
			}
			break;
		}
		break;
	}
	return (0);
}
