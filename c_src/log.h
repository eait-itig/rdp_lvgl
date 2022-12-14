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

#if !defined(_LOG_H)
#define _LOG_H

#include <unistd.h>
#include <pthread.h>

#include "erl_nif.h"

void log_setup(void);
void log_take_ownership(ErlNifPid owner);
void log_post_fork(const char *role, pid_t newpid);

enum log_level {
	LOG_DEBUG,
	LOG_WARN,
	LOG_ERROR
};

void _log_write(enum log_level lvl, const char *func, const char *file,
    uint line, const char *fmt, ...);

#if !defined(__FILE_NAME__)
#define __FILE_NAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)
#endif /* !__FILE_NAME__ */

#define log_debug(fmt...)	\
	_log_write(LOG_DEBUG, __func__, __FILE_NAME__, __LINE__, fmt)
#define log_warn(fmt...)	\
	_log_write(LOG_WARN,  __func__, __FILE_NAME__, __LINE__, fmt)
#define log_error(fmt...) 	\
	_log_write(LOG_ERROR, __func__, __FILE_NAME__, __LINE__, fmt)

#endif /* !_LOG_H */
