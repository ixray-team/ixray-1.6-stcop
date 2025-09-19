#pragma once
#pragma warning (disable:4661)
#include "xrLC_Light.h"

#ifndef __CUDACC__
#	include "../xrForms/cl_log.h"
#endif

#ifdef DEBUG
#define CL_NET_LOG
#endif