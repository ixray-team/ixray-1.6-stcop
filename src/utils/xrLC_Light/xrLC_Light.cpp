// xrLC_Light.cpp : Defines the entry point for the DLL application.
//
#include "stdafx.h"
#include "xrLc_globaldata.h"

#ifdef _MANAGED
#pragma managed(push, off)
#endif

b_params	&g_params()
{
	VERIFY(inlc_global_data());
	return inlc_global_data()->g_params();
}

#ifdef _MANAGED
#pragma managed(pop)
#endif

