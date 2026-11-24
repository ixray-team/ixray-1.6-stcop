#ifndef TSS_H
#define TSS_H
#pragma once

#include "tss_def.h"

enum	XRDX10RENDERSTATETYPE
{
	XRDX10RS_ALPHATOCOVERAGE		=	1024
};

class  CSimulator
{
public:
	SimulatorStates		container;
public:
	IC void				Invalidate	()									{ container.clear(); }
	IC void				SetRS(u32 I, u32 N, u32 V) { container.set_RS(N, V, I); }
	IC void				SetRS(u32 N, u32 V) { SetRS(u32(-1), N, V); }

	IC SimulatorStates& GetContainer() { return container; }
};

#endif
