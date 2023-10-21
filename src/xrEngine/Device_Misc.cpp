#include "stdafx.h"

// *****************************************************************************************
// Error handling

//----------------------------- FLAGS
static struct _DF {
	const char *	name;
	u32	mask;
} DF[] = {
	{"rsFullscreen",	rsFullscreen	},
	{"rsClearBB",		rsClearBB 		},
	{"rsVSync",			rsVSync 		},
	{"rsWireframe",		rsWireframe		},
    {NULL,0}
};

void CRenderDevice::DumpFlags()
{
	EngineLog("- Dumping device flags");
	_DF *p = DF;
	while (p->name) {
		EngineLog("* {} {}",p->name,psDeviceFlags.test(p->mask)?"on":"off");
		p++;
	}
}
