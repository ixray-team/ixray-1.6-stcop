#ifndef _HW_CAPS_
#define _HW_CAPS_
#pragma once

#define	CAP_VERSION(a,b)	(u32(a)*10 + u32(b))

class  CHWCaps
{
public:
	enum
	{
		MAX_GPUS		= 8
	};

public:
	struct		caps_Geometry
	{
		u32	dwRegisters		: 16;
	};
	struct		caps_Raster
	{
		u32	dwRegisters		: 16;
		u32	dwStages		: 4;		// number of tex-stages
	};
public:
	// force flags
	bool			bForceGPU_REF;
	bool			bForceGPU_SW;
	bool			bForceGPU_NonPure;
	bool			SceneMode;

	u32				iGPUNum;

	// device format
	u32				dwRefreshRate;

	// caps itself
	caps_Geometry	geometry		;
	caps_Raster		raster			;

	u32				id_device		;

	bool			bStencil;			// stencil buffer present
	bool			bScissor;			// scissor rect supported
	bool			bTableFog;			//

	void			Update(void);
};

extern ECORE_API CHWCaps Caps;
#endif
