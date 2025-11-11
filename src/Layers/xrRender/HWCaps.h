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
	BOOL			bForceGPU_REF;
	BOOL			bForceGPU_SW;
	BOOL			bForceGPU_NonPure;
	BOOL			SceneMode;

	u32				iGPUNum;

	// device format
	u32				dwRefreshRate;

	// caps itself
	caps_Geometry	geometry		;
	caps_Raster		raster			;

	u32				id_device		;

	BOOL			bStencil;			// stencil buffer present
	BOOL			bScissor;			// scissor rect supported
	BOOL			bTableFog;			//

	void			Update(void);
};

extern ECORE_API CHWCaps Caps;
#endif
