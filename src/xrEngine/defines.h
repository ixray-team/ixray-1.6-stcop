#pragma once

#ifdef ENGINE_API
#ifdef	DEBUG_DRAW
	ENGINE_API	extern BOOL			bDebug;
#else
	#define bDebug 0
#endif

#define _RELEASE(x)			{ if(x) { (x)->Release();       (x)=NULL; } }
#define _SHOW_REF(msg, x)   { if(x) { x->AddRef(); Msg("%s %d", msg,u32(x->Release()));}}

// textures
ENGINE_API extern	int		psTextureLOD		;

// psGameFlags
enum {
	rsActorShadow					= (1ul<<0ul),
	rsDrawPortals					= (1ul<<1ul),
};


ENGINE_API extern	u32			psCurrentBPP		;
ENGINE_API extern	Flags32		psGameFlags			;

#endif 