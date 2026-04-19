#include "stdafx.h"

#ifdef DEBUG_DRAW
ECORE_API bool bDebug = false;
#endif

// Video
u32 psCurrentBPP = 32;
Flags32 psGameFlags = { rsActorShadow };

// textures 
int psTextureLOD = 0;
