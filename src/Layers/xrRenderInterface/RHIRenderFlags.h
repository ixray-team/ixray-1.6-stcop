#pragma once

#define _RELEASE(x)			{ if(x) { (x)->Release();       (x)=NULL; } }
#define _SHOW_REF(msg, x)   { if(x) { x->AddRef(); Msg("%s %d", msg,u32(x->Release()));}}

// psDeviceFlags
enum 
{
	rsFullscreen = (1ul << 0ul),
	rsClearBB = (1ul << 1ul),
	rsVSync = (1ul << 2ul),
	rsWireframe = (1ul << 3ul),
	rsOcclusion = (1ul << 4ul),
	rsStatistic = (1ul << 5ul),
	rsDetails = (1ul << 6ul),
	rsRefresh60hz = (1ul << 7ul),
	rsConstantFPS = (1ul << 8ul),
	rsDrawStatic = (1ul << 9ul),
	rsDrawDynamic = (1ul << 10ul),
	rsDisableObjectsAsCrows = (1ul << 11ul),

	rsOcclusionDraw = (1ul << 12ul),
	rsOcclusionStats = (1ul << 13ul),

	mtSound = (1ul << 14ul),
	mtPhysics = (1ul << 15ul),
	mtNetwork = (1ul << 16ul),
	mtParticles = (1ul << 17ul),

	rsCameraPos = (1ul << 18ul),
	rsR2 = (1ul << 19ul),
	rsR4 = (1ul << 20ul),

	rsDeviceActive = (1ul << 21ul),
	// 22-32 bit - reserved to Editor
};

// psGameFlags
enum 
{
	rsActorShadow = (1ul << 0ul),
	rsDrawPortals = (1ul << 1ul),
};

// release version always has "mt_*" enabled
extern RHI_API Flags32 psDeviceFlags;
extern RHI_API Flags32 psGameFlags;