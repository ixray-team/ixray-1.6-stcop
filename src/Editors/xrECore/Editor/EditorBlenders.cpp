#include "stdafx.h"

// R1 blender classes of shaders.xr that the game renderer does not implement. The editors
// create them so the Shader Editor keeps these shaders when it saves the library; every other
// class comes from the renderer (see CRender::blender_create).
#include "../../../Layers/xrRender/Blender_Screen_GRAY.h"
#include "../../../Layers/xrRender/Blender_LaEmB.h"
#include "../../../Layers/xrRender/Blender_Shadow_World.h"
#include "../../../Layers/xrRender/Blender_Blur.h"

IBlender* CreateEditorBlender(CLASS_ID Class)
{
	switch (Class)
	{
	case B_SCREEN_GRAY:		return new CBlender_Screen_GRAY();
	case B_LaEmB:			return new CBlender_LaEmB();
	case B_SHADOW_WORLD:	return new CBlender_ShWorld();
	case B_BLUR:			return new CBlender_Blur();
	}

	return nullptr;
}