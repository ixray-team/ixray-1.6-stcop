#include "stdafx.h"

#include "BlenderDefault.h"
#include "Blender_default_aref.h"
#include "Blender_Vertex.h"
#include "Blender_Vertex_aref.h"
#include "../xrRender/Blender_Screen_SET.h"
#include "Blender_Screen_GRAY.h"
#include "../xrRender/Blender_Editor_Wire.h"
#include "../xrRender/Blender_Editor_Selection.h"
#include "Blender_LaEmB.h"
#include "../xrRender/Blender_Lm(EbB).h"
#include "../xrRender/Blender_BmmD.h"
#include "Blender_Shadow_World.h"
#include "Blender_Blur.h"
#include "Blender_Model.h"
#include "../xrRender/Blender_Model_EbB.h"
#include "../xrRender/Blender_detail_still.h"
#include "../xrRender/Blender_tree.h"
#include "../xrRender/Blender_Particle.h"

IBlender*	CRender::blender_create	(CLASS_ID cls)
{	
	switch (cls)
	{
	case B_DEFAULT:			return new CBlender_default			();		
	case B_DEFAULT_AREF:	return new CBlender_default_aref	();	
	case B_VERT:			return new CBlender_Vertex			();		
	case B_VERT_AREF:		return new CBlender_Vertex_aref		();	
	case B_SCREEN_SET:		return new CBlender_Screen_SET		();	
	case B_SCREEN_GRAY:		return new CBlender_Screen_GRAY		();	
	case B_EDITOR_WIRE:		return new CBlender_Editor_Wire		();	
	case B_EDITOR_SEL:		return new CBlender_Editor_Selection();
	case B_LaEmB:			return new CBlender_LaEmB			();		
	case B_LmEbB:			return new CBlender_LmEbB			();		
	case B_BmmD:			return new CBlender_BmmD			();			
	case B_SHADOW_WORLD:	return new CBlender_ShWorld			();		
	case B_BLUR:			return new CBlender_Blur			();			
	case B_MODEL:			return new CBlender_Model			();		
	case B_MODEL_EbB:		return new CBlender_Model_EbB		();	
	case B_DETAIL:			return new CBlender_Detail_Still	();	
	case B_TREE:			return new CBlender_Tree			();	
	case B_PARTICLE:		return new CBlender_Particle		();
	}
	return 0;
}