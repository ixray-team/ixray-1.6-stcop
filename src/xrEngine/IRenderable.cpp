#include "stdafx.h"
#include "../xrCDB/ISpatial.h"
#include "IRenderable.h"
#include "ICollidable.h"

IRenderable::IRenderable()
{
	SpatialComponent = xr_make_shared<ISpatial>(g_SpatialSpace, this);
	SpatialComponent->spatial.type |= STYPE_RENDERABLE;

	renderable.xform.identity			();
	renderable.visual					= nullptr;
	renderable.pROS						= nullptr;
	renderable.pROS_Allowed				= TRUE;
}

extern ENGINE_API xr_atomic_bool g_bRendering; 
IRenderable::~IRenderable()
{
	VERIFY								(!g_bRendering); 
	Render->model_Delete				(renderable.visual);
	if (renderable.pROS)				Render->ros_destroy					(renderable.pROS);
	renderable.visual					= nullptr;
	renderable.pROS						= nullptr;
}

IRender_ObjectSpecific*				IRenderable::renderable_ROS				()	
{
	if (0==renderable.pROS && renderable.pROS_Allowed)		renderable.pROS	= Render->ros_create(this);
	return renderable.pROS	;
}
