#ifndef IRENDERABLE_H_INCLUDED
#define IRENDERABLE_H_INCLUDED

#include "Render.h"

//////////////////////////////////////////////////////////////////////////
// definition ("Renderable")
class ENGINE_API IRenderable:
	public ISpatialOwner
{
public:
	struct 
	{
		Fmatrix							xform						;
		IRenderVisual*					visual						;
		IRender_ObjectSpecific*			pROS						;
		bool							pROS_Allowed				;
	}	renderable;

public:
										IRenderable					();
	virtual								~IRenderable				();
	IRender_ObjectSpecific*				renderable_ROS				()	;

	virtual	void						renderable_Render			()	= 0;
	virtual	bool						renderable_ShadowGenerate	()	{ return false; };
	virtual	bool						renderable_ShadowReceive	()	{ return false; };


	virtual IRenderable* dcast_Renderable() override { return this; }
};

#endif // IRENDERABLE_H_INCLUDED