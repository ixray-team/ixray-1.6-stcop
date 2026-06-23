#pragma once

#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/FmeshRender.h"
#include "../../Include/xrRender/RenderVisual.h"

#define VLOAD_NOVERTICES		(1<<0)

// The class itself
class					CKinematicsAnimated;
class					CKinematics;
class					IParticleCustom;

struct IRender_Mesh	
{
	// format
	ref_geom					rm_geom;

	// verts
	IRHIBuffer* p_rm_Vertices;
	u32							vBase;
	u32							vCount;

	// indices
	IRHIBuffer* p_rm_Indices;
	u32							iBase;
	u32							iCount;
	u32							dwPrimitives;

	IRender_Mesh				()				{ p_rm_Vertices=nullptr; p_rm_Indices=nullptr;						}
	virtual ~IRender_Mesh		();
private:
	IRender_Mesh				(const IRender_Mesh& other);
	void	operator=			( const IRender_Mesh& other);
};

