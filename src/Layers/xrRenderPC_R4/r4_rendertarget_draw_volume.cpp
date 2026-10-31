#include "stdafx.h"
#include "du_sphere_part.h"
#include "du_cone.h"
#include "du_sphere.h"

void CRenderTarget::draw_volume		(GfxCmdList& Cmd, light* L)
{
	switch(L->flags.type) {
	case IRender_Light::REFLECTED	:
	case IRender_Light::POINT		:
		Cmd.SetGeometry		(g_accum_point);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_SPHERE_NUMFACES * 3, g_accum_point_alloc.IndexBase, g_accum_point_alloc.VertexBase);
		break;
	case IRender_Light::SPOT		:
		Cmd.SetGeometry		(g_accum_spot);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_CONE_NUMFACES * 3, g_accum_spot_alloc.IndexBase, g_accum_spot_alloc.VertexBase);
		break;
	case IRender_Light::OMNIPART	:	
		Cmd.SetGeometry		(g_accum_omnipart);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_SPHERE_PART_NUMFACES * 3, g_accum_omnip_alloc.IndexBase, g_accum_omnip_alloc.VertexBase);
		break;
	default:
		break;
	}
}
