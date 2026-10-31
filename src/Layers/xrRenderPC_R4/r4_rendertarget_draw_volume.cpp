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
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_SPHERE_NUMFACES * 3, 0, 0);
		break;
	case IRender_Light::SPOT		:
		Cmd.SetGeometry		(g_accum_spot);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_CONE_NUMFACES * 3, 0, 0);
		break;
	case IRender_Light::OMNIPART	:	
		Cmd.SetGeometry		(g_accum_omnipart);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, DU_SPHERE_PART_NUMFACES * 3, 0, 0);
		break;
	default:
		break;
	}
}
