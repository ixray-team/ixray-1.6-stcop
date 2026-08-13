#include "stdafx.h"

#include "base_face.h"

#include "tcf.h"
#include "xrLC_GlobalData.h"
#include "utils/xrLC/Build.h"
#include "xrEngine/Shader_xrLC.h"

Fvector2* base_Face::getTC0()
{
	return tc[0].uv;
}

base_Face::base_Face()
{
	basis_tangent[0].set	(0,0,0);
	basis_tangent[1].set	(0,0,0);
	basis_tangent[2].set	(0,0,0);
	basis_binormal[0].set	(0,0,0);
	basis_binormal[1].set	(0,0,0);
	basis_binormal[2].set	(0,0,0);
}

bool base_Face::RenderEqualTo(Tface<base_Vertex>* F)
{
	if (F->dwMaterial != dwMaterial || F->flags.bSharedMaterial != flags.bSharedMaterial)
	{
		return false;
	}
	return true;
}

void base_Face::AddChannel(Fvector2& p1, Fvector2& p2, Fvector2& p3)
{
	_TCF TC;
	TC.uv[0] = p1;
	TC.uv[1] = p2;
	TC.uv[2] = p3;
	tc.push_back(TC);
}

bool base_Face::hasImplicitLighting() const
{
	if (!Shader().flags.bRendering)
	{
		return false;
	}
	VERIFY( inlc_global_data() );
	auto& T = CBuild::GetTexture(dwMaterial, flags.bSharedMaterial);
	return (T.THM.flags.test(STextureParams::flImplicitLighted));
};