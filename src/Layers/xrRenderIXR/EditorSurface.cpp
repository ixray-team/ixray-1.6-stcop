/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "EditorSurface.h"
#include "r1_blender_tex.h"

namespace
{
	// The SDK names maps it does not ship: a surface takes one only where its file is there
	bool IsShipped(const shared_str& Name)
	{
		string_path Path;
		return Name.size() && FS.exist(Path, _game_textures_, Name.c_str(), ".dds");
	}
}

bool IsSurfaceBlender(IBlender* Blender)
{
	static const CLASS_ID Surfaces[] = { B_DEFAULT, B_DEFAULT_AREF, B_VERT, B_VERT_AREF, B_LmBmmD, B_LaEmB, B_LmEbB, B_B, B_BmmD, B_TREE, B_MODEL, B_MODEL_EbB };
	const CLASS_ID Class = Blender ? Blender->getDescription().CLS : CLASS_ID(0);
	return std::find(std::begin(Surfaces), std::end(Surfaces), Class) != std::end(Surfaces);
}

void CompileEditorSurface(CBlender_Compile& C)
{
	C.BT->IBlender::Compile(C);
	const IBlender::SurfaceAlpha Alpha = C.BT->GetSurfaceAlpha();

	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
	case ShaderVariant::NormalLQ:
	{
		const bool IsDetailed = C.bDetail_Diffuse && C.detail_texture;
		C.bDetail_Bump = C.bDetail_Bump && IsDetailed && IsShipped(DEV->m_textures_description.GetBumpName(C.detail_texture));
		const R1Bump Bump(C, IsDetailed ? "model_def_hq_dt" : "model_def_hq", IsShipped(DEV->m_textures_description.GetBumpName(C.L_textures[0])));
		C.r_Pass(Bump.Name, Bump.Name, true, true, Alpha.IsDepthWritten, Alpha.IsBlended, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, Alpha.IsTested, Alpha.Ref);
		r1_tex(C, "s_base", C.L_textures[0]);

		if (IsDetailed)
		{
			r1_tex(C, "s_detail", C.detail_texture);
		}

		Bump.Bind(C);
		C.r_End();
		break;
	}
	case ShaderVariant::Shadow:
		if (Alpha.IsTested && Alpha.Ref)
		{
			C.AddDefine("USE_AREF", "1");
		}

		C.r_Pass("shadow_model", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
