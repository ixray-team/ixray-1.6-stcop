// Blender_Recorder.cpp: implementation of the CBlender_Compile class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "../ResourceManager.h"
#include "Blender_Recorder.h"
#include "Blender.h"

#include "../dxRenderDeviceRender.h"

static int ParseName(const char* N)
{
	if (0==xr_strcmp(N,"$null"))	return -1;
	if (0==xr_strcmp(N,"$base0"))	return	0;
	if (0==xr_strcmp(N,"$base1"))	return	1;
	if (0==xr_strcmp(N,"$base2"))	return	2;
	if (0==xr_strcmp(N,"$base3"))	return	3;
	if (0==xr_strcmp(N,"$base4"))	return	4;
	if (0==xr_strcmp(N,"$base5"))	return	5;
	if (0==xr_strcmp(N,"$base6"))	return	6;
	if (0==xr_strcmp(N,"$base7"))	return	7;
	return -1;
}

void	CBlender_Compile::_cpp_Compile	(ShaderElement* _SH)
{
	SH =			_SH;
	RS.Invalidate	();

	//	TODO: Check if we need such wired system for
	//	base texture name detection. Perhapse it's done for
	//	optimization?

	// Analyze possibility to detail this shader
	detail_texture	= nullptr;
	detail_scaler	= nullptr;
	const char*	base	= nullptr;
	if (bDetail && BT->canBeDetailed())
	{
		// 
		sh_list& lst=	L_textures;
		int id		=	ParseName(BT->oT_Name);
		base		=	BT->oT_Name;
		if (id>=0)	
		{
			if (id>=int(lst.size()))	Debug.fatal(DEBUG_INFO,"Not enought textures for shader. Base texture: '%s'.",*lst[0]);
			base	=	*lst [id];
		}
//.		if (!dxRenderDeviceRender::Instance().Resources->_GetDetailTexture(base,detail_texture,detail_scaler))	bDetail	= false;
		if (!DEV->m_textures_description.GetDetailTexture(base,detail_texture,detail_scaler))	bDetail	= false;
	} 
	else 
	{
		////////////////////
		//	Igor
		//	Need this to correct base to detect steep parallax.
		if (BT->canUseSteepParallax())
		{
			sh_list& lst=	L_textures;
			int id		=	ParseName(BT->oT_Name);
			base		=	BT->oT_Name;
			if (id>=0)	
			{
				if (id>=int(lst.size()))	Debug.fatal(DEBUG_INFO,"Not enought textures for shader. Base texture: '%s'.",*lst[0]);
				base	=	*lst [id];
			}
		}
		//	Igor
		////////////////////

		bDetail	= false;
	}

	// Validate for R1 or R2
	bDetail_Diffuse	= false;
	bDetail_Bump	= false;


	if(bDetail)
	{
		DEV->m_textures_description.GetTextureUsage(base, bDetail_Diffuse, bDetail_Bump);

		//	Detect the alowance of detail bump usage here.
		if (!(ps_r2_ls_flags.test(R2FLAG_DETAIL_BUMP)))
		{
			bDetail_Diffuse |= bDetail_Bump;
			bDetail_Bump = false;
		}
	}

	bUseSteepParallax = DEV->m_textures_description.UseSteepParallax(base) 
		&& BT->canUseSteepParallax();
/*
	if (DEV->m_textures_description.UseSteepParallax(base))
	{
		bool bSteep = BT->canUseSteepParallax();
		DEV->m_textures_description.UseSteepParallax(base);
		bUseSteepParallax = true;
	}
*/	
	TessMethod = 0;

	// Compile
	BT->Compile		(*this);
}

void	CBlender_Compile::SetParams		(int iPriority, bool bStrictB2F)
{
	SH->flags.iPriority		= iPriority;
	SH->flags.bStrictB2F	= bStrictB2F;
	if (bStrictB2F){			
		if (Device.IsEditorMode())
		{
			if (1!=(SH->flags.iPriority/2))
			{
				Log("!If StrictB2F true then Priority must div 2.");
				SH->flags.bStrictB2F	= false;
			}
		}
		else
		{
			VERIFY(1==(SH->flags.iPriority/2));
		}
    }
	//SH->Flags.bLighting		= false;
}

void	CBlender_Compile::PassSET_ZB		(bool bZTest, bool bZWrite, bool bInvertZTest)
{
	RS.SetRS	(D3DRS_ZFUNC,			bZTest?(bInvertZTest?D3DCMP_GREATER:D3DCMP_LESSEQUAL):D3DCMP_ALWAYS);
	RS.SetRS	(D3DRS_ZWRITEENABLE,	BC(bZWrite));
}
void	CBlender_Compile::PassSET_ablend_mode(u32 idx, bool bABlend, u32 abSRC, u32 abDST)
{
	if (bABlend && D3DBLEND_ONE == abSRC && D3DBLEND_ZERO == abDST)
	{
		bABlend = false;
	}

	RS.SetRS(idx, D3DRS_ALPHABLENDENABLE, BC(bABlend));

	RS.SetRS(idx, D3DRS_SRCBLEND, bABlend ? abSRC : D3DBLEND_ONE);
	RS.SetRS(idx, D3DRS_DESTBLEND, bABlend ? abDST : D3DBLEND_ZERO);

	// UI targets need coverage alpha, rather than alpha multiplied by itself.
	const bool IsUIBlend = iElement == ShaderVariant::UI && bABlend;
	RS.SetRS(idx, D3DRS_SRCBLENDALPHA, IsUIBlend ? D3DBLEND_ONE : (bABlend ? abSRC : D3DBLEND_ONE));
	RS.SetRS(idx, D3DRS_DESTBLENDALPHA, bABlend ? abDST : D3DBLEND_ZERO);
}

void CBlender_Compile::PassSET_ablend_mode(bool bABlend, u32 abSRC, u32 abDST)
{
	PassSET_ablend_mode(u32(-1), bABlend, abSRC, abDST);
}

void CBlender_Compile::PassSET_ablend_aref(u32 idx, bool bATest, u32 aRef)
{
	if (bATest)
	{
		RS.SetRS(idx, D3DRS_ALPHAREF, std::min(aRef, 255u));
	}
}

void CBlender_Compile::PassSET_ablend_aref(bool bATest, u32 aRef)
{
	PassSET_ablend_aref(u32(-1), bATest, aRef);
}

void CBlender_Compile::PassSET_Blend(u32 idx, bool bABlend, u32 abSRC, u32 abDST, bool bATest, u32 aRef)
{
	PassSET_ablend_mode(idx, bABlend, abSRC, abDST);
	PassSET_ablend_aref(idx, bATest, aRef);
}

void CBlender_Compile::PassSET_Blend(bool bABlend, u32 abSRC, u32 abDST, bool bATest, u32 aRef)
{
	PassSET_Blend(u32(-1), bABlend, abSRC, abDST, bATest, aRef);
}

void CBlender_Compile::r_Matrix(const char* name)
{
	int id = ParseName(name);
	passMatrices.push_back(DEV->_CreateMatrix((id >= 0 && id < (int)L_matrices.size()) ? *L_matrices[id] : name));
}
