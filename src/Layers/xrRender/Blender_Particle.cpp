#include "stdafx.h"
#include "../../xrEngine/EngineAPI.h"
#include "r1_blender_tex.h"


#include "Blender_Particle.h"

#define					oBlendCount	6

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Particle::CBlender_Particle()
{
	description.CLS		= B_PARTICLE;
	description.version	= 0;
	oBlend.Count		= oBlendCount;
	oBlend.IDselected	= 0;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oClamp.value		= TRUE;
}

CBlender_Particle::~CBlender_Particle()
{
	
}

void	CBlender_Particle::Save	( IWriter& fs	)
{
	IBlender::Save	(fs);

	// Blend mode
	xrP_TOKEN::Item	I;
	xrPWRITE_PROP	(fs,"Blending",	xrPID_TOKEN,     oBlend);
	I.ID = 0; xr_strcpy(I.str,"SET");			fs.w		(&I,sizeof(I));
	I.ID = 1; xr_strcpy(I.str,"BLEND");		fs.w		(&I,sizeof(I));
	I.ID = 2; xr_strcpy(I.str,"ADD");			fs.w		(&I,sizeof(I));
	I.ID = 3; xr_strcpy(I.str,"MUL");			fs.w		(&I,sizeof(I));
	I.ID = 4; xr_strcpy(I.str,"MUL_2X");		fs.w		(&I,sizeof(I));
	I.ID = 5; xr_strcpy(I.str,"ALPHA-ADD");	fs.w		(&I,sizeof(I));
	
	// Params
	xrPWRITE_PROP		(fs,"Texture clamp",xrPID_BOOL,		oClamp);
	xrPWRITE_PROP		(fs,"Alpha ref",	xrPID_INTEGER,	oAREF);
}

void	CBlender_Particle::Load	( IReader& fs, u16 version)
{
	IBlender::Load		(fs,version);

	xrPREAD_PROP		(fs,xrPID_TOKEN,		oBlend);	oBlend.Count =   oBlendCount;
	xrPREAD_PROP		(fs,xrPID_BOOL,			oClamp);
	xrPREAD_PROP		(fs,xrPID_INTEGER,		oAREF);
}


void CBlender_Particle::Compile	(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	if (LightingModeIsStatic() && !C.bEditor)
	{
		if (C.iElement > SE_R1_NORMAL_LQ) return;
		const D3DBLEND src[] = {D3DBLEND_ONE, D3DBLEND_SRCALPHA, D3DBLEND_ONE, D3DBLEND_DESTCOLOR, D3DBLEND_DESTCOLOR, D3DBLEND_SRCALPHA};
		const D3DBLEND dst[] = {D3DBLEND_ZERO, D3DBLEND_INVSRCALPHA, D3DBLEND_ONE, D3DBLEND_ZERO, D3DBLEND_SRCCOLOR, D3DBLEND_ONE};
		VERIFY(oBlend.IDselected < std::size(src));
		C.r_Pass("r1_particle", "r1_particle", oBlend.IDselected == 0, true, oBlend.IDselected == 0, oBlend.IDselected != 0, src[oBlend.IDselected], dst[oBlend.IDselected], true, oBlend.IDselected == 0 ? 200 : 0);
		r1_tex(C, "s_base", C.L_textures[0], !!oClamp.value);
		C.r_End();
		return;
	}

	if (C.bEditor)
	{
		switch (oBlend.IDselected)
		{
		case 0:	C.r_Pass("particle", "particle", TRUE, TRUE, TRUE, FALSE, D3DBLEND_ONE, D3DBLEND_ZERO, TRUE, 200);	break;	// SET
		case 1: C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, TRUE, 0);	break;	// BLEND
		case 2:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE, TRUE, 0);	break;	// ADD
		case 3:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO, TRUE, 0);	break;	// MUL
		case 4:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_DESTCOLOR, D3DBLEND_SRCCOLOR, TRUE, 0);	break;	// MUL_2X
		case 5:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_ONE, TRUE, 0);	break;	// ALPHA-ADD
		}
		C.r_dx10Texture("s_base", C.L_textures[0]);
		u32 hSampler = C.r_dx10Sampler("smp_base");
		if (oClamp.value && (hSampler != (u32)-1))
			C.i_Address(hSampler, D3DTADDRESS_CLAMP);
		C.r_End();
		return;
	}

	switch	(C.iElement) 
	{
		case SE_R2_HUD:
		case SE_R2_NORMAL_HQ:
		case SE_R2_NORMAL_LQ:
		{
			if (C.iElement != SE_R2_NORMAL_HQ)
			{
				RImplementation.addShaderOption("DISABLE_SOFT_PARTICLES", "1");
			}

			switch (oBlend.IDselected)
			{
				case 0:	C.r_Pass("particle", "particle", false, true, false);	break;	// SET
				case 1: C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, TRUE, 0);	break;	// BLEND
				case 2:	C.r_Pass("particle", "particle_add", FALSE, TRUE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE, TRUE, 0);	break;	// ADD
				case 3:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO, TRUE, 0);	break;	// MUL
				case 4:	C.r_Pass("particle", "particle", FALSE, TRUE, FALSE, TRUE, D3DBLEND_DESTCOLOR, D3DBLEND_SRCCOLOR, TRUE, 0);	break;	// MUL_2X
				case 5:	C.r_Pass("particle", "particle_add", FALSE, TRUE, FALSE, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_ONE, TRUE, 0);	break;	// ALPHA-ADD
			};

			C.r_dx10Texture("s_base", C.L_textures[0]);
			C.r_dx10Texture("s_position", "$user$position");

			C.r_dx10Sampler("smp_base");
			C.r_dx10Sampler("smp_linear");
			C.r_dx10Sampler("smp_nofilter");

			C.r_End();
			break;
		}
	};
}
