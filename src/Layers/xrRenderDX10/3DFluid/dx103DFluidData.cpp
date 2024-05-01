#include "stdafx.h"
#include "dx103DFluidData.h"

#include "dx103DFluidManager.h"

namespace
{
	const xr_token	simulation_type_token		[ ]=
	{
		{ "Fog",	dx103DFluidData::ST_FOG		},
		{ "Fire",  	dx103DFluidData::ST_FIRE	},
		{ 0,		0							}
	};

	const xr_token	emitter_type_token			[ ]=
	{
		{ "SimpleGaussian",	dx103DFluidEmitters::ET_SimpleGausian	},
		{ "SimpleDraught",  dx103DFluidEmitters::ET_SimpleDraught	},
		{ 0,		0							}
	};
}

ERHITextureFormat	dx103DFluidData::m_VPRenderTargetFormats[ VP_NUM_TARGETS ] = 
{
	FMT_A16B16G16R16F,	//	VP_VELOCITY0 
	FMT_R16F,			//	VP_PRESSURE
	FMT_R16F			//	VP_COLOR
};

dx103DFluidData::dx103DFluidData()
{
	TextureDesc desc;
	desc.NumMips		= 1;
	desc.Usage			= eUsageRenderTarget;
	desc.Width			=  FluidManager.GetTextureWidth();
	desc.Height			= FluidManager.GetTextureHeight();
	desc.Depth			=  FluidManager.GetTextureDepth();
	desc.TextureType	= eTextureType3D;

	for(int rtIndex=0; rtIndex<VP_NUM_TARGETS; rtIndex++)
	{
		desc.Format = m_VPRenderTargetFormats[rtIndex];
		CreateRTTextureAndViews( rtIndex, desc );
	}
}

dx103DFluidData::~dx103DFluidData()
{
	//	Allow real-time config reload
#ifdef	DEBUG
	FluidManager.DeregisterFluidData(this);
#endif	//	DEBUG

	for(int rtIndex=0; rtIndex<VP_NUM_TARGETS; rtIndex++)
	{
		DestroyRTTextureAndViews( rtIndex );
	}
}

void dx103DFluidData::CreateRTTextureAndViews(int rtIndex, TextureDesc& TexDesc)
{
	// Create the texture
	m_pRTTextures[rtIndex] = g_RenderRHI->CreateAPITexture(&TexDesc, nullptr);
	
	// Create the render target view
	
	RenderTargetCreationDesc DescRT;
	DescRT.Format = TexDesc.Format;
	DescRT.ViewDimension =  RHI_RTV_DIMENSION_TEXTURE3D;
	DescRT.Texture3D.FirstWSlice = 0;
	DescRT.Texture3D.MipSlice = 0;
	DescRT.Texture3D.WSize = TexDesc.Depth;

	m_pRenderTargetViews[rtIndex] = g_RenderRHI->CreateAPIRenderTargetView( m_pRTTextures[rtIndex], &DescRT );

	float color[4] = {0, 0, 0, 0 };
	g_RenderRHI->ClearRenderTargetView( m_pRenderTargetViews[rtIndex], color );
}

void dx103DFluidData::DestroyRTTextureAndViews(int rtIndex)
{
	_RELEASE(m_pRTTextures[rtIndex]);
	_RELEASE(m_pRenderTargetViews[rtIndex]);
}

void dx103DFluidData::Load(IReader *data)
{
	//	Version 3

	xr_string	Profile;
	data->r_string(Profile);

	//	Prepare transform
	data->r( &m_Transform, sizeof(m_Transform) );

	//	Read obstacles
	u32 uiObstCnt = data->r_u32();
	m_Obstacles.reserve(uiObstCnt);
	for(u32 i=0; i<uiObstCnt; ++i)
	{
		Fmatrix		ObstTransform;
		data->r( &ObstTransform, sizeof(ObstTransform) );
		m_Obstacles.push_back(ObstTransform);
	}

	ParseProfile(Profile);
}

void dx103DFluidData::ParseProfile(const xr_string &Profile)
{
	string_path fn;
	FS.update_path(fn, "$game_config$",Profile.c_str());
	
	CInifile	ini(fn,TRUE,TRUE,FALSE);

	Msg("Reading fog volume config: %s", fn);

	m_Settings.m_SimulationType = ST_FOG;
	m_Settings.m_fHemi = 0.2f;
	m_Settings.m_fConfinementScale = 0.06f;
	m_Settings.m_fDecay = 0.994f;
	m_Settings.m_fGravityBuoyancy = 0.0f;

	Fmatrix WorldToFluid;
	{
		Fmatrix InvFluidTranform;
		Fmatrix Scale;
		Fmatrix Translate;
		Fmatrix TranslateScale;
		//	Convert to 0..intDim space since it is used by simulation
		//Scale.scale((float)m_iTextureWidth-1, (float)m_iTextureHeight-1, (float)m_iTextureDepth-1);
		//Translate.translate(0.5, 0.5, 0.5);
		//It seems that y axis is inverted in fluid simulation, so shange maths a bit
		Fvector	vGridDim;
		vGridDim.set( (float)FluidManager.GetTextureWidth(), (float)FluidManager.GetTextureHeight(), (float)FluidManager.GetTextureDepth() );
		Scale.scale(vGridDim.x-1, -(vGridDim.y-1), vGridDim.z-1 );
		Translate.translate(0.5, -0.5, 0.5);
		//	Actually it is mul(Translate, Scale).
		//	Our matrix multiplication is not correct.
		TranslateScale.mul(Scale, Translate);
		InvFluidTranform.invert(m_Transform);
		WorldToFluid.mul(TranslateScale,InvFluidTranform);
	}

	//	Read Volume data
	if (ini.line_exist("volume", "Type"))
		m_Settings.m_SimulationType = (SimulationType)ini.r_token( "volume", "Type", simulation_type_token);

	if (ini.line_exist("volume", "Hemi"))
		m_Settings.m_fHemi = ini.r_float("volume", "Hemi");

	if (ini.line_exist("volume", "ConfinementScale"))
		m_Settings.m_fConfinementScale = ini.r_float("volume", "ConfinementScale");

	if (ini.line_exist("volume", "Decay"))
		m_Settings.m_fDecay = ini.r_float("volume", "Decay");

	if (ini.line_exist("volume", "GravityBuoyancy"))
		m_Settings.m_fGravityBuoyancy = ini.r_float("volume", "GravityBuoyancy");
	

	u32 iEmittersNum = ini.r_u32("volume", "EmittersNum");

	m_Emitters.resize(iEmittersNum);

	for ( u32 i=0; i<iEmittersNum; ++i )
	{
		string32	EmitterSectionName;
		CEmitter	&Emitter = m_Emitters[i];
		ZeroMemory(&Emitter, sizeof(Emitter));
		xr_sprintf(EmitterSectionName, "emitter%02d", i);

		Emitter.m_eType = (dx103DFluidEmitters::EmitterType)ini.r_token( EmitterSectionName, "Type", emitter_type_token);

		if (ini.line_exist(EmitterSectionName, "Position"))
			Emitter.m_vPosition = ini.r_fvector3(EmitterSectionName, "Position");
		else
		{
			Emitter.m_vPosition = ini.r_fvector3(EmitterSectionName, "WorldPosition");
			WorldToFluid.transform(Emitter.m_vPosition);
		}

		Emitter.m_fRadius = ini.r_float(EmitterSectionName, "Radius");

		Emitter.m_InvSigma_2 = ini.r_float(EmitterSectionName, "Sigma");
		VERIFY(Emitter.m_InvSigma_2>0);
		Emitter.m_InvSigma_2 = 1.0f / _sqr(Emitter.m_InvSigma_2);

		Emitter.m_vFlowVelocity = ini.r_fvector3(EmitterSectionName, "FlowDirection");
		float fFlowSpeed = ini.r_float(EmitterSectionName, "FlowSpeed");
		Emitter.m_vFlowVelocity.mul(fFlowSpeed);
		
		Emitter.m_fDensity = ini.r_float(EmitterSectionName, "Density");

		Emitter.m_bApplyDensity = ini.r_bool(EmitterSectionName, "ApplyDensity") ? true : false;
		Emitter.m_bApplyImpulse = ini.r_bool(EmitterSectionName, "ApplyImpulse") ? true : false;

		switch (Emitter.m_eType)
		{
		case dx103DFluidEmitters::ET_SimpleDraught:
			Emitter.m_DraughtParams.m_fPeriod	= ini.r_float(EmitterSectionName, "DraughtPeriod");
			Emitter.m_DraughtParams.m_fPhase	= ini.r_float(EmitterSectionName, "DraughtPhase");
			Emitter.m_DraughtParams.m_fAmp		= ini.r_float(EmitterSectionName, "DraughtAmp");
			VERIFY( Emitter.m_DraughtParams.m_fPeriod > 0.0001f );
			break;
		default:
			break;
		}
	}

	//	Allow real-time config reload
#ifdef	DEBUG
	FluidManager.RegisterFluidData(this, Profile);
#endif	//	DEBUG
}

//	Allow real-time config reload
#ifdef	DEBUG
void dx103DFluidData::ReparseProfile(const xr_string &Profile)
{
	m_Emitters.clear();
	ParseProfile(Profile);
}
#endif	//	DEBUG