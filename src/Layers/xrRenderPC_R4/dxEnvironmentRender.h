#ifndef dxEnvironmentRender_included
#define dxEnvironmentRender_included
#pragma once

#include "../../Include/xrRender/EnvironmentRender.h"

#include "blenders/Blender.h"
#include "../../xrEngine/EngineAPI.h"
class CBlender_skybox		: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: combiner";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override
	{
		C.r_Pass("sky", "sky", false, true, false);

		C.r_dx10Texture("s_sky0", "$null");
		C.r_dx10Texture("s_sky1", "$null");

		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_linear");
		C.r_dx10Sampler("smp_base");

		C.PassSET_ZB(LightingModeIsStaticRender(), false);
		C.r_End ();
	}
};

class dxEnvDescriptorRender : public IEnvDescriptorRender
{
	friend class dxEnvDescriptorMixerRender;
public:
	virtual void OnDeviceCreate(CEnvDescriptor &owner) override;
	virtual void OnDeviceDestroy() override;

	virtual void Copy(IEnvDescriptorRender &_in) override;
private:
	ref_texture			sky_texture		;
	ref_texture			sky_texture_env	;
	ref_texture			clouds_texture	;
};

class dxEnvDescriptorMixerRender : public IEnvDescriptorMixerRender
{
public:
	virtual void Copy(IEnvDescriptorMixerRender &_in) override;

	virtual void Destroy() override;
	virtual void Clear() override;
	virtual void lerp(IEnvDescriptorRender *inA, IEnvDescriptorRender *inB) override;
//private:
public:
	STextureList		sky_r_textures;		
	STextureList		sky_r_textures_env;	
	STextureList		clouds_r_textures;	
};

class dxEnvironmentRender : public IEnvironmentRender
{
public:
	dxEnvironmentRender();
	virtual void Copy(IEnvironmentRender &_in) override;

	virtual void OnFrame(CEnvironment &env) override;
	virtual void OnLoad() override;
	virtual void OnUnload() override;
	virtual void RenderSky(CEnvironment &env) override;
	virtual void RenderClouds(CEnvironment &env) override;
	virtual void OnDeviceCreate() override;
	virtual void OnDeviceDestroy() override;
	virtual particles_systems::library_interface const& particles_systems_library	() override;

private:
	CBlender_skybox			m_b_skybox;

	ref_shader				sh_2sky;
	ref_geom				sh_2geom;

	ref_shader				clouds_sh;
	ref_geom				clouds_geom;

	ref_texture				tsky0,tsky1;
};

#endif	//	EnvironmentRender_included