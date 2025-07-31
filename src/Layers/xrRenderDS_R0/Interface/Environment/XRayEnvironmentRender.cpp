#include "stdafx.h"

CDS0_EnvDescriptorMixerRender::CDS0_EnvDescriptorMixerRender()
{
}

void CDS0_EnvDescriptorMixerRender::Copy(IEnvDescriptorMixerRender& _in)
{

}

void CDS0_EnvDescriptorMixerRender::Destroy()
{
}

void CDS0_EnvDescriptorMixerRender::Clear()
{
	Destroy();
}

void CDS0_EnvDescriptorMixerRender::lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB)
{
	Destroy();
	
}

CDS0_EnvDescriptorRender::CDS0_EnvDescriptorRender()
{
}

void CDS0_EnvDescriptorRender::Copy(IEnvDescriptorRender& _in)
{

}

void CDS0_EnvDescriptorRender::OnDeviceCreate(CEnvDescriptor& owner)
{
	
}

void CDS0_EnvDescriptorRender::OnDeviceDestroy()
{

}

#include "../../XrEngine/xr_efflensflare.h"
CDS0_EnvironmentRender::CDS0_EnvironmentRender()
{

}

CDS0_EnvironmentRender::~CDS0_EnvironmentRender()
{
}

void CDS0_EnvironmentRender::Copy(IEnvironmentRender & _in)
{
	R_ASSERT(0);
}

void CDS0_EnvironmentRender::OnFrame(CEnvironment& env)
{
}



void CDS0_EnvironmentRender::OnLoad()
{
}

void CDS0_EnvironmentRender::OnUnload()
{
}
#pragma pack(push,1)
struct v_skybox {
	Fvector3	p;
	u32			color;
	Fvector3	uv[2];

	void		set(Fvector3& _p, u32 _c, Fvector3& _tc)
	{
		p = _p;
		color = _c;
		uv[0] = _tc;
		uv[1] = _tc;
	}
};
#pragma pack(pop)

void CDS0_EnvironmentRender::RenderSky(CEnvironment& env)
{
}
#pragma pack(push,1)
struct v_clouds {
	Fvector3	p;
	u32			color;
	u32			intensity;
	void		set(Fvector3& _p, u32 _c, u32 _i)
	{
		p = _p;
		color = _c;
		intensity = _i;
	}
};
#pragma pack(pop)

void CDS0_EnvironmentRender::RenderClouds(CEnvironment& env)
{

}


void CDS0_EnvironmentRender::OnDeviceCreate()
{
	
}

void CDS0_EnvironmentRender::OnDeviceDestroy()
{

}
particles_systems::library_interface *null = 0;
particles_systems::library_interface const & CDS0_EnvironmentRender::particles_systems_library()
{
	return *null;
}
