#include "stdafx.h"

XRayEnvDescriptorMixerRender::XRayEnvDescriptorMixerRender()
{
}

void XRayEnvDescriptorMixerRender::Copy(IEnvDescriptorMixerRender& _in)
{

}

void XRayEnvDescriptorMixerRender::Destroy()
{
}

void XRayEnvDescriptorMixerRender::Clear()
{
	Destroy();
}

void XRayEnvDescriptorMixerRender::lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB)
{
	Destroy();
	
}

XRayEnvDescriptorRender::XRayEnvDescriptorRender()
{
}

void XRayEnvDescriptorRender::Copy(IEnvDescriptorRender& _in)
{

}

void XRayEnvDescriptorRender::OnDeviceCreate(CEnvDescriptor& owner)
{
	
}

void XRayEnvDescriptorRender::OnDeviceDestroy()
{

}

#include "../../XrEngine/xr_efflensflare.h"
XRayEnvironmentRender::XRayEnvironmentRender()
{

}

XRayEnvironmentRender::~XRayEnvironmentRender()
{
}

void XRayEnvironmentRender::Copy(IEnvironmentRender & _in)
{
	R_ASSERT(0);
}

void XRayEnvironmentRender::OnFrame(CEnvironment& env)
{
}



void XRayEnvironmentRender::OnLoad()
{
}

void XRayEnvironmentRender::OnUnload()
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

void XRayEnvironmentRender::RenderSky(CEnvironment& env)
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

void XRayEnvironmentRender::RenderClouds(CEnvironment& env)
{

}


void XRayEnvironmentRender::OnDeviceCreate()
{
	
}

void XRayEnvironmentRender::OnDeviceDestroy()
{

}
particles_systems::library_interface *null = 0;
particles_systems::library_interface const & XRayEnvironmentRender::particles_systems_library()
{
	return *null;
}
