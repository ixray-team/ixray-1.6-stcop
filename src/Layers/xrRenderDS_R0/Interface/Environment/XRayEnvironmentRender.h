#pragma once
class XRayEnvDescriptorMixerRender :public  IEnvDescriptorMixerRender
{
public:
	XRayEnvDescriptorMixerRender();
	virtual void Copy(IEnvDescriptorMixerRender& _in);

	virtual void Destroy();
	virtual void Clear();
	virtual void lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB);


};


class XRayEnvDescriptorRender :public IEnvDescriptorRender
{
public:
	XRayEnvDescriptorRender();
	virtual void Copy(IEnvDescriptorRender& _in);

	virtual void OnDeviceCreate(CEnvDescriptor& owner);
	virtual void OnDeviceDestroy();
};


class XRayEnvironmentRender :public IEnvironmentRender
{
public:
	XRayEnvironmentRender();
	virtual ~XRayEnvironmentRender();
	virtual void	Copy(IEnvironmentRender& _in) ;
	virtual void	OnFrame(CEnvironment& env) ;
	virtual void	OnLoad() ;
	virtual void	OnUnload() ;
	virtual void	RenderSky(CEnvironment& env) ;
	virtual void	RenderClouds(CEnvironment& env) ;
	virtual void	OnDeviceCreate() ;
	virtual void	OnDeviceDestroy() ;
	virtual particles_systems::library_interface const& particles_systems_library();
private:

};