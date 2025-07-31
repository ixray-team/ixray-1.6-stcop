#pragma once
class CDS0_EnvDescriptorMixerRender :
	public IEnvDescriptorMixerRender
{
public:
	CDS0_EnvDescriptorMixerRender();
	virtual void Copy(IEnvDescriptorMixerRender& _in);

	virtual void Destroy();
	virtual void Clear();
	virtual void lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB);


};


class CDS0_EnvDescriptorRender :
	public IEnvDescriptorRender
{
public:
	CDS0_EnvDescriptorRender();
	virtual void Copy(IEnvDescriptorRender& _in);

	virtual void OnDeviceCreate(CEnvDescriptor& owner);
	virtual void OnDeviceDestroy();
};


class CDS0_EnvironmentRender :
	public IEnvironmentRender
{
public:
	CDS0_EnvironmentRender();
	virtual ~CDS0_EnvironmentRender();
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