#pragma once
class CDS0_EnvDescriptorMixerRender :
	public IEnvDescriptorMixerRender
{
public:
	CDS0_EnvDescriptorMixerRender();
	virtual void Copy(IEnvDescriptorMixerRender& _in) override;

	virtual void Destroy() override;
	virtual void Clear() override;
	virtual void lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB) override;


};


class CDS0_EnvDescriptorRender :
	public IEnvDescriptorRender
{
public:
	CDS0_EnvDescriptorRender();
	virtual void Copy(IEnvDescriptorRender& _in) override;

	virtual void OnDeviceCreate(CEnvDescriptor& owner) override;
	virtual void OnDeviceDestroy() override;
};


class CDS0_EnvironmentRender :
	public IEnvironmentRender
{
public:
	CDS0_EnvironmentRender();
	virtual ~CDS0_EnvironmentRender() override;
	virtual void	Copy(IEnvironmentRender& _in) override;
	virtual void	OnFrame(CEnvironment& env) override;
	virtual void	OnLoad() override;
	virtual void	OnUnload() override;
	virtual void	RenderSky(CEnvironment& env) override;
	virtual void	RenderClouds(CEnvironment& env) override;
	virtual void	OnDeviceCreate() override;
	virtual void	OnDeviceDestroy() override;
	virtual particles_systems::library_interface const& particles_systems_library() override;
private:

};