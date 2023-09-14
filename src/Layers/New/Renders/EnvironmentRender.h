#pragma once
#include "../../Include/xrRender/EnvironmentRender.h"

namespace Rendering
{
	class EnvDescriptorRender : public IEnvDescriptorRender
	{
	public:
		~EnvDescriptorRender();
		void Copy(IEnvDescriptorRender& _in) override;

		void OnDeviceCreate(CEnvDescriptor& owner) override;
		void OnDeviceDestroy() override;
	};	

	class EnvDescriptorMixerRender : public IEnvDescriptorMixerRender
	{
	public:
		~EnvDescriptorMixerRender();
		void Copy(IEnvDescriptorMixerRender& _in) override;

		void Destroy() override;
		void Clear() override;
		void lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB) override;
	};	
	
	class EnvironmentRender : public IEnvironmentRender
	{
	public:
		~EnvironmentRender();
		void Copy(IEnvironmentRender& _in) override;
		void OnFrame(CEnvironment& env) override;
		void OnLoad() override;
		void OnUnload() override;
		void RenderSky(CEnvironment& env) override;
		void RenderClouds(CEnvironment& env) override;
		void OnDeviceCreate() override;
		void OnDeviceDestroy() override;
		particles_systems::library_interface const& particles_systems_library() override;
	};
}