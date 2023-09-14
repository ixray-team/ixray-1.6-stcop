#include "pch.h"

using namespace Rendering;

///////////////////////////////////////////////////////////
shared_str TempGroupId = "aaaa";

class ParticleLibrary : public particles_systems::library_interface {
public:
	PS::CPGDef const* const* particles_group_begin() const override;
	PS::CPGDef const* const* particles_group_end() const override;
	void particles_group_next(PS::CPGDef const* const*& iterator) const override;
	shared_str const& particles_group_id(PS::CPGDef const& particles_group) const override;
}; 

PS::CPGDef const* const* 
ParticleLibrary::particles_group_begin() const
{
	return nullptr;
}

PS::CPGDef const* const* 
ParticleLibrary::particles_group_end() const
{
	return nullptr;
}

void
ParticleLibrary::particles_group_next(PS::CPGDef const* const*& iterator) const
{

}

shared_str const&
ParticleLibrary::particles_group_id(PS::CPGDef const& particles_group) const
{
	return TempGroupId;
}

static ParticleLibrary ParticleLibraryImpl;
///////////////////////////////////////////////////////////

EnvironmentRender::~EnvironmentRender()
{
}

void
EnvironmentRender::Copy(IEnvironmentRender& _in)
{
}

void
EnvironmentRender::OnFrame(CEnvironment& env)
{
}

void 
EnvironmentRender::OnLoad()
{
}

void 
EnvironmentRender::OnUnload()
{
}

void
EnvironmentRender::RenderSky(CEnvironment& env)
{
}

void 
EnvironmentRender::RenderClouds(CEnvironment& env)
{
}

void 
EnvironmentRender::OnDeviceCreate()
{
}

void
EnvironmentRender::OnDeviceDestroy()
{
}

particles_systems::library_interface const& 
EnvironmentRender::particles_systems_library()
{
	return ParticleLibraryImpl;
}

EnvDescriptorRender::~EnvDescriptorRender()
{
}

void 
EnvDescriptorRender::Copy(IEnvDescriptorRender& _in)
{
}

void 
EnvDescriptorRender::OnDeviceCreate(CEnvDescriptor& owner)
{
}

void 
EnvDescriptorRender::OnDeviceDestroy()
{
}

EnvDescriptorMixerRender::~EnvDescriptorMixerRender()
{
}

void 
EnvDescriptorMixerRender::Copy(IEnvDescriptorMixerRender& _in)
{
}

void
EnvDescriptorMixerRender::Destroy()
{
}

void
EnvDescriptorMixerRender::Clear()
{
}

void 
EnvDescriptorMixerRender::lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB)
{
}
