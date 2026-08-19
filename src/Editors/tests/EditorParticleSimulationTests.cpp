#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorParticleSimulation.h"

#include <cmath>
#include <iostream>

namespace
{
template <typename TAction>
void WriteAction(CMemoryWriter& Writer, TAction& Action)
{
	Writer.w_u32(static_cast<u32>(Action.type));
	Action.Save(Writer);
}

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	PAPI::PASource Source;
	Source.type = PAPI::PASourceID;
	Source.position = PAPI::pDomain(PAPI::PDPoint, 0.0f, 0.0f, 0.0f);
	Source.velocity = PAPI::pDomain(PAPI::PDPoint, 0.0f, 1.0f, 0.0f);
	Source.rot = PAPI::pDomain(PAPI::PDPoint, 0.0f, 0.0f, 0.0f);
	Source.rot_vel = PAPI::pDomain(PAPI::PDPoint, 0.0f, 1.0f, 0.0f);
	Source.size = PAPI::pDomain(PAPI::PDPoint, 0.1f, 0.1f, 0.1f);
	Source.color = PAPI::pDomain(PAPI::PDPoint, 1.0f, 0.5f, 0.25f);
	Source.AlighRotVelocityToVelocity = true;
	Source.random_alpha = false;
	Source.alpha = 1.0f;
	Source.alpha2 = 1.0f;
	Source.particle_rate = 30.0f;
	Source.age = 0.0f;
	Source.age_sigma = 0.0f;
	Source.parent_vel.set(0.0f, 0.0f, 0.0f);
	Source.parent_motion = 0.0f;

	PAPI::PAMove Move;
	Move.type = PAPI::PAMoveID;
	PAPI::PAKillOld KillOld;
	KillOld.type = PAPI::PAKillOldID;
	KillOld.age_limit = 1.0f;
	KillOld.kill_less_than = false;

	CMemoryWriter Actions;
	Actions.w_u32(3);
	WriteAction(Actions, Source);
	WriteAction(Actions, Move);
	WriteAction(Actions, KillOld);

	TiramisuEditorParticleSimulation Simulation;
	if (!Simulation.Initialize(
			xr_span(
				static_cast<const u8*>(Actions.pointer()),
				Actions.size()
			),
			64
		))
	{
		return Fail("PAPI simulation initialization failed");
	}
	Fmatrix Transform = Fidentity;
	Transform.c.set(10.0f, 0.0f, 0.0f);
	Simulation.SetTransform(Transform);
	Simulation.Play();
	Simulation.Update(0.1f);

	xr_vector<FEditorSimulatedParticle> Particles;
	Simulation.CopyParticles(Particles);
	if (Particles.empty() || Particles.size() > 64)
	{
		return Fail("PAPI source action produced an invalid particle count");
	}
	for (const FEditorSimulatedParticle& Particle : Particles)
	{
		if (!std::isfinite(Particle.Position.x) ||
			!std::isfinite(Particle.Position.y) ||
			!std::isfinite(Particle.Position.z) ||
			std::abs(Particle.Position.x - 10.0f) > 0.001f ||
			Particle.Position.y <= 0.0f ||
			std::abs(Particle.Velocity.y - 1.0f) > 0.001f)
		{
			return Fail("PAPI transform or move action was not preserved");
		}
	}

	xr_vector<FEditorParticleSimulationEvent> Events;
	Simulation.ConsumeEvents(Events);
	if (Events.empty())
	{
		return Fail("PAPI source action did not report birth events");
	}
	for (const FEditorParticleSimulationEvent& Event : Events)
	{
		if (Event.Type != EEditorParticleSimulationEventType::Birth ||
			Event.ParticleIndex >= 64 ||
			!std::isfinite(Event.Particle.Position.x) ||
			std::abs(Event.Particle.Position.x - 10.0f) > 0.001f)
		{
			return Fail("PAPI birth event contains invalid particle data");
		}
	}
	Simulation.ConsumeEvents(Events);
	if (!Events.empty())
	{
		return Fail("PAPI event stream was not consumed exactly once");
	}

	bool HasDeathEvent = false;
	for (u32 UpdateIndex = 0; UpdateIndex < 40; ++UpdateIndex)
	{
		Simulation.Update(0.1f);
		Simulation.ConsumeEvents(Events);
		HasDeathEvent = HasDeathEvent || std::ranges::any_of(
			Events,
			[](const FEditorParticleSimulationEvent& Event)
			{
				return Event.Type ==
					EEditorParticleSimulationEventType::Death;
			}
		);
	}
	if (!HasDeathEvent)
	{
		return Fail("PAPI kill action did not report death events");
	}

	Simulation.Stop(true);
	for (u32 UpdateIndex = 0;
		 UpdateIndex < 40 && Simulation.IsPlaying();
		 ++UpdateIndex)
	{
		Simulation.Update(0.1f);
	}
	Simulation.CopyParticles(Particles);
	if (!Particles.empty() || Simulation.IsPlaying())
	{
		return Fail("Deferred particle stop did not drain the simulation");
	}

	Simulation.Play();
	Simulation.Update(0.1f);
	Simulation.CopyParticles(Particles);
	if (Particles.empty())
	{
		return Fail("Particle simulation did not restart after deferred stop");
	}
	Simulation.Stop(false);
	Simulation.CopyParticles(Particles);
	if (!Particles.empty() || Simulation.IsPlaying())
	{
		return Fail("Immediate particle stop did not clear the simulation");
	}

	auto InitializeAnimated = [&](TiramisuEditorParticleSimulation& Target)
	{
		if (!Target.Initialize(
				xr_span(
					static_cast<const u8*>(Actions.pointer()),
					Actions.size()
				),
				64
			))
		{
			return false;
		}
		FEditorParticleAnimationSettings Animation;
		Animation.FrameCount = 8;
		Animation.FrameSpeed = 12.0f;
		Animation.RandomSeed = 0x12345678u;
		Animation.Enabled = true;
		Animation.RandomFrame = true;
		Animation.RandomPlayback = true;
		Target.SetAnimationSettings(Animation);
		Target.SetTransform(Transform);
		Target.Play();
		Target.Update(0.1f);
		return true;
	};
	TiramisuEditorParticleSimulation AnimatedA;
	TiramisuEditorParticleSimulation AnimatedB;
	if (!InitializeAnimated(AnimatedA) || !InitializeAnimated(AnimatedB))
	{
		return Fail("animated PAPI simulation initialization failed");
	}
	xr_vector<FEditorSimulatedParticle> AnimatedParticlesA;
	xr_vector<FEditorSimulatedParticle> AnimatedParticlesB;
	AnimatedA.CopyParticles(AnimatedParticlesA);
	AnimatedB.CopyParticles(AnimatedParticlesB);
	if (AnimatedParticlesA.empty() ||
		AnimatedParticlesA.size() != AnimatedParticlesB.size())
	{
		return Fail("animated PAPI simulation produced invalid particles");
	}
	bool HasAnimatedFrame = false;
	for (size_t Index = 0; Index < AnimatedParticlesA.size(); ++Index)
	{
		const u16 FrameA = AnimatedParticlesA[Index].Frame;
		const u16 FrameB = AnimatedParticlesB[Index].Frame;
		if (FrameA != FrameB || FrameA > 8u * 255u)
		{
			return Fail("particle frame animation is not deterministic");
		}
		HasAnimatedFrame = HasAnimatedFrame || FrameA != 0;
	}
	if (!HasAnimatedFrame)
	{
		return Fail("particle frame animation did not advance");
	}
	return 0;
}
