#include "stdafx.h"
#include "TiramisuEditorParticleSimulation.h"

namespace
{
constexpr float ParticleSimulationStep = 0.033f;
constexpr u32 MaxSimulationStepsPerFrame = 3;
constexpr size_t MaxPendingParticleEvents = 1u << 20;

[[nodiscard]] FEditorSimulatedParticle CopyParticle(
	const PAPI::Particle& Source
)
{
	return {
		Source.pos,
		Source.posB,
		Source.vel,
		Source.size,
		Source.rot_vel,
		Source.rot.x,
		Source.color,
		Source.frame
	};
}
}

bool TiramisuEditorParticleSimulation::Initialize(
	const xr_span<const u8> CompiledActions,
	const u32 MaxParticles
)
{
	if (Initialized || CompiledActions.empty() || MaxParticles == 0)
	{
		return false;
	}
	IReader Reader(
		const_cast<u8*>(CompiledActions.data()),
		CompiledActions.size()
	);
	Holder.LoadActions(Reader);
	Holder.SetMaxParticles(MaxParticles);
	Holder.SetCallback(
		&TiramisuEditorParticleSimulation::OnParticleBirth,
		&TiramisuEditorParticleSimulation::OnParticleDeath,
		this,
		0
	);
	Initialized = true;
	return true;
}

void TiramisuEditorParticleSimulation::SetTransform(
	const Fmatrix& LocalToWorld
)
{
	if (!Initialized)
	{
		return;
	}
	const Fvector Velocity = {0.0f, 0.0f, 0.0f};
	Holder.Transform(LocalToWorld, Velocity);
}

void TiramisuEditorParticleSimulation::SetAnimationSettings(
	const FEditorParticleAnimationSettings& Settings
)
{
	Animation = Settings;
	Animation.FrameCount = std::max(Animation.FrameCount, 1);
	RandomState = Animation.RandomSeed == 0 ? 1 : Animation.RandomSeed;
}

void TiramisuEditorParticleSimulation::Play()
{
	if (!Initialized)
	{
		return;
	}
	Holder.PlayEffect();
	Playing = true;
	DeferredStopRequested = false;
}

void TiramisuEditorParticleSimulation::Stop(const bool Deferred)
{
	if (!Initialized)
	{
		return;
	}
	Holder.StopEffect(Deferred);
	DeferredStopRequested = Deferred;
	if (!Deferred)
	{
		Playing = false;
		TimeAccumulator = 0.0f;
		PendingEvents.clear();
	}
}

void TiramisuEditorParticleSimulation::Update(
	const float DeltaSeconds
)
{
	if (!Initialized || !Playing || !std::isfinite(DeltaSeconds) ||
		DeltaSeconds <= 0.0f)
	{
		return;
	}
	TimeAccumulator += std::min(DeltaSeconds, 0.25f);
	u32 StepCount = static_cast<u32>(
		TimeAccumulator / ParticleSimulationStep
	);
	StepCount = std::min(StepCount, MaxSimulationStepsPerFrame);
	TimeAccumulator -= StepCount * ParticleSimulationStep;
	for (u32 Step = 0; Step < StepCount; ++Step)
	{
		Holder.Update(ParticleSimulationStep);
		AdvanceAnimation(ParticleSimulationStep);
	}
	if (DeferredStopRequested && Holder.GetParticlesCount() == 0)
	{
		Playing = false;
		DeferredStopRequested = false;
	}
}

void TiramisuEditorParticleSimulation::CopyParticles(
	xr_vector<FEditorSimulatedParticle>& OutParticles
)
{
	OutParticles.clear();
	if (!Initialized)
	{
		return;
	}
	PAPI::Particle* Particles = nullptr;
	u32 ParticleCount = 0;
	Holder.GetParticles(Particles, ParticleCount);
	OutParticles.reserve(ParticleCount);
	for (u32 Index = 0; Index < ParticleCount; ++Index)
	{
		OutParticles.push_back(CopyParticle(Particles[Index]));
	}
}

void TiramisuEditorParticleSimulation::ConsumeEvents(
	xr_vector<FEditorParticleSimulationEvent>& OutEvents
)
{
	OutEvents = std::move(PendingEvents);
	PendingEvents.clear();
}

void TiramisuEditorParticleSimulation::OnParticleBirth(
	void* Owner,
	const u32 Parameter,
	PAPI::Particle& Particle,
	const u32 ParticleIndex
)
{
	(void)Parameter;
	auto* Simulation =
		static_cast<TiramisuEditorParticleSimulation*>(Owner);
	if (Simulation->Animation.RandomFrame)
	{
		const u32 Frame = Simulation->NextRandomU32() %
			static_cast<u32>(Simulation->Animation.FrameCount);
		Particle.frame = static_cast<u16>(Frame * 255u);
	}
	if (Simulation->Animation.Enabled &&
		Simulation->Animation.RandomPlayback &&
		(Simulation->NextRandomU32() & 1u) != 0)
	{
		Particle.flags.set(PAPI::Particle::ANIMATE_CCW, true);
	}
	Simulation->AppendEvent(
		EEditorParticleSimulationEventType::Birth,
		Particle,
		ParticleIndex
	);
}

void TiramisuEditorParticleSimulation::OnParticleDeath(
	void* Owner,
	const u32 Parameter,
	PAPI::Particle& Particle,
	const u32 ParticleIndex
)
{
	(void)Parameter;
	static_cast<TiramisuEditorParticleSimulation*>(Owner)->AppendEvent(
		EEditorParticleSimulationEventType::Death,
		Particle,
		ParticleIndex
	);
}

void TiramisuEditorParticleSimulation::AppendEvent(
	const EEditorParticleSimulationEventType Type,
	const PAPI::Particle& Particle,
	const u32 ParticleIndex
)
{
	if (PendingEvents.size() >= MaxPendingParticleEvents)
	{
		return;
	}
	PendingEvents.push_back({Type, ParticleIndex, CopyParticle(Particle)});
}

void TiramisuEditorParticleSimulation::AdvanceAnimation(
	const float DeltaSeconds
)
{
	if (!Animation.Enabled || Animation.FrameCount <= 0 ||
		!std::isfinite(Animation.FrameSpeed))
	{
		return;
	}
	PAPI::Particle* Particles = nullptr;
	u32 ParticleCount = 0;
	Holder.GetParticles(Particles, ParticleCount);
	const float FrameDelta = Animation.FrameSpeed * DeltaSeconds;
	for (u32 Index = 0; Index < ParticleCount; ++Index)
	{
		PAPI::Particle& Particle = Particles[Index];
		const float Direction =
			Particle.flags.is(PAPI::Particle::ANIMATE_CCW) ? -1.0f : 1.0f;
		float Frame = static_cast<float>(Particle.frame) / 255.0f +
			Direction * FrameDelta;
		while (Frame > static_cast<float>(Animation.FrameCount))
		{
			Frame -= static_cast<float>(Animation.FrameCount);
		}
		while (Frame < 0.0f)
		{
			Frame += static_cast<float>(Animation.FrameCount);
		}
		Particle.frame = static_cast<u16>(std::floor(Frame * 255.0f));
	}
}

u32 TiramisuEditorParticleSimulation::NextRandomU32() noexcept
{
	RandomState ^= RandomState >> 12u;
	RandomState ^= RandomState << 25u;
	RandomState ^= RandomState >> 27u;
	return static_cast<u32>(
		(RandomState * 2685821657736338717ull) >> 32u
	);
}
