#pragma once

#include "../../../xrCore/xrCore.h"
#include "../../xrRender/particle_core/particle_holder.h"

struct FEditorSimulatedParticle
{
	Fvector Position;
	Fvector PreviousPosition;
	Fvector Velocity;
	Fvector Size;
	Fvector RotationVelocity;
	float Rotation = 0.0f;
	Fcolor Color = {1.0f, 1.0f, 1.0f, 1.0f};
	u16 Frame = 0;
};

enum class EEditorParticleSimulationEventType : u8
{
	Birth,
	Death
};

struct FEditorParticleSimulationEvent
{
	EEditorParticleSimulationEventType Type =
		EEditorParticleSimulationEventType::Birth;
	u32 ParticleIndex = 0;
	FEditorSimulatedParticle Particle;
};

struct FEditorParticleAnimationSettings
{
	s32 FrameCount = 1;
	float FrameSpeed = 0.0f;
	u64 RandomSeed = 1;
	bool Enabled = false;
	bool RandomFrame = false;
	bool RandomPlayback = false;
};

// Выполняет исходные PAPI actions без legacy model, shader и dynamic VB.
// Объект принадлежит render thread и выдаёт только CPU sprite records.
class TiramisuEditorParticleSimulation final
{
public:
	[[nodiscard]] bool Initialize(
		xr_span<const u8> CompiledActions,
		u32 MaxParticles
	);
	void SetTransform(const Fmatrix& LocalToWorld);
	void SetAnimationSettings(
		const FEditorParticleAnimationSettings& Settings
	);
	void Play();
	void Stop(bool Deferred);
	void Update(float DeltaSeconds);
	void CopyParticles(
		xr_vector<FEditorSimulatedParticle>& OutParticles
	);
	void ConsumeEvents(
		xr_vector<FEditorParticleSimulationEvent>& OutEvents
	);

	[[nodiscard]] bool IsInitialized() const noexcept
	{
		return Initialized;
	}
	[[nodiscard]] bool IsPlaying() const noexcept
	{
		return Playing;
	}

private:
	static void OnParticleBirth(
		void* Owner,
		u32 Parameter,
		PAPI::Particle& Particle,
		u32 ParticleIndex
	);
	static void OnParticleDeath(
		void* Owner,
		u32 Parameter,
		PAPI::Particle& Particle,
		u32 ParticleIndex
	);
	void AppendEvent(
		EEditorParticleSimulationEventType Type,
		const PAPI::Particle& Particle,
		u32 ParticleIndex
	);
	void AdvanceAnimation(float DeltaSeconds);
	[[nodiscard]] u32 NextRandomU32() noexcept;

	PAPI::ParticleHolder Holder;
	xr_vector<FEditorParticleSimulationEvent> PendingEvents;
	FEditorParticleAnimationSettings Animation;
	u64 RandomState = 1;
	float TimeAccumulator = 0.0f;
	bool Initialized = false;
	bool Playing = false;
	bool DeferredStopRequested = false;
};
