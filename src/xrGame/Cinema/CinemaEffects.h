#pragma once
#include "CinemaCurve.h"
#include "../ParticlesObject.h"
#include "../../xrEngine/Render.h"

namespace Cinema
{
class LightRuntime
{
public:
	explicit LightRuntime(u32 ObjectId);
	~LightRuntime();
	void Apply(const LightData& Setup);
	void Place(const Fmatrix& World, const LightData& Values, bool IsShown);
	u32 Id = 0;
	shared_str Error;

private:
	ref_light Handle;
	LightData Applied;
	bool HasApplied = false;
};

enum class EmitState : u8
{
	Cut,
	Live,
	Released
};

struct ParticleCommand
{
	Fmatrix Transform = Fidentity;
	Fvector Velocity = Fvector().set(0.0f, 0.0f, 0.0f);
	float Size = 1.0f;
	float Advance = 0.0f;
	float Preroll = 0.0f;
	u32 Restart = 0;
	EmitState State = EmitState::Cut;
	bool ShouldRetrigger = false;
};

class ParticleObject final : public CParticlesObject
{
public:
	explicit ParticleObject(const char* Effect);
	void Update(u32 Dt, CFrustum* View) override;
	void Post(const ParticleCommand& Next);

private:
	xrCriticalSection Lock;
	ParticleCommand Pending;
	u32 Restarted = 0;
	EmitState Emitting = EmitState::Cut;
	double Remainder = 0.0;
};

struct ParticleDrive
{
	Fmatrix World = Fidentity;
	float Tau = 0.0f;
	float Played = 0.0f;
	float Rate = 0.0f;
	float Preroll = 0.0f;
	float Size = 1.0f;
	bool IsShown = false;
	bool IsAfter = false;
	bool ShouldRetrigger = false;
};

class ParticleRuntime
{
public:
	explicit ParticleRuntime(u32 ObjectId);
	~ParticleRuntime();
	void Apply(const ParticleData& Setup);
	void Drive(const ParticleDrive& Input);
	u32 Id = 0;
	shared_str Error;
	Curves::AreaTable SpeedArea;

private:
	void Drop();
	xr_shared_ptr<ParticleObject> System;
	shared_str AppliedEffect;
	ParticleCommand Command;
	Fvector LastPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	float LastTau = 0.0f;
	u32 RebuiltAt = 0;
};
}

namespace Cinema::Effects
{
bool IsEffect(const shared_str& Name);
void ListEffects(xr_vector<shared_str>& Out);
}
