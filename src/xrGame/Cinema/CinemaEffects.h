#pragma once
#include "CinemaCurve.h"
#include "../ParticlesObject.h"
#include "../../xrEngine/Render.h"
#include "../../Include/xrRender/WallMarkArray.h"

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
	float Opacity = 1.0f;
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
	float Idle = 0.0f;
	float Rate = 0.0f;
	float Preroll = 0.0f;
	float Size = 1.0f;
	float Opacity = 1.0f;
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

namespace Cinema
{
class DecalRuntime
{
public:
	explicit DecalRuntime(u32 ObjectId) : Id(ObjectId) {}
	~DecalRuntime() { Drop(); }
	void Apply(const DecalData& Setup, const xr_vector<shared_str>& Shaders);
	void Drive(const Fmatrix& World, float Width, float Height, bool IsShown);
	bool Trace(const Fmatrix& World, Fvector& Point, u32& Triangle, float& Distance) const;
	IWallMarkArray* Marks() const { return Array->empty() ? nullptr : &*Array; }
	u32 Id = 0;
	shared_str Error;
	float LastTau = -1.0f;

private:
	void Drop();
	FactoryPtr<IWallMarkArray> Array;
	shared_str AppliedTexture, AppliedShader;
	StaticWallmarkHandle::WallmarkHandlePtr Handle;
	Fvector PlacedAt = Fvector().set(0.0f, 0.0f, 0.0f);
	u32 PlacedTriangle = 0;
	float PlacedWidth = -1.0f, PlacedHeight = -1.0f, PlacedSpin = 0.0f;
	bool IsApplied = false;
};

struct SoundDrive
{
	Fvector Point;
	float Tau = 0.0f;
	float Volume = 1.0f;
	float Pitch = 1.0f;
	bool IsHeard = false;
};

class SoundRuntime
{
public:
	explicit SoundRuntime(u32 ObjectId) : Id(ObjectId) {}
	~SoundRuntime() { Voice.destroy(); }
	void Apply(const SoundData& Setup);
	void Drive(const SoundDrive& State);
	float Length() const { return Total; }
	u32 Id = 0;

private:
	ref_sound Voice;
	shared_str File;
	float Offset = 0.0f, Near = 0.0f, Far = 0.0f, Total = 0.0f;
	u32 Flags = 0, PlayedFlags = u32(-1);
};
}

namespace Cinema::Effects
{
bool IsEffect(const shared_str& Name);
void ListEffects(xr_vector<shared_str>& Out);
}
