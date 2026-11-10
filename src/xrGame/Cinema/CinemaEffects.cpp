#include "StdAfx.h"
#include "CinemaEffects.h"
#include "../GamePersistent.h"
#include "../../Include/xrRender/EnvironmentRender.h"
#include "../../Include/xrRender/particles_systems_library_interface.hpp"
#include "../../xrEngine/ParticleAnimCurveInterface.h"

namespace Cinema::Effects
{
constexpr u32 RebuildInterval = 100;

bool IsEffect(const shared_str& Name)
{
	if (!Name.size() || g_dedicated_server)
	{
		return false;
	}
	const xr_vector<shared_str>& Names = GamePersistent().Environment().m_pRender->particles_systems_library().vec_all_particles();
	PS::IPACLibrary* Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	return std::find(Names.begin(), Names.end(), Name) != Names.end() && (!Library || !Library->FindIPAC(Name.c_str()));
}

void ListEffects(xr_vector<shared_str>& Out)
{
	if (g_dedicated_server)
	{
		return;
	}
	PS::IPACLibrary* Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	for (const shared_str& Name : GamePersistent().Environment().m_pRender->particles_systems_library().vec_all_particles())
	{
		if (Name.size() && (!Library || !Library->FindIPAC(Name.c_str())))
		{
			Out.push_back(Name);
		}
	}
}
}

namespace Cinema
{
LightRuntime::LightRuntime(u32 ObjectId) :
	Id(ObjectId), Handle(::Render->light_create())
{
	if (!Handle)
	{
		Error = "The renderer could not create a light.";
		return;
	}
	Handle->set_hud_mode(false);
	Handle->set_occq_mode(true);
}

LightRuntime::~LightRuntime()
{
	Handle.destroy();
}

void LightRuntime::Apply(const LightData& Setup)
{
	const bool IsTypeChanged = !HasApplied || Setup.Kind != Applied.Kind || Setup.HasShadow != Applied.HasShadow;
	if (!Handle || (!IsTypeChanged && Setup.IsVolumetric == Applied.IsVolumetric && Setup.Texture == Applied.Texture))
	{
		return;
	}
	if (IsTypeChanged)
	{
		Handle->set_shadow(false);
		Handle->set_type(Setup.Kind == LightKind::Spot ? IRender_Light::SPOT : IRender_Light::POINT);
		Handle->set_shadow(Setup.HasShadow);
	}
	Handle->set_volumetric(Setup.IsVolumetric);
	string_path File;
	const bool IsWanted = Setup.Kind == LightKind::Spot && Setup.Texture.size();
	const bool IsFound = IsWanted && Setup.Texture.size() <= MaxTextureBytes && FS.exist(File, _game_textures_, Setup.Texture.c_str(), ".dds");
	Handle->set_texture(IsFound ? Setup.Texture.c_str() : nullptr);
	Error = IsWanted && !IsFound ? shared_str("Texture not found") : shared_str();
	Applied = Setup;
	HasApplied = true;
}

void LightRuntime::Place(const Fmatrix& World, const LightData& Values, bool IsShown)
{
	if (!Handle)
	{
		return;
	}
	if (IsShown)
	{
		Handle->set_position(World.c);
		Handle->set_rotation(World.k, World.i);
		Handle->set_color(Values.Color.x * Values.Intensity, Values.Color.y * Values.Intensity, Values.Color.z * Values.Intensity);
		Handle->set_range(Values.Range);
		Handle->set_cone(deg2rad(Values.Cone));
		Handle->set_virtual_size(Values.VirtualSize);
		Handle->set_volumetric_quality(Values.VolumeQuality);
		Handle->set_volumetric_intensity(Values.VolumeIntensity);
		Handle->set_volumetric_distance(Values.VolumeDistance);
	}
	Handle->set_active(IsShown);
}

ParticleObject::ParticleObject(const char* Effect) :
	CParticlesObject(Effect, false, true)
{
}

void ParticleObject::Post(const ParticleCommand& Next)
{
	xrCriticalSectionGuard Guard(Lock);
	const float Carried = Next.Restart == Pending.Restart ? Pending.Advance : 0.0f;
	Pending = Next;
	Pending.Advance += Carried;
}

void ParticleObject::Update(u32, CFrustum* View)
{
	ParticleCommand Next;
	{
		xrCriticalSectionGuard Guard(Lock);
		Next = Pending;
		Pending.Advance = 0.0f;
	}
	const bool IsRestart = Next.Restart != Restarted;
	if (IsRestart)
	{
		Restarted = Next.Restart;
		Stop(false);
		Play(false);
		Emitting = EmitState::Live;
	}
	if (Next.State != Emitting)
	{
		Stop(Next.State == EmitState::Released);
	}
	Emitting = Next.State;
	UpdateParent(Next.Transform, Next.Velocity);
	SetScale(Next.Size);
	Remainder = std::min((IsRestart ? Next.Preroll * 1000.0 : Remainder) + Next.Advance * 1000.0, MaxPreroll * 1000.0);
	do
	{
		const u32 Elapsed = static_cast<u32>(std::min(Remainder, PrerollStep * 1000.0));
		Remainder -= Elapsed;
		CParticlesObject::Update(Elapsed, View);
		if (Elapsed && Emitting == EmitState::Live && Next.ShouldRetrigger && !IsLooped() && m_bPlaying && !IsPlaying())
		{
			Stop(false);
			Play(false);
		}
	}
	while (Remainder >= 1.0);
}

ParticleRuntime::ParticleRuntime(u32 ObjectId) :
	Id(ObjectId), Error("No effect")
{
}

ParticleRuntime::~ParticleRuntime()
{
	Drop();
}

void ParticleRuntime::Apply(const ParticleData& Setup)
{
	if (Setup.Effect == AppliedEffect)
	{
		return;
	}
	Drop();
	AppliedEffect = Setup.Effect;
	Command = {};
	const bool IsKnown = Effects::IsEffect(Setup.Effect);
	Error = IsKnown ? shared_str() : shared_str(Setup.Effect.size() ? "Effect not found" : "No effect");
	if (IsKnown)
	{
		System = xr_make_shared<ParticleObject>(Setup.Effect.c_str());
		GamePersistent().ps_active_deffer.push_back(System);
	}
}

void ParticleRuntime::Drive(const ParticleDrive& Input)
{
	const bool IsContinuous = Input.Played >= 0.0f && std::fabs(Input.Tau - LastTau - Input.Played) <= 1e-3f;
	const bool IsJump = Input.IsShown && (Command.State != EmitState::Live || !IsContinuous);
	const bool IsHeld = IsJump && Command.State == EmitState::Live && Input.Played == 0.0f && Device.dwTimeContinual - RebuiltAt < Effects::RebuildInterval;
	const bool IsRestart = IsJump && !IsHeld;
	const bool IsMoving = IsContinuous && !IsRestart && Input.Played > 0.0f;
	Command.Restart += IsRestart ? 1 : 0;
	Command.Preroll = IsRestart ? Input.Preroll : Command.Preroll;
	Command.State = Input.IsShown ? EmitState::Live : Input.IsAfter && IsContinuous && Command.State != EmitState::Cut ? EmitState::Released : EmitState::Cut;
	Command.Advance = IsRestart || Command.State == EmitState::Cut ? 0.0f : Input.Played * Input.Rate;
	Command.Transform = Input.World;
	Command.Velocity.sub(Input.World.c, LastPosition).mul(IsMoving ? 1.0f / Input.Played : 0.0f);
	Command.Size = Input.Size;
	Command.ShouldRetrigger = Input.ShouldRetrigger;
	RebuiltAt = IsRestart ? Device.dwTimeContinual : RebuiltAt;
	LastTau = IsHeld ? LastTau : Input.Tau;
	LastPosition = Input.World.c;
	if (System)
	{
		System->Post(Command);
	}
}

void ParticleRuntime::Drop()
{
	if (System)
	{
		System->m_NeedDestroy = true;
	}
	System.reset();
}
}
