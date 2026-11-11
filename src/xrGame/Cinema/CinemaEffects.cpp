#include "StdAfx.h"
#include "CinemaEffects.h"
#include "../GamePersistent.h"
#include "../Level.h"
#include "CinemaActorRuntime.h"
#include "../../Include/xrRender/EnvironmentRender.h"
#include "../../Include/xrRender/particles_systems_library_interface.hpp"
#include "../../xrEngine/ParticleAnimCurveInterface.h"

namespace Cinema
{
void SoundRuntime::Apply(const SoundData& Setup)
{
	Offset = Setup.Offset;
	Flags = (Setup.IsLooped ? sm_Looped : 0u) | (Setup.IsWorld ? 0u : sm_2D);
	PlayedFlags = Near == Setup.MinDistance && Far == Setup.MaxDistance ? PlayedFlags : u32(-1);
	Near = Setup.MinDistance;
	Far = Setup.MaxDistance;
	if (File == Setup.File)
	{
		return;
	}
	Voice.destroy();
	PlayedFlags = u32(-1);
	File = Setup.File;
	Total = 0.0f;
	string_path Path;
	if (File.size() && FS.exist(Path, "$game_sounds$", File.c_str(), ".ogg"))
	{
		Voice.create(File.c_str(), st_Effect, sg_SourceType);
		Total = XRay::Sound::Mixer::GetSourceDuration(File.c_str());
	}
}

void SoundRuntime::Drive(const SoundDrive& State)
{
	if (!Voice._p)
	{
		return;
	}
	const float Whole = Offset + State.Tau;
	const bool IsLooped = (Flags & sm_Looped) && Total > 0.0f;
	const float At = IsLooped ? std::fmod(Whole, Total) : Whole;
	if (!State.IsHeard || At < 0.0f || (!IsLooped && Total > 0.0f && At >= Total - 0.02f))
	{
		if (PlayedFlags != u32(-1))
		{
			Voice.stop();
			PlayedFlags = u32(-1);
		}
		return;
	}
	const float Drift = std::fabs(XRay::Sound::Mixer::GetPlaytime(Voice._p->slot) - At);
	if (PlayedFlags != Flags)
	{
		(Flags & sm_2D) ? Voice.play(nullptr, Flags) : Voice.play_at_pos(nullptr, State.Point, Flags);
		XRay::Sound::Mixer::Seek(Voice._p->slot, At);
		if (!(Flags & sm_2D) && Far > 0.0f)
		{
			Voice.set_range(std::min(Near, Far), Far);
		}
		PlayedFlags = Flags;
	}
	else if (std::fabs(State.Pitch - 1.0f) < 0.01f && (IsLooped ? std::min(Drift, Total - Drift) : Drift) > 0.3f)
	{
		XRay::Sound::Mixer::Seek(Voice._p->slot, At);
	}
	if (!(Flags & sm_2D))
	{
		Voice.set_position(State.Point);
	}
	Voice.set_volume(State.Volume);
	Voice.set_frequency(State.Pitch);
}
}

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

void ParticleObject::Update(u32, CFrustum*)
{
	ParticleCommand Next;
	{
		xrCriticalSectionGuard Guard(Lock);
		Next = Pending;
		Pending.Advance = 0.0f;
	}
	const bool IsRestart = Next.Restart != Restarted;
	UpdateParent(Next.Transform, Next.Velocity);
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
	SetOpacity(Next.Opacity);
	Remainder = std::min((IsRestart ? Next.Preroll * 1000.0 : Remainder) + Next.Advance * 1000.0, MaxPreroll * 1000.0);
	do
	{
		const u32 Elapsed = static_cast<u32>(std::min(Remainder, PrerollStep * 1000.0));
		Remainder -= Elapsed;
		CParticlesObject::Update(Elapsed, nullptr);
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
	Command.Advance = IsRestart || Command.State == EmitState::Cut ? 0.0f : (Input.Played + Input.Idle) * Input.Rate;
	Command.Transform = Input.World;
	Command.Velocity.sub(Input.World.c, LastPosition).mul(IsMoving ? 1.0f / Input.Played : 0.0f);
	Command.Size = Input.Size;
	Command.Opacity = Input.Opacity;
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

void DecalRuntime::Apply(const DecalData& Setup, const xr_vector<shared_str>& Shaders)
{
	if (IsApplied && Setup.Texture == AppliedTexture && Setup.Shader == AppliedShader)
	{
		return;
	}
	IsApplied = true;
	AppliedTexture = Setup.Texture;
	AppliedShader = Setup.Shader;
	Drop();
	Array->clear();
	const bool IsKnown = !Setup.Shader.size() || std::binary_search(Shaders.begin(), Shaders.end(), Setup.Shader, ShaderLess);
	Error = !Setup.Texture.size() ? shared_str("No texture") : !IsKnown ? shared_str("Shader not in shaders.xr") : shared_str();
	if (Error.size())
	{
		return;
	}
	Setup.Shader.size() ? Array->AppendMark(Setup.Shader.c_str(), Setup.Texture.c_str()) : Array->AppendMark(Setup.Texture.c_str());
}

bool DecalRuntime::Trace(const Fmatrix& World, Fvector& Point, u32& Triangle, float& Distance) const
{
	Fvector Up = World.j, Down;
	Up.normalize_safe();
	Down.invert(Up);
	const Fvector Start = Fvector().mad(World.c, Up, 0.5f);
	collide::rq_result Result;
	if (!g_pGameLevel || !Level().ObjectSpace.RayPick(Start, Down, 1.0f, collide::rqtStatic, Result, nullptr) || Result.element < 0)
	{
		return false;
	}
	const xr_vector<CDB::TRI>& Tris = Level().ObjectSpace.GetStaticTris();
	Triangle = static_cast<u32>(Result.element);
	Distance = Result.range;
	Point.mad(Start, Down, Result.range);
	return Triangle < Tris.size() && !Tris[Triangle].suppress_wm;
}

void DecalRuntime::Drive(const Fmatrix& World, float Width, float Height, bool IsShown)
{
	Fvector Point, Up = World.j, Side, Front;
	Up.normalize_safe();
	Side.crossproduct(std::fabs(Up.y) < 0.99f ? Fvector().set(0.0f, 1.0f, 0.0f) : Fvector().set(0.0f, 0.0f, 1.0f), Up).normalize_safe();
	Front.crossproduct(Up, Side);
	const float Spin = std::atan2(World.k.dotproduct(Front), World.k.dotproduct(Side));
	u32 Triangle = 0;
	float Distance = 0.0f;
	const bool IsPlaced = IsShown && !Array->empty() && Width > EPS_L && Height > EPS_L && Trace(World, Point, Triangle, Distance);
	if (IsPlaced && (!Handle || Handle->IsValid()) && Point.similar(PlacedAt, 1e-4f) && Triangle == PlacedTriangle && fsimilar(Width, PlacedWidth) && fsimilar(Height, PlacedHeight) && fsimilar(Spin, PlacedSpin))
	{
		return;
	}
	Drop();
	if (!IsPlaced)
	{
		return;
	}
	Handle = ::Render->add_DynamicWallmark(Array->GenerateWallmark(), Point, Width, Height, Spin, &Level().ObjectSpace.GetStaticTris()[Triangle], Level().ObjectSpace.GetStaticVerts().data());
	PlacedAt = Point;
	PlacedTriangle = Triangle;
	PlacedWidth = Width;
	PlacedHeight = Height;
	PlacedSpin = Spin;
}

void DecalRuntime::Drop()
{
	PlacedWidth = -1.0f;
	if (Handle && Handle->IsValid())
	{
		Handle->Destroy();
	}
	Handle.reset();
}
}
