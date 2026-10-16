#include "StdAfx.h"
#include "ActorLightPerception.h"
#include "Actor.h"
#include "Creature.h"
#include "Torch.h"
#include "Weapon.h"
#include "inventory.h"
#include "Level.h"
#include "memory_manager.h"
#include "visual_memory_manager.h"
#include "sound_memory_manager.h"
#include "hit_memory_manager.h"
#include "memory_space_impl.h"
#include "ai_object_location.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrEngine/MonsterLogicTelemetry.h"
#include "HUDManager.h"
#ifndef MASTER_GOLD
#include "ai_debug.h"
#endif

namespace ActorLightTrace
{
struct Result
{
	collide::rq_result Hit;
	float Transparency = 1.f;
	CObject* Owner = nullptr;
	CObject* Target = nullptr;
};

bool AcceptObject(const collide::ray_defs&, CObject* Object, void* Data)
{
	const auto& Trace = *static_cast<Result*>(Data);
	for (CObject* Parent = Object; Parent; Parent = Parent->H_Parent())
	{
		if (Parent == Trace.Owner)
		{
			return false;
		}
	}
	return true;
}

bool AcceptHit(collide::rq_result& Hit, void* Data)
{
	auto& Trace = *static_cast<Result*>(Data);
	// Reaching the source's collision shell proves visibility of a source inside it.
	if (Hit.O && Hit.O == Trace.Target)
	{
		return false;
	}
	if (!Hit.O)
	{
		const CDB::TRI& Triangle = Level().ObjectSpace.GetStaticTris()[Hit.element];
		Trace.Transparency *= GMLib.GetMaterialByIdx(Triangle.material)->fVisTransparencyFactor;
		if (Trace.Transparency > 0.34f)
		{
			return true;
		}
	}
	Trace.Hit = Hit;
	return false;
}

bool IsHudLightActive(CHudItem* Item)
{
	const THudLightTorch* Light = Item ? Item->GetHudLight() : nullptr;
	return Light && Light->GetTorchInstalled() && Light->GetTorchActive() && Light->RenderLight && Item->GetState() != CHUDState::eHidden;
}
} // namespace ActorLightTrace

void ActorLightPerception::Load()
{
	Enabled = pSettings->section_exist("gunslinger_base");
	if (Enabled)
	{
		WeaponDistance = pSettings->read_if_exists<float>("gunslinger_base", "weapon_torch_treasure_dist", 45.f);
		HeadlampDistance = pSettings->read_if_exists<float>("gunslinger_base", "headlamp_treasure_dist", 45.f);
		HandDistance = pSettings->read_if_exists<float>("gunslinger_base", "lefthanded_torch_treasure_dist", 20.f);
		RevealDistance = pSettings->read_if_exists<float>("gunslinger_base", "light_palevo_dist", 20.f);
		NoticeDistance = pSettings->read_if_exists<float>("gunslinger_base", "light_see_dist", 60.f);
	}
	Reset();
}

void ActorLightPerception::Reset()
{
	SourceFrame = u32(-1);
	NextDebugTime = 0;
	SourceDistance = -1.f;
	HitObjectId = u16(-1);
	HasLightPoint = false;
	RayResults.r_clear();
}

void ActorLightPerception::UpdateSource(CActor& Player)
{
	if (SourceFrame == Device.dwFrame)
	{
		return;
	}
	SourceFrame = Device.dwFrame;
	SourceDistance = -1.f;
	HitObjectId = u16(-1);
	HasLightPoint = false;

	PIItem HeadItem = Player.inventory().ItemFromSlot(TORCH_SLOT);
	CTorch* Headlamp = HeadItem ? HeadItem->cast_torch() : nullptr;
	if (Headlamp && Headlamp->torch_active())
	{
		SourceDistance = HeadlampDistance;
	}
	else if (ActorLightTrace::IsHudLightActive(Player.GetDevice()))
	{
		SourceDistance = HandDistance;
	}
	else
	{
		PIItem ActiveItem = Player.inventory().ActiveItem();
		CWeapon* Weapon = ActiveItem ? ActiveItem->cast_weapon() : nullptr;
		if (ActorLightTrace::IsHudLightActive(Weapon))
		{
			SourceDistance = WeaponDistance;
		}
	}
	if (SourceDistance < 0.f)
	{
		return;
	}

	// Gunslinger uses the camera axis, with headlamp > hand torch > weapon priority.
	SourcePosition = Device.vCameraPosition;
	const Fvector Direction = Device.vCameraDirection;
	ActorLightTrace::Result Trace;
	Trace.Owner = &Player;
	// Hits beyond the illumination cutoff cannot affect either detection level.
	Trace.Hit.range = std::min(1000.f, SourceDistance / 0.99f + EPS_L);
	const collide::ray_defs Ray(SourcePosition, Direction, Trace.Hit.range, CDB::OPT_CULL, collide::rqtBoth);
	RayResults.r_clear();
	Level().ObjectSpace.RayQuery(RayResults, Ray, ActorLightTrace::AcceptHit, &Trace, ActorLightTrace::AcceptObject, &Player);
	HitObjectId = Trace.Hit.O ? Trace.Hit.O->ID() : u16(-1);
	const float Distance = Trace.Hit.range * 0.99f;
	HasLightPoint = (Trace.Hit.O || Trace.Hit.valid()) && Distance <= SourceDistance;
	LightPoint.mad(SourcePosition, Direction, Distance);
	RayResults.r_clear();
}

ActorLightPerception::Visibility ActorLightPerception::SeePoint(CCreature& Observer, const Fvector& Point, CObject* Target)
{
	// Preserve the original head approximation and forward hemisphere check.
	Fvector Eye = Observer.Position();
	Eye.y += 2.f;
	Fvector Direction;
	Direction.sub(Point, Eye);
	const float Distance = Direction.magnitude();
	if (Distance > std::max(RevealDistance, NoticeDistance))
	{
		return Visibility::None;
	}
	if (Distance > EPS_L)
	{
		Direction.div(Distance);
		if (Observer.Direction().dotproduct(Direction) < 0.f)
		{
			return Visibility::None;
		}
		ActorLightTrace::Result Trace;
		Trace.Owner = &Observer;
		Trace.Target = Target;
		Trace.Hit.range = Distance;
		const collide::ray_defs Ray(Eye, Direction, Distance, CDB::OPT_CULL, collide::rqtBoth);
		RayResults.r_clear();
		Level().ObjectSpace.RayQuery(RayResults, Ray, ActorLightTrace::AcceptHit, &Trace, ActorLightTrace::AcceptObject, &Observer);
		RayResults.r_clear();
		if (Trace.Hit.range * 1.01f < Distance)
		{
			return Visibility::None;
		}
	}
	return Distance <= RevealDistance ? Visibility::Revealed : Visibility::Suspicious;
}

void ActorLightPerception::Report(CActor& Player, CCreature& Observer, const char* Stage, Visibility Seen)
{
	if (!g_MonsterLogicTelemetry.DrawEnabled.load(std::memory_order_relaxed) ||
		HUD().GetCurrentRayQuery().O != &Observer || s32(Device.dwTimeGlobal - NextDebugTime) < 0)
	{
		return;
	}
	NextDebugTime = Device.dwTimeGlobal + 1000;
	Msg("[actor_light] observer=%s id=%u stage=%s enabled=%u source_range=%.2f beam_hit=%u spot=%u result=%u server=%u view=%u vision=%u hostile=%u ignored=%u",
		*Observer.cName(),
		u32(Observer.ID()),
		Stage,
		u32(Enabled),
		SourceDistance,
		u32(HitObjectId),
		u32(HasLightPoint),
		u32(Seen),
		u32(OnServer()),
		u32(Level().CurrentViewEntity() == &Player),
		u32(Observer.memory().visual().enabled()),
		u32(Observer.is_relation_enemy(&Player)),
		u32(Player.m_bEntityIgnoredByMonsters));
}

void ActorLightPerception::Update(CActor& Player, CCreature& Observer)
{
	if (!Enabled)
	{
		Report(Player, Observer, "no_gunslinger_base");
		return;
	}
	if (!IsGameTypeSingleCompatible() || !OnServer() || !Observer.g_Alive() || Observer.getDestroy() ||
		!Player.g_Alive() || Player.getDestroy() || Level().CurrentViewEntity() != &Player ||
		Player.m_bEntityIgnoredByMonsters || !Observer.is_relation_enemy(&Player))
	{
		Report(Player, Observer, "filtered");
		return;
	}
#ifndef MASTER_GOLD
	if (psAI_Flags.test(aiIgnoreActor))
	{
		Report(Player, Observer, "ai_ignore_actor");
		return;
	}
#endif
	UpdateSource(Player);
	if (SourceDistance < 0.f)
	{
		Report(Player, Observer, "no_active_torch");
		return;
	}

	Visibility Seen = Visibility::None;
	Fvector SeenPoint = SourcePosition;
	if (HasLightPoint && HitObjectId == Observer.ID())
	{
		Seen = Visibility::Revealed;
	}
	else
	{
		if (HasLightPoint)
		{
			Seen = SeePoint(Observer, LightPoint);
			SeenPoint = LightPoint;
		}
		if (Seen != Visibility::Revealed)
		{
			const Visibility SourceSeen = SeePoint(Observer, SourcePosition, &Player);
			if (SourceSeen > Seen)
			{
				Seen = SourceSeen;
				SeenPoint = SourcePosition;
			}
		}
	}
	Report(Player, Observer, "visibility", Seen);
	if (Seen == Visibility::None)
	{
		return;
	}

	const bool Revealed = Seen == Visibility::Revealed;
	// Scripted sleepers disable vision and wake through the normal on_sound event.
	// Gunslinger delivers this stimulus at the observer, regardless of actor distance.
	const int SoundType = Revealed ? SOUND_TYPE_WEAPON_SHOOTING : SOUND_TYPE_WEAPON | SOUND_TYPE_AMBIENT;
	Observer.sound_callback(&Player, SoundType, Observer.Position(), 100.f);
	if (CBaseMonster* Monster = Observer.cast_base_monster())
	{
		Monster->NoticeActorLight(Player, SeenPoint, Revealed);
		return;
	}

	Observer.memory().sound().NoticeActorLight(Player);
	if (Revealed)
	{
		// The original shooting stimulus adds hit memory, without an actual damage event.
		MemorySpace::CHitObject Evidence{};
		Evidence.fill(&Player, &Observer, u64(0));
		Evidence.m_direction.set(0.f, 0.f, 0.f);
		Evidence.m_bone_index = BI_NONE;
		Evidence.m_amount = 0.f;
		Observer.memory().hit().add(Evidence);
	}
}
