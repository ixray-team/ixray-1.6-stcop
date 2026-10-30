#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundFlyingGrab.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/CharacterPhysicsSupport.h"
#include "xrGame/PHMovementControl.h"
#include "xrGame/EntityCondition.h"
#include "xrPhysics/PhysicsShell.h"
#include "Include/xrRender/Kinematics.h"

namespace
{
struct SGrab
{
	u16 Victim;
	u32 OwnerGeneration;
	u32 DeathTime;
	u32 RequestedAt;
	u32 StartedAt = 0;
	u16 Element = 0;
	u16 Head = u16(-1);
	Fvector PreviousMouth = {0.f, 0.f, 0.f};
	Fvector MouthVelocity = {0.f, 0.f, 0.f};
	u32 LastUpdate = 0;
};
xr_map<u16, SGrab> Grabs;

bool Mouth(CBaseMonster& Owner, u16& Bone, Fvector& Position)
{
	IKinematics* Skeleton = PKinematics(Owner.Visual());
	if (!Skeleton)
	{
		return false;
	}
	const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Owner.cNameSect());
	Bone = Skeleton->LL_BoneID(Profile.GrabBone);
	if (Bone == u16(-1))
	{
		return false;
	}
	Skeleton->LL_GetBoneWorldPosition(Bone, Owner.XFORM(), Position);
	Position.mad(Owner.Direction(), Profile.GrabMouthOffset * Owner.GetVisualScale());
	return _valid(Position);
}

void Throw(CBaseMonster& Owner, CPhysicsShell& Shell, const SGrab& Grab)
{
	const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Owner.cNameSect());
	Fvector Velocity = Grab.MouthVelocity;
	if (Velocity.magnitude() > 15.f)
	{
		Velocity.normalize_safe().mul(15.f);
	}
	Velocity.mad(Owner.Direction(), Profile.GrabThrowSpeed);
	Velocity.y -= Profile.GrabThrowSpeed;
	Shell.set_LinearVel(Velocity);
}
}

void CELGroundFlyingGrab::ConfirmHit(CEntityAlive& Victim, u16 Attacker, float PreviousHealth)
{
	if (!CELSharedSettings::IsEnabled() || !OnServer() || Victim.UsedAI_Locations() || Victim.g_Alive() || PreviousHealth <= 0.f || Grabs.size() >= 8)
	{
		return;
	}
	CEntityAlive* Entity = ExtendedEntityes::FindEntity(Attacker);
	CBaseMonster* Owner = Entity ? smart_cast<CBaseMonster*>(Entity) : nullptr;
	CBaseMonster* Body = smart_cast<CBaseMonster*>(&Victim);
	if (!Owner || !Body || !Owner->g_Alive() || !Owner->is_jumping() || !Owner->is_relation_enemy(&Victim))
	{
		return;
	}
	const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Owner->cNameSect());
	if (!Profile.CanGrabFlying || PreviousHealth > Victim.conditions().GetMaxHealth() * Profile.GrabHealthThreshold)
	{
		return;
	}
	for (const auto& Pair : Grabs)
	{
		if (Pair.second.Victim == Victim.ID())
		{
			return;
		}
	}
	u16 Bone;
	Fvector Position;
	if (!Mouth(*Owner, Bone, Position))
	{
		return;
	}
	Fvector VictimCenter;
	Victim.Center(VictimCenter);
	if (Position.distance_to(VictimCenter) > Victim.Radius() + .6f * _sqrt(Owner->GetVisualScale()))
	{
		return;
	}
	Grabs[Attacker] = {u16(Victim.ID()), ExtendedEntityes::OwnerGeneration(Attacker), Body->GetLevelDeathTime(), Device.dwTimeGlobal};
}

void CELGroundFlyingGrab::Update()
{
	for (auto It = Grabs.begin(); It != Grabs.end();)
	{
		SGrab& Grab = It->second;
		CEntityAlive* OwnerEntity = ExtendedEntityes::FindEntity(It->first);
		CBaseMonster* Owner = OwnerEntity ? smart_cast<CBaseMonster*>(OwnerEntity) : nullptr;
		CEntityAlive* Victim = ExtendedEntityes::FindEntity(Grab.Victim);
		CBaseMonster* Body = Victim ? smart_cast<CBaseMonster*>(Victim) : nullptr;
		if (!Owner || !Body || !Owner->g_Alive() || Owner->GetScriptControl() || Owner->m_script_processing_active || Body->g_Alive() || ExtendedEntityes::OwnerGeneration(It->first) != Grab.OwnerGeneration || Body->GetLevelDeathTime() != Grab.DeathTime || !CELSharedSettings::IsEnabled())
		{
			It = Grabs.erase(It);
			continue;
		}
		CPhysicsShell* Shell = Body->PPhysicsShell();
		if (!Shell || !Shell->isActive() || !Shell->get_ElementsNumber())
		{
			if (Device.dwTimeGlobal - Grab.RequestedAt > 500u)
			{
				It = Grabs.erase(It);
			}
			else
			{
				++It;
			}
			continue;
		}
		if (Grab.StartedAt && Grab.LastUpdate == Device.dwTimeGlobal)
		{
			++It;
			continue;
		}
		const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Owner->cNameSect());
		Fvector MouthPosition;
		if (!Mouth(*Owner, Grab.Head, MouthPosition))
		{
			It = Grabs.erase(It);
			continue;
		}
		if (!Grab.StartedAt)
		{
			const float OwnerMass = Owner->character_physics_support()->movement()->GetMass();
			if (Shell->getMass() > OwnerMass * Profile.GrabMassRatio)
			{
				It = Grabs.erase(It);
				continue;
			}
			float Nearest = FLT_MAX;
			for (u16 Index = 0; Index < std::min<u16>(32, Shell->get_ElementsNumber()); ++Index)
			{
				Fvector Position;
				Shell->get_ElementByStoreOrder(Index)->GetGlobalPositionDynamic(&Position);
				const float Distance = Position.distance_to_sqr(MouthPosition);
				if (Distance < Nearest)
				{
					Nearest = Distance;
					Grab.Element = Index;
				}
			}
			const float ContactLimit = std::min(.75f, .3f * _sqrt(Owner->GetVisualScale())) + std::min(.3f, Body->Radius());
			if (Nearest > ContactLimit * ContactLimit)
			{
				It = Grabs.erase(It);
				continue;
			}
			Grab.StartedAt = Grab.LastUpdate = Device.dwTimeGlobal;
			Grab.PreviousMouth = MouthPosition;
		}
		if (Grab.Element >= Shell->get_ElementsNumber())
		{
			It = Grabs.erase(It);
			continue;
		}
		CPhysicsElement* Element = Shell->get_ElementByStoreOrder(Grab.Element);
		Fvector Position;
		Fvector Velocity;
		Element->GetGlobalPositionDynamic(&Position);
		Element->get_LinearVel(Velocity);
		const float Delta = float(Device.dwTimeGlobal - Grab.LastUpdate) * .001f;
		if (Delta > .001f)
		{
			Grab.MouthVelocity.sub(MouthPosition, Grab.PreviousMouth).div(Delta);
		}
		Grab.PreviousMouth = MouthPosition;
		Grab.LastUpdate = Device.dwTimeGlobal;
		if (Device.dwTimeGlobal - Grab.StartedAt >= Profile.GrabDuration || (!Owner->is_jumping() && Device.dwTimeGlobal - Grab.StartedAt >= 200u))
		{
			Throw(*Owner, *Shell, Grab);
			It = Grabs.erase(It);
			continue;
		}
		Fvector Force;
		Force.sub(MouthPosition, Position);
		if (Force.magnitude() > Owner->Radius() + Body->Radius() + 1.f)
		{
			// Break a lost contact rather than dragging a corpse through the world.
			It = Grabs.erase(It);
			continue;
		}
		Force.mul(100.f);
		Fvector RelativeVelocity;
		RelativeVelocity.sub(Velocity, Grab.MouthVelocity);
		Force.mad(RelativeVelocity, -18.f);
		const float Acceleration = Force.magnitude();
		if (Acceleration > 60.f)
		{
			Force.mul(60.f / Acceleration);
		}
		// ODE accumulates forces until its next fixed step. Preserve impulse across render rates.
		const float StepWeight = fixed_step > EPS_L ? std::min(.05f, Delta) / fixed_step : 0.f;
		Force.mul(Element->getMass() * StepWeight);
		Element->applyForce(Force.x, Force.y, Force.z);
		++It;
	}
}

void CELGroundFlyingGrab::FillDebug(u16 OwnerIdentifier, SELDebugRecord& Record)
{
	const auto Found = Grabs.find(OwnerIdentifier);
	if (Found != Grabs.end())
	{
		string128 Label;
		xr_sprintf(Label, "ЗАХВАТ ЛЕТУНА #%u: %s", Found->second.Victim, Found->second.StartedAt ? "физическое удержание" : "ожидание мёртвого ragdoll");
		Record.Senses.push_back(Label);
	}
}

void CELGroundFlyingGrab::ReleaseOwner(u16 Identifier)
{
	Grabs.erase(Identifier);
}

void CELGroundFlyingGrab::Remove(u16 Identifier)
{
	for (auto It = Grabs.begin(); It != Grabs.end();)
	{
		if (It->first == Identifier || It->second.Victim == Identifier)
		{
			It = Grabs.erase(It);
		}
		else
		{
			++It;
		}
	}
}

void CELGroundFlyingGrab::Clear()
{
	Grabs.clear();
}
