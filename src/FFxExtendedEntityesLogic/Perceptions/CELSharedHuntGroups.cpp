#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai/monsters/control_animation_base.h"
#include "xrGame/ai_monster_space.h"

namespace
{
struct SMember
{
	u16 Identifier;
	u32 Generation;
	Fvector Position;
	float Distance;
	u32 AttackStartedAt;
};
struct SStatus
{
	u32 Generation = 0;
	u32 At = 0;
	u32 AttackStartedAt = 0;
	float Stamina = 1.f;
	u32 PositionAt = 0;
	Fvector PlannedPosition = {0.f, 0.f, 0.f};
	bool IsBlocking = false;
	u16 PlannedTarget = u16(-1);
};
xr_map<u16, SStatus> Statuses;
xr_map<u64, xr_vector<SMember>> Groups;
u32 SampleAt = 0;
u64 Key(const CBaseMonster& Owner, u16 Target)
{
	return (u64(Owner.g_Team()) << 40) | (u64(Owner.g_Squad()) << 32) | (u64(Owner.g_Group()) << 24) | Target;
}
void Refresh()
{
	if (SampleAt && Device.dwTimeGlobal - SampleAt < 100u)
	{
		return;
	}
	SampleAt = Device.dwTimeGlobal;
	Groups.clear();
	for (auto It = Statuses.begin(); It != Statuses.end();)
	{
		if (It->second.Generation != ExtendedEntityes::OwnerGeneration(It->first) || Device.dwTimeGlobal - It->second.At > 2000u)
		{
			It = Statuses.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (const u16 Identifier : ExtendedEntityes::Owners())
	{
		CEntityAlive* Entity = ExtendedEntityes::FindEntity(Identifier);
		CBaseMonster* Monster = Entity ? smart_cast<CBaseMonster*>(Entity) : nullptr;
		if (!Monster || !Monster->g_Alive() || Monster->getDestroy())
		{
			continue;
		}
		const CEntityAlive* Target = Monster->EnemyMan.get_enemy();
		if (!Target || !Target->g_Alive() || ExtendedEntityes::IsIgnored(*Target))
		{
			continue;
		}
		SStatus& Status = Statuses[Identifier];
		Status.Generation = ExtendedEntityes::OwnerGeneration(Identifier);
		Status.At = Device.dwTimeGlobal;
		if (Monster->anim().m_tAction == ACT_ATTACK)
		{
			if (!Status.AttackStartedAt)
			{
				Status.AttackStartedAt = Device.dwTimeGlobal;
			}
		}
		else
		{
			Status.AttackStartedAt = 0;
		}
		const float Freshness = std::max(.25f, Status.Stamina);
		const float Score = Monster->Position().distance_to_sqr(Monster->EnemyMan.get_enemy_position()) / (Freshness * Freshness);
		Groups[Key(*Monster, Target->ID())].push_back({Identifier, Status.Generation, Monster->Position(), Score, Status.AttackStartedAt});
	}
	for (auto& Group : Groups)
	{
		std::sort(Group.second.begin(), Group.second.end(), [](const SMember& A, const SMember& B) { return A.Distance != B.Distance ? A.Distance < B.Distance : A.Identifier < B.Identifier; });
	}
}
}

SELHuntGroup CELSharedHuntGroups::Observe(CBaseMonster& Owner, u16 TargetIdentifier, float Radius)
{
	Refresh();
	SELHuntGroup Result;
	const auto Found = Groups.find(Key(Owner, TargetIdentifier));
	if (Found == Groups.end())
	{
		return Result;
	}
	Result.Count = 0;
	float Nearest = FLT_MAX;
	bool FoundOwner = false;
	for (const SMember& Member : Found->second)
	{
		if (Member.Generation != ExtendedEntityes::OwnerGeneration(Member.Identifier) || Member.Position.distance_to_sqr(Owner.Position()) > Radius * Radius)
		{
			continue;
		}
		CEntityAlive* Entity = ExtendedEntityes::FindEntity(Member.Identifier);
		CBaseMonster* Ally = Entity ? smart_cast<CBaseMonster*>(Entity) : nullptr;
		if (!Ally || !Ally->g_Alive() || Ally->getDestroy() || (Ally != &Owner && !CELSharedEnemySharing::CanReceive(Owner, *Ally)))
		{
			continue;
		}
		if (Member.Identifier == Owner.ID())
		{
			FoundOwner = true;
			Result.Rank = Result.Count;
		}
		++Result.Count;
		if (Ally == &Owner)
		{
			continue;
		}
		const auto Status = Statuses.find(Member.Identifier);
		if (Status != Statuses.end() && Status->second.IsBlocking && Status->second.PlannedTarget == TargetIdentifier && Device.dwTimeGlobal - Status->second.PositionAt < 1000u)
		{
			Result.HasBlockingPosition = true;
			Result.BlockingPosition = Status->second.PlannedPosition;
		}
		Fvector Away;
		Away.sub(Owner.Position(), Member.Position);
		const float Gap = Away.magnitude();
		if (Gap < Nearest)
		{
			Nearest = Gap;
			Result.AllyPosition = Member.Position;
			Result.HasAlly = true;
			Result.AllyAttackStartedAt = Member.AttackStartedAt;
		}
		if (Gap > EPS_L && Gap < 3.f)
		{
			Result.Separation.mad(Away, (3.f - Gap) / Gap);
		}
	}
	if (!FoundOwner)
	{
		Result.Rank = Result.Count++;
	}
	return Result;
}

void CELSharedHuntGroups::ReportPosition(CBaseMonster& Owner, const Fvector& Position, u16 TargetIdentifier, bool IsBlocking)
{
	SStatus& Status = Statuses[u16(Owner.ID())];
	Status.Generation = ExtendedEntityes::OwnerGeneration(Owner.ID());
	Status.PositionAt = Status.At = Device.dwTimeGlobal;
	Status.PlannedPosition = Position;
	Status.IsBlocking = IsBlocking;
	Status.PlannedTarget = TargetIdentifier;
}

void CELSharedHuntGroups::ReportStamina(CBaseMonster& Owner, float Stamina)
{
	SStatus& Status = Statuses[u16(Owner.ID())];
	Status.Generation = ExtendedEntityes::OwnerGeneration(Owner.ID());
	Status.At = Device.dwTimeGlobal;
	Status.Stamina = clampr(Stamina, 0.f, 1.f);
}

void CELSharedHuntGroups::Clear()
{
	Statuses.clear();
	Groups.clear();
	SampleAt = 0;
}
