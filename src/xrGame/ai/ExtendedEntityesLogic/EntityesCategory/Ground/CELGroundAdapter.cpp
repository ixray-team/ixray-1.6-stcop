#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/CELGroundAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_space.h"
#include "level_graph.h"

// Читает штатный предел высоты только у вида с настоящим контроллером прыжка.
CELGroundAdapter::CELGroundAdapter(CBaseMonster& OwnerMonster) : CELBaseAdapter(OwnerMonster)
{
	const char* Section = OwnerMonster.cNameSect().c_str();
	if (OwnerMonster.com_man().get_jump_control() && pSettings->line_exist(Section, "jump_max_height"))
	{
		JumpHeight = pSettings->r_float(Section, "jump_max_height");
		if (!_valid(JumpHeight) || JumpHeight < 0.f)
		{
			JumpHeight = 0.f;
		}
	}
}

// Не назначает бесконечную погоню за телом летуна выше ближней атаки и штатного прыжка.
bool CELGroundAdapter::CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const
{
	if (Candidate.UsedAI_Locations())
	{
		return true;
	}
	Fbox OwnerBounds;
	OwnerBounds.xform(OwnerMonster.BoundingBox(), OwnerMonster.XFORM());
	float TargetBottom = KnownPosition.y + Candidate.BoundingBox().min.y;
	if (CanSee(Candidate))
	{
		Fbox TargetBounds;
		TargetBounds.xform(Candidate.BoundingBox(), Candidate.XFORM());
		TargetBottom = TargetBounds.min.y;
	}
	const float MeleeReach = std::max(0.f, OwnerMonster.MeleeChecker.get_max_distance());
	float GroundHeight = OwnerMonster.Position().y;
	if (ai().get_level_graph() && ai().level_graph().valid_vertex_position(KnownPosition))
	{
		const u32 GroundVertex = ai().level_graph().vertex_id(KnownPosition);
		if (ai().level_graph().valid_vertex_id(GroundVertex))
		{
			// Склон под целью не равен высоте полёта: оцениваем атаку с местной наземной опоры.
			GroundHeight = ai().level_graph().vertex_plane_y(GroundVertex, KnownPosition.x, KnownPosition.z);
		}
	}
	const float ReachHeight = GroundHeight + std::max(OwnerBounds.max.y - OwnerMonster.Position().y, JumpHeight) + MeleeReach;
	return _valid(TargetBottom) && TargetBottom <= ReachHeight;
}
