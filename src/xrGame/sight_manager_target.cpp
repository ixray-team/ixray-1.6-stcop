////////////////////////////////////////////////////////////////////////////
//	Module 		: sight_manager_target.cpp
//	Created 	: 27.12.2003
//  Modified 	: 08.04.2008
//	Author		: Dmitriy Iassenev
//	Description : sight manager target functions
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "sight_manager.h"
#include "ai/stalker/ai_stalker.h"
#include "stalker_movement_manager_smart_cover.h"
#include "level_graph.h"
#include "ai_space.h"
#include "detail_path_manager.h"
#include "Actor.h"
#include "../xrEngine/CameraBase.h"

void CSightManager::SetPointLookAngles(const Fvector& tPosition, float& yaw, float& pitch, Fvector const& look_position, const CGameObject* object)
{
	Fvector my_position = look_position;
	Fvector target = tPosition;
	if (!aim_target(my_position, target, object))
	{
		target = tPosition;
		my_position = look_position;
	}

	target.sub(my_position);
	if (fis_zero(target.square_magnitude()))
	{
		target.set(0.f, 0.f, 1.f);
	}

	target.getHP(yaw, pitch);
	VERIFY(_valid(yaw));
	VERIFY(_valid(pitch));
	yaw *= -1;
	pitch *= -1;
}

void CSightManager::SetFirePointLookAngles(const Fvector& tPosition, float& yaw, float& pitch, Fvector const& look_position, const CGameObject* object)
{
	SetPointLookAngles(tPosition, yaw, pitch, look_position, object);
}

bool CSightManager::aim_target(Fvector& my_position, Fvector& aim_target, const CGameObject* object) const
{
	if (!object)
	{
		return false;
	}

	if (m_object->aim_bone_id().size())
	{
		m_object->aim_target(aim_target, object);
		return true;
	}

	CGameObject* game_object = const_cast<CGameObject*>(object);
	CActor* actor = game_object->cast_actor();
	CEntity* entity = game_object->cast_entity();

	if (entity && entity->g_Alive() && (actor || game_object->cast_stalker()))
	{
		if (actor && actor->HUDview())
		{
			aim_target = actor->cam_Active()->vPosition;
		}
		else
		{
			IKinematics* kinematics = PKinematics(object->Visual());
			kinematics->LL_GetBoneWorldPosition(kinematics->LL_BoneID("bip01_head"), object->XFORM(), aim_target);
		}
		return true;
	}

	if (!object->use_center_to_aim())
	{
		return false;
	}

	if (actor)
	{
		m_object->Visual()->dcast_PKinematics()->CalculateBBox(false);
	}

	m_object->Center(my_position);

	// hack: the actor model is animated with a 20cm shift
	m_object->XFORM().transform_tiny(my_position, Fvector().set(.2f, my_position.y - m_object->Position().y, 0.f));
	return true;
}

void CSightManager::SetDirectionLook()
{
	MonsterSpace::SBoneRotation& head = object().movement().m_head;
	if (GetDirectionAngles(head.target.yaw, head.target.pitch))
	{
		head.target.yaw *= -1;
		head.target.pitch = 0.f;
	}
	else
	{
		head.target = head.current;
	}

	object().movement().m_body.target = head.target;
}

void CSightManager::SetLessCoverLook(const ILevelGraph::CVertex* tpNode, bool bDifferenceLook)
{
	SetDirectionLook();

	if (m_object->movement().detail().path().empty())
	{
		return;
	}

	SetLessCoverLook(tpNode, MAX_HEAD_TURN_ANGLE, bDifferenceLook);
}

void CSightManager::SetLessCoverLook(const ILevelGraph::CVertex* tpNode, float fMaxHeadTurnAngle, bool bDifferenceLook)
{
	if (tpNode == nullptr || tpNode == ai().level_graph().empty_vertex())
		return;
	const ILevelGraph& level_graph = ai().level_graph();
	const auto& detail = m_object->movement().detail();
	const float body_yaw = object().movement().m_body.target.yaw;

	float range = 0.f, fAngleOfView = 0.f;
	m_object->update_range_fov(range, fAngleOfView, m_object->eye_range, m_object->eye_fov);
	fAngleOfView = deg2rad(fAngleOfView) / 2.f;

	const ILevelGraph::CVertex* tpNextNode = nullptr;
	if (bDifferenceLook && !detail.path().empty() && detail.path().size() - 1 > detail.curr_travel_point_index())
	{
		const Fvector& next_point = detail.path()[detail.curr_travel_point_index() + 1].position;
		ILevelGraph::const_iterator i, e;
		level_graph.begin(tpNode, i, e);
		for (; i != e; ++i)
		{
			const u32 node_id = level_graph.value(tpNode, i);
			if (!level_graph.valid_vertex_id(node_id))
				continue;
			{

				continue;
			}

			const ILevelGraph::CVertex* node = level_graph.vertex(node_id);
			if (level_graph.inside(node, next_point))
			{
				tpNextNode = node;
				break;
			}
		}
	}

	float fMaxSquare = -1.f;
	float fBestAngle = object().movement().m_head.target.yaw;

	if (!tpNextNode)
	{
		for (float fIncrement = body_yaw - fMaxHeadTurnAngle; fIncrement <= body_yaw + fMaxHeadTurnAngle; fIncrement += fMaxHeadTurnAngle / 18.f)
		{
			const float fSquare = level_graph.compute_high_square(-fIncrement, fAngleOfView, tpNode);
			if (fSquare > fMaxSquare)
			{
				fMaxSquare = fSquare;
				fBestAngle = fIncrement;
			}
		}
	}
	else
	{
		for (float fIncrement = body_yaw - fMaxHeadTurnAngle; fIncrement <= body_yaw + fMaxHeadTurnAngle; fIncrement += 2 * fMaxHeadTurnAngle / 60.f)
		{
			const float fSquare = level_graph.compute_high_square(-fIncrement, fAngleOfView, tpNextNode) - level_graph.compute_high_square(-fIncrement, fAngleOfView, tpNode);
			if (fSquare > fMaxSquare || (fsimilar(fSquare, fMaxSquare, EPS_L) && std::abs(fIncrement - body_yaw) < std::abs(fBestAngle - body_yaw)))
			{
				fMaxSquare = fSquare;
				fBestAngle = fIncrement;
			}
		}
	}

	object().movement().m_head.target.yaw = angle_normalize_signed(fBestAngle);
	object().movement().m_head.target.pitch = 0;
	VERIFY(_valid(object().movement().m_head.target.yaw));
}

bool CSightManager::GetDirectionAngles(float& yaw, float& pitch)
{
	const auto& detail = m_object->movement().detail();
	const auto& path = detail.path();
	if (!detail.has_next_travel_point())
	{
		return GetDirectionAnglesByPrevPositions(yaw, pitch);
	}

	const u32 index = detail.curr_travel_point_index();
	Fvector().sub(path[index + 1].position, path[index].position).getHP(yaw, pitch);
	return true;
}

bool CSightManager::GetDirectionAnglesByPrevPositions(float& yaw, float& pitch)
{
	const u32 count = m_object->ps_Size();
	if (count < 2)
	{
		return false;
	}

	const Fvector previous = m_object->ps_Element(count - 2).vPosition;
	const Fvector current = m_object->ps_Element(count - 1).vPosition;
	VERIFY(_valid(previous));
	VERIFY(_valid(current));

	const Fvector direction = Fvector().sub(current, previous);
	if (direction.magnitude() < EPS_L)
	{
		return false;
	}

	direction.getHP(yaw, pitch);
	VERIFY(_valid(yaw));
	VERIFY(_valid(pitch));
	return true;
}
