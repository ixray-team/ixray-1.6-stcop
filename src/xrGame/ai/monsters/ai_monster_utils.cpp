#include "StdAfx.h"
#include "ai_monster_utils.h"
#include "../../Entity.h"
#include "../../ai_object_location.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "../../../Include/xrRender/Kinematics.h"
#include "basemonster/base_monster.h"

#include "../../ai_object_location_impl.h"

// проверить, находится ли объект entity на ноде
// возвращает позицию объекта, если он находится на ноде, или центр его ноды
Fvector get_valid_position(const CEntity *entity, const Fvector &actual_position) 
{
	if (
		ai().level_graph().valid_vertex_id(entity->ai_location().level_vertex_id()) &&
		ai().level_graph().valid_vertex_position(entity->Position()) && 
		ai().level_graph().inside(entity->ai_location().level_vertex_id(), entity->Position())
		)
		return			(actual_position);
	else
		return			(ai().level_graph().vertex_position(entity->ai_location().level_vertex()));
}

// возвращает true, если объект entity находится на ноде
bool object_position_valid(const CEntity *entity)
{
	return				(
		ai().level_graph().valid_vertex_id(entity->ai_location().level_vertex_id()) &&
		ai().level_graph().valid_vertex_position(entity->Position()) && 
		ai().level_graph().inside(entity->ai_location().level_vertex_id(), entity->Position())
		);
}

Fvector get_bone_position	(CObject *object, LPCSTR bone_name)
{
	u16 bone_id			= PKinematics(object->Visual())->LL_BoneID				(bone_name);
	CBoneInstance &bone = PKinematics(object->Visual())->LL_GetBoneInstance	(bone_id);

	Fmatrix	global_transform;
	global_transform.mul	(object->XFORM(),bone.mTransform);

	return	(global_transform.c);
}

Fvector get_head_position(CObject *object) 
{
	pcstr bone_name		=	"bip01_head";
	if ( CBaseMonster* monster = object != nullptr ? object->cast_base_monster() : nullptr)
	{
		bone_name		=	monster->get_head_bone_name();
	}

	return get_bone_position(object, bone_name);
}
