//.
#include "pch_script.h"
#include "mounted_turret.h"
#include "script_game_object.h"

using namespace luabind;

void CMountedTurret::SetNpcOwner(CScriptGameObject *script_game_object)
{
	if (script_game_object != nullptr)
	{
		CGameObject *game_object = smart_cast<CGameObject*>(&script_game_object->object());
		R_ASSERT(game_object);
		SetNpcOwner(game_object);
	} else
		CHolderCustom::SetNpcOwner(nullptr);
}

void CMountedTurret::SetEnemy(CScriptGameObject *script_game_object)
{
	R_ASSERT(script_game_object!=nullptr);

	Fvector3 pos;
	CGameObject *game_object = smart_cast<CGameObject*>(&script_game_object->object());
	R_ASSERT(game_object);
	game_object->Center(pos);
	SetDesiredEnemyPos(pos);
}

Fvector3 CMountedTurret::GetFirePoint() const
{ 
	return m_fire_pos; 
}

#pragma optimize("s",on)
void CMountedTurret::script_register(lua_State *L)
{
	module(L)
	[
		class_<CMountedTurret, bases<CGameObject, CHolderCustom>>("CMountedTurret")
			.def(constructor<>())
			.def("Action",										&CMountedTurret::Action)
			.def("SetEnemy",									&CMountedTurret::SetEnemy)
			.def("GetFirePoint",								&CMountedTurret::GetFirePoint)
			.def("GetFireDir",									&CMountedTurret::GetFireDir)
			.def("SetNpcOwner",									(void (CMountedTurret::*)(CScriptGameObject *)) &CMountedTurret::SetNpcOwner)
			.def("SetParam",									(void (CMountedTurret::*)(int, Fvector2)) &CMountedTurret::SetParam)
			.def("SetParam",									(void (CMountedTurret::*)(int, Fvector3)) &CMountedTurret::SetParam)
			.enum_("turret_action")
			[
				value("eActivate",								int(CMountedTurret::eActivate)),
				value("eDeactivate",							int(CMountedTurret::eDeactivate)),
				value("eDesiredDir",							int(CMountedTurret::eDesiredDir)),
				value("eDesiredEnemyDir",						int(CMountedTurret::eDesiredEnemyDir)),
				value("eDesiredEnemyPos",						int(CMountedTurret::eDesiredEnemyPos)),
				value("eFireStart",								int(CMountedTurret::eFireStart)),
				value("eFireStop",								int(CMountedTurret::eFireStop))

			]
	];
}