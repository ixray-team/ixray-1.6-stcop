////////////////////////////////////////////////////////////////////////////
//	Module 		: actor_script.cpp
//	Created 	: 17.01.2008
//  Modified 	: 17.01.2008
//	Author		: Dmitriy Iassenev
//	Description : actor script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "actor.h"
#include "level_changer.h"

using namespace luabind;

bool isFirstPerson() { return Actor()->active_cam() == eacFirstEye; }
void setFirstPerson() { Actor()->cam_Set(eacFirstEye); }
void setThirdPerson() { Actor()->cam_Set(eacLookAt); }

bool isGodMode() { return psActorFlags.test(AF_GODMODE); }

#pragma optimize("s",on)
void CActor::script_register(lua_State* L)
{
	module(L)
		[
			class_<CActor, CGameObject>("CActor")
				.def(constructor<>()),
				class_<CLevelChanger, CGameObject>("CLevelChanger")
				.def(constructor<>()),
				def("is_first_person", isFirstPerson),
				def("set_first_person", setFirstPerson),
				def("set_third_person", setThirdPerson),
				def("is_god_mode", isGodMode),
				
				class_<enum_exporter<EBoostParams>>("EBoostParams").enum_("eboostparams")[
					value("eBoostHpRestore", eBoostHpRestore),
					value("eBoostPowerRestore", eBoostPowerRestore),
					value("eBoostRadiationRestore", eBoostRadiationRestore),
					value("eBoostBleedingRestore", eBoostBleedingRestore),
					value("eBoostMaxWeight", eBoostMaxWeight),
					value("eBoostRadiationProtection", eBoostRadiationProtection),
					value("eBoostTelepaticProtection", eBoostTelepaticProtection),
					value("eBoostChemicalBurnProtection", eBoostChemicalBurnProtection),
					value("eBoostBurnImmunity", eBoostBurnImmunity),
					value("eBoostShockImmunity", eBoostShockImmunity),
					value("eBoostRadiationImmunity", eBoostRadiationImmunity),
					value("eBoostTelepaticImmunity", eBoostTelepaticImmunity),
					value("eBoostChemicalBurnImmunity", eBoostChemicalBurnImmunity),
					value("eBoostExplImmunity", eBoostExplImmunity),
					value("eBoostStrikeImmunity", eBoostStrikeImmunity),
					value("eBoostFireWoundImmunity", eBoostFireWoundImmunity),
					value("eBoostWoundImmunity", eBoostWoundImmunity),
					value("eBoostMaxCount", eBoostMaxCount)
			],

			class_<enum_exporter<EMovementStates>>("EMovementStates").enum_("emovementstates")
			[
				value("eOld", eOld),
				value("eWishful", eWishful),
				value("eReal", eReal)
			],

			class_<enum_exporter<EMoveCommand>>("EMoveCommand").enum_("emovecommand")
			[
				value("mcFwd", mcFwd),
				value("mcBack", mcBack),
				value("mcLStrafe", mcLStrafe),
				value("mcRStrafe", mcRStrafe),
				value("mcCrouch", mcCrouch),
				value("mcAccel", mcAccel),
				value("mcTurn", mcTurn),
				value("mcJump", mcJump),
				value("mcFall", mcFall),
				value("mcLanding", mcLanding),
				value("mcLanding2", mcLanding2),
				value("mcClimb", mcClimb),
				value("mcSprint", mcSprint),
				value("mcLLookout", mcLLookout),
				value("mcRLookout", mcRLookout),
				value("mcAnyMove", mcAnyMove),
				value("mcAnyAction", mcAnyAction),
				value("mcAnyState", mcAnyState),
				value("mcLookout", mcLookout),
				value("mcJumpSeq", mcJumpSeq)

			]
		];
}
