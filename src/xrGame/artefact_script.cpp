#include "StdAfx.h"
#include "pch_script.h"
#include "Artefact.h"
#include "MercuryBall.h"
#include "GraviArtifact.h"
#include "BastArtifact.h"
#include "BlackGraviArtifact.h"
#include "ElectricBall.h"
#include "cta_game_artefact.h"

using namespace luabind;

#pragma optimize("s",on)
void CArtefact::script_register(lua_State *L)
{
	module(L)
	[
		class_<CArtefact ,CGameObject>("CArtefact")
			.def(constructor<>())
			.def("FollowByPath", &CArtefact::FollowByPath)
			.def("SwitchVisibility", &CArtefact::SwitchVisibility)
			.def("GetAfRank", &CArtefact::GetAfRank)
			.def("GetJumpHeightModifier", &CArtefact::GetJumpHeightModifier)
			.def("SetJumpHeightModifier", &CArtefact::SetJumpHeightModifier)
			.def("GetMovementSpeedModifier", &CArtefact::GetMovementSpeedModifier)
			.def("SetMovementSpeedModifier", &CArtefact::SetMovementSpeedModifier)
			.def("GetEquipmentDurabilityModifier", &CArtefact::GetEquipmentDurabilityModifier)
			.def("GetInventoryWeightModifier", &CArtefact::GetInventoryWeightModifier)
			.def("SetInventoryWeightModifier", &CArtefact::SetInventoryWeightModifier)
			.def("SetEquipmentDurabilityModifier", &CArtefact::SetEquipmentDurabilityModifier),

		class_<CMercuryBall			,CArtefact>("CMercuryBall").def(constructor<>()),
		class_<CBlackGraviArtefact	,CArtefact>("CBlackGraviArtefact").def(constructor<>()),
		class_<CBastArtefact		,CArtefact>("CBastArtefact").def(constructor<>()),
		class_<CElectricBall		,CArtefact>("CElectricBall").def(constructor<>()),
		class_<CGraviArtefact		,CArtefact>("CGraviArtefact").def(constructor<>())
	];

	luabind::object artefact_class = luabind::get_globals(L)["CArtefact"];

	auto MakeAliasLambda = [&artefact_class, L](const char* TypeID)
	{
		lua_pushstring(L, TypeID);
		artefact_class.pushvalue();
		lua_settable(L, LUA_GLOBALSINDEX);
	};

	MakeAliasLambda("CDummyArtefact");
	MakeAliasLambda("CZudaArtefact");
	MakeAliasLambda("CThornArtefact");
	MakeAliasLambda("CRustyHairArtefact");
	MakeAliasLambda("CFadedBall");
	MakeAliasLambda("CBlackDrops");
	MakeAliasLambda("CGalantineArtefact");
}
