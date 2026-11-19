#include "StdAfx.h"
#include "pch_script.h"
#include "Artefact.h"
#include "MercuryBall.h"
#include "GraviArtifact.h"
#include "BastArtifact.h"
#include "BlackGraviArtifact.h"
#include "ElectricBall.h"
#include "cta_game_artefact.h"

void RegisterArtefactAliases(lua_State* L)
{
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
