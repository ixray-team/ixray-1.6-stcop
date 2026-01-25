#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/Artefact.h"
#include "../xrGame/MercuryBall.h"
#include "../xrGame/GraviArtifact.h"
#include "../xrGame/BlackDrops.h"
#include "../xrGame/Needles.h"
#include "../xrGame/BastArtifact.h"
#include "../xrGame/BlackGraviArtifact.h"
#include "../xrGame/DummyArtifact.h"
#include "../xrGame/ZudaArtifact.h"
#include "../xrGame/ThornArtifact.h"
#include "../xrGame/FadedBall.h"
#include "../xrGame/ElectricBall.h"
#include "../xrGame/RustyHairArtifact.h"
#include "../xrGame/GalantineArtifact.h"
#include "../xrGame/cta_game_artefact.h"

using namespace luabind;

#pragma optimize("s",on)
void CArtefact::script_register(lua_State *L)
{
	module(L)
	[
		class_<CArtefact			,CGameObject>("CArtefact")
		.def(						constructor<>() )
		.def("FollowByPath",		&CArtefact::FollowByPath)
		.def("SwitchVisibility",	&CArtefact::SwitchVisibility)
		.def("GetAfRank",			&CArtefact::GetAfRank),

		class_<CMercuryBall			,CArtefact>("CMercuryBall").def		(constructor<>()),
		class_<CBlackDrops			,CArtefact>("CBlackDrops").def		(constructor<>()),
		class_<CBlackGraviArtefact	,CArtefact>("CBlackGraviArtefact").def(constructor<>()),
		class_<CBastArtefact		,CArtefact>("CBastArtefact").def		(constructor<>()),
		class_<CDummyArtefact		,CArtefact>("CDummyArtefact").def		(constructor<>()),
		class_<CZudaArtefact		,CArtefact>("CZudaArtefact").def		(constructor<>()),
		class_<CThornArtefact		,CArtefact>("CThornArtefact").def		(constructor<>()),
		class_<CFadedBall			,CArtefact>("CFadedBall").def			(constructor<>()),
		class_<CElectricBall		,CArtefact>("CElectricBall").def		(constructor<>()),
		class_<CRustyHairArtefact	,CArtefact>("CRustyHairArtefact").def	(constructor<>()),
		class_<CGalantineArtefact	,CArtefact>("CGalantineArtefact").def	(constructor<>()),
		class_<CGraviArtefact		,CArtefact>("CGraviArtefact").def		(constructor<>())
	];
}
