#include "stdafx.h"
#include "pch_script.h"
#include "CustomDetector2.h"
#include "SimpleDetector2.h"
#include "CustomDetector.h"
#include "SimpleDetector.h"
#include "AdvancedAfDetector.h"
#include "EliteAfDetector.h"

using namespace luabind;

#pragma optimize("s",on)
void CCustomDetectorR::script_register(lua_State *L)
{
	module(L)
	[
		class_<CCustomDetectorR, CGameObject>("CCustomDetectorR").def(constructor<>()),//base
		class_<CCustomDetector, CGameObject>("CCustomDetector").def(constructor<>()),
		class_<CSimpleDetector, CGameObject>("CSimpleDetector").def(constructor<>()), //hud
		class_<CSimpleDetectorR, CGameObject>("CSimpleDetectorR").def(constructor<>()),
		class_<CAdvancedDetector, CGameObject>("CAdvancedDetector").def(constructor<>()),
		class_<CEliteDetector, CGameObject>("CEliteDetector").def(constructor<>())
	];
}