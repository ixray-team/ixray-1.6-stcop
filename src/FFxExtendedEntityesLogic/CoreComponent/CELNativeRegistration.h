#pragma once
#include <functional>
#include "FFxExtendedEntityesLogic/CoreComponent/CELNativeEntityes.h"
#include "xrServerEntities/object_factory_impl.h"
#include "xrServerEntities/xrServer_Objects_ALife_Monsters.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowServer.h"
#if defined(FFX_EXTENDED_ENTITYES_LOGIC_BUILD) && !defined(NO_XR_GAME)
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Entityes/CELKomar.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Entityes/CELDuten.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Entityes/CELBat.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Entityes/CELStrekoza.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowEntity.h"
#endif

namespace ExtendedEntityes
{
#if defined(FFX_EXTENDED_ENTITYES_LOGIC_BUILD) || defined(NO_XR_GAME)
inline void RegisterNativeClasses(const std::function<void(CObjectItemAbstract*)>& RegisterItem)
{
#ifndef NO_SINGLE
#ifndef NO_XR_GAME
	RegisterItem(new CObjectItemClientServer<CELKomar, CSE_ALifeMonsterBase>(ExtendedEntityesClasses::Komar, "extended_komar"));
	RegisterItem(new CObjectItemClientServer<CELDuten, CSE_ALifeMonsterBase>(ExtendedEntityesClasses::Duten, "extended_duten"));
	RegisterItem(new CObjectItemClientServer<CELBat, CSE_ALifeMonsterBase>(ExtendedEntityesClasses::Bat, "extended_bat"));
	RegisterItem(new CObjectItemClientServer<CELStrekoza, CSE_ALifeMonsterBase>(ExtendedEntityesClasses::Strekoza, "extended_strekoza"));
	RegisterItem(new CObjectItemClientServer<CELCrowEntity, CELCrowServer>(ExtendedEntityesClasses::Crow, "scavenger_crow"));
#else
	RegisterItem(new CObjectItemSingle<CSE_ALifeMonsterBase, false>(ExtendedEntityesClasses::Komar, "extended_komar"));
	RegisterItem(new CObjectItemSingle<CSE_ALifeMonsterBase, false>(ExtendedEntityesClasses::Duten, "extended_duten"));
	RegisterItem(new CObjectItemSingle<CSE_ALifeMonsterBase, false>(ExtendedEntityesClasses::Bat, "extended_bat"));
	RegisterItem(new CObjectItemSingle<CSE_ALifeMonsterBase, false>(ExtendedEntityesClasses::Strekoza, "extended_strekoza"));
	RegisterItem(new CObjectItemSingle<CELCrowServer, false>(ExtendedEntityesClasses::Crow, "scavenger_crow"));
#endif
#endif
}
#else
void RegisterNativeClasses(const std::function<void(CObjectItemAbstract*)>& RegisterItem);
#endif
} // namespace ExtendedEntityes
