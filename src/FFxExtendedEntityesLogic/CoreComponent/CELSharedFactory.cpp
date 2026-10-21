#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/CoreComponent/CELSharedFactory.h"
#include "FFxExtendedEntityesLogic/CoreComponent/CELNativeEntityes.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingAdapter.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/CELFlyingNonAttackingBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELScavengerCrow.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowEntity.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundAdapter.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/CELGroundAttackBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Passive/CELGroundNonAttackingBase.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrServerEntities/clsid_game.h"

#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELBloodsucker.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELBoar.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELCat.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELChimera.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELDog.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELFlesh.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELFracture.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELPseudoDog.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELPseudoGigant.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELRat.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELSnork.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELTushkano.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/CELZombie.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/Psi/CELBurer.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/Psi/CELController.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/Psi/CELPoltergeist.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/Psi/CELPsyDog.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Entityes/Psi/CELPsyDogPhantom.h"

// Создаёт реализацию расширения для зарегистрированного игрового типа.
xr_unique_ptr<IELLogic> CELSharedFactory::Create(CBaseMonster& OwnerMonster)
{
	const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*OwnerMonster.cNameSect());
	if (ExtendedEntityesClasses::IsFlying(OwnerMonster.CLS_ID))
	{
		xr_unique_ptr<IELAdapter> FlyingAdapter = xr_unique_ptr<IELAdapter>(xr_make_unique<CELFlyingAdapter>(OwnerMonster).release());
		if (OwnerMonster.CLS_ID == ExtendedEntityesClasses::Crow)
		{
			CELCrowEntity* Bird = smart_cast<CELCrowEntity*>(&OwnerMonster);
			if (Bird)
			{
				return xr_unique_ptr<IELLogic>(xr_make_unique<CELScavengerCrow>(std::move(FlyingAdapter), Profile, *Bird).release());
			}
		}
		if (Profile.IsPassive)
		{
			return xr_unique_ptr<IELLogic>(xr_make_unique<CELFlyingNonAttackingBase>(std::move(FlyingAdapter), Profile).release());
		}
		return xr_unique_ptr<IELLogic>(xr_make_unique<CELFlyingAttackBase>(std::move(FlyingAdapter), Profile).release());
	}
	xr_unique_ptr<IELAdapter> Adapter = xr_unique_ptr<IELAdapter>(xr_make_unique<CELGroundAdapter>(OwnerMonster).release());
	if (Profile.IsPassive)
	{
		return xr_unique_ptr<IELLogic>(xr_make_unique<CELGroundNonAttackingBase>(std::move(Adapter), Profile).release());
	}
	// Species dispatch runs once when the common ECS component creates its implementation.
	switch (OwnerMonster.CLS_ID)
	{
		case CLSID_AI_BLOODSUCKER: return xr_unique_ptr<IELLogic>(xr_make_unique<CELBloodsucker>(std::move(Adapter), Profile).release());
		case CLSID_AI_BOAR: return xr_unique_ptr<IELLogic>(xr_make_unique<CELBoar>(std::move(Adapter), Profile).release());
		case CLSID_AI_CAT: return xr_unique_ptr<IELLogic>(xr_make_unique<CELCat>(std::move(Adapter), Profile).release());
		case CLSID_AI_CHIMERA: return xr_unique_ptr<IELLogic>(xr_make_unique<CELChimera>(std::move(Adapter), Profile).release());
		case CLSID_AI_DOG_BLACK: return xr_unique_ptr<IELLogic>(xr_make_unique<CELDog>(std::move(Adapter), Profile).release());
		case CLSID_AI_FLESH: return xr_unique_ptr<IELLogic>(xr_make_unique<CELFlesh>(std::move(Adapter), Profile).release());
		case CLSID_AI_FRACTURE: return xr_unique_ptr<IELLogic>(xr_make_unique<CELFracture>(std::move(Adapter), Profile).release());
		case CLSID_AI_DOG_RED: return xr_unique_ptr<IELLogic>(xr_make_unique<CELPseudoDog>(std::move(Adapter), Profile).release());
		case CLSID_AI_GIANT: return xr_unique_ptr<IELLogic>(xr_make_unique<CELPseudoGigant>(std::move(Adapter), Profile).release());
		case CLSID_AI_RAT: return xr_unique_ptr<IELLogic>(xr_make_unique<CELRat>(std::move(Adapter), Profile).release());
		case CLSID_AI_SNORK: return xr_unique_ptr<IELLogic>(xr_make_unique<CELSnork>(std::move(Adapter), Profile).release());
		case CLSID_AI_TUSHKANO: return xr_unique_ptr<IELLogic>(xr_make_unique<CELTushkano>(std::move(Adapter), Profile).release());
		case CLSID_AI_ZOMBIE: return xr_unique_ptr<IELLogic>(xr_make_unique<CELZombie>(std::move(Adapter), Profile).release());
		case CLSID_AI_BURER: return xr_unique_ptr<IELLogic>(xr_make_unique<CELBurer>(std::move(Adapter), Profile).release());
		case CLSID_AI_CONTROLLER: return xr_unique_ptr<IELLogic>(xr_make_unique<CELController>(std::move(Adapter), Profile).release());
		case CLSID_AI_POLTERGEIST: return xr_unique_ptr<IELLogic>(xr_make_unique<CELPoltergeist>(std::move(Adapter), Profile).release());
		case CLSID_AI_DOG_PSY: return xr_unique_ptr<IELLogic>(xr_make_unique<CELPsyDog>(std::move(Adapter), Profile).release());
		case CLSID_AI_DOG_PSY_PHANTOM: return xr_unique_ptr<IELLogic>(xr_make_unique<CELPsyDogPhantom>(std::move(Adapter), Profile).release());
		default: return xr_unique_ptr<IELLogic>(xr_make_unique<CELGroundAttackBase>(std::move(Adapter), Profile).release());
	}
}
