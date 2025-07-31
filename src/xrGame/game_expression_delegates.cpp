#include "StdAfx.h"
#include "../xrCore/XmlParser/Expression.h"
#include "Actor.h"
#include "CustomOutfit.h"
#include "EntityCondition.h"
#include "Inventory.h"
#include "ActorHelmet.h"
#include "ActorCondition.h"
#include "PDA.h"
#include "Level.h"
#include "actor_mp_client.h"

inline CActor* GetActor()
{
    if (IsGameTypeSingle())
        return Actor();

    return smart_cast<CActor*>(Level().CurrentViewEntity());
}

#define DECLARE_OUTFIT_PROTECTION_DELEGATE(protectionType) \
float GetOutfit##protectionType##Protection() \
{ \
    if (CCustomOutfit* ActorOutfit = GetActor()->GetOutfit()) \
    { \
        return ActorOutfit->GetDefHitTypeProtection(ALife::protectionType); \
    } \
    return 0.0f; \
}

DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeBurn);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeShock);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeChemicalBurn);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeRadiation);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeTelepatic);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeWound);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeFireWound);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeStrike);
DECLARE_OUTFIT_PROTECTION_DELEGATE(eHitTypeExplosion);

#undef DECLARE_OUTFIT_PROTECTION_DELEGATE

#define DECLARE_HELMET_PROTECTION_DELEGATE(protectionType) \
float GetHelmet##protectionType##Protection() \
{ \
    PIItem itm = GetActor()->inventory().ItemFromSlot(HELMET_SLOT); \
    CHelmet* helmet = smart_cast<CHelmet*>(itm); \
    if (helmet != nullptr) \
    { \
        return helmet->GetDefHitTypeProtection(ALife::protectionType); \
    } \
    return 0.0f; \
}

DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeBurn);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeShock);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeChemicalBurn);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeRadiation);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeTelepatic);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeWound);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeFireWound);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeStrike);
DECLARE_HELMET_PROTECTION_DELEGATE(eHitTypeExplosion);

#undef DECLARE_HELMET_PROTECTION_DELEGATE

#define DECLARE_BOOSTER_PROTECTION_DELEGATE(boosterType) \
float GetBooster##boosterType() \
{ \
    return GetActor()->conditions().GetBoosterValueByType(boosterType); \
}

DECLARE_BOOSTER_PROTECTION_DELEGATE(eBoostRadiationProtection);
DECLARE_BOOSTER_PROTECTION_DELEGATE(eBoostChemicalBurnProtection);
DECLARE_BOOSTER_PROTECTION_DELEGATE(eBoostTelepaticProtection);

#undef DECLARE_BOOSTER_PROTECTION_DELEGATE

#define DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(protectionType) \
float GetArtefacts##protectionType##Protection() \
{ \
    return GetActor()->GetProtection_ArtefactsOnBelt(ALife::protectionType); \
}

DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeBurn);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeShock);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeChemicalBurn);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeRadiation);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeTelepatic);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeWound);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeFireWound);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeStrike);
DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE(eHitTypeExplosion);

#undef DECLARE_ARTEFACTS_ON_BELT_PROTECTION_DELEGATE

#define DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(protectionType) \
float GetZoneMaxPower##protectionType() \
{ \
    return GetActor()->conditions().GetZoneMaxPower(ALife::protectionType); \
}

DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeBurn);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeShock);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeChemicalBurn);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeRadiation);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeTelepatic);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeWound);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeFireWound);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeStrike);
DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE(eHitTypeExplosion);

#undef DECLARE_ZONEMAXPOWER_PROTECTION_DELEGATE

int GetActorPDAContactsName()	{return (int)GetActor()->GetPDA()->ActiveContactsNum();	        }
const char* GetPlayerName()		{ return GetActor()->Name();					    			    }

float GetPlayerHealth()			            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetHealth();				    }
float GetPlayerPsyHealth()		            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetPsy();				        }
float GetPlayerRad()			            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetRadiation();			    }
float GetPlayerPower()			            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetPower();				    }
float GetPlayerSatiety()		            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetSatiety();				    }
float GetPlayerBleedingSpeed()	            { return GetActor() == nullptr ? 0.f : GetActor()->conditions().BleedingSpeed();				}
float GetPlayerRestoreSpeed()	            { return GetActor() == nullptr ? 0.f : GetActor()->GetRestoreSpeed(ALife::ePowerRestoreSpeed) / GetActor()->conditions().GetMaxPowerRestoreSpeed();}
float GetOutfiteMaxFireWoundProtection()    { return GetActor() == nullptr ? 0.f : GetActor()->conditions().GetMaxFireWoundProtection();	}
float GetPlayerArmor()                      
{
    // P.S. да, знаю что это лютая васня, но что поделать
    if (GetActor()->GetOutfit() != nullptr && GetActor()->GetHelmet() != nullptr)
        return (GetActor()->GetOutfit()->GetCondition() * 0.5f) + (GetActor()->GetHelmet()->GetCondition() * 0.5f);
    else if (GetActor()->GetOutfit() != nullptr)
        return GetActor()->GetOutfit()->GetCondition();
    else if (GetActor()->GetHelmet() != nullptr)
        return GetActor()->GetHelmet()->GetCondition();
    else
        return 0.f;
}

void RegisterExpressionDelegates ()
{
    //Actor outfit protections
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitBurnProtection",				GetOutfiteHitTypeBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitShockProtection",			GetOutfiteHitTypeShockProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitChemicalBurnProtection",		GetOutfiteHitTypeChemicalBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitRadiationProtection",		GetOutfiteHitTypeRadiationProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitTelepaticProtection",		GetOutfiteHitTypeTelepaticProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitWoundProtection",			GetOutfiteHitTypeWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitFireWoundProtection",		GetOutfiteHitTypeFireWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitStrikeProtection",			GetOutfiteHitTypeStrikeProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitExplosionProtection",		GetOutfiteHitTypeExplosionProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorOutfitMaxFireWoundProtection",		GetOutfiteMaxFireWoundProtection);

    //Helmet outfit protections
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetBurnProtection",				GetHelmeteHitTypeBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetShockProtection",			GetHelmeteHitTypeShockProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetChemicalBurnProtection",		GetHelmeteHitTypeChemicalBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetRadiationProtection",		GetHelmeteHitTypeRadiationProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetTelepaticProtection",		GetHelmeteHitTypeTelepaticProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetWoundProtection",			GetHelmeteHitTypeWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetFireWoundProtection",		GetHelmeteHitTypeFireWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetStrikeProtection",			GetHelmeteHitTypeStrikeProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorHelmetExplosionProtection",		GetHelmeteHitTypeExplosionProtection);
    
    //Booster protections
    g_uiExpressionMgr->RegisterVariable("fltActorBoostRadiationProtection",			GetBoostereBoostRadiationProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorBoostTelepaticProtection",			GetBoostereBoostTelepaticProtection);
    g_uiExpressionMgr->RegisterVariable("fltActorBoostChemicalBurnProtection",		GetBoostereBoostChemicalBurnProtection);

    //Artefacts on belt protection
    g_uiExpressionMgr->RegisterVariable("fltArtefactsBurnProtection",				GetArtefactseHitTypeBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsShockProtection",				GetArtefactseHitTypeShockProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsChemicalBurnProtection",		GetArtefactseHitTypeChemicalBurnProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsRadiationProtection",			GetArtefactseHitTypeRadiationProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsTelepaticProtection",			GetArtefactseHitTypeTelepaticProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsWoundProtection",				GetArtefactseHitTypeWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsFireWoundProtection",			GetArtefactseHitTypeFireWoundProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsStrikeProtection",				GetArtefactseHitTypeStrikeProtection);
    g_uiExpressionMgr->RegisterVariable("fltArtefactsExplosionProtection",			GetArtefactseHitTypeExplosionProtection);

    //Zone max power (maximum of protection value)
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerBurnProtection",				GetZoneMaxPowereHitTypeBurn);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerShockProtection",			GetZoneMaxPowereHitTypeShock);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerChemicalBurnProtection",		GetZoneMaxPowereHitTypeChemicalBurn);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerRadiationProtection",		GetZoneMaxPowereHitTypeRadiation);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerTelepaticProtection",		GetZoneMaxPowereHitTypeTelepatic);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerWoundProtection",			GetZoneMaxPowereHitTypeWound);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerFireWoundProtection",		GetZoneMaxPowereHitTypeFireWound);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerStrikeProtection",			GetZoneMaxPowereHitTypeStrike);
    g_uiExpressionMgr->RegisterVariable("fltZoneMaxPowerExplosionProtection",		GetZoneMaxPowereHitTypeExplosion);

    //Actor variables
    g_uiExpressionMgr->RegisterVariable("intPDAActiveContacts",						GetActorPDAContactsName);
    g_uiExpressionMgr->RegisterVariable("strPlayerName",								GetPlayerName);
    g_uiExpressionMgr->RegisterVariable("fltPlayerHealth",							GetPlayerHealth);
    g_uiExpressionMgr->RegisterVariable("fltPlayerRad",							    GetPlayerRad);
    g_uiExpressionMgr->RegisterVariable("fltPlayerPsy",							    GetPlayerPsyHealth);
	g_uiExpressionMgr->RegisterVariable("fltPlayerPower",							GetPlayerPower);
	g_uiExpressionMgr->RegisterVariable("fltPlayerSatiety",							GetPlayerSatiety);
	g_uiExpressionMgr->RegisterVariable("fltPlayerRestoreSpeed",						GetPlayerRestoreSpeed);
	g_uiExpressionMgr->RegisterVariable("fltPlayerBleedingSpeed",		    		GetPlayerBleedingSpeed);
	g_uiExpressionMgr->RegisterVariable("fltPlayerArmor",							GetPlayerArmor);
}