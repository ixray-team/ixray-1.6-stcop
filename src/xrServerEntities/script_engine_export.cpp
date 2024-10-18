////////////////////////////////////////////////////////////////////////////
//	Module 		: script_engine_export.cpp
//	Created 	: 01.04.2004
//  Modified 	: 22.06.2004
//	Author		: Еблан Конченный
//	Description : XRay Script Engine export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "script_engine_export.h"

#include "alife_human_brain.h"
#include "alife_monster_brain.h"

#ifdef XRSE_FACTORY_EXPORTS

#else
#	include "PHSimpleCalls.h"
#	include "eatable_item.h"
#	include "RadioactiveZone.h"
#	include "ZoneCampfire.h"
#	include "alife_online_offline_group_brain.h"
#	include "ui/FractionState.h"
#	include "../../xrUI/Widgets/UIListBox.h"
#	include "ai/crow/ai_crow.h"
#	include "ui/UIActorMenu.h"
#	include	"ActorCondition.h"
#endif

void export_classes	(lua_State *L)
{
	CScriptEngine::script_register(L);
	CScriptNetPacket::script_register(L);
	CScriptFcolor::script_register(L);
	CScriptFlags::script_register(L);
	CScriptFmatrix::script_register(L);
	CScriptFvector::script_register(L);
	CScriptIniFile::script_register(L);
	CScriptReader::script_register(L);
	CScriptRTokenList::script_register(L);
	CScriptSoundType::script_register(L);
	CScriptTokenList::script_register(L);
	
	CObjectFactory::script_register(L);
	CPureServerObject::script_register(L);
	CSE_Abstract::script_register(L);
	CSE_Shape::script_register(L);
	CSE_Spectator::script_register(L);
	CSE_Temporary::script_register(L);
	CSE_PHSkeleton::script_register(L);
	CSE_Visual::script_register(L);
	CSE_Motion::script_register(L);
	CSE_AbstractVisual::script_register(L);
	CSE_ALifeSchedulable::script_register(L);
	CSE_ALifeGraphPoint::script_register(L);
	CSE_ALifeObject::script_register(L);
	CSE_ALifeGroupAbstract::script_register(L);
	CSE_ALifeDynamicObject::script_register(L);
	CSE_ALifeDynamicObjectVisual::script_register(L);
	CSE_ALifePHSkeletonObject::script_register(L);
	CSE_ALifeSpaceRestrictor::script_register(L);
	CSE_ALifeLevelChanger::script_register(L);
	CSE_ALifeSmartZone::script_register(L);
	CSE_ALifeObjectPhysic::script_register(L);
	CSE_ALifeObjectHangingLamp::script_register(L);
	CSE_ALifeObjectProjector::script_register(L);
	CSE_ALifeHelicopter::script_register(L);
	CSE_ALifeCar::script_register(L);
	CSE_ALifeObjectBreakable::script_register(L);
	CSE_ALifeObjectClimable::script_register(L);
	CSE_ALifeMountedWeapon::script_register(L);
	CSE_ALifeTeamBaseZone::script_register(L);
	CSE_ALifeInventoryBox::script_register(L);
	CSE_ALifeInventoryItem::script_register(L);
	CSE_ALifeItem::script_register(L);
	CSE_ALifeItemTorch::script_register(L);
	CSE_ALifeItemAmmo::script_register(L);
	CSE_ALifeItemWeapon::script_register(L);
	CSE_ALifeItemWeaponMagazined::script_register(L);
	CSE_ALifeItemWeaponMagazinedWGL::script_register(L);
	CSE_ALifeItemWeaponShotGun::script_register(L);
	CSE_ALifeItemWeaponAutoShotGun::script_register(L);
	CSE_ALifeItemDetector::script_register(L);
	CSE_ALifeItemArtefact::script_register(L);
	CSE_ALifeItemPDA::script_register(L);
	CSE_ALifeItemDocument::script_register(L);
	CSE_ALifeItemGrenade::script_register(L);
	CSE_ALifeItemExplosive::script_register(L);
	CSE_ALifeItemBolt::script_register(L);
	CSE_ALifeItemsNotSave::script_register(L);
	CSE_ALifeItemHelmet::script_register(L);
	CSE_ALifeItemCustomOutfit::script_register(L);
	//CSE_ALifeStationaryMgun::script_register(L);
	CSE_ALifeTraderAbstract::script_register(L);
	CSE_ALifeTrader::script_register(L);
	CSE_ALifeCustomZone::script_register(L);
	CSE_ALifeAnomalousZone::script_register(L);
	CSE_ALifeTorridZone::script_register(L);
	CSE_ALifeZoneVisual::script_register(L);
	CSE_ALifeCreatureAbstract::script_register(L);
	CSE_ALifeMonsterAbstract::script_register(L);
	CSE_ALifeMonsterBase::script_register(L);
	CSE_ALifeCreatureActor::script_register(L);
	CSE_ALifeCreatureCrow::script_register(L);
	CSE_ALifeCreaturePhantom::script_register(L);
	CSE_ALifeMonsterRat::script_register(L);
	CSE_ALifeMonsterZombie::script_register(L);
	CSE_ALifePsyDogPhantom::script_register(L);
	CSE_ALifeHumanAbstract::script_register(L);
	CSE_ALifeHumanStalker::script_register(L);
	CSE_ALifeOnlineOfflineGroup::script_register(L);
	CSE_SmartCover::script_register(L);
	
#ifdef XRSE_FACTORY_EXPORTS
	CScriptPropertiesListHelper::script_register(L);
#else
	
	gamespy_gp_account_manager::script_register(L);
	gamespy_gp_suggest_nicks_cb::script_register(L);
	gamespy_gp_account_operation_cb::script_register(L);
	gamespy_gp_account_profiles_cb::script_register(L);
	gamespy_gp_found_email_cb::script_register(L);
	CScriptActionBase::script_register(L);
	CScriptActionPlanner::script_register(L);
	CScriptActionPlannerAction::script_register(L);
	CScriptGameObject::script_register(L);
	CALifeMonsterDetailPathManager::script_register(L);
	CALifeMonsterMovementManager::script_register(L);
	CALifeMonsterPatrolPathManager::script_register(L);
	//CALifeOnlineOfflineGroupBrain::script_register(L);
	CALifeSimulator::script_register(L);
	CALifeSmartTerrainTask::script_register(L);
	CClientSpawnManager::script_register(L);
	console_registrator::script_register(L);
	CCoverPoint::script_register(L);
	demo_player_info::script_register(L);
	demo_info::script_register(L);
	CEF_Storage::script_register(L);
	fs_registrator::script_register(L);
	CGameTask::script_register(L);
	DLL_PureScript::script_register(L);
	ISheduledScript::script_register(L);
	IRenderableScript::script_register(L);
	ICollidableScript::script_register(L);
	CObjectScript::script_register(L);
	CPhysicObject::script_register(L);
	CExplosive::script_register(L);
	CF1::script_register(L);
	CBlendScript::script_register(L);
	IRender_VisualScript::script_register(L);
	IKinematicsAnimatedScript::script_register(L);
	CScriptGameDifficulty::script_register(L);
	CHairsZone::script_register(L);
	CHangingLamp::script_register(L);
	CHelicopter::script_register(L);
	CHolderCustom::script_register(L);
	key_binding_registrator::script_register(L);
	CLevel::script_register(L);
	gamespy_gp_profile::script_register(L);
	gamespy_gp_login_operation_cb::script_register(L);
	gamespy_gp_login_manager::script_register(L);
	CMemoryInfo::script_register(L);
	CMincer::script_register(L);
	CMosquitoBald::script_register(L);
	CParticleParams::script_register(L);
	CPatrolPathParams::script_register(L);
	CPhraseDialogExporter::script_register(L);
	CPHCallOnStepCondition::script_register(L);
	CPHExpireOnStepCondition::script_register(L);
	CPHConstForceAction::script_register(L);
	cphysics_element_scripted::script_register(L);
	cphysics_joint_scripted::script_register(L);
	cphysics_shell_scripted::script_register(L);
	cphysics_world_scripted::script_register(L);
	gamespy_profile_store_operation_cb::script_register(L);
	profile_data_script_registrator::script_register(L);
	gamespy_profile_profile_store::script_register(L);
	CScriptPropertyEvaluator::script_register(L);
	CPropertyStorage::script_register(L);
	//CRadioactiveZone::script_register(L);
	CRGD5::script_register(L);
	CSavedGameWrapper::script_register(L);
	CScope::script_register(L);
	CScriptActionCondition::script_register(L);
	CScriptAnimationAction::script_register(L);
	CScriptBinderObject::script_register(L);
	CScriptEffector::script_register(L);
	CScriptEntityAction::script_register(L);
	CScriptHit::script_register(L);
	lanim_registrator::script_register(L);
	CScriptMonsterAction::script_register(L);
	CScriptMonsterHitInfo::script_register(L);
	CScriptMovementAction::script_register(L);
	CScriptObjectAction::script_register(L);
	CScriptParticles::script_register(L);
	CScriptParticleAction::script_register(L);
	CScriptRenderDevice::script_register(L);
	CScriptSound::script_register(L);
	CScriptSoundAction::script_register(L);
	CScriptSoundInfo::script_register(L);
	UIRegistrator::script_register(L);
	CScriptWatchAction::script_register(L);
	CScriptWorldPropertyWrapper::script_register(L);
	CScriptWorldStateWrapper::script_register(L);
	CScriptZone::script_register(L);
	smart_cover__object::script_register(L);
	CSmartZone::script_register(L);
	CSpaceRestrictor::script_register(L);
	CStalkerOutfit::script_register(L);
	CTorch::script_register(L);
	//FactionState::script_register(L);
	FractionState::script_register(L);
	CWeaponAK74::script_register(L);
	CWeaponBinoculars::script_register(L);
	CWeaponBM16::script_register(L);
	CWeaponFN2000::script_register(L);
	CWeaponFORT::script_register(L);
	CWeaponGroza::script_register(L);
	CWeaponHPSA::script_register(L);
	CWeaponKnife::script_register(L);
	CWeaponLR300::script_register(L);
	CWeaponPM::script_register(L);
	CWeaponRG6::script_register(L);
	CWeaponRPG7::script_register(L);
	CWeaponShotgun::script_register(L);
	CWeaponSVD::script_register(L);
	CWeaponSVU::script_register(L);
	CWeaponUSP45::script_register(L);
	CWeaponVal::script_register(L);
	CWeaponVintorez::script_register(L);
	CWeaponWalther::script_register(L);
	//CZoneCampfire::script_register(L);

	CArtefact::script_register(L);
	CAI_Crow::script_register(L);
	CAI_Bloodsucker::script_register(L);
	CAI_Boar::script_register(L);
	CBurer::script_register(L);
	CCat::script_register(L);
	CChimera::script_register(L);
	CController::script_register(L);
	CAI_Dog::script_register(L);
	CAI_Flesh::script_register(L);
	CFracture::script_register(L);
	CPoltergeist::script_register(L);
	CAI_PseudoDog::script_register(L);
	CPsyDog::script_register(L);
	CPsyDogPhantom::script_register(L);
	CPseudoGigant::script_register(L);
	CSnork::script_register(L);
	CTushkano::script_register(L);
	CZombie::script_register(L);
	CAI_Stalker::script_register(L);
	CAI_Trader::script_register(L);
	CCar::script_register(L);

	CActor::script_register(L);
	CEatableItem::script_register(L);
	
	CALifeMonsterBrain::script_register(L);
	CALifeHumanBrain::script_register(L);
	CGameGraph::script_register(L);
	CUIActorMenu::script_register(L);
//	CInventoryBox::script_register(L);
	CActorCondition::script_register(L);
//	CAntirad::script_register(L);
//	CBottleItem::script_register(L);
	CCustomOutfit::script_register(L);
	CCustomZone::script_register(L);
//	CFoodItem::script_register(L);
	CHudItem::script_register(L);
	CInventoryItem::script_register(L);
	CInventoryOwner::script_register(L);
//	CMedkit::script_register(L);
	CPhysicsShellHolder::script_register(L);
//	CWeaponAmmo::script_register(L);
#endif
}
