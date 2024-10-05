#pragma once

namespace GameObject {
	enum ECallbackType {
		eTradeStart = u32(0),
		eTradeStop,
		eTradeSellBuyItem,
		eTradePerformTradeOperation,

		eZoneEnter,
		eZoneExit,
		eExitLevelBorder,
		eEnterLevelBorder,
		eDeath,

		ePatrolPathInPoint,

		eInventoryPda,
		eInventoryInfo,
		eArticleInfo,
		eTaskStateChange,
		eMapLocationAdded,

		eUseObject,

		eHit,

		eSound,

		eActionTypeMovement,
		eActionTypeWatch,
		eActionTypeRemoved,
		eActionTypeAnimation,
		eActionTypeSound,
		eActionTypeParticle,
		eActionTypeObject,

		eActorSleep,

		eHelicopterOnPoint,
		eHelicopterOnHit,

		eOnItemTake,
		eOnItemDrop,

		eScriptAnimation,
		
		eTraderGlobalAnimationRequest,
		eTraderHeadAnimationRequest,
		eTraderSoundEnd,

		eInvBoxItemTake,
		eWeaponNoAmmoAvailable,

		eKeyPress,
		eKeyRelease,
		eKeyHold,
		eMouseMove,
		eMouseWheel,

		eOnWeaponFired,
		eOnWeaponJammed,
		eOnWeaponZoomIn,
		eOnWeaponZoomOut,
		eOnWeaponMagazineEmpty,

		eItemToBelt,
		eItemToSlot,
		eItemToRuck,
		eOnFootStep,

		eAttachVehicle,
		eDetachVehicle,
		eUseVehicle,

		eActorBeforeDeath,
		eActorHudAnimationEnd,

		eDummy = u32(-1),
	};
};

