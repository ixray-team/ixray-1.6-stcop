#pragma once

#define CMD_START	(1<<0)
#define CMD_STOP	(1<<1)

#define SLOTS_TOTAL			15

enum ESlotId : u8 {
	NO_ACTIVE_SLOT	= 0,
	KNIFE_SLOT		= 1,//btn1			was (0)			!!!
	PISTOL_SLOT,	//btn2 PISTOL_SLOT	was (1)
	RIFLE_SLOT,		//btn3 RIFLE_SLOT	was (2)
	GRENADE_SLOT,	//btn4 GRENADE_SLOT	was (3)
	APPARATUS_SLOT,	//btn5 BINOCULAR_SLOT
	BOLT_SLOT,		//btn6 BOLT_SLOT
	OUTFIT_SLOT,	// outfit
	PDA_SLOT,		// pda
	DETECTOR_SLOT,	// detector
	TORCH_SLOT,		// torch
	ARTEFACT_SLOT,	// artefact
	HELMET_SLOT,
	PNV_SLOT,
	ANOM_DET_SLOT,
	RIFLE_2_SLOT,
	LAST_SLOT		= RIFLE_2_SLOT
};

#define RUCK_HEIGHT			280
#define RUCK_WIDTH			7

class CInventoryItem;
class CInventory;

typedef CInventoryItem*				PIItem;
typedef xr_vector<PIItem>			TIItemContainer;
typedef u8							TSlotId;

enum EItemPlace
{			
	eItemPlaceUndefined = 0,
	eItemPlaceSlot,
	eItemPlaceBelt,
	eItemPlaceRuck
};

struct SInvItemPlace
{
	union{
		struct{
			u8 type				: 3; // 8 possible types
			u8 slot_id			: 5; // 32 possible slots
		};
		u8	value;
	};
};
extern u32	INV_STATE_LADDER;
extern u32	INV_STATE_CAR;
extern u32	INV_STATE_BLOCK_ALL;
extern u32	INV_STATE_INV_WND;
extern u32	INV_STATE_BUY_MENU;
