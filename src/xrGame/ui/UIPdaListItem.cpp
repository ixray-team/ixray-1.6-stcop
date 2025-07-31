#include "stdafx.h"

#include "UIPdaListItem.h"
#include "../actor.h"
#include "UIInventoryUtilities.h"
#include "../../xrEngine/string_table.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

#include "character_info.h"

#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "..\InventoryOwner.h"
#include "UICharacterInfo.h"
#include "../../xrUI/Widgets/UIStatic.h"

#define			PDA_CONTACT_CHAR		"pda_character.xml"

CUIPdaListItem::CUIPdaListItem()
{
	UIMask = nullptr;
	UIInfo = nullptr;
}

CUIPdaListItem::~CUIPdaListItem()
{
}

void CUIPdaListItem::Init(float x, float y, float width, float height)
{
	//inherited::Init								(x, y, width, height);
	inherited::SetWndRect						(Frect().set(x, y, width, height));

	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, PDA_CONTACT_CHAR);

	CUIXmlInit xml_init;
	UIInfo = new CUICharacterInfo			();
	UIInfo->SetAutoDelete						(true);
	AttachChild									(UIInfo);
	UIInfo->InitCharacterInfo					(Fvector2().set(0, 0), Fvector2().set(width, height), PDA_CONTACT_CHAR);

	if (uiXml.NavigateToNode					("mask_frame_window", 0))
	{
		UIMask = new CUIFrameWindow			();
		UIMask->SetAutoDelete					(true);
		xml_init.InitFrameWindow				(uiXml, "mask_frame_window", 0, UIMask);
	}

	xml_init.InitAutoStaticGroup				(uiXml,"pda_char_auto_statics", 0, this);
}

void CUIPdaListItem::InitCharacter(CInventoryOwner* pInvOwner)
{
	VERIFY										(pInvOwner);
	UIInfo->InitCharacter						(pInvOwner);
}
