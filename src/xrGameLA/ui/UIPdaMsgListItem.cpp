//////////////////////////////////////////////////////////////////////
// UIPdaMsgListItem.cpp: элемент окна списка в основном 
// экране для сообщений PDA
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "UIPdaMsgListItem.h"
#include "../Entity.h"
#include "../character_info.h"
#include "UIInventoryUtilities.h"
#include "xrUIXmlParser.h"
#include "UIXmlInit.h"
#include "uicoloranimatorwrapper.h"
#include "../object_broker.h"

#define PDA_MSG_MAINGAME_CHAR "maingame_pda_msg.xml"

using namespace InventoryUtilities;


void CUIPdaMsgListItem::SetFont(CGameFont* pFont){
	UIMsgText.SetFont(pFont);
}

void CUIPdaMsgListItem::InitPdaMsgListItem(const Fvector2& size)
{
	inherited::SetWndSize	(size);

	CUIXml					uiXml;
	uiXml.Load				(CONFIG_PATH, UI_PATH,PDA_MSG_MAINGAME_CHAR);

	CUIXmlInit				xml_init;
	AttachChild				(&UIIcon);
	xml_init.InitStatic		(uiXml, "icon_static", 0, &UIIcon);

	AttachChild(&UIName);
	if(uiXml.NavigateToNode	("name_static",0))
		xml_init.InitTextWnd	(uiXml, "name_static", 0, &UIName);
	else
	{
		UIName.Show			(false);
		UIName.Enable		(false);
	}
	AttachChild				(&UIMsgText);
	xml_init.InitTextWnd		(uiXml, "text_static", 0, &UIMsgText);	
}

void CUIPdaMsgListItem::SetTextColor(u32 color){
	UIMsgText.SetTextColor	(color);
}

void CUIPdaMsgListItem::SetColor(u32 color){
	UIIcon.SetTextureColor(color);
}

void CUIPdaMsgListItem::InitCharacter(CInventoryOwner* pInvOwner)
{
	VERIFY(pInvOwner);

	string256 str;
	xr_sprintf(str, "name: %s", pInvOwner->Name());
	UIName.SetText			(str);

	UIIcon.InitTexture		( pInvOwner->CharacterInfo().IconName().c_str() );
}
