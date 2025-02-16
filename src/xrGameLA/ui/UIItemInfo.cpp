#include "stdafx.h"
#include "pch_script.h"

#include "uiiteminfo.h"
#include "uistatic.h"
#include "UIXmlInit.h"

#include "UIProgressBar.h"
#include "UIScrollView.h"

#include "../ai_space.h"
#include "../script_engine.h"
#include "../script_game_object.h"
#include "../../xrEngine/string_table.h"
#include "../Inventory_Item.h"
#include "UIInventoryUtilities.h"
#include "../PhysicsShellHolder.h"
#include "UIWpnParams.h"
#include "UIArtifactParams.h"
#include "UIOutfitParams.h"



#define  INV_GRID_WIDTH2  40.0f
#define  INV_GRID_HEIGHT2 40.0f
CUIItemInfo::CUIItemInfo()
{
	UICondProgresBar			= NULL;
	UICondition					= NULL;
	UICost						= NULL;
	UIWeight					= NULL;
	UIItemImage					= NULL;
	UIDesc						= NULL;
	UIWpnParams					= NULL;
	UIArtefactParams			= NULL;
	UIOutfitParams				= NULL;
	UIName						= NULL;
	m_pInvItem					= NULL;
	m_b_force_drawing			= false;
}

CUIItemInfo::~CUIItemInfo()
{
	xr_delete					(UIWpnParams);
	xr_delete					(UIArtefactParams);
	xr_delete					(UIOutfitParams);
}

void CUIItemInfo::Init(LPCSTR xml_name){

	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, xml_name);

	CUIXmlInit					xml_init;

	if(uiXml.NavigateToNode("main_frame",0))
	{
		Frect wnd_rect;
		wnd_rect.x1		= uiXml.ReadAttribFlt("main_frame", 0, "x", 0);
		wnd_rect.y1		= uiXml.ReadAttribFlt("main_frame", 0, "y", 0);

		wnd_rect.x2		= uiXml.ReadAttribFlt("main_frame", 0, "width", 0);
		wnd_rect.y2		= uiXml.ReadAttribFlt("main_frame", 0, "height", 0);
		
		inherited::SetWndRect(wnd_rect.x1, wnd_rect.y1, wnd_rect.x2, wnd_rect.y2);
	}

	if(uiXml.NavigateToNode("static_name",0))
	{
		UIName						= new CUITextWnd();	 
		AttachChild					(UIName);		
		UIName->SetAutoDelete		(true);
		xml_init.InitTextWnd		(uiXml, "static_name", 0,	UIName);
	}
	if(uiXml.NavigateToNode("static_weight",0))
	{
		UIWeight				= new CUITextWnd();	 
		AttachChild				(UIWeight);		
		UIWeight->SetAutoDelete(true);
		xml_init.InitTextWnd		(uiXml, "static_weight", 0,			UIWeight);
	}

	if(uiXml.NavigateToNode("static_cost",0))
	{
		UICost					= new CUITextWnd();	 
		AttachChild				(UICost);
		UICost->SetAutoDelete	(true);
		xml_init.InitTextWnd		(uiXml, "static_cost", 0,			UICost);
	}

	if(uiXml.NavigateToNode("static_condition",0))
	{
		UICondition					= new CUIStatic();	 
		AttachChild					(UICondition);
		UICondition->SetAutoDelete	(true);
		xml_init.InitStatic			(uiXml, "static_condition", 0,		UICondition);
	}

	if(uiXml.NavigateToNode("condition_progress",0))
	{
		UICondProgresBar			= new CUIProgressBar(); AttachChild(UICondProgresBar);UICondProgresBar->SetAutoDelete(true);
		xml_init.InitProgressBar	(uiXml, "condition_progress", 0, UICondProgresBar);
	}

	if(uiXml.NavigateToNode("descr_list",0))
	{
		UIWpnParams						= new CUIWpnParams();
		UIArtefactParams				= new CUIArtefactParams();
		UIOutfitParams					= new CUIOutfitParams();
		UIWpnParams->InitFromXml		(uiXml);
		UIArtefactParams->InitFromXml	(uiXml);
		UIOutfitParams->InitFromXml		(uiXml);
		UIDesc							= new CUIScrollView(); 
		AttachChild						(UIDesc);		
		UIDesc->SetAutoDelete			(true);
		m_desc_info.bShowDescrText		= !!uiXml.ReadAttribInt("descr_list",0,"only_text_info", 1);
		xml_init.InitScrollView			(uiXml, "descr_list", 0, UIDesc);
		xml_init.InitFont				(uiXml, "descr_list:font", 0, m_desc_info.uDescClr, m_desc_info.pDescFont);
	}	

	if (uiXml.NavigateToNode("image_static", 0))
	{	
		UIItemImage					= new CUIStatic();	 
		AttachChild					(UIItemImage);	
		UIItemImage->SetAutoDelete	(true);
		xml_init.InitStatic			(uiXml, "image_static", 0, UIItemImage);
		UIItemImage->TextureOn		();

		UIItemImage->TextureOff			();
//		UIItemImage->ClipperOn			();
		UIItemImage->SetAlignment		(waNone);

		UIItemImageRect				= UIItemImage->GetWndRect();
	}

	xml_init.InitAutoStaticGroup	(uiXml, "auto", 0, this);
}

void CUIItemInfo::InitItemInfo(Fvector2 pos, Fvector2 size, LPCSTR xml_name)
{
	inherited::SetWndPos	(pos);
	inherited::SetWndSize	(size);
	Init					(xml_name);
}

bool				IsGameTypeSingle();

void CUIItemInfo::InitItem(CInventoryItem* pInvItem)
{
	m_pInvItem				= pInvItem;
	if(!m_pInvItem)			return;

	string256				str;

	if(UIName)
	{
		UIName->SetText		(pInvItem->Name());
	}
	if(UIWeight)
	{
		xr_sprintf				(str, "%3.2f kg", pInvItem->Weight());
		UIWeight->SetText	(str);
	}
	if( UICost && IsGameTypeSingle() )
	{
		xr_sprintf				(str, "%d RU", pInvItem->Cost());		// will be owerwritten in multiplayer
		UICost->SetText		(str);
		UICost->Show(true);
	}
	else
		UICost->Show(false);


	if(UICondProgresBar)
	{
		float cond							= pInvItem->GetConditionToShow();
		UICondProgresBar->Show				(true);
		UICondProgresBar->SetProgressPos	( cond*100.0f+1.0f-EPS );
	}

	if(UIDesc)
	{
		UIDesc->Clear						();
		VERIFY								(0==UIDesc->GetSize());
		TryAddWpnInfo						(*pInvItem);
		TryAddArtefactInfo					(*pInvItem);
		TryAddOutfitInfo					(*pInvItem);
		if(m_desc_info.bShowDescrText)
		{
			CUITextWnd* pItem					= new CUITextWnd();
			pItem->SetTextColor					(m_desc_info.uDescClr);
			pItem->SetFont						(m_desc_info.pDescFont);
			pItem->SetWidth						(UIDesc->GetDesiredChildWidth());
			pItem->SetTextComplexMode			(true);

			luabind::functor<LPCSTR> fDescr;
			if (ai().script_engine().functor("ui_wpn_params.GetDescription", fDescr))
			{
				pItem->SetText					(fDescr(pInvItem->object().lua_game_object()));
			}
			else
			{
				pItem->SetText					(*pInvItem->ItemDescription());
			}
			pItem->AdjustHeightToText			();
			UIDesc->AddWindow					(pItem, true);
		}
		UIDesc->ScrollToBegin				();
	}
	if(UIItemImage)
	{
		// Загружаем картинку
		UIItemImage->SetShader				(InventoryUtilities::GetEquipmentIconsShader());

		Irect item_grid_rect				= pInvItem->GetInvGridRect();
		Frect texture_rect;
		texture_rect.lt.set					(item_grid_rect.x1*INV_GRID_WIDTH,	item_grid_rect.y1*INV_GRID_HEIGHT);
		texture_rect.rb.set					(item_grid_rect.x2*INV_GRID_WIDTH,	item_grid_rect.y2*INV_GRID_HEIGHT);
		texture_rect.rb.add					(texture_rect.lt);
		UIItemImage->GetUIStaticItem().SetTextureRect(texture_rect);

		UIItemImage->TextureOn				();
		UIItemImage->SetStretchTexture		(true);


		Fvector2 v_r	= {item_grid_rect.x2*INV_GRID_WIDTH, item_grid_rect.y2*INV_GRID_HEIGHT};

	
		v_r.x		*= UI().get_current_kx();

		float width = v_r.x;
		float height = v_r.y;
		if (width > UIItemImageRect.width())
		{
			float coeff = UIItemImageRect.width()/width;
			width *= coeff;
			height *= coeff;
		}

		if (height > UIItemImageRect.height())
		{
			float coeff = UIItemImageRect.height()/height;
			width *= coeff;
			height *= coeff;
		}

		float x = UIItemImageRect.x1;
		float y = UIItemImageRect.y1;

		x += UIItemImageRect.x2/2-width/2;
		y += UIItemImageRect.y2/2-height/2;

		UIItemImage->SetWndPos					(x, y);
		UIItemImage->SetWidth					(width);
		UIItemImage->SetHeight					(height);
	}
}

void CUIItemInfo::UpdateCondition()
{
	if (UICondProgresBar)
	{
		float cond = m_pInvItem->GetConditionToShow();
		UICondProgresBar->Show(true);
		UICondProgresBar->SetProgressPos(cond*100.0f + 1.0f - EPS);
	}
}

void CUIItemInfo::TryAddWpnInfo(CInventoryItem& wpn){
	if (UIWpnParams->Check(wpn))
	{
		UIWpnParams->SetInfo(wpn);
		UIDesc->AddWindow(UIWpnParams,false);
	}
}

void CUIItemInfo::TryAddArtefactInfo	(CInventoryItem& itm)
{
	if (UIArtefactParams->Check(itm))
	{
		UIArtefactParams->SetInfo(itm);
		UIDesc->AddWindow(UIArtefactParams, false);
	}
}

void CUIItemInfo::TryAddOutfitInfo(CInventoryItem& itm)
{
	if (UIOutfitParams->Check(itm))
	{

		UIOutfitParams->SetInfo(itm);
		UIDesc->AddWindow(UIOutfitParams, false);

	}
}

void CUIItemInfo::Draw()
{
	if(m_pInvItem || m_b_force_drawing)
		inherited::Draw();
}
