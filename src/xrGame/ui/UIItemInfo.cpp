#include "StdAfx.h"
#include "pch_script.h"

#include "UIItemInfo.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UIXmlInit.h"

#include "../../xrUI/Widgets/UIItemStateDisplay.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "Inventory.h"
#include "InventoryVolumeSystem.h"
#include "../../xrEngine/string_table.h"
#include "../inventory_item.h"
#include "UIInventoryUtilities.h"
#include "../PhysicsShellHolder.h"
#include "UIWpnParams.h"
#include "UIKnifeParams.h"
#include "ui_af_params.h"
#include "UIInvUpgradeProperty.h"
#include "UIOutfitInfo.h"
#include "UIBoosterInfo.h"
#include "../Weapon.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../eatable_item.h"
#include "UICellItem.h"
#include "UIGrenadeParams.h"

extern const char* g_inventory_upgrade_xml;

#define INV_GRID_WIDTH2(SCALE_ICON) (40.0f * SCALE_ICON)
#define INV_GRID_HEIGHT2(SCALE_ICON) (40.0f * SCALE_ICON)

CUIItemInfo::CUIItemInfo()
{
	UIItemImageSize.set			(0.0f,0.0f);
	
	UICost						= nullptr;
	UITradeTip					= nullptr;
	UIWeight					= nullptr;
	UIItemImage					= nullptr;
	UIDesc						= nullptr;
	UIConditionWnd				= nullptr;
	UIWpnParams					= nullptr;
	UIKnifeParams				= nullptr;
	UIGrenadeParams				= nullptr;
	UIProperties				= nullptr;
	UIOutfitInfo				= nullptr;
	UIBoosterInfo				= nullptr;
	UIArtefactParams			= nullptr;
	UIOutfitParams				= nullptr;
	UIBackpackParams			= nullptr;
	UIName						= nullptr;
	UIBackground				= nullptr;
	UICondition					= nullptr;
	UICondProgresBar			= nullptr;
	m_pInvItem					= nullptr;
	m_b_FitToHeight				= false;
	m_complex_desc				= false;
}

CUIItemInfo::~CUIItemInfo()
{
	xr_delete	(UIConditionWnd);
	xr_delete	(UIWpnParams);
	xr_delete	(UIKnifeParams);
	xr_delete	(UIGrenadeParams);
	xr_delete	(UIArtefactParams);
	xr_delete	(UIOutfitParams);
	xr_delete	(UIBackpackParams);
	xr_delete	(UIProperties);
	xr_delete	(UIOutfitInfo);
	xr_delete	(UIBoosterInfo);
}

bool CUIItemInfo::InitItemInfo(const char* xml_name)
{
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, xml_name);

	if (uiXml.GetNodesNum(uiXml.GetRoot(), nullptr) == 0)
		return false;

	CUIXmlInit					xml_init;

	if(uiXml.NavigateToNode("main_frame",0))
	{
		Frect wnd_rect;
		wnd_rect.x1		= uiXml.ReadAttribFlt("main_frame", 0, "x", 0);
		wnd_rect.y1		= uiXml.ReadAttribFlt("main_frame", 0, "y", 0);

		wnd_rect.x2		= uiXml.ReadAttribFlt("main_frame", 0, "width", 0);
		wnd_rect.y2		= uiXml.ReadAttribFlt("main_frame", 0, "height", 0);
		wnd_rect.x2		+= wnd_rect.x1;
		wnd_rect.y2		+= wnd_rect.y1;
		inherited::SetWndRect(wnd_rect);
		
		delay			= uiXml.ReadAttribInt("main_frame", 0, "delay", 500);
	}
	if(uiXml.NavigateToNode("background_frame",0))
	{
		UIBackground				= new CUIFrameWindow();
		UIBackground->SetAutoDelete	(true);
		AttachChild					(UIBackground);
		xml_init.InitFrameWindow	(uiXml, "background_frame", 0,	UIBackground);
	}
	m_complex_desc = false;
	if(uiXml.NavigateToNode("static_name",0))
	{
		UIName						= new CUIStatic();
		AttachChild					(UIName);		
		UIName->SetAutoDelete		(true);
		xml_init.InitStatic		(uiXml, "static_name", 0,	UIName);
		m_complex_desc				= ( uiXml.ReadAttribInt("static_name", 0, "complex_desc", 0) == 1 );
	}
	if(uiXml.NavigateToNode("static_weight",0))
	{
		UIWeight				= new CUIStatic();
		AttachChild				(UIWeight);		
		UIWeight->SetAutoDelete(true);
		xml_init.InitStatic		(uiXml, "static_weight", 0,			UIWeight);
	}

	if(uiXml.NavigateToNode("static_cost",0))
	{
		UICost					= new CUIStatic();
		AttachChild				(UICost);
		UICost->SetAutoDelete	(true);
		xml_init.InitStatic		(uiXml, "static_cost", 0,			UICost);
	}
	
	if(uiXml.NavigateToNode("static_condition",0)) // for SoC
	{
		UICondition					= new CUIStatic();	 
		AttachChild					(UICondition);
		UICondition->SetAutoDelete	(true);
		xml_init.InitStatic			(uiXml, "static_condition", 0,		UICondition);
	}
	
	if(uiXml.NavigateToNode("condition_progress",0))
	{
		UICondProgresBar = new CUIItemStateDisplay();
		AttachChild(UICondProgresBar);
		UICondProgresBar->SetAutoDelete(true);
		xml_init.InitItemStateDisplay(uiXml, "condition_progress", 0, UICondProgresBar);
	}

	if(uiXml.NavigateToNode("static_no_trade",0))
	{
		UITradeTip					= new CUIStatic();
		AttachChild					(UITradeTip);
		UITradeTip->SetAutoDelete	(true);
		xml_init.InitStatic		(uiXml, "static_no_trade", 0,		UITradeTip);
	}

	if(uiXml.NavigateToNode("descr_list",0))
	{
		UIConditionWnd					= new CUIConditionParams();
		UIConditionWnd->InitFromXml		(uiXml);
		UIWpnParams						= new CUIWpnParams();
		UIWpnParams->InitFromXml		(uiXml);
		UIKnifeParams					= new CUIKnifeParams();
		UIKnifeParams->InitFromXml		(uiXml);
		UIGrenadeParams					= new CUIGrenadeParams();
		UIGrenadeParams->InitFromXml	(uiXml);

		UIArtefactParams				= new CUIArtefactParams(CUIArtefactParams::CParamType::eParamTypeArtefact);
		UIArtefactParams->InitFromXml	(uiXml);

		UIOutfitParams = new CUIArtefactParams(CUIArtefactParams::CParamType::eParamTypeOutfit);
		UIOutfitParams->InitFromXml(uiXml);

		UIBackpackParams = new CUIArtefactParams(CUIArtefactParams::CParamType::eParamTypeBackpack);
		UIBackpackParams->InitFromXml(uiXml);

		if (uiXml.NavigateToNode("booster_params"))
		{
			UIBoosterInfo = new CUIBoosterInfo();
			UIBoosterInfo->InitFromXml(uiXml);
		}

		//UIDesc_line						= new CUIStatic();
		//AttachChild						(UIDesc_line);	
		//UIDesc_line->SetAutoDelete		(true);
		//xml_init.InitStatic				(uiXml, "description_line", 0, UIDesc_line);

		if ( ai().get_alife() ) // (-designer)
		{
			UIProperties					= new UIInvUpgPropertiesWnd();
			UIProperties->init_from_xml		("actor_menu_item.xml");
		}

		UIDesc							= new CUIScrollView(); 
		AttachChild						(UIDesc);		
		UIDesc->SetAutoDelete			(true);
		m_desc_info.bShowDescrText		= !!uiXml.ReadAttribInt("descr_list",0,"only_text_info", 1);
		m_b_FitToHeight					= !!uiXml.ReadAttribInt("descr_list",0,"fit_to_height", 0);
		xml_init.InitScrollView			(uiXml, "descr_list", 0, UIDesc);
		xml_init.InitFont				(uiXml, "descr_list:font", 0, m_desc_info.uDescClr, m_desc_info.pDescFont);
	}	

	if (uiXml.NavigateToNode("image_static", 0))
	{	
		UIItemImage					= new CUI3dStatic();	 
		AttachChild					(UIItemImage);	
		UIItemImage->SetAutoDelete	(true);
		xml_init.InitStatic			(uiXml, "image_static", 0, UIItemImage);
		UIItemImage->TextureOn		();

		UIItemImage->TextureOff			();
		UIItemImageSize.set				(UIItemImage->GetWidth(),UIItemImage->GetHeight());
	}
	if ( uiXml.NavigateToNode( "outfit_info", 0 ) )
	{
		UIOutfitInfo				= new CUIOutfitInfo();
		UIOutfitInfo->InitFromXml	(uiXml);
	}

	xml_init.InitAutoStaticGroup	(uiXml, "auto", 0, this);
	return true;
}

void CUIItemInfo::InitItemInfo(Fvector2 pos, Fvector2 size, const char* xml_name)
{
	inherited::SetWndPos	(pos);
	inherited::SetWndSize	(size);
	InitItemInfo			(xml_name);
}

bool	IsGameTypeSingle();

void CUIItemInfo::InitItem(CUICellItem* pCellItem, CInventoryItem* pCompareItem, u32 item_price, const char* trade_tip, bool overrideCorrectionByWeight)
{
	if(!pCellItem)
	{
		m_pInvItem			= nullptr;
		Enable				(false);
		return;
	}

	PIItem pInvItem			= (PIItem)pCellItem->m_pData;
	m_pInvItem				= pInvItem;
	Enable					(nullptr != m_pInvItem);
	if(!m_pInvItem)			return;

	Fvector2				pos;	pos.set( 0.0f, 0.0f );
	string256				str;
	if ( UIName )
	{
		UIName->SetText		(pInvItem->NameItem());
		UIName->AdjustHeightToText();
		pos.y = UIName->GetWndPos().y + UIName->GetHeight() + 4.0f;
	}
	if ( UIWeight )
	{
		const char*  kg_str = g_pStringTable->translate( "st_kg" ).c_str();
		float	weight = pInvItem->m_pInventory ? pInvItem->m_pInventory->CalcItemWeight(pInvItem) : pInvItem->Weight();
		
		if ( !weight )
		{
			if ( CWeaponAmmo* ammo = dynamic_cast<CWeaponAmmo*>(pInvItem) )
			{
				// its helper item, m_boxCur is zero, so recalculate via CInventoryItem::Weight()
				weight = pInvItem->m_pInventory ? pInvItem->m_pInventory->CalcItemWeight(pInvItem) : pInvItem->CInventoryItem::Weight();
				for( u32 j = 0; j < pCellItem->ChildsCount(); ++j )
				{
					PIItem jitem	= (PIItem)pCellItem->Child(j)->m_pData;
					weight += jitem->m_pInventory ? jitem->m_pInventory->CalcItemWeight(jitem) : jitem->CInventoryItem::Weight();
				}

			}
		}

		if (CInventoryVolumeSystem::Get().IsEnabled())
		{
			xr_sprintf(str, "%3.2f %s | V %3.2f", weight, kg_str, CInventoryVolumeSystem::Get().GetItemVolume(*pInvItem));
		}
		else
		{
			xr_sprintf(str, "%3.2f %s", weight, kg_str);
		}
		UIWeight->SetText	(str);
		
		pos.x = UIWeight->GetWndPos().x;
		if ( m_complex_desc )
		{
			UIWeight->SetWndPos	(pos);
		}
	}

	if (UICost)
	{
		if (IsGameTypeSingleCompatible() && item_price != u32(-1) && pInvItem->IsDrawCost())
		{
			xr_sprintf(str, "%d RU", item_price);// will be owerwritten in multiplayer
			UICost->SetText(str);
			pos.x = UICost->GetWndPos().x;
			if (m_complex_desc)
			{
				UICost->SetWndPos(pos);
			}
			UICost->Show(true);
		}
		else
		{
			UICost->Show(false);
		}
	}

	if (UICondProgresBar)
	{
		UICondProgresBar->Show(true);

		const InventoryUtilities::ConditionDisplayParams display =
			InventoryUtilities::GetConditionDisplayParams(pInvItem);

		CEatableItem* eatableItem = pInvItem->cast_eatable_item();
		if (UICondProgresBar->GetPercentFormat() == CUIItemStateDisplay::EPercentFormat::Portion &&
			eatableItem != nullptr &&
			display.usePortion &&
			display.portionMax > 1)
		{
			UICondProgresBar->SetPortion(display.portionCurrent, display.portionMax);
		}
		else
		{
			UICondProgresBar->SetState(display.state);
		}
	}

	if ( UITradeTip && IsGameTypeSingleCompatible())
	{
		pos.y = UITradeTip->GetWndPos().y;
		if ( UIWeight && m_complex_desc )
		{
			pos.y = UIWeight->GetWndPos().y + UIWeight->GetHeight() + 4.0f;
		}

		if(trade_tip==nullptr)
			UITradeTip->Show(false);
		else
		{
			UITradeTip->SetText(g_pStringTable->translate(trade_tip).c_str());
			UITradeTip->AdjustHeightToText();
			UITradeTip->SetWndPos(pos);
			UITradeTip->Show(true);
		}
	}
	
	if ( UIDesc )
	{
		pos = UIDesc->GetWndPos();
		if ( UIWeight && !overrideCorrectionByWeight)
			pos.y = UIWeight->GetWndPos().y + UIWeight->GetHeight() + 4.0f;

		if(UITradeTip && trade_tip!=nullptr)
			pos.y = UITradeTip->GetWndPos().y + UITradeTip->GetHeight() + 4.0f;

		UIDesc->SetWndPos		(pos);
		UIDesc->Clear			();
		VERIFY					(0==UIDesc->GetSize());
		if(m_desc_info.bShowDescrText)
		{
			CUIStatic* pItem					= new CUIStatic();
			pItem->SetTextColor					(m_desc_info.uDescClr);
			pItem->SetFont						(m_desc_info.pDescFont);
			pItem->SetWidth						(UIDesc->GetDesiredChildWidth());
			pItem->SetTextComplexMode			(true);
			pItem->SetText(pInvItem->IsUsedAdditionalDescription() ? *pInvItem->GetExtendedUnionDescription() : *pInvItem->ItemDescription());
			pItem->AdjustHeightToText			();
			UIDesc->AddWindow					(pItem, true);
		}
		TryAddConditionInfo					(*pInvItem, pCompareItem);
		TryAddWpnInfo						(*pInvItem, pCompareItem);
		TryAddKnifeInfo						(*pInvItem, pCompareItem);
		TryAddGrenadeInfo					(*pInvItem, pCompareItem);
		TryAddArtefactInfo					(*pInvItem);
		TryAddOutfitInfo					(*pInvItem, pCompareItem);
		TryAddUpgradeInfo					(*pInvItem);
		TryAddBoosterInfo					(*pInvItem);

		if(m_b_FitToHeight)
		{
			UIDesc->SetWndSize				(Fvector2().set(UIDesc->GetWndSize().x, UIDesc->GetPadSize().y) );
			Fvector2 new_size;
			new_size.x						= GetWndSize().x;
			new_size.y						= UIDesc->GetWndPos().y+UIDesc->GetWndSize().y+20.0f;
			new_size.x						= std::max(105.0f, new_size.x);
			new_size.y						= std::max(105.0f, new_size.y);
			
			SetWndSize						(new_size);
			if(UIBackground)
				UIBackground->SetWndSize	(new_size);
		}

		UIDesc->ScrollToBegin				();
	}
	if(UIItemImage)
    {
        if (psActorFlags.test(AF_3D_ICONS_INV))
        {
            UIItemImage->SetVisual(pInvItem->m_3d_static_visual_name);
            UIItemImage->SetScaleFactor(pInvItem->m_3d_static_scale);
            UIItemImage->SetXYZ(pInvItem->m_3d_static_rotate);
        }
        else
            UIItemImage->SetVisual(nullptr);

        // Р—Р°РіСЂСѓР¶Р°РµРј РєР°СЂС‚РёРЅРєСѓ
		UIItemImage->SetShader(InventoryUtilities::GetEquipmentIconsShader(m_pInvItem->IconsTexture.c_str()));

		Irect item_grid_rect = pInvItem->GetInvGridRect();
		float scaleIcon = m_pInvItem->ScaleIcon;
		Frect texture_rect = {};
		texture_rect.lt.set(item_grid_rect.x1 * INV_GRID_WIDTH(scaleIcon), item_grid_rect.y1 * INV_GRID_HEIGHT(scaleIcon));
		texture_rect.rb.set(item_grid_rect.x2 * INV_GRID_WIDTH(scaleIcon), item_grid_rect.y2 * INV_GRID_HEIGHT(scaleIcon));
		texture_rect.rb.add(texture_rect.lt);
		UIItemImage->GetUIStaticItem().SetTextureRect(texture_rect);
		UIItemImage->TextureOn				();
		UIItemImage->SetStretchTexture		(true);

		Fvector2 v_r = {};

		v_r = { item_grid_rect.x2 * INV_GRID_WIDTH2(scaleIcon) / scaleIcon,
			item_grid_rect.y2 * INV_GRID_HEIGHT2(scaleIcon) / scaleIcon };

		v_r.x								*= UI().get_current_kx();


		UIItemImage->GetUIStaticItem().SetSize	(v_r);
		UIItemImage->SetWidth					(v_r.x);
		UIItemImage->SetHeight					(v_r.y);
    }
}

void CUIItemInfo::TryAddConditionInfo(CInventoryItem& pInvItem, CInventoryItem* pCompareItem)
{
	if (pInvItem.IsUsingCondition())
	{
		UIConditionWnd->SetInfo(pCompareItem, pInvItem);
		UIDesc->AddWindow(UIConditionWnd, false);
	}
	else
	{
		UIDesc->RemoveWindow(UIConditionWnd);
	}
}

void CUIItemInfo::TryAddWpnInfo(CInventoryItem& pInvItem, CInventoryItem* pCompareItem)
{
	if (UIWpnParams->Check(pInvItem))
	{
		UIWpnParams->SetInfo(pCompareItem, pInvItem);
		UIDesc->AddWindow(UIWpnParams, false);
	}
}

void CUIItemInfo::TryAddKnifeInfo(CInventoryItem& pInvItem, CInventoryItem* pCompareItem)
{
	if (UIKnifeParams->Check(pInvItem))
	{
		UIKnifeParams->SetInfo(pCompareItem, pInvItem);
		UIDesc->AddWindow(UIKnifeParams, false);
	}
}

void CUIItemInfo::TryAddGrenadeInfo(CInventoryItem& pInvItem, CInventoryItem* pCompareItem)
{
	if (UIGrenadeParams->Check(pInvItem))
	{
		UIGrenadeParams->SetInfo(pCompareItem, pInvItem);
		UIDesc->AddWindow(UIGrenadeParams, false);
	}
}

void CUIItemInfo::TryAddArtefactInfo	(CInventoryItem& pInvItem)
{
	if (UIArtefactParams->Check(pInvItem.object().cNameSect()))
	{
		UIArtefactParams->SetInfo(pInvItem);
		UIDesc->AddWindow(UIArtefactParams, false);
	}
}

void CUIItemInfo::TryAddOutfitInfo(CInventoryItem& pInvItem, CInventoryItem* pCompareItem)
{
	CCustomOutfit* outfit = pInvItem.cast_outfit();
	CHelmet* helmet = pInvItem.cast_helmet();
	CBackpack* backpack = pInvItem.cast_backpack();

	if (outfit && UIOutfitInfo)
	{
		CCustomOutfit* comp_outfit = pCompareItem ? pCompareItem->cast_outfit() : nullptr;
		UIOutfitInfo->UpdateInfo(outfit, comp_outfit);
		UIDesc->AddWindow(UIOutfitInfo, false);
	}

	if (helmet && UIOutfitInfo)
	{
		CHelmet* comp_helmet = pCompareItem ? pCompareItem->cast_helmet() : nullptr;
		UIOutfitInfo->UpdateInfo(helmet, comp_helmet);
		UIDesc->AddWindow(UIOutfitInfo, false);
	}

	if (UIOutfitParams && outfit)
	{
		UIOutfitParams->SetInfo(pInvItem);
		UIDesc->AddWindow(UIOutfitParams, false);
	}

	if (UIBackpackParams && backpack)
	{
		UIBackpackParams->SetInfo(pInvItem);
		UIDesc->AddWindow(UIBackpackParams, false);
	}
}

void CUIItemInfo::TryAddUpgradeInfo(CInventoryItem& pInvItem)
{
	if (pInvItem.upgardes().size() && UIProperties)
	{
		UIProperties->set_item_info(pInvItem);
		UIDesc->AddWindow(UIProperties, false);
	}
}

void CUIItemInfo::TryAddBoosterInfo(CInventoryItem& pInvItem)
{
	CEatableItem* food = pInvItem.cast_eatable_item();
	if (food && UIBoosterInfo)
	{
		UIBoosterInfo->SetInfo(pInvItem.object().cNameSect());
		UIDesc->AddWindow(UIBoosterInfo, false);
	}
}

void CUIItemInfo::Draw()
{
	if(m_pInvItem)
		inherited::Draw();
}

void CUIItemInfo::ScrollDown()
{
	if (UIDesc && !m_b_FitToHeight)
		UIDesc->ScrollToEnd();
}

void CUIItemInfo::ScrollUp()
{
	if (UIDesc && !m_b_FitToHeight)
		UIDesc->ScrollToBegin();
}