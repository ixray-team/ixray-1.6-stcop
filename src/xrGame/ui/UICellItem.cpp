#include "StdAfx.h"
#include "UICellItem.h"
#include "../../xrUI/UICursor.h"
#include "../inventory_item.h"
#include "UIDragDropListEx.h"
#include "../../xrEngine/xr_input.h"
#include "../Level.h"
#include "object_broker.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIItemStateDisplay.h"
#include "../../xrUI/UIVectorBinding.h"
#include "../eatable_item.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Actor.h"
#include "../xrGame/ui/UIInventoryUtilities.h"
#include "CustomOutfit.h"
#include "PowerBank.h"
#include "IPowerManager.h"

namespace
{
bool IsSvgTexturePath(const shared_str& path)
{
	if (!path.size())
	{
		return false;
	}

	const char* ext = strext(path.c_str());
	return ext && !_stricmp(ext, ".svg");
}

Fvector2 CalcInvCellAnchorPos(
	const Fvector2& cellSize,
	const Fvector2& childSize,
	CInventoryItem::EInvCellAnchor anchor,
	const Fvector2& inset)
{
	// XML x/y (stored as inset) are the only padding from the corner.
	const float padX = inset.x;
	const float padY = inset.y;

	Fvector2 pos;
	switch (anchor)
	{
	case CInventoryItem::EInvCellAnchor::TopLeft:
		pos.set(padX, padY);
		break;
	case CInventoryItem::EInvCellAnchor::TopRight:
		pos.set(cellSize.x - childSize.x - padX, padY);
		break;
	case CInventoryItem::EInvCellAnchor::BottomLeft:
		pos.set(padX, cellSize.y - childSize.y - padY);
		break;
	case CInventoryItem::EInvCellAnchor::BottomRight:
	default:
		pos.set(cellSize.x - childSize.x - padX, cellSize.y - childSize.y - padY);
		break;
	}
	return pos;
}
} // namespace

CUICellItem* CUICellItem::m_mouse_selected_item = nullptr;

CUICellItem::CUICellItem()
{
	m_pParentList		= nullptr;
	m_ownerContentGeneration = 0;
	m_pData				= nullptr;
	m_custom_draw		= nullptr;
	m_text				= nullptr;
//-	m_mark				= nullptr;
	m_custom_text		= nullptr;
	m_custom_mark		= nullptr;
	m_filter_icon		= nullptr;
	m_upgrade			= nullptr;
	m_pConditionState	= nullptr;
	m_pConditionState_filter = nullptr;
	m_drawn_frame		= 0;
	SetAccelerator		(0);
	m_b_destroy_childs	= true;
	m_selected			= false;
	m_select_armament	= false;
	m_select_equipped	= false;
	m_cur_mark			= false;
	m_has_upgrade		= false;
	m_with_custom_text	= false;
	m_with_custom_mark	= false;
	m_text_use_anchor	= false;
	m_text_anchor		= 0;
	m_text_anchor_inset.set(0.f, 0.f);
	m_condition_use_anchor = false;
	m_condition_anchor = 0;
	m_condition_anchor_inset.set(0.f, 0.f);

	init();
}

CUICellItem::~CUICellItem()
{
	if (m_b_destroy_childs) 
	{
		delete_data(m_childs);
	}

	delete_data		(m_custom_draw);
	m_pCellsConditions.clear();
}

void CUICellItem::init()
{
	if (!uiXml.Load(CONFIG_PATH, UI_PATH, "actor_menu_item.xml"))
	{
		return;
	}
	
	m_text					= new CUIStatic();
	m_text->SetAutoDelete	( true );
	AttachChild				( m_text );
	CUIXmlInit::InitStatic	( uiXml, "cell_item_text", 0, m_text );
	m_text->Show			( false );

	m_text_use_anchor = false;
	m_text_anchor = static_cast<u8>(CInventoryItem::EInvCellAnchor::BottomRight);
	m_text_anchor_inset.set(0.f, 0.f);
	if (uiXml.NavigateToNode("cell_item_text", 0))
	{
		const char* anchorStr = uiXml.ReadAttrib("cell_item_text", 0, "anchor", nullptr);
		if (anchorStr && anchorStr[0])
		{
			m_text_use_anchor = true;
			m_text_anchor = static_cast<u8>(CInventoryItem::ParseInvCellAnchor(anchorStr));
			// With anchor enabled, XML x/y are treated as inset from the corner.
			m_text_anchor_inset = m_text->GetWndPos();
		}
	}

/*	m_mark					= new CUIStatic();
	m_mark->SetAutoDelete	( true );
	AttachChild				( m_mark );
	CUIXmlInit::InitStatic	( uiXml, "cell_item_mark", 0, m_mark );
	m_mark->Show			( false );*/

	m_upgrade				= new CUIStatic();
	m_upgrade->SetAutoDelete( true );
	AttachChild				( m_upgrade );
	CUIXmlInit::InitStatic	( uiXml, "cell_item_upgrade", 0, m_upgrade );
	m_upgrade_pos			= m_upgrade->GetWndPos();
	m_upgrade->Show			( false );

	m_pConditionState_filter = new CUIProgressBar();
	m_pConditionState_filter->SetAutoDelete(true);
	AttachChild(m_pConditionState_filter);
	CUIXmlInit::InitProgressBar(uiXml, "condition_progess_bar", 0, m_pConditionState_filter);
	m_pConditionState_filter->Show(false);

	if (uiXml.NavigateToNode("cell_item_custom_mark", 0))
	{
		m_filter_icon = new CUIStatic();
		m_filter_icon->SetAutoDelete(true);
		AttachChild(m_filter_icon);
		CUIXmlInit::InitStatic(uiXml, "cell_item_custom_mark", 0, m_filter_icon);
		m_filter_icon->Show(false);
	}


	m_pConditionState = new CUIItemStateDisplay();
	m_pConditionState->SetAutoDelete(true);
	AttachChild(m_pConditionState);
	CUIXmlInit::InitItemStateDisplay(uiXml, "condition_progess_bar", 0, m_pConditionState);
	m_pConditionState->Show(true);

	m_condition_use_anchor = false;
	m_condition_anchor = static_cast<u8>(CInventoryItem::EInvCellAnchor::BottomRight);
	m_condition_anchor_inset.set(0.f, 0.f);
	if (uiXml.NavigateToNode("condition_progess_bar", 0))
	{
		const char* anchorStr = uiXml.ReadAttrib("condition_progess_bar", 0, "anchor", nullptr);
		if (anchorStr && anchorStr[0])
		{
			m_condition_use_anchor = true;
			m_condition_anchor = static_cast<u8>(CInventoryItem::ParseInvCellAnchor(anchorStr));
			// With anchor enabled, XML x/y are treated as inset from the corner.
			m_condition_anchor_inset = m_pConditionState->GetWndPos();
		}
	}

	if (uiXml.NavigateToNode("cell_item_custom_text", 0))
	{
		m_custom_text = new CUIStatic();
		m_custom_text->SetAutoDelete(true);
		AttachChild(m_custom_text);

		CUIXmlInit::InitStatic(uiXml, "cell_item_custom_text", 0, m_custom_text);
		m_custom_text_pos = m_custom_text->GetWndPos();
		m_custom_text->Show(false);
	}

	if (uiXml.NavigateToNode("cell_item_custom_mark", 0))
	{
		m_custom_mark = new CUIStatic();
		m_custom_mark->SetAutoDelete(true);
		AttachChild(m_custom_mark);
		CUIXmlInit::InitStatic(uiXml, "cell_item_custom_mark", 0, m_custom_mark);
		m_custom_mark_pos = m_custom_mark->GetWndPos();
		m_custom_mark->Show(false);
	}
}

void CUICellItem::Draw()
{	
	m_drawn_frame		= Device.dwFrame;
	
	inherited::Draw();
	if(m_custom_draw) 
		m_custom_draw->OnDraw(this);
};

void AplyFilterIcon(const shared_str& sect_name, CUIStatic* _static, float width, float height)
{
	if (!sect_name.size())
	{
		return;
	}

	Frect texture_rect;
	float scaleIcon = READ_IF_EXISTS(pSettings, r_float, sect_name, "inv_scale", 1.0f);

	texture_rect.x1 = pSettings->r_float(sect_name, "inv_grid_x") * INV_GRID_WIDTH(scaleIcon);
	texture_rect.y1 = pSettings->r_float(sect_name, "inv_grid_y") * INV_GRID_HEIGHT(scaleIcon);
	texture_rect.x2 = pSettings->r_float(sect_name, "inv_grid_width") * INV_GRID_WIDTH(scaleIcon);
	texture_rect.y2 = pSettings->r_float(sect_name, "inv_grid_height") * INV_GRID_HEIGHT(scaleIcon);

	texture_rect.rb.add(texture_rect.lt);

	_static->GetUIStaticItem().SetTextureRect(texture_rect);
	_static->SetStretchTexture(true);

	const char* icons_texture = READ_IF_EXISTS(pSettings, r_string, sect_name, "icons_texture", nullptr);
	_static->SetShader(InventoryUtilities::GetEquipmentIconsShader(icons_texture));

	float h = height * EngineExternal().GetWeaponIconScaling();
	float w = width * EngineExternal().GetWeaponIconScaling();

	if (texture_rect.width() > 2.01f * INV_GRID_WIDTH(scaleIcon))
	{
		w = INV_GRID_WIDTH(scaleIcon) * 1.5f;
	}

	_static->SetWidth(w * UI().get_current_kx() / scaleIcon);
	_static->SetHeight(h / scaleIcon);
}

void CUICellItem::Update()
{
	EnableHeading(m_pParentList->GetVerticalPlacement());
	if(Heading())
	{
		SetHeading			( 90.0f * (PI/180.0f) );
		SetHeadingPivot		(Fvector2().set(0.0f,0.0f), Fvector2().set(0.0f,GetWndSize().y), true);
	}else
		ResetHeadingPivot	();

	inherited::Update();
	
	if ( CursorOverWindow() )
	{
		Frect clientArea;
		m_pParentList->GetClientArea(clientArea);
		Fvector2 cp			= GetUICursor().GetCursorPosition();
		if(clientArea.in(cp))
			GetMessageTarget()->SendMessage(this, DRAG_DROP_ITEM_FOCUSED_UPDATE, nullptr);
	}
	
	PIItem item = (PIItem)m_pData;

	Ivector2 itm_grid_size = GetGridSize();
	if (m_pParentList->GetVerticalPlacement())
	{
		std::swap(itm_grid_size.x, itm_grid_size.y);
	}

	Ivector2 cell_size = m_pParentList->CellSize();
	Ivector2 cell_space = m_pParentList->CellsSpacing();

	if (item) 
	{
		if (IPowerManager* oPowerManager = smart_cast<IPowerManager*>(item))
		{
			if (!oPowerManager->initialized || !(oPowerManager->GetUsePowerCell() && oPowerManager->IsPowerCellInstalled()))
			{
				if (m_pCellsConditions.size())
				{
					m_pCellsConditions[0]->SetProgressPos(0);
				}
			}
			else
			{
				if (m_pCellsConditions.size() && IsChild(m_pCellsConditions[0]))
				{
					const Fvector2 pos{
						1.f,
						itm_grid_size.y * (cell_size.y + cell_space.y) - (m_pCellsConditions[0]->GetHeight() + 16.f)
					};

					m_pCellsConditions[0]->SetWndPos(pos);
					m_pCellsConditions[0]->SetProgressPos(0);
					m_pCellsConditions[0]->Show(true);
					if (oPowerManager->m_power_cell.current_power > 0 && oPowerManager->m_power_cell.max_power > 0)
					{
						m_pCellsConditions[0]->SetProgressPos(((oPowerManager->m_power_cell.current_power * 100) / oPowerManager->m_power_cell.max_power) / 100);
					}
				}
				else
				{
					CUIProgressBar* bar = new CUIProgressBar();
					bar->SetProgressPos(0.f);
					bar->Show(false);
					AttachChild(bar);
					CUIXmlInit::InitProgressBar(GetXml(), "condition_progess_bar", 0, bar);
					m_pCellsConditions.push_back(bar);
				}
			}
		}

		if (PowerBank* pb = smart_cast<PowerBank*>(item))
		{
			if (m_pCellsConditions.empty())
			{
				for (int i = 0; i < pb->m_max_count_power_cells; i++)
				{
					CUIProgressBar* bar = new CUIProgressBar();
					bar->SetAutoDelete(true);
					AttachChild(bar);
					CUIXmlInit::InitProgressBar(GetXml(), "condition_progess_bar", 0, bar);
					bar->SetProgressPos(0.f);
					bar->Show(false);
					m_pCellsConditions.push_back(bar);
				}
			}

			size_t cnt = m_pCellsConditions.size();
			size_t cnt_cells = pb->m_power_cells.size();

			for (size_t i = 0; i < cnt; i++)
			{
				if (CUIProgressBar* bar = m_pCellsConditions[i])
				{
					const Fvector2 pos{
						1.f,
						itm_grid_size.y * (cell_size.y + cell_space.y) - bar->GetHeight() - (10.f + (6.f * i))
					};

					bar->SetWndPos(pos);
					bar->SetProgressPos(iCeil(pb->GetCalculatedCondition() * 13.0f) / 13.0f);
					bar->SetProgressPos(0);
					bar->Show(true);
				}
			}

			for (size_t i = 0; i < cnt_cells; i++)
			{
				if (CUIProgressBar* bar = m_pCellsConditions[i])
				{
					bar->SetProgressPos(((pb->m_power_cells[i].current_power * 100) / pb->m_power_cells[i].max_power) / 100);
				}
			}
			return;
		}

		if (IAntigas* antigas = smart_cast<IAntigas*>(item->cast_inventory_item()))
		{
			if (antigas->IsAllowed() && antigas->IsFilterInstalled()) 
			{
				float filter_condition = antigas->GetFilterCondition();

				if (m_pConditionState_filter != nullptr) 
				{
					m_pConditionState_filter->SetWndPos(Fvector2().set(
						1.f,
						itm_grid_size.y * (cell_size.y + cell_space.y) - m_pConditionState_filter->GetHeight() - 10.f
					));
					m_pConditionState_filter->SetProgressPos(iCeil(filter_condition * 13.0f) / 13.0f);
					m_pConditionState_filter->Show(true);
				}

				if (m_filter_icon != nullptr)
				{
					m_filter_icon->SetWndPos(Fvector2().set(
						antigas->GetFilterIconOffsetX(),
						antigas->GetFilterIconOffsetY()
					));

					AplyFilterIcon(
						antigas->GetFilterSection(),
						m_filter_icon,
						antigas->GetFilterIconWidth(),
						antigas->GetFilterIconHeight()
					);

					m_filter_icon->Show(true);
				}
			}
			else
			{
				if (m_pConditionState_filter != nullptr) 
				{
					m_pConditionState_filter->SetProgressPos(0.f);
					m_pConditionState_filter->Show(false);
				}

				if (m_filter_icon != nullptr)
				{
					m_filter_icon->Show(false);
				}
			}
		}
		else
		{
			if (m_pConditionState_filter != nullptr) 
			{
				m_pConditionState_filter->Show(false);
			}

			if (m_filter_icon != nullptr)
			{
				m_filter_icon->Show(false);
			}
		}
	}

    m_has_upgrade = item ? item->has_any_upgrades() : false;
    if (m_upgrade)
    {
        if (item)
        {
            //		Fvector2 size      = GetWndSize();
            //		Fvector2 up_size = m_upgrade->GetWndSize();
            //		pos.x = size.x - up_size.x - 4.0f;
            Fvector2 pos;
            pos.set(m_upgrade_pos);
            if (ChildsCount())
            {
                const float textSize = m_text ? m_text->GetWndSize().x : 0.f;
                pos.x += textSize + 2.0f;
            }
            m_upgrade->SetWndPos(pos);
        }
        m_upgrade->Show(m_has_upgrade);
    }
	UpdateItemTextAnchor();
	UpdateCustomMarksAndText();
}

void CUICellItem::UpdateCustomMarksAndText()
{
	PIItem item = static_cast<PIItem>(m_pData);
	m_with_custom_text = false;
	m_with_custom_mark = false;

	if (!item)
	{
		if (m_custom_text)
		{
			m_custom_text->Show(false);
		}
		if (m_custom_mark)
		{
			m_custom_mark->Show(false);
		}
		return;
	}

	const Fvector2 cellSize = GetWndSize();

	if (m_custom_text)
	{
		string32 usesBuf = {};
		const char* textToShow = nullptr;
		bool useStringTable = false;

		if (item->m_custom_text.size())
		{
			textToShow = item->m_custom_text.c_str();
			useStringTable = true;
		}
		else if (item->m_custom_text_auto_uses)
		{
			if (CEatableItem* eatable = item->cast_eatable_item())
			{
				if (eatable->GetMaxUses() > 1)
				{
					xr_sprintf(usesBuf, "%u/%u",
						static_cast<u32>(eatable->GetRemainingUses()),
						static_cast<u32>(eatable->GetMaxUses()));
					textToShow = usesBuf;
				}
			}
		}

		m_with_custom_text = textToShow != nullptr;
		if (m_with_custom_text)
		{
			const Fvector2 textSize = m_custom_text->GetWndSize();
			m_custom_text->SetWndPos(CalcInvCellAnchorPos(
				cellSize, textSize, item->m_custom_text_anchor, item->m_custom_text_offset));

			if (useStringTable)
			{
				m_custom_text->TextItemControl()->SetTextST(textToShow);
			}
			else
			{
				m_custom_text->TextItemControl()->SetText(textToShow);
			}

			if (item->m_custom_text_clr_inv != 0)
			{
				m_custom_text->TextItemControl()->SetTextColor(item->m_custom_text_clr_inv);
			}
			if (item->m_custom_text_font != nullptr)
			{
				m_custom_text->TextItemControl()->SetFont(item->m_custom_text_font);
			}
		}
		m_custom_text->Show(m_with_custom_text);
	}

	if (m_custom_mark)
	{
		m_with_custom_mark = item->m_custom_mark;
		if (m_with_custom_mark)
		{
			if (item->m_custom_mark_size.x > 0.f && item->m_custom_mark_size.y > 0.f)
			{
				m_custom_mark->SetWndSize(item->m_custom_mark_size);
			}
			else if (item->m_custom_mark_size.x < 0.f || item->m_custom_mark_size.y < 0.f)
			{
				R_ASSERT(!"item_custom_mark_size < 0.f");
			}

			const Fvector2 markSize = m_custom_mark->GetWndSize();
			m_custom_mark->SetWndPos(CalcInvCellAnchorPos(
				cellSize, markSize, item->m_custom_mark_anchor, item->m_custom_mark_offset));

			if (item->m_custom_mark_texture.size())
			{
				if (IsSvgTexturePath(item->m_custom_mark_texture))
				{
					SVGTintRGBA tint{};
					if (item->m_custom_mark_clr != 0)
					{
						tint.SetFromColourDword(item->m_custom_mark_clr);
					}
					CUIVectorBinding::ApplyVectorPathToStatic(
						*m_custom_mark,
						item->m_custom_mark_texture.c_str(),
						markSize.x,
						markSize.y,
						tint);
					m_custom_mark->SetStretchTexture(true);
				}
				else
				{
					m_custom_mark->InitTextureEx(item->m_custom_mark_texture.c_str());
					if (item->m_custom_mark_clr != 0)
					{
						m_custom_mark->SetTextureColor(item->m_custom_mark_clr);
					}
				}
			}
			else if (item->m_custom_mark_clr != 0)
			{
				m_custom_mark->SetTextureColor(item->m_custom_mark_clr);
			}
		}
		m_custom_mark->Show(m_with_custom_mark);
	}
}

bool CUICellItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if ( mouse_action == WINDOW_LBUTTON_DOWN )
	{
		GetMessageTarget()->SendMessage( this, DRAG_DROP_ITEM_LBUTTON_CLICK, nullptr );
		GetMessageTarget()->SendMessage( this, DRAG_DROP_ITEM_SELECTED, nullptr );
		m_mouse_selected_item = this;
		return false;
	}
	else if ( mouse_action == WINDOW_MOUSE_MOVE )
	{
		if ( pInput->LeftMouseButtonPressed() && m_mouse_selected_item && m_mouse_selected_item == this )
		{
			GetMessageTarget()->SendMessage( this, DRAG_DROP_ITEM_DRAG, nullptr );
			return true;
		}
	}
	else if ( mouse_action == WINDOW_LBUTTON_DB_CLICK )
	{
		GetMessageTarget()->SendMessage( this, DRAG_DROP_ITEM_DB_CLICK, nullptr );
		return true;
	}
	else if ( mouse_action == WINDOW_RBUTTON_DOWN )
	{
		GetMessageTarget()->SendMessage( this, DRAG_DROP_ITEM_RBUTTON_CLICK, nullptr );
		return true;
	}
	
	m_mouse_selected_item = nullptr;
	return false;
};

bool g_Adjust3dIcon = false;
float g_Adjust3dIconValue = 0.1f;
bool CUICellItem::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
        if (g_Adjust3dIcon && m_bCursorOverWindow)
        {
            bool bCtrl = !!pInput->iGetAsyncKeyState(SDL_SCANCODE_LCTRL);
            float val = (bCtrl ? -1.f : 1.f) * g_Adjust3dIconValue;
            Fvector xyz = GetXYZ();
            float fScale = GetScaleFactor();
            if (dik == SDL_SCANCODE_Z)
                xyz.x += deg2rad(val * 10.f);
            else if (dik == SDL_SCANCODE_X)
                xyz.y += deg2rad(val * 10.f);
            else if (dik == SDL_SCANCODE_C)
                xyz.z += deg2rad(val * 10.f);
            else if (dik == SDL_SCANCODE_V)
                fScale += val;
            else if (dik == SDL_SCANCODE_B)
            {
                PIItem itm = (PIItem)m_pData;
                Msg("[%s]", itm->m_section_id.c_str());
                string256 tmpStr;
                xr_sprintf(tmpStr, "3d_static_rotate\t\t\t= %f,%f,%f",
                           rad2deg(xyz.x),
                           rad2deg(xyz.y),
                           rad2deg(xyz.z));
                Log(tmpStr);
                xr_sprintf(tmpStr, "3d_static_scale\t\t\t= %f",
                           fScale);
                Log(tmpStr);
            }
            SetScaleFactor(fScale);
            SetXYZ(xyz);
            return true;
        }
		if (GetAccelerator() == dik)
		{
			GetMessageTarget()->SendMessage(this, DRAG_DROP_ITEM_DB_CLICK, NULL);
			return		true;
		}
	}
	return inherited::OnKeyboardAction(dik, keyboard_action);
}

CUIDragItem* CUICellItem::CreateDragItem()
{
	CUIDragItem* tmp;
	tmp = new CUIDragItem(this);
	Frect r;
	GetAbsoluteRect(r);

	if( m_UIStaticItem.GetFixedLTWhileHeading() )
	{
		float t1,t2;
		t1				= r.width();
		t2				= r.height()*UI().get_current_kx();

		Fvector2 cp = GetUICursor().GetCursorPosition();

		r.x1			= (cp.x-t2/2.0f);
		r.y1			= (cp.y-t1/2.0f);
		r.x2			= r.x1 + t2;
		r.y2			= r.y1 + t1;
	}
	tmp->Init(GetShader(), r, GetUIStaticItem().GetTextureRect());
	if (psActorFlags.test(AF_3D_ICONS_INV))
	{
		tmp->wnd()->SetVisual(GetVisual());
		Fvector xyz = GetXYZ();
		if (m_pParentList->GetVerticalPlacement())
		{
			xyz.x -= deg2rad(90.f);
		}
		tmp->wnd()->SetXYZ(xyz);
		tmp->wnd()->SetScaleFactor(GetScaleFactor());
		tmp->wnd()->SetBonesVisible(GetVisual()->dcast_PKinematics());
	}
	else
	{
		tmp->wnd()->SetVisual(nullptr);
	}
	return tmp;
}

void CUICellItem::SetOwnerList(CUIDragDropListEx* p)	
{
	m_pParentList = p;
	m_ownerContentGeneration = (p != nullptr) ? p->ContentGeneration() : 0;
}

bool CUICellItem::HasValidInventoryBinding() const
{
	return m_pData != nullptr;
}

bool CUICellItem::IsOwnerListValid() const
{
	return m_pParentList != nullptr
		&& m_ownerContentGeneration != 0
		&& m_ownerContentGeneration == m_pParentList->ContentGeneration();
}

void CUICellItem::UpdateConditionProgressBar()
{
	if (!m_pConditionState)
	{
		return;
	}

	if (!IsOwnerListValid() || !m_pParentList->GetConditionProgBarVisibility())
	{
		m_pConditionState->Show(false);
		return;
	}

	PIItem itm = (PIItem)m_pData;

	if (itm == nullptr)
	{
		m_pConditionState->Show(false);
		return;
	}

	Ivector2 itm_grid_size = GetGridSize();
	if (m_pParentList->GetVerticalPlacement())
	{
		std::swap(itm_grid_size.x, itm_grid_size.y);
	}

	Ivector2 cell_size = m_pParentList->CellSize();
	Ivector2 cell_space = m_pParentList->CellsSpacing();

	const InventoryUtilities::ConditionDisplayParams display =
		InventoryUtilities::GetConditionDisplayParams(itm);

	if (!itm->IsUsingCondition())
	{
		m_pConditionState->Show(false);
		return;
	}

	if (m_condition_use_anchor)
	{
		m_pConditionState->SetWndPos(CalcInvCellAnchorPos(
			GetWndSize(),
			m_pConditionState->GetWndSize(),
			static_cast<CInventoryItem::EInvCellAnchor>(m_condition_anchor),
			m_condition_anchor_inset));
	}
	else
	{
		// Legacy layout: bottom strip with fixed left inset.
		const float x = 1.0f;
		const float y = itm_grid_size.y * (cell_size.y + cell_space.y) - m_pConditionState->GetHeight() - 2.0f;
		m_pConditionState->SetWndPos(Fvector2().set(x, y));
	}

	m_pConditionState->m_bUseGradient = !display.disableGradient;

	if (display.hideBackground)
	{
		m_pConditionState->ShowBackground(false);
	}

	CEatableItem* eatableItem = itm->cast_eatable_item();
	if (m_pConditionState->GetPercentFormat() == CUIItemStateDisplay::EPercentFormat::Portion &&
		eatableItem != nullptr &&
		display.usePortion &&
		display.portionMax > 1)
	{
		m_pConditionState->SetPortion(display.portionCurrent, display.portionMax);
	}
	else
	{
		m_pConditionState->SetState(display.state);
	}

	m_pConditionState->Show(true);
}

bool CUICellItem::EqualTo(CUICellItem* itm)
{
	return (m_grid_size.x==itm->GetGridSize().x) && (m_grid_size.y==itm->GetGridSize().y);
}

u32 CUICellItem::ChildsCount()
{
	return (u32)m_childs.size();
}

void CUICellItem::PushChild(CUICellItem* c)
{
	R_ASSERT(c->ChildsCount()==0);
	VERIFY				(this!=c);
	m_childs.push_back	(c);
	UpdateItemText		();
}

CUICellItem* CUICellItem::PopChild(CUICellItem* needed)
{
	CUICellItem* itm	= m_childs.back();
	m_childs.pop_back	();
	
	if(needed)
	{	
	  if(itm!=needed)
		std::swap		(itm->m_pData, needed->m_pData);
	}else
	{
		std::swap		(itm->m_pData, m_pData);
	}
	UpdateItemText		();
	R_ASSERT			(itm->ChildsCount()==0);
	itm->SetOwnerList	(nullptr);
	return				itm;
}

bool CUICellItem::HasChild(CUICellItem* item)
{
	return (m_childs.end() != std::find(m_childs.begin(), m_childs.end(), item) );
}

void CUICellItem::UpdateItemText()
{
    string32 tempStr;
    const char* finalText = nullptr;
    if (ChildsCount())
    {
        xr_sprintf(tempStr, "x%d", ChildsCount() + 1);
        finalText = tempStr;
    }

    if (m_text)
    {
        m_text->Show(nullptr != finalText);
        m_text->SetText(finalText);
		UpdateItemTextAnchor();
    }
    else
    {
        this->SetText(finalText);
    }
}

void CUICellItem::UpdateItemTextAnchor()
{
	if (!m_text || !m_text_use_anchor)
	{
		return;
	}

	m_text->SetWndPos(CalcInvCellAnchorPos(
		GetWndSize(),
		m_text->GetWndSize(),
		static_cast<CInventoryItem::EInvCellAnchor>(m_text_anchor),
		m_text_anchor_inset));
}

void CUICellItem::Mark( bool status )
{
	m_cur_mark = status;
}

void CUICellItem::SetCustomDraw(ICustomDrawCellItem* c)
{
	if (m_custom_draw)
		xr_delete(m_custom_draw);
	m_custom_draw = c;
}

// -------------------------------------------------------------------------------------------------

CUIDragItem::CUIDragItem(CUICellItem* parent)
{
	m_custom_draw					= nullptr;
	m_back_list						= nullptr;
	m_pParent						= parent;
	AttachChild						(&m_static);
	Device.seqRender.Add			(static_cast<pureRender*>(this), REG_PRIORITY_LOW - 5000);
	Device.seqFrame.Add				(static_cast<pureFrame*>(this), REG_PRIORITY_LOW - 5000);
	_deviceSequencesRegistered		= true;
	VERIFY							(m_pParent->GetMessageTarget());
}

void CUIDragItem::UnregisterDeviceSequences()
{
	if (!_deviceSequencesRegistered)
	{
		return;
	}

	Device.seqRender.Remove(static_cast<pureRender*>(this));
	Device.seqFrame.Remove(static_cast<pureFrame*>(this));
	_deviceSequencesRegistered = false;
}

CUIDragItem::~CUIDragItem()
{
	UnregisterDeviceSequences();
	delete_data(m_custom_draw);
}

void CUIDragItem::SetCustomDraw(ICustomDrawDragItem* c)
{
	if (m_custom_draw)
		xr_delete(m_custom_draw);
	m_custom_draw = c;
}

void CUIDragItem::Init(const ui_shader& sh, const Frect& rect, const Frect& text_rect)
{
	SetWndRect						(rect);
	m_static.SetShader				(sh);
	m_static.SetTextureRect			(text_rect);
	m_static.SetWndPos				(Fvector2().set(0.0f,0.0f));
	m_static.SetWndSize				(GetWndSize());
	m_static.TextureOn				();
	m_static.SetTextureColor		(color_rgba(255,255,255,170));
	m_static.SetStretchTexture		(true);
	m_pos_offset.sub				(rect.lt, GetUICursor().GetCursorPosition());
}

bool CUIDragItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if(mouse_action == WINDOW_LBUTTON_UP)
	{
		m_pParent->GetMessageTarget()->SendMessage(m_pParent,DRAG_DROP_ITEM_DROP,nullptr);
		return true;
	}
	return false;
}

void CUIDragItem::OnRender()
{
	Draw			();
}

void CUIDragItem::OnFrame()
{
	Update			();
}

void CUIDragItem::Draw()
{
	Fvector2 tmp;
	tmp.sub					(GetWndPos(), GetUICursor().GetCursorPosition());
	tmp.sub					(m_pos_offset);
	tmp.mul					(-1.0f);
	MoveWndDelta			(tmp);
	inherited::Draw			();
	if(m_custom_draw) 
		m_custom_draw->OnDraw(this);
}

void CUIDragItem::SetBackList(CUIDragDropListEx* l)
{
	if(m_back_list)
		m_back_list->OnDragEvent(this, false);

	m_back_list					= l;

	if(m_back_list)
		l->OnDragEvent			(this, true);
}

Fvector2 CUIDragItem::GetPosition()
{
	return Fvector2().add(m_pos_offset, GetUICursor().GetCursorPosition());
}

