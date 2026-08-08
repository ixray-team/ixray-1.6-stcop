#include "StdAfx.h"
#include "UIRadialMenu.h"

#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Inventory.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/UICursor.h"
#include "../../xrUI/Widgets/UI3dStatic.h"

CUIRadialMenu::CUIRadialMenu()
{
	Init();
}

void CUIRadialMenu::Init(CUIXml* pXml)
{
	CUIXmlInit::InitWindow(*pXml, "main", 0, this);

	// load sounds
	XML_NODE* stored_root = pXml->GetLocalRoot();
	XML_NODE* node = pXml->NavigateToNode("radial_menu", 0);
	pXml->SetLocalRoot(node);

	pXml->SetLocalRoot(pXml->NavigateToNode("action_sounds", 0));
	::Sound->create(sounds[eSndOpen], pXml->Read("snd_open", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndClose], pXml->Read("snd_close", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndSwitch], pXml->Read("snd_switch_slot", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndSelect], pXml->Read("snd_select_slot", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndFireMode], pXml->Read("snd_fire_mode", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndGrenadeMode], pXml->Read("snd_grenade_mode", 0, nullptr), st_Effect, sg_SourceType);
	pXml->SetLocalRoot(stored_root);

	pXml->SetLocalRoot(pXml->NavigateToNode("radial_menu:general", 0));

	backgroundSize.x = pXml->ReadAttribFlt("size_back", 0, "width", 0.f);
	backgroundSize.y = pXml->ReadAttribFlt("size_back", 0, "height", 0.f);

	iconSize.x = pXml->ReadAttribFlt("size_icon", 0, "width", 0.f);
	iconSize.y = pXml->ReadAttribFlt("size_icon", 0, "height", 0.f);

	backgroundPivot.x = pXml->ReadAttribFlt("pivot", 0, "x", 0.f);
	backgroundPivot.y = pXml->ReadAttribFlt("pivot", 0, "y", 0.f);

	textureDefault = pXml->Read("texture_default", 0, nullptr);
	textureSelected = pXml->Read("texture_selected", 0, nullptr);
	textureFocused = pXml->Read("texture_focused", 0, nullptr);
	textureFocusedSelected = pXml->Read("texture_focused_and_selected", 0, nullptr);

	pXml->SetLocalRoot(stored_root);
	pXml->SetLocalRoot(node);

	sectors_count = (u32)pXml->GetNodesNum(pXml->GetLocalRoot(), "element");

	//read slots
	for (u32 i = 0; i < sectors_count; ++i)
	{
		u32 slotId = pXml->ReadAttribInt("element", i, "slot", 0);
		slotsInSectors.push_back(slotId);

		CUIStatic* back = new CUIStatic();
		back->InitTexture(textureDefault.c_str());
		back->SetStretchTexture(true);

		back->EnableHeading(true);
		float heading = deg2rad(pXml->ReadAttribFlt("element", i, "angle", 0.f));
		back->SetHeading(heading);
		back->SetWndSize(backgroundSize);

		Fvector2 offset{UI_BASE_WIDTH, UI_BASE_HEIGHT};
		offset.mad(offset, backgroundPivot, -2.0f);
		offset.div(2.0f);

		back->SetHeadingPivot(backgroundPivot, offset, false);

		slotBackgrounds.push_back(back);

		CUI3dStatic* icon = new CUI3dStatic();

		icon->SetWndSize(backgroundSize);
		icon->SetAlignment(EWindowAlignment::waCenter);
		icon->SetStretchTexture(true);

		const char* iconTag = "element:icon";
		shared_str iconTexture = pXml->Read(iconTag, i, "");
		if (iconTexture.size())
		{
			icon->InitTexture(iconTexture.c_str());
			icon->SetWidth(icon->GetWidth() * UI().get_current_kx());

			Frect rect;
			rect.x1 = pXml->ReadAttribFlt(iconTag, i, "x", 0);
			rect.y1 = pXml->ReadAttribFlt(iconTag, i, "y", 0);
			rect.x2 = rect.x1 + pXml->ReadAttribFlt(iconTag, i, "width", 0);
			rect.y2 = rect.y1 + pXml->ReadAttribFlt(iconTag, i, "height", 0);

			if (rect.width() != 0 && rect.height() != 0)
			{
				icon->SetTextureRect(rect);
			}
		}

		Fvector2 posIcon{UI_BASE_WIDTH/2,UI_BASE_HEIGHT/2};
		posIcon.x -= sin(heading) * ((backgroundPivot.y - (backgroundSize.x) / 2) * UI().get_current_kx());
		posIcon.y -= cos(heading) * (backgroundPivot.y - (backgroundSize.y / 2));
		/* if (slotId == NO_ACTIVE_SLOT)
		{
			posIcon.x -= (icon->GetWidth() / 2);
			posIcon.y -= (icon->GetHeight() / 2);
		}*/

		icon->SetWndPos(posIcon);
		Fvector2 cellSize{iconSize};
		float width = pXml->ReadAttribFlt("element", i, "width");
		float height = pXml->ReadAttribFlt("element", i, "height");
		if (!fis_zero(width))
		{
			cellSize.x = width;
		}
		if (!fis_zero(height))
		{
			cellSize.y = height;
		}
		slotIconDefaultSizes.push_back(cellSize);

		slotIcons.push_back(icon);
	}

	sector = PI_MUL_2 / sectors_count;

	pXml->SetLocalRoot(stored_root);
	// Gamepad legend
	m_pGamepadLegend = new CUIGamepadLegend();
	m_pGamepadLegend->SetAutoDelete(true);
	AttachChild(m_pGamepadLegend);
	CUIXmlInit::InitGamepadLegend(*pXml, "gamepad_legend", 0, m_pGamepadLegend);

	isInitialized = true;
}

void CUIRadialMenu::PlaySnd(eRadialMenuSndAction a)
{
	if (sounds[a].handle())
		sounds[a].play(nullptr, sm_2D);
}

void CUIRadialMenu::Show(bool status)
{
	if (status)
	{
		selected_index = -1;
		bWaitForZeroRStick = false;
		PlaySnd(eSndOpen);
	}
	else
	{
		PlaySnd(eSndClose);
	}

	inherited::Show(status);
}

void CUIRadialMenu::OnActivateSectorClicked()
{
	TryActivateSelectedSector();
	selected_index = -1;
	bWaitForZeroRStick = true;
}

bool CUIRadialMenu::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	Fvector2 pos = GetUICursor().GetCursorPositionDelta();
	if (mouse_action == WINDOW_MOUSE_MOVE)
	{
		if (!fis_zero(pos.x) || !fis_zero(pos.y))
		{
			if (!bWaitForZeroRStick && std::abs(pos.magnitude()) > 30.f)
			{
				Fvector2 vec{pos.x, -pos.y};
				float angle = vec.getH() + PI_MUL_2;
				int focus_index = iCeil((angle * sectors_count) / PI_MUL_2) % sectors_count;
				if (focus_index != selected_index)
				{
					PlaySnd(eSndSwitch);
				}
				selected_index = focus_index;
			}
		}
		else
		{
			bWaitForZeroRStick = false;
		}
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}

bool CUIRadialMenu::OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action)
{
	if (key == 1)
	{
		if (!fis_zero(value.x) || !fis_zero(value.y))
		{
			if (!bWaitForZeroRStick && std::abs(value.magnitude()) > 0.75f)
			{
				Fvector2 vec{value.x, -value.y};
				float angle = vec.getH() + PI_MUL_2;
				int focus_index = iCeil((angle * sectors_count) / PI_MUL_2) % sectors_count;
				if (focus_index != selected_index)
				{
					PlaySnd(eSndSwitch);
				}
				selected_index = focus_index;
			}
		}
		else 
		{
			bWaitForZeroRStick = false;
		}
	}
	return inherited::OnGamepadStickAction(key, value, gamepad_action);
}

bool CUIRadialMenu::OnTouchpadAction(Fvector2 value)
{
	if (!fis_zero(value.x) || !fis_zero(value.y))
	{
		if (!bWaitForZeroRStick && std::abs(value.magnitude()) > 0.5f)
		{
			Fvector2 vec{value.x, -value.y};
			float angle = vec.getH() + PI_MUL_2;
			int focus_index = iCeil((angle * sectors_count) / PI_MUL_2) % sectors_count;
			if (focus_index != selected_index)
			{
				PlaySnd(eSndSwitch);
			}
			selected_index = focus_index;
		}
	}
	else 
	{
		bWaitForZeroRStick = false;
	}
	return inherited::OnTouchpadAction(value);
}
