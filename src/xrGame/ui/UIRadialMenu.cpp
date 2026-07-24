#include "StdAfx.h"
#include "UIRadialMenu.h"

#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Inventory.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/UICursor.h"
#include "../../xrUI/Widgets/UI3dStatic.h"

void draw_arc(ui_shader& shader, float x, float y, float radius, float radius_inside, float angle1, float angle2, u32 color1, u32 color2, u32 quality) {
	const float angInc = (angle2 - angle1) / static_cast<float>(quality);
	const float cosInc = cos(angInc);
	const float sinInc = sin(angInc);

	UIRender->StartPrimitive(2 * quality + 2, IUIRender::ptTriStrip, IUIRender::ePointType::pttTL);

	float xc = cos(angle1);
	float yc = sin(angle1);

	for (unsigned iAng = 0; iAng <= quality; ++iAng) 
	{
		Fvector2 pos;
		UI().ClientToScreenScaled(pos, x + (xc * radius_inside * UI().get_current_kx()), y + yc * radius_inside);
		UIRender->PushPoint(pos.x, pos.y, 0, color1, 0, 0);

		UI().ClientToScreenScaled(pos, x + (xc * radius * UI().get_current_kx()), y + yc * radius);
		UIRender->PushPoint(pos.x, pos.y, 0, color2, 0, 0);

		float xcNew = cosInc * xc - sinInc * yc;
		yc = sinInc * xc + cosInc * yc;
		xc = xcNew;
	}

	UIRender->SetShader(*shader);
	UIRender->FlushPrimitive();
}

// max side length an item occupies in the atlas grid is 6 (long sniper rifles)
// dont scaleup small items too much
void CUIRadialMenu::DrawItem(CUIStatic* st, TexturedRectDrawData& trdd, u32 color_mask)
{
	st->SetStretchTexture(true);

	float scale = float(st->GetTextureRect().height()) / st->GetTextureRect().width();

	float width = trdd.width;
	float height = trdd.height * scale;

	float x1 = trdd.x - width/2;
	float y1 = trdd.y - height / 2;

	Fvector2 pos{x1, y1};
	st->SetWndPos(pos);
	st->SetWidth(width);
	st->SetHeight(height);
	st->SetTextureColor(color_mask);
	st->Draw();
}


CUIRadialMenu::CUIRadialMenu()
	:m_pGamepadLegend(nullptr)
{
	Init();
}
CUIRadialMenu::~CUIRadialMenu()
{
	xr_delete(crosshair_shader);
	crosshair_shader = nullptr;
}


void CUIRadialMenu::Init(CUIXml* pXml)
{
	CUIXmlInit::InitWindow(*pXml, "main", 0, this);

	// load sounds
	XML_NODE* stored_root = pXml->GetLocalRoot();
	pXml->SetLocalRoot(pXml->NavigateToNode("action_sounds", 0));
	::Sound->create(sounds[eSndOpen], pXml->Read("snd_open", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndClose], pXml->Read("snd_close", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndSwitch], pXml->Read("snd_switch_slot", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndSelect], pXml->Read("snd_select_slot", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndFireMode], pXml->Read("snd_fire_mode", 0, nullptr), st_Effect, sg_SourceType);
	::Sound->create(sounds[eSndGrenadeMode], pXml->Read("snd_grenade_mode", 0, nullptr), st_Effect, sg_SourceType);
	pXml->SetLocalRoot(stored_root);

	const char* pPath = "wheel_menu_params";
	gap				= pXml->ReadAttribFlt(pPath, 0, "sector_gap_angle", M_PI / 84);
	sectors_count	= pXml->ReadAttribInt(pPath, 0, "sector_count", 8);
	starting_angle	= pXml->ReadAttribFlt(pPath, 0, "starting_angle", 0.0f);
	safezone_height_factor		= pXml->ReadAttribFlt(pPath, 0, "safezone_height_factor", 0.0f);
	inner_radius_ratio			= pXml->ReadAttribFlt(pPath, 0, "inner_radius_ratio", 0.0f);
	selected_radius_factor		= pXml->ReadAttribFlt(pPath, 0, "selected_radius_factor", 0.0f);
	sector = (2 * M_PI) / sectors_count - gap;

	textureDefault = pXml->Read("default_arc:texture", 0, nullptr);
	textureSelected = pXml->Read("selected_arc:texture", 0, nullptr);
	textureFocused = pXml->Read("focused_arc:texture", 0, nullptr);
	textureFocusedSelected = pXml->Read("focused_and_selected_arc:texture", 0, nullptr);

	//read slots
	XML_NODE* node = pXml->NavigateToNode(pPath, 0);
	pXml->SetLocalRoot(node);
	for (int i = 0; i < sectors_count; ++i)
	{
		int slotId = pXml->ReadAttribInt("slot", i, "id", 0);
		slotsInSectors.push_back((u32)slotId);

		CUIStatic* back = new CUIStatic();
		back->InitTexture(textureDefault.c_str());
		back->SetStretchTexture(true);
		back->EnableHeading(true);
		slotBackgrounds.push_back(back);

		CUI3dStatic* icon = new CUI3dStatic();
		slotIcons.push_back(icon);
	}

	sector_inner_side_color			= CUIXmlInit::GetColor(*pXml, "sector_inner_side_color", 0, 0x0);
	sector_outer_side_color			= CUIXmlInit::GetColor(*pXml, "sector_outer_side_color", 0, 0x0);
	selected_color					= CUIXmlInit::GetColor(*pXml, "color_selected", 0, 0x0);//0xFFFF9944
	deselected_color				= CUIXmlInit::GetColor(*pXml, "color_deselected", 0, 0x0);//0xFFFF9944
	pXml->SetLocalRoot(stored_root);

	center_x = UI_BASE_WIDTH / 2.0f;
	center_y = UI_BASE_HEIGHT / 2.0f;
	radius = center_y - UI_BASE_HEIGHT * safezone_height_factor;
	inner_radius = radius * inner_radius_ratio;
	selected_radius = inner_radius * selected_radius_factor;

	// Create shaders
	crosshair_shader = new ui_shader();
	(*crosshair_shader)->create("hud\\crosshair");

	// Gamepad legend
	m_pGamepadLegend = new CUIGamepadLegend();
	m_pGamepadLegend->SetAutoDelete(true);
	AttachChild(m_pGamepadLegend);

	CUIXmlInit::InitGamepadLegend(*pXml, "gamepad_legend", 0, m_pGamepadLegend);
	emptyIconName = pXml->Read("texture_empty_hands", 0, "ui_rm_icon_empty");
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
				float angle = atan2(pos.y, pos.x) + 2 * M_PI;
				int focus_index = iFloor(((angle - starting_angle) * sectors_count) / (2 * M_PI)) % sectors_count;
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
				float angle = atan2(value.y, value.x) + 2 * M_PI;
				int focus_index = iFloor(((angle - starting_angle) * sectors_count) / (2 * M_PI)) % sectors_count;
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
