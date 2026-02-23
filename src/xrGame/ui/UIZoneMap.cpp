#include "StdAfx.h"
#include "UIZoneMap.h"

#include "InfoPortion.h"
#include "PDA.h"

#include "Grenade.h"
#include "Level.h"
#include "game_cl_base.h"

#include "Actor.h"
#include "ai_space.h"
#include "game_graph.h"

#include "ui/UIMap.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/uiabstract.h"
#include "ui/UIInventoryUtilities.h"
#include "../map_manager.h"
#include "../game_cl_single.h"

static bool ParseFrameInset(LPCSTR str, float& insetLeft, float& insetTop, float& insetRight, float& insetBottom)
{
	if (!str || !str[0])
	{
		return false;
	}
	int n = sscanf(str, "%f,%f,%f,%f", &insetLeft, &insetTop, &insetRight, &insetBottom);
	if (n == 4)
	{
		return true;
	}
	if (n == 2)
	{
		insetRight = insetLeft;
		insetBottom = insetTop;
		return true;
	}
	if (n == 1)
	{
		insetTop = insetRight = insetBottom = insetLeft;
		return true;
	}
	return false;
}

//////////////////////////////////////////////////////////////////////////

CUIZoneMap::CUIZoneMap()
:m_current_map_idx(u8(-1)),
visible(true)
{	
	m_clock_wnd = nullptr;
	m_Counter_text = nullptr;
	m_Counter = nullptr;
	m_pointerDistanceText = nullptr;
	disabled = false;
}

CUIZoneMap::~CUIZoneMap()
{
}

void CUIZoneMap::Init()
{
	CUIXml uiXml;
	uiXml.Load						(CONFIG_PATH, UI_PATH, "zone_map.xml");

	CUIXmlInit						xml_init;

	float dummyInsets[4];
	const LPCSTR frameInsetStr = uiXml.ReadAttrib("minimap", 0, "frame_inset", nullptr);
	const bool useFrameInset = frameInsetStr && ParseFrameInset(frameInsetStr, dummyInsets[0], dummyInsets[1], dummyInsets[2], dummyInsets[3]);

	if (useFrameInset)
	{
		float insetL, insetT, insetR, insetB;
		ParseFrameInset(frameInsetStr, insetL, insetT, insetR, insetB);

		xml_init.InitWindow(uiXml, "minimap", 0, &m_background);
		xml_init.InitTexture(uiXml, "minimap:background", 0, &m_background, true);

		const float explicitLevelW = uiXml.ReadAttribFlt("minimap", 0, "level_frame_width", 0.0f);
		const float explicitLevelH = uiXml.ReadAttribFlt("minimap", 0, "level_frame_height", 0.0f);
		const bool useExplicitLevelFrame = (explicitLevelW > 0.0f && explicitLevelH > 0.0f);

		SAnchorData& bgAnchor = m_background.GetAnchorData();
		if (bgAnchor.useAnchors)
		{
			SAnchorData& clipAnchor = m_clipFrame.GetAnchorData();
			clipAnchor = bgAnchor;
			if (useExplicitLevelFrame)
			{
				const float bgW = uiXml.ReadAttribFlt("minimap", 0, "width", 226.0f);
				const float bgH = uiXml.ReadAttribFlt("minimap", 0, "height", 226.0f);
				const float offsetX = uiXml.ReadAttribFlt("minimap", 0, "offset_x", 0.0f);
				const float offsetY = uiXml.ReadAttribFlt("minimap", 0, "offset_y", 0.0f);
				const float levelOffsetX = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_x", 0.0f);
				const float levelOffsetY = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_y", 0.0f);
				const float padL = (bgW - explicitLevelW) * 0.5f + levelOffsetX;
				const float padT = (bgH - explicitLevelH) * 0.5f + levelOffsetY;
				clipAnchor.offsetMin.x = offsetX + padL;
				clipAnchor.offsetMin.y = offsetY + padT;
				clipAnchor.offsetMax.x = offsetX + padL + explicitLevelW;
				clipAnchor.offsetMax.y = offsetY + padT + explicitLevelH;
			}
			else
			{
				const float levelOffsetX = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_x", 0.0f);
				const float levelOffsetY = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_y", 0.0f);
				clipAnchor.offsetMin.x += insetL + levelOffsetX;
				clipAnchor.offsetMin.y += insetT + levelOffsetY;
				clipAnchor.offsetMax.x -= insetR - levelOffsetX;
				clipAnchor.offsetMax.y -= insetB - levelOffsetY;
			}
			m_clipFrame.SetAlignment(m_background.GetAlignment());
			m_clipFrame.SetScaleMode(m_background.GetScaleMode());
		}
		else
		{
			Fvector2 pos = m_background.GetWndPos();
			Fvector2 size = m_background.GetWndSize();
			if (useExplicitLevelFrame)
			{
				const float levelOffsetX = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_x", 0.0f);
				const float levelOffsetY = uiXml.ReadAttribFlt("minimap", 0, "level_frame_offset_y", 0.0f);
				const float padL = (size.x - explicitLevelW) * 0.5f + levelOffsetX;
				const float padT = (size.y - explicitLevelH) * 0.5f + levelOffsetY;
				m_clipFrame.SetWndPos(Fvector2().set(pos.x + padL, pos.y + padT));
				m_clipFrame.SetWndSize(Fvector2().set(explicitLevelW, explicitLevelH));
			}
			else
			{
				m_clipFrame.SetWndPos(Fvector2().set(pos.x + insetL, pos.y + insetT));
				m_clipFrame.SetWndSize(Fvector2().set(size.x - insetL - insetR, size.y - insetT - insetB));
			}
		}
	}
	else
	{
		xml_init.InitStatic				(uiXml, "minimap:background",	0, &m_background);
		xml_init.InitWindow				(uiXml, "minimap:level_frame",	0, &m_clipFrame);
	}

	xml_init.InitStatic				(uiXml, "minimap:center",		0, &m_center);
	
	if (uiXml.NavigateToNode("minimap:clock_wnd", 0))
		m_clock_wnd						= UIHelper::CreateStatic(uiXml, "minimap:clock_wnd", &m_background);

	if (IsGameTypeSingle() && uiXml.NavigateToNode("minimap:background:dist_text", 0))
	{
		m_pointerDistanceText = UIHelper::CreateStatic(uiXml, "minimap:background:dist_text", &m_background);
	}

	m_activeMap						= new CUIMiniMap();
	m_clipFrame.AttachChild			(m_activeMap);
	m_activeMap->SetAutoDelete		(true);
	m_activeMap->SetScaleMode		(m_clipFrame.GetScaleMode());

	m_activeMap->EnableHeading		(true);

	int roundedAttr = uiXml.ReadAttribInt(useFrameInset ? "minimap" : "minimap:level_frame", 0, "rounded", -1);
	bool isRounded = (roundedAttr >= 0) ? (roundedAttr != 0) : m_background.WndSizeIsProbablyRelative();
	m_activeMap->SetRounded(isRounded);

	legacyMapMode = !uiXml.NavigateToNode("minimap:static_counter"); // St4lker0k765: может есть варианты и получше, 
																				   // но пока это единственное что приходит на ум, увы
	xml_init.InitStatic				(uiXml, "minimap:compass", 0, &m_compass);

	if (!legacyMapMode)
		m_background.AttachChild		(&m_compass);

	m_clipFrame.AttachChild			(&m_center);
	m_center.SetScaleMode			(m_clipFrame.GetScaleMode());

	m_zoneMapRoot.SetWindowName		("zone_map");
	m_zoneMapRoot.Show				(false);
	m_zoneMapRoot.AttachChild		(&m_background);
	m_zoneMapRoot.AttachChild		(&m_clipFrame);

	visible = true;

	Fvector2 temp;
	const float k = UI().get_current_kx();

	if (!m_clipFrame.GetUseAnchors() && m_clipFrame.WndRectIsProbablyRelative())
	{
		temp = m_clipFrame.GetWndSize();
		temp.y *= UI_BASE_HEIGHT * k;
		temp.x = temp.y / k;
		m_clipFrame.SetWndSize(temp);

		temp = m_clipFrame.GetWndPos();
		temp.x *= k;
		m_clipFrame.SetWndPos(temp.mul(UI_BASE_HEIGHT));
	}

	if (!m_background.GetUseAnchors() && m_background.WndSizeIsProbablyRelative())
	{
		m_background.SetHeight(m_background.GetHeight() * UI_BASE_HEIGHT);
		m_background.SetWidth(m_background.GetHeight() * k);

		m_clipFrame.GetWndRect().getcenter(temp);
		m_background.SetWndPos(temp);
	}

    temp = m_clipFrame.GetWndSize();
    m_center.SetWndPos(temp.div(2.0f));

    if (m_compass.WndPosIsProbablyRelative())
    {
        temp = m_compass.GetWndPos();
        temp.mul(m_background.GetWndSize());
        m_compass.SetWndPos(temp);
    }

    if (m_clock_wnd && m_clock_wnd->WndPosIsProbablyRelative())
    {
        temp = m_clock_wnd->GetWndPos();
        temp.mul(m_background.GetWndSize());
        m_clock_wnd->SetWndPos(temp);
    }

	if ( IsGameTypeSingleCompatible() )
	{
		if (legacyMapMode)
			return;

		m_Counter = new CUIStatic();
		xml_init.InitStatic			(uiXml, "minimap:static_counter", 0, m_Counter);
		m_background.AttachChild	(m_Counter);

		m_Counter_text = new CUIStatic();
		xml_init.InitStatic		(uiXml, "minimap:static_counter:text_static", 0, m_Counter_text);
		m_Counter_text->SetText( "" );
		m_Counter->AttachChild		(m_Counter_text);

        if (m_Counter->WndPosIsProbablyRelative())
        {
            temp = m_Counter->GetWndPos();
            temp.mul(m_background.GetWndSize());
            m_Counter->SetWndPos(temp);
        }
	}

}

void CUIZoneMap::Render			()
{
	if ( !visible || disabled )
		return;

	xrCriticalSectionGuard guard(Level().MapManager().UpdateCS);

	m_zoneMapRoot.Draw();
}

void CUIZoneMap::Update()
{
	if (disabled)
	{
		return;
	}

	CObject* obj = Level().CurrentViewEntity();
	CActor* pActor = obj != nullptr ? obj->cast_actor() : nullptr;
	if (pActor == nullptr)
	{
		return;
	}

	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	bool renderHUD = noHUDonMaster ? g_SingleGameDifficulty < egdStalker : true;
	if (!(Device.dwFrame % 20) && IsGameTypeSingleCompatible() && renderHUD)
	{
		string16	text_str;
		xr_strcpy(text_str, sizeof(text_str), "");

		CPda* pda = pActor->GetPDA();
		if (pda)
		{
			u32 cn = pda->ActiveContactsNum();
			if (cn > 0)
			{
				xr_sprintf(text_str, sizeof(text_str), "%d", cn);
			}
		}
		if (m_Counter_text)
			m_Counter_text->SetText(text_str);
	}
	if (!renderHUD && m_Counter_text)
		m_Counter_text->SetText("");

	UpdateRadar(Device.vCameraPosition);
	float h, p;
	Device.vCameraDirection.getHP(h, p);
	SetHeading(-h);

	if (m_clock_wnd)
		m_clock_wnd->TextItemControl()->SetText(InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes).c_str());
}

void CUIZoneMap::SetHeading		(float angle)
{
	m_activeMap->SetHeading(angle);
	m_compass.SetHeading(angle);
};

void CUIZoneMap::UpdateRadar		(Fvector pos)
{
	m_clipFrame.Update();
	m_background.Update();

	Fvector2 clipSize = m_clipFrame.GetWndSize();
	m_center.SetWndPos(Fvector2().set(clipSize.x * 0.5f, clipSize.y * 0.5f));

	m_activeMap->SetActivePoint( pos );

	if (m_pointerDistanceText)
	{
		if (m_activeMap->GetPointerDistance() > 0.5f)
		{
			string64 str;
			if (legacyMapMode)
				xr_sprintf(str, "%.1f m.", m_activeMap->GetPointerDistance());
			else
				xr_sprintf(str, "%.0f m", m_activeMap->GetPointerDistance());
			m_pointerDistanceText->SetText(str);
		}
		else
		{
			m_pointerDistanceText->SetText("");
		}
	}
}

bool CUIZoneMap::ZoomIn()
{
	return true;
}

bool CUIZoneMap::ZoomOut()
{
	return true;
}

void CUIZoneMap::SetupCurrentMap()
{
	m_activeMap->Initialize			(Level().name(), "hud\\default");

	m_clipFrame.Update();
	m_background.Update();

	Frect r;
	m_clipFrame.GetAbsoluteRect		(r);	
	m_activeMap->WorkingArea().set	(r);
	
	Fvector2						wnd_size;
	float zoom_factor				= float(m_clipFrame.GetWidth())/100.0f;

	LPCSTR ln						= Level().name().c_str();
	if(	pGameIni->section_exist(ln) )
	{
		if(pGameIni->line_exist(ln, "minimap_zoom"))
			zoom_factor *= pGameIni->r_float(ln, "minimap_zoom");
	}else
	if(g_pGameLevel->pLevel->section_exist("minimap_zoom"))
	{
		zoom_factor *= g_pGameLevel->pLevel->r_float("minimap_zoom", "value");
	}
	wnd_size.x						= m_activeMap->BoundRect().width()*zoom_factor;
	wnd_size.y						= m_activeMap->BoundRect().height()*zoom_factor;
	m_activeMap->SetWndSize			(wnd_size);
}

void CUIZoneMap::OnSectorChanged(int sector)
{
	if(!g_pGameLevel->pLevel->section_exist("sub_level_map") )
		return;
	u8			map_idx = u8(-1);
	string64	s_sector;
	xr_sprintf	(s_sector, "%d", sector);
	
	if(!g_pGameLevel->pLevel->line_exist("sub_level_map", s_sector) )
		return;

	map_idx		= g_pGameLevel->pLevel->r_u8("sub_level_map", s_sector);
	if(m_current_map_idx == map_idx)
		return;

	m_current_map_idx = map_idx;

	string_path sub_texture;
	xr_sprintf(sub_texture,"%s#%d", m_activeMap->m_texture.c_str(), m_current_map_idx);
	
	if(map_idx==u8(-1))
		xr_sprintf(sub_texture,"%s", m_activeMap->m_texture.c_str());

	m_activeMap->InitTextureEx(sub_texture, m_activeMap->m_shader_name.c_str());
}

void CUIZoneMap::Counter_ResetClrAnimation()
{
	if (m_Counter_text)
		m_Counter_text->ResetColorAnimation();
}
