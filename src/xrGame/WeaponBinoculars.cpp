#include "StdAfx.h"
#include "WeaponBinoculars.h"

#include "../xrEngine/xr_level_controller.h"

#include "Level.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "WeaponBinocularsVision.h"
#include "object_broker.h"
#include "Inventory.h"

void CWeaponBinoculars::Load(const char* section)
{
	inherited::Load(section);

	// Sounds
	m_bVision = !!pSettings->r_bool(section, "vision_present");

	if (m_bVision)
	{
		if (TBinocularsVision* Vision = GetOrCreateComponent<TBinocularsVision>())
		{
			Vision->Load(section);
		}
	}

	m_flags.set(FUsingCondition, pSettings->read_if_exists<bool>(section, "use_condition", false));
}

bool CWeaponBinoculars::Action(u16 cmd, u32 flags)
{
	switch (cmd)
	{
	case kWPN_FIRE:
		return inherited::Action(kWPN_ZOOM, flags);
	}

	return inherited::Action(cmd, flags);
}

void CWeaponBinoculars::LoadSounds(const char* section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_zoomin", "sndZoomIn", false, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_zoomout", "sndZoomOut", false, SOUND_TYPE_ITEM_USING);
}

void GetZoomData(const float scope_factor, float& delta, float& min_zoom_factor)
{
	float def_fov = float(g_fov);
	float min_zoom_k = 0.3f;
	float zoom_step_count = 3.0f;
	float delta_factor_total = def_fov - scope_factor;
	VERIFY(delta_factor_total > 0);
	min_zoom_factor = def_fov - delta_factor_total * min_zoom_k;
	delta = (delta_factor_total * (1 - min_zoom_k)) / zoom_step_count;
}

static float LastBinocZoomFactor = 0.0f;

void CWeaponBinoculars::OnZoomIn()
{
	if (H_Parent() && !IsZoomed())
	{
		m_sounds.StopSound("sndZoomOut");
		bool b_hud_mode = (Level().CurrentEntity() == H_Parent());
		m_sounds.PlaySound("sndZoomIn", H_Parent()->Position(), H_Parent(), b_hud_mode);
	}

	inherited::OnZoomIn();

	if (LastBinocZoomFactor)
	{
		m_fRTZoomFactor = LastBinocZoomFactor;
	}
	else
	{
		m_fRTZoomFactor = CurrentZoomFactor();
	}

	float delta, min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);
	clamp(m_fRTZoomFactor, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
	SetZoomFactor(m_fRTZoomFactor);
}

void CWeaponBinoculars::OnZoomOut()
{
	if (H_Parent() && IsZoomed() && !IsRotatingToZoom())
	{
		m_sounds.StopSound("sndZoomIn");
		bool b_hud_mode = (Level().CurrentEntity() == H_Parent());
		m_sounds.PlaySound("sndZoomOut", H_Parent()->Position(), H_Parent(), b_hud_mode);
	}

	inherited::OnZoomOut();
}

bool CWeaponBinoculars::net_Spawn(CSE_Abstract* DC)
{
	inherited::net_Spawn(DC);
	return					true;
}

void CWeaponBinoculars::net_Destroy()
{
	inherited::net_Destroy();
	DestroyComponent<TBinocularsVision>();
}

extern u32 hud_adj_mode;
void CWeaponBinoculars::UpdateCL()
{
	inherited::UpdateCL();

	if (AllowBore())
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : NULL;
		if (pActor && !pActor->AnyMove() && this == pActor->inventory().ActiveItem())
		{
			if (hud_adj_mode == 0 && GetState() == eIdle && (Device.dwTimeGlobal - m_dw_curr_substate_time > 20000))
			{
				SwitchState(eBore);
				ResetSubStateTime();
			}
		}
	}

	//manage visible entities here...
	if (m_bVision && H_Parent() && IsZoomed() && !IsRotatingToZoom())
	{
		if (TBinocularsVision* Vision = GetComponent<TBinocularsVision>())
		{
			Vision->Update();
		}
	}
}

bool CWeaponBinoculars::render_item_ui_query()
{
	bool b_is_active_item = m_pInventory && m_pInventory->ActiveItem() == this;
	return b_is_active_item && H_Parent() && IsZoomed() && !IsRotatingToZoom();
}

void CWeaponBinoculars::render_item_ui()
{
	if (m_bVision)
	{
		if (TBinocularsVision* Vision = GetComponent<TBinocularsVision>())
		{
			Vision->Draw();
		}
	}

	inherited::render_item_ui();
}

void CWeaponBinoculars::ZoomInc()
{
	float delta, min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);

	float f = GetZoomFactor() - delta;
	clamp(f, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
	SetZoomFactor(f);
	LastBinocZoomFactor = f;
}

void CWeaponBinoculars::ZoomDec()
{
	float delta, min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);

	float f = GetZoomFactor() + delta;
	clamp(f, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
	SetZoomFactor(f);
	LastBinocZoomFactor = f;
}

void CWeaponBinoculars::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	save_data(m_fRTZoomFactor, output_packet);
}

void CWeaponBinoculars::load(IReader& input_packet)
{
	inherited::load(input_packet);
	load_data(m_fRTZoomFactor, input_packet);
}

void CWeaponBinoculars::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CWeaponBinoculars")
	{
		inherited::Serialize(Object);
		Object << m_fRTZoomFactor;
	}
}

bool CWeaponBinoculars::GetBriefInfo(II_BriefInfo& info)
{
	info.clear();
	info.name._set(m_nameShort);
	info.icon._set(cNameSect());
	return true;
}

void CWeaponBinoculars::net_Relcase(CObject* object)
{
	if (TBinocularsVision* Vision = GetComponent<TBinocularsVision>())
	{
		Vision->remove_links(object);
	}
}