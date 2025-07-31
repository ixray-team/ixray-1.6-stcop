
#include "stdafx.h"
#include "team_capture_zone.h"
#include "xrserver_objects_alife_monsters.h"
#include "hit.h"
#include "Actor.h"
#include "level.h"
#include "xrserver.h"
#include "game_cl_base.h"
#include "map_manager.h"
#include "map_location.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrengine/xr_collide_form.h"
#ifdef DEBUG
#	include "debug_renderer.h"
#endif

CTeamCaptureZone::CTeamCaptureZone()
{
	
}

CTeamCaptureZone::~CTeamCaptureZone()
{
}

void CTeamCaptureZone::reinit()
{
	inherited::reinit();
}

void CTeamCaptureZone::Center(Fvector& C) const
{
	XFORM().transform_tiny(C, CFORM()->getSphere().P);
}

float CTeamCaptureZone::Radius() const
{
	return						(CFORM()->getRadius());
}

BOOL CTeamCaptureZone::net_Spawn(CSE_Abstract* DC)
{
	CCF_Shape* l_pShape = new CCF_Shape (this);
	collidable.model = l_pShape;
#if 0
	CSE_Abstract* l_tpAbstract = (CSE_Abstract*)(DC);
	CSE_ALifeTeamCaptureZone* l_tpALifeScriptZone = smart_cast<CSE_ALifeTeamCaptureZone*>(l_tpAbstract);
	R_ASSERT(l_tpALifeScriptZone);

	feel_touch.clear();

	for (u32 i = 0; i < l_tpALifeScriptZone->shapes.size(); ++i) {
		CSE_Shape::shape_def& S = l_tpALifeScriptZone->shapes[i];
		switch (S.type) {
		case 0: {
			l_pShape->add_sphere(S.data.sphere);
			break;
		}
		case 1: {
			l_pShape->add_box(S.data.box);
			break;
		}
		}
	}

	m_Team = l_tpALifeScriptZone->m_team;

	BOOL						bOk = inherited::net_Spawn(DC);
	if (bOk) {
		l_pShape->ComputeBounds();
		Fvector					P;
		XFORM().transform_tiny(P, CFORM()->getSphere().P);
		setEnabled(TRUE);
	}

	if (GameID() != eGameIDSingle && !g_dedicated_server)
	{
		char BaseMapLocation[1024];
		xr_sprintf(BaseMapLocation, "mp_team_base_%d_location", m_Team);
		(Level().MapManager().AddMapLocation(BaseMapLocation, ID()))->EnablePointer();

	};

	return (bOk);
#else
#	pragma todo(FX to Vodka: NET Online code)
	return false;
#endif
}

void CTeamCaptureZone::net_Destroy()
{
	if (!g_dedicated_server)
		Level().MapManager().OnObjectDestroyNotify(ID());

	inherited::net_Destroy();
};

void CTeamCaptureZone::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	const Fsphere& s = CFORM()->getSphere();
	Fvector						P;
	XFORM().transform_tiny(P, s.P);
	feel_touch_update(P, s.R);

	capture_Update(dt);
}

void CTeamCaptureZone::capture_Update(u32 dt) {
	if (v_actor.empty()) {
		if (m_Capture > 0.0f)
		{
			m_Capture = fmaxf(m_Capture - dt * 0.0001f, 0.0f);
		}
		else if (m_Capture < 0.0f) {
			m_Capture = fminf(m_Capture + dt * 0.0001f, 0.0f);
		}
		return;
	}

	m_Conflict = false;

	for (CObject* actor : v_actor)
	{
		game_PlayerState* ps = Game().GetPlayerByGameID(actor->ID());
		if (m_CapturedTeam == -1)
		{
			m_CapturedTeam = ps->team;
		}
		else if (m_CapturedTeam != ps->team)
		{
			m_Conflict = true;
		}
	}

	if (m_Conflict || m_CapturedTeam == -1 || m_CapturedTeam == m_Team)
	{
		return;
	}

	if (m_CapturingTeam == m_CapturedTeam)
	{
		m_Capture += dt * (0.0002f * v_actor.size());
	}
	else
	{
		m_Capture -= dt * 0.0002f;
	}
	m_CapturingTeam = m_CapturedTeam;

	if (m_Capture >= 1.0f)
	{
		update_Capture(m_CapturedTeam);
	}
}

void CTeamCaptureZone::update_Capture(u8 team)
{
	/*
	m_Team = m_CapturedTeam;
	m_Capture = 0.0f;
	m_CapturingTeam = -1;
	
	NET_Packet P;
	u_EventGen(P, GE_GAME_EVENT, ID());
	P.w_u16(GAME_EVENT_TEAM_POINT_CAPTURED);
	P.w_u8(m_Team);
	u_EventSend(P, net_flags(TRUE, TRUE));
	*/
}

void CTeamCaptureZone::feel_touch_new(CObject* tpObject)
{
	if (OnServer() && smart_cast<CActor*>(tpObject))
	{
		v_actor.push_back(tpObject);
	};
}

void CTeamCaptureZone::feel_touch_delete(CObject* tpObject)
{
	if (OnServer() && smart_cast<CActor*>(tpObject))
	{
		
		if (!v_actor.empty())
		{
			size_t i = 0;
			for (CObject* actor : v_actor)
			{
				if (actor->ID() == tpObject->ID())
				{
					v_actor.erase(v_actor.begin() + i);
				}
				i++;
			}
		}

	};
}

BOOL CTeamCaptureZone::feel_touch_contact(CObject* O)
{
	CActor* pActor = smart_cast<CActor*>(O);
	if (!pActor) return (FALSE);
	return ((CCF_Shape*)CFORM())->Contact(O);
}

