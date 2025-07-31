#pragma once

#include "GameObject.h"
#include "../xrEngine/Feel_Touch.h"

class CTeamCaptureZone : 
	public CGameObject, 
	public Feel::Touch 
{
protected:
	u8		m_Team;
	float	m_Capture;
	u8		m_CapturedTeam;
	u8		m_CapturingTeam;
	bool	m_Conflict;
	xr_vector<CObject*> v_actor;

public:
	typedef	CGameObject	inherited;

	CTeamCaptureZone();
	virtual			~CTeamCaptureZone();
	virtual void	reinit();
	virtual BOOL	net_Spawn(CSE_Abstract* DC);
	virtual void	net_Destroy();

	virtual void	Center(Fvector& C)	const;
	virtual float	Radius() const;

	virtual void	shedule_Update(u32 dt);
	void			capture_Update(u32 dt);
	void			update_Capture(u8 team);
	virtual void	feel_touch_new(CObject* O);
	virtual void	feel_touch_delete(CObject* O);
	virtual BOOL	feel_touch_contact(CObject* O);

	virtual u8		GetZoneTeam() { return m_Team; };
};