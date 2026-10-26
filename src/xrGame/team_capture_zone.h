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
	virtual ~CTeamCaptureZone() override;
	virtual void	reinit() override;
	virtual bool	net_Spawn(CSE_Abstract* DC) override;
	virtual void	net_Destroy() override;

	virtual void	Center(Fvector& C)	const override;
	virtual float	Radius() const override;

	virtual void	shedule_Update(u32 dt) override;
	void			capture_Update(u32 dt);
	void			update_Capture(u8 team);
	virtual void	feel_touch_new(CObject* O) override;
	virtual void	feel_touch_delete(CObject* O) override;
	virtual bool	feel_touch_contact(CObject* O) override;

	virtual u8		GetZoneTeam() { return m_Team; };
};