////////////////////////////////////////////////////////////////////////////
//	Module 		: team_base_zone.h
//	Created 	: 27.04.2004
//  Modified 	: 27.04.2004
//	Author		: Dmitriy Iassenev
//	Description : Team base zone object
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "GameObject.h"
#include "../xrEngine/Feel_Touch.h"

class CTeamBaseZone : public CGameObject, public Feel::Touch {
protected:
	u8		m_Team;
public:
	typedef	CGameObject	inherited;

	CTeamBaseZone		();
	virtual ~CTeamBaseZone		() override;
	virtual void	reinit				() override;
	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;

	virtual void	Center				(Fvector &C)	const override;
	virtual float	Radius				() const override;

	virtual void	shedule_Update		(u32 dt) override;
	virtual void	feel_touch_new		(CObject* O) override;
	virtual void	feel_touch_delete	(CObject* O) override;
	virtual bool	feel_touch_contact	(CObject* O) override;

	virtual u8		GetZoneTeam			()	{ return m_Team; };

	virtual CTeamBaseZone* cast_team_base_zone() override { return this; }

#ifdef DEBUG
	virtual void	OnRender			() override;
#endif
};