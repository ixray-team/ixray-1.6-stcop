////////////////////////////////////////////////////////////////////////////
//	Module 		: level_changer.h
//	Created 	: 10.07.2003
//  Modified 	: 10.07.2003
//	Author		: Dmitriy Iassenev
//	Description : Level change object
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "GameObject.h"
#include "../xrEngine/Feel_Touch.h"
#include "game_graph_space.h"

class CLevelChanger : public CGameObject, public Feel::Touch {
private:
	typedef	CGameObject	inherited;

private:
	GameGraph::_GRAPH_ID	m_game_vertex_id;
	u32						m_level_vertex_id;
	Fvector					m_position;
	Fvector					m_angles;
	float					m_entrance_time;
	shared_str				m_invite_str;
	bool					m_b_enabled;
	bool					condWork;

	void					update_actor_invitation	();
	bool					m_bSilentMode;
	bool					get_reject_pos(Fvector& p, Fvector& r);

public:
	virtual ~CLevelChanger		() override;
	virtual bool		net_Spawn			(CSE_Abstract* DC) override;
	virtual void		net_Destroy			() override;
	virtual void		Center				(Fvector& C) const override;
	virtual float		Radius				() const override;
	virtual void		shedule_Update		(u32 dt) override;
	virtual void		feel_touch_new		(CObject* O) override;
	virtual bool		feel_touch_contact	(CObject* O) override;
	GameGraph::_GRAPH_ID NextVertexId() const { return m_game_vertex_id; }
	virtual bool		IsVisibleForZones() override { return false;}
	void				EnableLevelChanger	(bool b)				{m_b_enabled=b;}
	bool				IsLevelChangerEnabled() const				{return m_b_enabled;}
	void				SetLEvelChangerInvitationStr(const char* str)	{m_invite_str = str;}
	//serialization
	virtual bool net_SaveRelevant() override;
	virtual void save(NET_Packet &output_packet) override;
	virtual void load(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	virtual CGameObject* cast_game_object() override { return this; }
	virtual CLevelChanger* cast_level_changer() override { return this; }

#ifdef DEBUG_DRAW
	virtual void		OnRender() override;
#endif
};
