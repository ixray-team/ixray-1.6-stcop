////////////////////////////////////////////////////////////////////////////
//	Module 		: script_zone.h
//	Created 	: 10.10.2003
//  Modified 	: 10.10.2003
//	Author		: Dmitriy Iassenev
//	Description : Script zone object
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "space_restrictor.h"
#include "../xrEngine/Feel_Touch.h"
#include "../xrScripts/script_space_forward.h"
#include "../xrScripts/script_export_space.h"

class CScriptGameObject;

class CScriptZone : public CSpaceRestrictor, public Feel::Touch {
public:
	typedef	CSpaceRestrictor inherited;

					CScriptZone			();
	virtual ~CScriptZone		() override;
	virtual void	reinit				() override;
	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;
	virtual void	net_Relcase			(CObject* O) override;
	virtual void	shedule_Update		(u32 dt) override;
	virtual void	feel_touch_new		(CObject* O) override;
	virtual void	feel_touch_delete	(CObject* O) override;
	virtual bool	feel_touch_contact	(CObject* O) override;
			bool	active_contact		(ALife::_OBJECT_ID id) const;
	virtual bool	IsVisibleForZones	() override {return false;}
	virtual bool	register_schedule	() const override {return true;}
	virtual CSpaceRestrictor* cast_restrictor() override { return this; }
	virtual CScriptZone* cast_script_zone() override { return this; }
	virtual CGameObject* cast_game_object() override { return this; }

#ifdef DEBUG
	virtual void	OnRender			() override;
#endif
	DECLARE_SCRIPT_REGISTER_FUNCTION
};