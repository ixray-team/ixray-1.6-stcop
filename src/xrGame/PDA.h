#pragma once

#include "../xrEngine/feel_touch.h"
#include "hud_item_object.h"

#include "InfoPortionDefs.h"
#include "character_info_defs.h"

#include "PdaMsg.h"

class CInventoryOwner;
class CPda;

using PDA_LIST = xr_vector<CPda*>;
using PDA_LIST_it = PDA_LIST::iterator;

class CPda :
	public CHudItemObject,
	public Feel::Touch
{
	typedef	CHudItemObject inherited;
public:
											CPda					();
	virtual									~CPda					();

	virtual BOOL 							net_Spawn				(CSE_Abstract* DC);
	virtual void 							Load					(LPCSTR section);
	virtual void 							net_Destroy				();


	virtual void 							OnH_A_Chield			();
	virtual void 							OnH_B_Independent		(bool just_before_destroy);

	virtual void 							shedule_Update			(u32 dt);

	virtual void 							feel_touch_new			(CObject* O);
	virtual void 							feel_touch_delete		(CObject* O);
	virtual BOOL 							feel_touch_contact		(CObject* O);


	virtual u16								GetOriginalOwnerID		() {return m_idOriginalOwner;}
	virtual CInventoryOwner*				GetOriginalOwner		();
	virtual CObject*						GetOwnerObject			();

	virtual void							PlayAnimIdle			();
	virtual void							OnStateSwitch			(u32 S) override;
	virtual void							OnAnimationEnd			(u32 state) override;

			void							TurnOn					() {m_bTurnedOff = false;}
			void							TurnOff					() {m_bTurnedOff = true;}
	
			bool 							IsActive				() {return IsOn();}
			bool 							IsOn					() {return !m_bTurnedOff;}
			bool 							IsOff					() {return m_bTurnedOff;}


			void							ActivePDAContacts		(xr_vector<CPda*>& res);
			CPda*							GetPdaFromOwner			(CObject* owner);
			u32								ActiveContactsNum		()							{return (u32)m_active_contacts.size();}
			void							PlayScriptFunction		();
			bool							CanPlayScriptFunction	() {if(!xr_strcmp(m_functor_str, "")) return false; return true;};
	virtual void							UpdateXForm();

	virtual void							OnMoveToRuck			(const SInvItemPlace& prev) override;

	virtual	u8								GetCurrentHudOffsetIdx	();
	virtual void							UpdateHudAdditonal		(Fmatrix&);
	virtual void							UpdateCL				();
	virtual bool							Action					(u16 cmd, u32 flags);

	virtual void							OnHiddenItem			() override;
	virtual void							OnActiveItem			() override;

	virtual void							save					(NET_Packet &output_packet);
	virtual void							load					(IReader &input_packet);

//*	virtual LPCSTR							Name					();
public:
	bool m_bZoomed = false;
	float m_fZoomfactor = 0.f;

protected:
	void									UpdateActiveContacts	();


	xr_vector<CObject*>						m_active_contacts;
	float									m_fRadius;

	u16										m_idOriginalOwner;
	shared_str								m_SpecificChracterOwner;
	xr_string								m_sFullName;

	bool									m_bTurnedOff;
	shared_str								m_functor_str;
};
