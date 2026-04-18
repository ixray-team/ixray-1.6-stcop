#pragma once

#include "../xrEngine/Feel_Touch.h"
#include "inventory_item_object.h"

#include "InfoPortionDefs.h"

#include "PdaMsg.h"
#include "IPowerManager.h"

class CInventoryOwner;
class CPda;

using PDA_LIST = xr_vector<CPda*>;
using PDA_LIST_it = PDA_LIST::iterator;

class CPda final :
	public CInventoryItemObject,
	public Feel::Touch,
	public IPowerManager
{
	using inherited = CInventoryItemObject;
public:
	CPda() = default;
	virtual	~CPda() = default;

	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void Load(const char* section) override;
	virtual void net_Destroy() override;

	virtual void OnH_A_Chield() override;
	virtual void OnH_B_Independent(bool just_before_destroy) override;

	virtual void shedule_Update(u32 dt) override;

	virtual void feel_touch_new(CObject* O) override;
	virtual void feel_touch_delete(CObject* O) override;
	virtual bool feel_touch_contact(CObject* O) override;

	virtual u16	GetOriginalOwnerID() { return m_idOriginalOwner; }
	virtual CInventoryOwner* GetOriginalOwner();
	virtual CObject* GetOwnerObject();

	void TurnOn() { m_bTurnedOff = false; }
	void TurnOff() { m_bTurnedOff = true; }

	bool IsActive() { return IsOn(); }
	bool IsOn() { return !m_bTurnedOff; }
	bool IsOff() { return m_bTurnedOff; }

	void ActivePDAContacts(xr_vector<CInventoryOwner*>& res);
	CInventoryOwner* GetOwner(CObject* owner);
	u32	ActiveContactsNum() { return (u32)m_active_contacts.size(); }
	xr_vector<CObject*>	ActiveContacts() { return m_active_contacts; }
	void PlayScriptFunction();
	bool CanPlayScriptFunction() { if (!xr_strcmp(m_functor_str, "")) return false; return true; };


	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;

	virtual CPda* cast_pda() override { return this; }

protected:
	void UpdateActiveContacts();

	xr_vector<CObject*>	m_active_contacts = {};
	float m_fRadius = 0.0f;

	u16	m_idOriginalOwner = u16(-1);
	shared_str m_SpecificChracterOwner = nullptr;
	xr_string m_sFullName;

	bool m_bTurnedOff = true;
	shared_str m_functor_str;
};
