#pragma once
#include "../xrScripts/script_export_space.h"
#include "inventory_item_object.h"


// ADD(CInteractiveObject	,CSE_ALifeInteractiveObject	,CLSID_OBJECT_INTERACT	,"obj_interact");
// CLSID_OBJECT_INTERACT	MK_CLSID('I','N','T','E','R','A','C','T')

class CInteractiveObject :
	public CGameObject
{
	using inherited = CGameObject;


public:
	bool m_can_take = true;
	shared_str m_tip_text;
	shared_str m_tip_text_default;
	xr_vector<xr_string> m_bone_names;
	xr_string m_spawn_section;
	xr_vector<ref_sound> m_use_sounds;
	u8 left_uses = 0;

	CInteractiveObject();
	~CInteractiveObject();

	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual void net_Relcase(CObject* O) override;

	virtual void UpdateCL() override;
	virtual void net_Destroy() override;
	virtual BOOL net_Spawn(CSE_Abstract* DC) override;

	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;

	virtual BOOL net_SaveRelevant() override  { return TRUE; }

	void SetText();
	void DestroySoundsArray(xr_vector<ref_sound>& soundsArray);
	virtual void Load(LPCSTR section) override;
	void SetVisible(shared_str bone_name, BOOL bVisibility);
	void ParseRandomSounds(LPCSTR section, LPCSTR soundParameter, xr_vector<ref_sound>& soundsArray);
	void ParseBones(LPCSTR section, LPCSTR bonesParameter, xr_vector<xr_string>& _array);
	ICF ref_sound& GetRandomSound(xr_vector<ref_sound>& soundsArray) { return soundsArray[::Random.randI(soundsArray.size())]; }
	void OnUse();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};