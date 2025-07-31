#pragma once

#include "inventory_item_object.h"
#include "HudSound.h"
#include "../xrScripts/script_export_space.h"

class CLAItem;

class CTorch : public CInventoryItemObject {
private:
    typedef	CInventoryItemObject	inherited;

protected:
	float			fBrightness;
	CLAItem*		lanim;

	u16				guid_bone;
	shared_str		light_trace_bone;

	float			m_delta_h;
	Fvector2		m_prev_hp;
	bool			m_switched_on;
	ref_light		light_render;
	ref_light		light_omni;
	ref_glow		glow_render;
	Fvector			m_focus;
	ref_sound 		m_switch_sound;
private:
	inline	bool	can_use_dynamic_lights	();

public:
					CTorch					();
	virtual			~CTorch					();

	virtual void	Load					(LPCSTR section);
	virtual BOOL	net_Spawn				(CSE_Abstract* DC);
	virtual void	net_Destroy				();
	virtual void	net_Export				(NET_Packet& P);				// export to server
	virtual void	net_Import				(NET_Packet& P);				// import from server

	virtual void	OnH_A_Chield			();
	virtual void	OnH_B_Independent		(bool just_before_destroy);

	virtual void	UpdateCL				();

			void	Switch					();
			void	Switch					(bool light_on);
			bool	torch_active			() const;

	virtual bool	can_be_attached			() const;

			bool	IsSwitched				() const {return m_switched_on;}

	//CAttachableItem
	virtual	void	enable					(bool value);

	HUD_SOUND_COLLECTION	m_sounds;

	enum EStats{
		eTorchActive				= (1<<0),
		eAttached					= (1<<1)
	};

public:

	virtual bool			use_parent_ai_locations	() const
	{
		return				(!H_Parent());
	}
	virtual void	create_physic_shell		();
	virtual void	activate_physic_shell	();
	virtual void	setup_physic_shell		();

	virtual void	afterDetach				();
	virtual void	renderable_Render		();

	virtual CTorch* cast_torch() { return this; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};