#pragma once

#include "inventory_item_object.h"
#include "HudSound.h"
#include "../xrScripts/script_export_space.h"
#include "IPowerManager.h"

class CLAItem;

class CTorch final : public CInventoryItemObject, public IPowerManager
{
	using inherited = CInventoryItemObject;

protected:
	float fBrightness = 1.0f;
	CLAItem* lanim = nullptr;

	u16 guid_bone = BI_NONE;
	shared_str light_trace_bone;

	float m_delta_h = 0.0f;
	Fvector2 m_prev_hp;
	bool m_switched_on = false;
	ref_light light_render = {};
	ref_light light_omni = {};
	ref_glow glow_render = {};
	Fvector	m_focus = zero_vel;
	ref_sound m_switch_sound = {};

private:
	inline bool can_use_dynamic_lights();

public:
	CTorch();
	virtual	~CTorch();

	virtual void Load(const char* section) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void net_Export(NET_Packet& P) override;				// export to server
	virtual void net_Import(NET_Packet& P) override;				// import from server

	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;

	virtual void OnH_A_Chield() override;
	virtual void OnH_B_Independent(bool just_before_destroy) override;

	virtual void	OnMoveToSlot(const SInvItemPlace& prev);
	virtual void	OnMoveToRuck(const SInvItemPlace& prev);
	virtual void	UpdateCL() override;

	void Switch();
	void Switch(bool light_on);
	bool torch_active() const;

	virtual bool can_be_attached() const override;

	bool IsSwitched() const { return m_switched_on; }

	//CAttachableItem
	virtual	void enable(bool value) override;

	HUD_SOUND_COLLECTION m_sounds;

	enum EStats
	{
		eTorchActive = (1 << 0),
		eAttached = (1 << 1)
	};

public:
	virtual bool use_parent_ai_locations() const override
	{
		return (!H_Parent());
	}
	virtual void create_physic_shell() override;
	virtual void activate_physic_shell() override;
	virtual void setup_physic_shell() override;

	virtual void afterDetach() override;
	virtual void renderable_Render() override;

	virtual CTorch* cast_torch() { return this; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};