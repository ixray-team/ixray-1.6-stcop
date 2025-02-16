#pragma once

#include "inventory_item_object.h"
//#include "night_vision_effector.h"
#include "hudsound.h"
#include "script_export_space.h"

class CLAItem;
class CNightVisionEffector;
class CMonsterEffector;

class CTorch : public CInventoryItemObject {
private:
    typedef	CInventoryItemObject	inherited;

protected:
	float			fBrightness;
	CLAItem*		lanim;
	float			time2hide;

	u16				guid_bone;
	shared_str		light_trace_bone;

	float			m_delta_h;
	Fvector2		m_prev_hp;
	bool			m_switched_on;
	ref_light		light_render;
	ref_light		light_omni;
	ref_glow		glow_render;
	Fvector			m_focus;
	// lost alpha start
	u32				m_battery_duration;
	u16 			m_current_battery_state;
	float			m_battery_state; //tatarinrafa:no AlifeItem stuff..
	float			fUnchanreRate;
	bool			m_actor_item;
private:
	inline	bool	can_use_dynamic_lights	();

public:
					CTorch					();
	virtual			~CTorch					();

	virtual void	Load				(LPCSTR section);
	virtual BOOL	net_Spawn			(CSE_Abstract* DC);
	virtual void	net_Destroy			();
	virtual void	net_Export			(NET_Packet& P);				// export to server
	virtual void	net_Import			(NET_Packet& P);				// import from server

	virtual void	OnH_A_Chield		();
	virtual void	OnH_B_Independent	(bool just_before_destroy);

	virtual void	UpdateCL			();
			
			void	UpdateBattery		(void);
			void	Switch				();
			void	Switch				(bool light_on);
			void	Broke				();

			void	Recharge(void);
			u32		GetBatteryStatus(void) const;
			void	SetBatteryStatus(u32 val);
			bool	IsSwitchedOn(void) const; 
			u32		GetBatteryLifetime(void) const;

	virtual bool	can_be_attached		() const;
 
public:
			void	SwitchNightVision		();
			void	SwitchNightVision		(bool light_on, bool use_sounds=true);

			bool	GetNightVisionStatus	() const { return m_bNightVisionOn; }
	CInventoryItem * CurrentNightVisionItem;
CNightVisionEffector* GetNightVision		()  { return m_night_vision; }
protected:
	bool					m_bNightVisionEnabled;
	bool					m_bNightVisionOn;

	CNightVisionEffector*	m_night_vision;
	HUD_SOUND_COLLECTION	m_sounds;
	ref_sound				sndBreaking;

	HUD_SOUND_ITEM				m_FlashlightSwitchSnd;

	float					m_RangeMax;
	// float					m_RangeMin;
	float					m_RangeCurve;

	enum EStats{
		eTorchActive				= (1<<0),
		eNightVisionActive			= (1<<1),
		eAttached					= (1<<2)
	};

	u8						Settings_from_ltx;

			void	LoadLightSettings	(CInifile* ini, LPCSTR section);

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
	virtual void	save					(NET_Packet &output_packet);
	virtual void	load					(IReader &input_packet);
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

class CNightVisionEffector
{
	CActor*					m_pActor;
	HUD_SOUND_COLLECTION	m_sounds;
public:
	enum EPlaySounds{
		eStartSound	= 0,
		eStopSound,
		eIdleSound,
		eBrokeSound
	};
				CNightVisionEffector(const shared_str& sect);
	void		Start		(const shared_str& sect, CActor* pA, bool play_sound=true);
	void		Stop		(const float factor, bool play_sound=true);
	bool		IsActive	();
	void		OnDisabled	(CActor* pA, bool play_sound=true);
	void		PlaySounds	(EPlaySounds which);
};

add_to_type_list(CTorch)
#undef script_type_list
#define script_type_list save_type_list(CTorch)
