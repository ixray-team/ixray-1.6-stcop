#pragma once

#include "hud_item_object.h"
#include "../xrSound/ai_sounds.h"

class CUIArtefactDetectorBase;

class CCustomDetectorR : public CHudItemObject
{
	typedef	CHudItemObject	inherited;
protected:
	CUIArtefactDetectorBase*			m_ui;
	bool			m_bFastAnimMode;
	bool			m_bNeedActivation;
public:
	CCustomDetectorR();
	virtual			~CCustomDetectorR();

	virtual BOOL 	net_Spawn(CSE_Abstract* DC);
	virtual void 	Load(LPCSTR section);

	virtual void 	OnH_A_Chield();
	virtual void 	OnH_B_Independent(bool just_before_destroy);

	virtual void 	shedule_Update(u32 dt);
	virtual void 	UpdateCL();


	bool 	IsWorking();

	virtual void	OnActiveItem();
	virtual void	OnHiddenItem();
	virtual void	OnStateSwitch(u32 S);
	virtual void	OnAnimationEnd(u32 state);
	virtual	void	UpdateXForm();
			void	OnMoveToSlot() override {};
			void	OnMoveToRuck() override {};

	// TODO: do we really need this "fast mode"?
	void			ToggleDetector(bool bFastMode);
	void			HideDetector(bool bFastMode, bool needToReactivate = false);
	void			HideDetectorInstantly(bool needToReactivate = false);
	void			ShowDetector(bool bFastMode);

	float							fdetect_radius;
	float							foverallrangetocheck;
	float							fortestrangetocheck;
	BOOL							for_test;
	BOOL							reaction_sound_off;
	xr_vector<shared_str>			af_types;
	LPCSTR	 						af_sounds_section;
	LPCSTR							closestart;
	LPCSTR							detector_section;
protected:
	void 			TurnDetectorInternal(bool b);

	void			UpdateVisibility();
	void			SwitchToBolt();
	bool			IsItemStateCompatible(CHudItem*);

	virtual void	UpdateWork();
	virtual void 	UpdateAf()				{};
	virtual void 	CreateUI()				{};

	bool			m_bWorking;
public:
	float							freqq;
	float							snd_timetime;
	float							cur_periodperiod;
	u8								feel_touch_update_delay;
	LPCSTR				detect_sndsnd_line;

	ref_sound			detect_snd;

	virtual bool			install_upgrade_impl(LPCSTR section, bool test);

};