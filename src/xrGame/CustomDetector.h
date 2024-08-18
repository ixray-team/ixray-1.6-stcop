#pragma once
#include "inventory_item_object.h"
#include "hudsound.h"
#include "CustomZone.h"
#include "Artefact.h"
#include "../xrSound/ai_sounds.h"

#include "CustomDetectorZones.h"

class CUIArtefactDetectorBase;

class CCustomDetector :		public CHudItemObject
{
	typedef	CHudItemObject	inherited;
protected:
	CUIArtefactDetectorBase*			m_ui;
	bool			m_bFastAnimMode;
	bool			m_bNeedActivation;
	u32				m_old_state;
public:
					CCustomDetector		();
	virtual			~CCustomDetector	();

	virtual BOOL 	net_Spawn			(CSE_Abstract* DC);
	virtual void 	Load				(LPCSTR section);

	virtual void 	OnH_A_Chield		();
	virtual void 	OnH_B_Independent	(bool just_before_destroy);

	virtual void 	shedule_Update		(u32 dt);
	virtual void 	UpdateCL			();

			void	switch_detector		();
			bool 	IsWorking			();
	virtual bool	need_renderable		();
	virtual void 	OnMoveToSlot		(const SInvItemPlace& prev);
	virtual void 	OnMoveToRuck		(const SInvItemPlace& prev);
			void	ShowingCallback		(CBlend*B);
	virtual void	OnActiveItem		();
	virtual void	OnHiddenItem		();
	virtual void	OnStateSwitch		(u32 S);
	virtual void	OnAnimationEnd		(u32 state);
	virtual	void	UpdateXForm			();
	virtual void	UpdateHudAdditonal	(Fmatrix& trans);
	void			ToggleDetector		(bool bFastMode);
	void			HideDetector		(bool bFastMode);
	void			ShowDetector		(bool bFastMode);
	float			m_fAfDetectRadius;
	virtual bool	CheckCompatibility	(CHudItem* itm);

	virtual u32		ef_detector_type	() const	{return 1;};

	virtual bool	NeedActivation		() const	{return m_bNeedActivation;}

	virtual bool	can_be_attached		() const;
	void 	TurnDetectorInternal(bool b);
	void	ForceHide					() { SwitchState(eHiding); }

	virtual void	PlayAnimIdle();

protected:
			bool	CheckCompatibilityInt		(CHudItem* itm, u16* slot_to_activate);
	void 			UpdateNightVisionMode		(bool b_off);
	void			UpdateVisibility			();
	virtual void	UpfateWork					();
	virtual void 	UpdateAf					()				{}
	virtual void 	CreateUI					()				{}
	void SetHideDetStateInWeapon() const;

	bool			m_bWorking;
	float			m_fAfVisRadius;

	CAfList			m_artefacts;
};