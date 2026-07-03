#pragma once
#include "hud_item_object.h"
#include "HudSound.h"
#include "../xrSound/ai_sounds.h"
#include <functional>

typedef std::function<void()> detector_fn_t;

class CCustomDevice : public CHudItemObject
{
	using inherited = CHudItemObject;
public:
	enum EDeviceStates : u8
	{
		eHandHide = eLastBaseState + 1,
		eHandDraw,
		eHandThrowStart,
		eHandThrowIdle,
		eHandThrowEnd,
		eHandKick1,
		eHandKick2,
		eHandLam,
		eHandAimStart,
		eHandAimEnd,
		eHandShoot,
		eHandDry,
		eHandJammed,
		eHandLightMisfire,
		eHandFiremode,
	};
protected:
	bool m_bFastAnimMode = false;
	bool m_bNeedActivation = false;
	bool m_bWorking = false;
	bool m_bIsZoomed = false;

	detector_fn_t hide_callback = nullptr;
public:
	CCustomDevice() = default;
	~CCustomDevice() override;

	bool net_Spawn(CSE_Abstract* DC) override;
	void LoadSounds(const char* section) override;

	void OnH_B_Independent(bool just_before_destroy) override;

	void shedule_Update(u32 dt) override;
	void UpdateCL() override;

	void switch_device();
	bool IsWorking();
	bool need_renderable() final override;
	void OnMoveToRuck(const SInvItemPlace& prev) override;
	void ShowingCallback(CBlend* B);
	void OnStateSwitch(u8 S) override;
	void OnAnimationEnd(u8 state) override;
	void UpdateXForm() override;
	void SwitchState(u8 S) final override;
	void UpdateHudAdditonal(Fmatrix& trans) override;
	void ToggleDetector(bool bFastMode, bool switching = false);
	void HideDetector(bool bFastMode, bool force = false);
	void ShowDetector(bool bFastMode);
	bool CheckCompatibility(CHudItem* itm) override;

	void PlayAnimIdle() final override;
	shared_str SetCurrentAimAnimation();

	void ClearCallback() { hide_callback = nullptr; }
	void HideAndSetCallback(const detector_fn_t fn);

	virtual u32	ef_detector_type() const { return 1; }

	bool NeedActivation() const { return m_bNeedActivation; }

	virtual bool can_be_attached() const;
	void PlayWpnFinishDetector();
	virtual void TurnDetectorInternal(bool b);
	void SwitchZoom();

	bool NeedBlockSprint() const;
	bool CanDrawHand() const;
	bool CanHideHand() const;
	bool CanThrowHand() const;
	bool CanKick() const;
	bool CanLam() const;
	bool CanFiremode() const;
	bool CanShooting(bool dry = false) const;
	bool CanJammed() const;
	bool CanLightMisfire() const;
	bool IsZoomed() const { return m_bIsZoomed; }

	virtual CCustomDevice* cast_custom_device() { return this; }

	virtual bool NeedMovementBlend() const final override;

protected:
	bool CheckCompatibilityInt(CHudItem* itm, u16* slot_to_activate);
	void UpdateVisibility();
	virtual void UpdateWork() {}
};