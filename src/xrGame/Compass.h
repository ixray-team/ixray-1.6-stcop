#pragma once

#include "CustomDetector.h"
#include "ui/ArtefactDetectorUI.h"

class CUICompass;

class CCompass final : public CCustomDetector
{
public:
	void Load(const char* section) override;
	void shedule_Update(u32 dt) override;
	void on_a_hud_attach() override;
	void on_b_hud_detach() override;

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void UpdateAf() override;
	void CreateUI() override;
	CUICompass& ui();
};

class CUICompass final : public CUIArtefactDetectorBase
{
public:
	~CUICompass() override = default;
	void update() override;

	void construct(CCompass* p);
	void SetValue(const float v1, const Fvector& v2) { m_target_dir = v2; }
	float CurrentYRotation() const { return m_cur_y_rot; }
	void ResetBoneCallbacks();
	void SetBoneCallbacks();

private:
	CCompass* m_parent = nullptr;
	Fvector m_target_dir = zero_vel;
	float m_cur_y_rot = 0.0f;
	float m_curr_ang_speed = 0.0f;
	u16 m_bid = BI_NONE;
};