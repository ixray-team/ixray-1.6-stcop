#pragma once

#include "CustomDetector.h"
#include "ui/ArtefactDetectorUI.h"

class CUICompass;

class CCompass : public CCustomDetector
{
public:
	void Load(LPCSTR section) override;
	void shedule_Update(u32 dt) override;
	void on_a_hud_attach() override;
	void on_b_hud_detach() override;

protected:
	void UpdateAf() override;
	void CreateUI() override;
	CUICompass& ui();
};

class CUICompass :public CUIArtefactDetectorBase
{
public:
	~CUICompass() override;
	void update() override;

	void construct(CCompass* p);
	void SetValue(const float v1, const Fvector& v2);
	float CurrentYRotation()	const;
	static void xr_stdcall BoneCallback(CBoneInstance* B);
	void ResetBoneCallbacks();
	void SetBoneCallbacks();

private:
	CCompass* m_parent;
	Fvector m_target_dir;
	float m_cur_y_rot;
	float m_curr_ang_speed;
	u16 m_bid;
};