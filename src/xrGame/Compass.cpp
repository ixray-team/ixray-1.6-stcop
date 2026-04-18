#include "StdAfx.h"
#include "Compass.h"
#include "../Include/xrRender/Kinematics.h"
#include "player_hud.h"

void CCompass::Load(const char* section)
{
	CHudItemObject::Load(section);
}

void CCompass::shedule_Update(u32 dt)
{
	CHudItemObject::shedule_Update(dt);

	if (!IsWorking())
	{
		return;
	}

	Position().set(H_Parent()->Position());
}

void CCompass::UpdateAf()
{
	ui().SetValue(0.0f, Fvector().set(0.0f, 0.0f, 1.0f));
}

void CCompass::CreateUI()
{
	R_ASSERT(nullptr == m_ui);

	m_ui = new CUICompass();
	ui().construct(this);
}

CUICompass& CCompass::ui()
{
	return *((CUICompass*)m_ui);
}

void CCompass::on_a_hud_attach()
{
	CCustomDetector::on_a_hud_attach();
	ui().SetBoneCallbacks();
}

void CCompass::on_b_hud_detach()
{
	CCustomDetector::on_b_hud_detach();
	ui().ResetBoneCallbacks();
}

void CUICompass::construct(CCompass* p)
{
	m_parent = p;
}

void CUICompass::update()
{
	attachable_hud_item* hid = m_parent->HudItemData();
	if (hid == nullptr)
	{
		return;
	}

	CUIArtefactDetectorBase::update();

	IKinematics* kin = hid->m_model;

	bool b_visible = !fis_zero(m_target_dir.magnitude());
	if (b_visible != kin->LL_GetBoneVisible(m_bid))
	{
		kin->LL_SetBoneVisible(m_bid, b_visible, TRUE);
	}

	if (!b_visible)
	{
		return;
	}

	Fvector	dest;
	Fmatrix	Mi;
	Mi.invert(hid->m_item_transform);
	Mi.transform_dir(dest, m_target_dir);

	float dest_y_rot = -dest.getH();

	m_cur_y_rot = angle_inertion_var(m_cur_y_rot, dest_y_rot, PI_DIV_4, PI_MUL_4, PI_MUL_2, Device.fTimeDelta);
}

void CUICompass::SetBoneCallbacks()
{
	attachable_hud_item* itm = m_parent->HudItemData();
	R_ASSERT(itm);

	IKinematics* kin = itm->m_model;

	m_bid = kin->LL_BoneID("arrow_joint");

	CBoneInstance& bi = kin->LL_GetBoneInstance(m_bid);
	bi.set_callback(bctCustom, [](CBoneInstance* B)
	{
		CUICompass* P = static_cast<CUICompass*>(B->callback_param());
		Fmatrix	rY;
		rY.rotateY(P->CurrentYRotation());
		B->mTransform.mulB_43(rY);
	}, this);

	float p = 0.0f, b = 0.0f;
	bi.mTransform.getHPB(m_cur_y_rot, p, b);
}

void CUICompass::ResetBoneCallbacks()
{
	attachable_hud_item* itm = m_parent->HudItemData();
	R_ASSERT(itm);

	IKinematics* kin = itm->m_model;

	u16 bid = kin->LL_BoneID("arrow_joint");

	CBoneInstance& bi = kin->LL_GetBoneInstance(bid);
	bi.reset_callback();
}