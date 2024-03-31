#include "stdafx.h"
#include "Compass.h"
#include "../Include/xrRender/Kinematics.h"
#include "player_hud.h"
#include "game_object_space.h"

void CCompass::Load(LPCSTR section)
{
	CHudItemObject::Load(section);

	m_sounds.LoadSound(section, "snd_draw", "sndShow");
	m_sounds.LoadSound(section, "snd_holster", "sndHide");
}

void CCompass::shedule_Update(u32 dt)
{
	CHudItemObject::shedule_Update(dt);

	if (!IsWorking())			return;

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
	m_target_dir.set(0, 0, 0);
	m_curr_ang_speed = 0.0f;
	m_cur_y_rot = 0.0f;
}

CUICompass::~CUICompass()
{
}

void CUICompass::SetValue(const float val1, const Fvector& val2)
{
	m_target_dir = val2;
}

void CUICompass::update()
{
	if (NULL == m_parent->HudItemData())	return;
	CUIArtefactDetectorBase::update();
	attachable_hud_item* itm = m_parent->HudItemData();
	R_ASSERT(itm);

	BOOL b_visible = !fis_zero(m_target_dir.magnitude());
	if (b_visible != itm->m_model->LL_GetBoneVisible(m_bid))
		itm->m_model->LL_SetBoneVisible(m_bid, b_visible, TRUE);

	if (!b_visible)
		return;

	Fvector							dest;
	Fmatrix							Mi;
	Mi.invert(itm->m_item_transform);
	Mi.transform_dir(dest, m_target_dir);

	float dest_y_rot = -dest.getH();

	m_cur_y_rot = angle_inertion_var(m_cur_y_rot,
		dest_y_rot,
		PI_DIV_4,
		PI_MUL_4,
		PI_MUL_2,
		Device.fTimeDelta);

}

void CUICompass::BoneCallback(CBoneInstance* B)
{
	CUICompass* P = static_cast<CUICompass*>(B->callback_param());
	Fmatrix							rY;
	rY.rotateY(P->CurrentYRotation());
	B->mTransform.mulB_43(rY);
}

void CUICompass::SetBoneCallbacks()
{
	attachable_hud_item* itm = m_parent->HudItemData();
	R_ASSERT(itm);
	m_bid = itm->m_model->LL_BoneID("arrow_joint");

	CBoneInstance& bi = itm->m_model->LL_GetBoneInstance(m_bid);
	bi.set_callback(bctCustom, BoneCallback, this);

	float p, b;
	bi.mTransform.getHPB(m_cur_y_rot, p, b);
}

void CUICompass::ResetBoneCallbacks()
{
	attachable_hud_item* itm = m_parent->HudItemData();
	R_ASSERT(itm);
	u16 bid = itm->m_model->LL_BoneID("arrow_joint");

	CBoneInstance& bi = itm->m_model->LL_GetBoneInstance(bid);
	bi.reset_callback();
}

float CUICompass::CurrentYRotation() const
{
	return m_cur_y_rot;
}