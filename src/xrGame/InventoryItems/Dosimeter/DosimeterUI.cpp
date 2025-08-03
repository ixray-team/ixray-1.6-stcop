#include "stdafx.h"
#include "DosimeterUI.h"

#include "../../../xrUI/ui_base.h"
#include "../../../xrUI/UIXmlInit.h"
#include "../../../xrUI/Widgets/UIStatic.h"

#include "../../HUDManager.h"
#include "../../Level.h"
#include "../../player_hud.h"
#include "../../ui/UIHudStatesWnd.h"
#include "../../ui/UIMainIngameWnd.h"
#include "../../Actor.h"
#include "../../RadioactiveZone.h"

void CUIDosimeter::construct(CDosimeter* p)
{
	m_parent = p;
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "ui_dosimeter.xml");

	CUIXmlInit::InitWindow(uiXml, "dosimeter", 0, this);

	m_wrk_area = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:wrk_area", 0, m_wrk_area);
	m_wrk_area->SetAutoDelete(true);
	AttachChild(m_wrk_area);

	m_seg1 = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:seg1", 0, m_seg1);
	m_seg1->SetAutoDelete(true);
	AttachChild(m_seg1);

	m_seg2 = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:seg2", 0, m_seg2);
	m_seg2->SetAutoDelete(true);
	AttachChild(m_seg2);

	m_seg3 = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:seg3", 0, m_seg3);
	m_seg3->SetAutoDelete(true);
	AttachChild(m_seg3);

	m_seg4 = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:seg4", 0, m_seg4);
	m_seg4->SetAutoDelete(true);
	AttachChild(m_seg4);

	m_workIndicator = new CUIStatic;
	CUIXmlInit::InitStatic(uiXml, "dosimeter:work", 0, m_workIndicator);
	m_workIndicator->SetAutoDelete(true);
	AttachChild(m_workIndicator);

	Fvector _map_attach_p = pSettings->r_fvector3(m_parent->cNameSect(), "ui_p");
	Fvector _map_attach_r = pSettings->r_fvector3(m_parent->cNameSect(), "ui_r");

	_map_attach_r.mul(PI / 180.f);
	m_map_attach_offset.setHPB(_map_attach_r.x, _map_attach_r.y, _map_attach_r.z);
	m_map_attach_offset.translate_over(_map_attach_p);

	m_workTick = 0;
	m_noiseTick = 0;
	m_noise = 0.0f;
}

void CUIDosimeter::update()
{
	CUIArtefactDetectorBase::update();

	CActor* pActor = smart_cast<CActor*>(Level().CurrentControlEntity());

	float rad = 0.f;
	if (pActor)
	{
		for (CObject* pFeelObject : pActor->q_nearest)
		{
			CRadioactiveZone* pRadZone = smart_cast<CRadioactiveZone*>(pFeelObject);

			if (pRadZone)
				rad += pRadZone->fHitPower;
		}
	}

	rad *= 1000;
	rad += m_noise;

	if (rad > 150)
	{
		rad = 150;
	}

	string16 s;
	sprintf_s(s, "%05.0lf", rad);
	string16 tex;
	sprintf_s(tex, "green_%c", s[1]);
	m_seg1->InitTextureEx(tex, "hud\\p3d");
	sprintf_s(tex, "green_%c", s[2]);
	m_seg2->InitTextureEx(tex, "hud\\p3d");
	sprintf_s(tex, "green_%c", s[3]);
	m_seg3->InitTextureEx(tex, "hud\\p3d");
	sprintf_s(tex, "green_%c", s[4]);
	m_seg4->InitTextureEx(tex, "hud\\p3d");

	if (Device.dwTimeGlobal > m_workTick + WORK_PERIOD)
	{
		m_workIndicator->Show(!m_workIndicator->IsShown());
		m_workTick = Device.dwTimeGlobal;
	}

	if (Device.dwTimeGlobal > m_noiseTick + NOISE_PERIOD)
	{
		m_noise = 3 * Random.randF();
		m_noiseTick = Device.dwTimeGlobal;
	}

	CUIWindow::Update();
}

void CUIDosimeter::Draw()
{
	Fmatrix LM;
	GetUILocatorMatrix(LM);

	IUIRender::ePointType bk = UI().m_currentPointType;

	UI().m_currentPointType = IUIRender::pttLIT;

	UIRender->CacheSetXformWorld(LM);
	UIRender->CacheSetCullMode(IUIRender::cmNONE);

	CUIWindow::Draw();

	UI().m_currentPointType = bk;
}

void CUIDosimeter::GetUILocatorMatrix(Fmatrix& _m)
{
	Fmatrix trans = m_parent->HudItemData()->m_item_transform;
	u16 bid = m_parent->HudItemData()->m_model->LL_BoneID("cover");
	Fmatrix cover_bone = m_parent->HudItemData()->m_model->LL_GetTransform(bid);
	_m.mul(trans, cover_bone);
	_m.mulB_43(m_map_attach_offset);
}