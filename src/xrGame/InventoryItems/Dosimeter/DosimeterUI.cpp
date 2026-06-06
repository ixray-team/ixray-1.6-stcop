#include "StdAfx.h"
#include "DosimeterUI.h"

#include "../../../xrUI/ui_base.h"
#include "../../../xrUI/UIXmlInit.h"
#include "../../../xrUI/Widgets/UIStatic.h"

#include "../../player_hud.h"
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
}

void CUIDosimeter::update()
{
	CUIArtefactDetectorBase::update();

	CObject* control_entity = Level().CurrentControlEntity();
	CActor* pActor = control_entity != nullptr ? control_entity->cast_actor() : nullptr;

	float rad = 0.0f;
	if (pActor)
	{
		for (ISpatialShared& SS : pActor->q_nearest)
		{
			ISpatial* S = SS.get();

			if (!S)
				continue;

			CObject* pFeelObject = S->dcast_CObject();

			if (!pFeelObject || pFeelObject->getDestroy())
				continue;

			CGameObject* game_object = pFeelObject->cast_game_object();

			if (game_object == nullptr)
				continue;

			CRadioactiveZone* pRadZone = game_object->cast_radioactive_zone();

			if (pRadZone == nullptr)
				continue;

			rad += pRadZone->fHitPower;
		}
	}

	rad *= 1000.0f;
	rad += m_noise;

	if (rad > 150.0f)
	{
		rad = 150.0f;
	}

	string16 s;
	xr_sprintf(s, "%05.0lf", rad);
	string16 tex;
	xr_sprintf(tex, "green_%c", s[1]);
	m_seg1->InitTextureEx(tex, "hud\\dosimeter");
	xr_sprintf(tex, "green_%c", s[2]);
	m_seg2->InitTextureEx(tex, "hud\\dosimeter");
	xr_sprintf(tex, "green_%c", s[3]);
	m_seg3->InitTextureEx(tex, "hud\\dosimeter");
	xr_sprintf(tex, "green_%c", s[4]);
	m_seg4->InitTextureEx(tex, "hud\\dosimeter");

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
	UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);

	CUIWindow::Draw();

	UI().m_currentPointType = bk;
}

void CUIDosimeter::GetUILocatorMatrix(Fmatrix& _m)
{
	attachable_hud_item* hid = m_parent->HudItemData();
	IKinematics* kin = hid->m_model;
	Fmatrix trans = hid->m_item_transform;
	u16 bid = kin->LL_BoneID("cover");
	Fmatrix cover_bone = kin->LL_GetTransform(bid);
	_m.mul(trans, cover_bone);
	_m.mulB_43(m_map_attach_offset);
}