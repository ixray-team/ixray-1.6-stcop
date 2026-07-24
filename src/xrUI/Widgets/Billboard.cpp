#include "stdafx.h"
#include "Billboard.h"
//#include "ui_base.h"
#include "UIXmlInit.h"

CBillboard::CBillboard(const char* xml_node)
{
	// TODO Emmis: ��� �� ������ ���� ���, �� �� ���� ���� ��������� �����
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "billboards.xml");
	CUIXmlInit::InitStatic(uiXml, xml_node, 0, this);

	m_offset.set(GetWndPos().x, GetWndPos().y, 0.0f);
}

CBillboard::~CBillboard()
{
}

void CBillboard::DrawBillboard(const Fvector& world_pos)
{
	m_currentWorldPosition = world_pos;

	Draw();
}

void CBillboard::DrawTexture()
{
	if (!m_bTextureEnable || !GetShader() || !GetShader()->inited())
	{
		return;
	}

	VERIFY(g_bRendering);
	UIRender->SetShader(*GetShader());
	UIRender->StartPrimitive(4, IUIRender::ptTriStrip, IUIRender::pttLIT);

	Fvector2 ts;
	UIRender->GetActiveTextureResolution(ts);

	const Fvector& T = Device.vCameraTop;
	const Fvector& R = Device.vCameraRight;

	Fvector pos = m_currentWorldPosition;
	pos.add(m_offset);

	float w = GetWndSize().x * 0.1f;
	float h = GetWndSize().y * 0.1f;

	Fvector corners[4];

	corners[0].x = -R.x * w + T.x * h;
	corners[0].y = -R.y * w + T.y * h;
	corners[0].z = -R.z * w + T.z * h;
	corners[0].add(pos);

	corners[1].x = R.x * w + T.x * h;
	corners[1].y = R.y * w + T.y * h;
	corners[1].z = R.z * w + T.z * h;
	corners[1].add(pos);

	corners[2].x = -R.x * w - T.x * h;
	corners[2].y = -R.y * w - T.y * h;
	corners[2].z = -R.z * w - T.z * h;
	corners[2].add(pos);

	corners[3].x = R.x * w - T.x * h;
	corners[3].y = R.y * w - T.y * h;
	corners[3].z = R.z * w - T.z * h;
	corners[3].add(pos);

	Frect textureRect = GetTextureRect();
	u32 textureColor = GetTextureColor();

	float u1 = textureRect.x1 / ts.x;
	float v1 = textureRect.y1 / ts.y;
	float u2 = textureRect.x2 / ts.x;
	float v2 = textureRect.y2 / ts.y;

	UIRender->PushPoint(corners[0].x, corners[0].y, corners[0].z, textureColor, u1, v1); // LT
	UIRender->PushPoint(corners[1].x, corners[1].y, corners[1].z, textureColor, u2, v1); // RT
	UIRender->PushPoint(corners[2].x, corners[2].y, corners[2].z, textureColor, u1, v2); // LB
	UIRender->PushPoint(corners[3].x, corners[3].y, corners[3].z, textureColor, u2, v2); // RB

	UIRender->CacheSetXformWorld(Fidentity);
	UIRender->FlushPrimitive();
}

void CBillboard::DrawWidgetText()
{
	if (!m_bTextEnable)
	{
		return;
	}

	Fvector3 pos = m_currentWorldPosition;
	pos.add(m_offset);

	Fvector4 v_res;
	Device.mFullTransform.transform(v_res, pos);

	if (v_res.z < 0 || v_res.w < 0)
	{
		return;
	}
	if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y > 1.f)
	{
		return;
	}

	float x = (1.f + v_res.x) / 2.f * (Device.TargetWidth);
	float y = (1.f - v_res.y) / 2.f * (Device.TargetHeight);

	if (m_pTextControl)
	{
		if (!fsimilar(m_pTextControl->m_wndSize.x, m_wndSize.x) || !fsimilar(m_pTextControl->m_wndSize.y, m_wndSize.y))
		{
			m_pTextControl->m_wndSize = m_wndSize;
			m_pTextControl->ParseText(true);
		}

		if (IsHighlightText() && xr_strlen(TextItemControl()->GetText()) > 0 && m_bEnableTextHighlighting)
		{
			DrawHighlightedText(); // TODO Emmis: �� �����������
		}
		else
		{
			TextItemControl()->DrawWS(x, y); // TODO Emmis: DrawWS(x, y) ��������� ������� ��� ������� ������ � 3�(�� ����), �� �������� ����������� ��������
		}
	}
}