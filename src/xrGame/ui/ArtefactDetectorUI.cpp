#include "StdAfx.h"
#include "ArtefactDetectorUI.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/xrUIXmlParser.h"

void CUIDetectorWave::InitFromXML(CUIXml& xml, const char* path)
{
	CUIXmlInit::InitFrameLine(xml, path, 0, this);
	m_step = xml.ReadAttribFlt(path, 0, "step");
}

void CUIDetectorWave::Update()
{
	Fvector2 P = GetWndPos();

	float dp = m_curr_v * Device.fTimeDelta;

	P.x += dp;

	if (P.x > 0.0f)
	{
		P.x -= m_step;
	}
	else if (P.x < -(2.0f * m_step))
	{
		P.x += m_step;
	}

	SetWndPos(P);
	inherited::Update();
}