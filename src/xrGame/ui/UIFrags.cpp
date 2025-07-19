#include "StdAfx.h"

#include "UIFrags.h"
#include "UIStats.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"

CUIFrags::CUIFrags(){	
	m_pBackT = new CUIStatic(); AttachChild(m_pBackT);
	m_pBackC = new CUIStatic(); AttachChild(m_pBackC);
	m_pBackB = new CUIStatic(); AttachChild(m_pBackB);
	m_pStats = new CUIStats();  AttachChild(m_pStats);
}

CUIFrags::~CUIFrags()
{
//	xr_delete(m_pStats);
	xr_delete(m_pBackT);
	xr_delete(m_pBackC);
	xr_delete(m_pBackB);
}

void CUIFrags::Init(CUIXml& xml_doc, LPCSTR path, LPCSTR backgrnd_path){
	m_pStats->InitStats(xml_doc, path, 0);
	InitBackground(xml_doc, backgrnd_path);	
}

void CUIFrags::InitBackground(CUIXml& xml_doc, LPCSTR path){
	string256 _path;
	CUIXmlInit::InitWindow(xml_doc, path, 0, this);
	CUIXmlInit::InitStatic(xml_doc, xr_strconcat(_path, path, ":back_c"), 0, m_pBackC);
	int count = xml_doc.ReadAttribInt(_path, 0, "count", 1);
	for (int i = 1; i < count; ++i)
	{
		CUIStatic* newStatic = new CUIStatic();
		AttachChild(newStatic);
		newStatic->SetAutoDelete(true);
		CUIXmlInit::InitStatic(xml_doc, xr_strconcat(_path, path, ":back_c"), 0, newStatic);
		Fvector2 pos = newStatic->GetWndPos();
		pos.y += (newStatic->GetHeight()*i);
		newStatic->SetWndPos(pos);
		//m_pBackC->GetStaticItem()->SetTile(1, count, 0, 0);
	}
	CUIXmlInit::InitStatic(xml_doc, xr_strconcat(_path, path, ":back_t"), 0, m_pBackT);
	CUIXmlInit::InitStatic(xml_doc, xr_strconcat(_path, path, ":back_b"), 0, m_pBackB);

}