#include "StdAfx.h"
#include "UIArtefactPanel.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UIXmlInit.h"

#include "../artefact.h"

using namespace InventoryUtilities;

bool g_artefacts_on_hud = true;

CUIArtefactPanel::CUIArtefactPanel()
{		
}

CUIArtefactPanel::~CUIArtefactPanel()
{
}

void CUIArtefactPanel::InitFromXML	(CUIXml& xml, LPCSTR path, int index)
{
	CUIXmlInit::InitWindow		(xml, path, index, this);
	m_cell_size.x				= xml.ReadAttribFlt(path, index, "cell_width");
	m_cell_size.y				= xml.ReadAttribFlt(path, index, "cell_height");
	m_fScale					= xml.ReadAttribFlt(path, index, "scale");
}

void CUIArtefactPanel::InitIcons(const xr_vector<const CArtefact*>& artefacts)
{
	m_si.SetShader(GetEquipmentIconsShader());
	m_vRects.clear();
	
	for(xr_vector<const CArtefact*>::const_iterator it = artefacts.begin();
		it != artefacts.end(); it++)
	{
		const CArtefact* artefact = *it;
		Frect rect;
		float scaleIcon = READ_IF_EXISTS(pSettings, r_float, artefact->cNameSect(), "inv_scale", 1.0f);
		rect.left = float(artefact->GetInvGridRect().x1 *INV_GRID_WIDTH(scaleIcon));
		rect.top = float(artefact->GetInvGridRect().y1 *INV_GRID_HEIGHT(scaleIcon));
		rect.right = rect.left + artefact->GetInvGridRect().x2 *INV_GRID_WIDTH(scaleIcon);
		rect.bottom = rect.top + artefact->GetInvGridRect().y2 *INV_GRID_HEIGHT(scaleIcon);
		m_vRects.push_back(rect);
	}
}

void CUIArtefactPanel::Draw()
{
	if (!g_artefacts_on_hud)
		return;

	const float iIndent = 1.0f;
	      float x = 0.0f;
		  float y = 0.0f;
		  float iHeight;
		  float iWidth;

	Frect				rect;
	GetAbsoluteRect		(rect);
	x					= rect.left;
	y					= rect.top;	
	
	float _s			= m_cell_size.x/m_cell_size.y;

	for (ITr it = m_vRects.begin(); it != m_vRects.end(); ++it)
	{
		const Frect& r = *it;		

		iHeight = m_fScale*(r.bottom - r.top);
		iWidth  = _s*m_fScale*(r.right - r.left);

		m_si.SetTextureRect(r);
		m_si.SetSize(Fvector2().set(iWidth, iHeight));

		m_si.SetPos(x, y);
		x = x + iIndent + iWidth;

        m_si.Render();
	}

	CUIWindow::Draw();
}
