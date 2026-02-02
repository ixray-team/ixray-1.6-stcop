#include "StdAfx.h"
#include "UIArtefactPanel.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UIXmlInit.h"

#include "../Artefact.h"
#include "Actor_Flags.h"

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
	m_static.SetShader(GetEquipmentIconsShader());
	m_vRects.clear();

	for(const CArtefact* af : artefacts)
	{
        InventoryIconParams icons_struct = GetInventoryIconParams(af->cNameSect().c_str());
        if (psActorFlags.test(AF_3D_ICONS_INV))
        {
            GetStatic()->SetVisual(icons_struct._3d_static_visual);
            GetStatic()->SetXYZ(icons_struct._3d_static_rotate);
            GetStatic()->SetScaleFactor(icons_struct._3d_static_scale);
        }
        else
            GetStatic()->SetVisual(nullptr);

		Frect rect;
		float scaleIcon = icons_struct.scaleIcon;
		rect.left = float(af->GetInvGridRect().x1 *INV_GRID_WIDTH(scaleIcon));
		rect.top = float(af->GetInvGridRect().y1 *INV_GRID_HEIGHT(scaleIcon));
		rect.right = rect.left + af->GetInvGridRect().x2 *INV_GRID_WIDTH(scaleIcon);
		rect.bottom = rect.top + af->GetInvGridRect().y2 *INV_GRID_HEIGHT(scaleIcon);
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

		m_static.SetTextureRect(r);
		m_static.SetWndSize(Fvector2().set(iWidth, iHeight));

		m_static.SetWndPos(Fvector2().set(x, y));
		x = x + iIndent + iWidth;	
		m_static.SetStretchTexture(true);
		m_static.Draw();
	}

	CUIWindow::Draw();
}
