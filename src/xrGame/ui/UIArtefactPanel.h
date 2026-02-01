#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
class CUIXml;
class CArtefact;

class CUIArtefactPanel : public CUIWindow
{
	typedef xr_vector<Frect>::const_iterator ITr;
	typedef xr_vector<CUI3dStatic*>::const_iterator ITsi;

public:
	CUIArtefactPanel			();
	~CUIArtefactPanel			();

	virtual void InitIcons		(const xr_vector<const CArtefact*>& artefacts);
	virtual void Draw			();
			void InitFromXML	(CUIXml& xml, LPCSTR path, int index);

protected:
	float						m_fScale;
	Fvector2					m_cell_size;
	xr_vector<Frect>            m_vRects;
	CUI3dStatic					m_static;
};