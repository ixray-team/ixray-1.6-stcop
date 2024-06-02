#pragma once
#include "CustomDetector.h"
#include "Level.h"

class CUIArtefactDetectorElite;

class CEliteDetector :public CCustomDetector
{
	typedef CCustomDetector	inherited;
public:
					CEliteDetector				();
	virtual			~CEliteDetector				();
	virtual void 	Load						(LPCSTR section);
	virtual void	render_item_3d_ui			();
	virtual bool	render_item_3d_ui_query		();
	virtual LPCSTR	ui_xml_tag					() const {return m_ui_xml_tag;}
protected:
	virtual void 	UpdateAf					();
	virtual void 	CreateUI					();
	CUIArtefactDetectorElite& ui				();
	LPCSTR			m_ui_xml_tag;
};



class CScientificDetector :public CEliteDetector
{
	typedef CEliteDetector	inherited;
public:
					CScientificDetector			();
	virtual			~CScientificDetector		();
	virtual void 	Load						(LPCSTR section);
	virtual void 	OnH_B_Independent			(bool just_before_destroy);
	virtual void 	shedule_Update				(u32 dt);
protected:
	virtual void	UpfateWork					();
	CZoneList		m_zones;
};

