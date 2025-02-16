#pragma once
#include "customdetector2.h"
#include "../feel_touch.h"

class CUIArtefactDetectorElite;

class CEliteDetector :	public CCustomDetectorR,
						public Feel::Touch
{
	typedef CCustomDetectorR	inherited;
public:
	CEliteDetector();
	virtual			~CEliteDetector();
	virtual void	render_item_3d_ui();
	virtual bool	render_item_3d_ui_query();
	virtual LPCSTR	ui_xml_tag() const { return "elite"; }


	virtual void			feel_touch_new(CObject* O);
	virtual void			feel_touch_delete(CObject* O);
	virtual BOOL			feel_touch_contact(CObject* O);
	//virtual BOOL			feel_touch_on_contact		(CObject* O);

	float					DetectorFeel(CObject* object);
protected:
	virtual void 	UpdateAf();
	virtual void 	CreateUI();
	CUIArtefactDetectorElite& ui();
};
