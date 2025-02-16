#pragma once
#include "CustomDetector2.h"
#include "../feel_touch.h"

class CUIArtefactDetectorAdv;

class CAdvancedDetector :public CCustomDetectorR,
						 public Feel::Touch
{
	typedef CCustomDetectorR	inherited;

public:
					CAdvancedDetector();
	virtual			~CAdvancedDetector();

	virtual void			feel_touch_new(CObject* O);
	virtual void			feel_touch_delete(CObject* O);
	virtual BOOL			feel_touch_contact(CObject* O);

	virtual void			on_a_hud_attach();
	//virtual void			on_b_hud_detach();

	float					DetectorFeel(CObject* object);

protected:
	virtual void 	UpdateAf();
	virtual void 	CreateUI();
	CUIArtefactDetectorAdv&	ui();

};
