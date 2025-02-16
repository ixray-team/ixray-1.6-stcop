#pragma once
#include "customdetector2.h"
#include "../feel_touch.h"

class CUIArtefactDetectorSimple;

class CSimpleDetector :	public CCustomDetectorR,
						public Feel::Touch
{
	typedef CCustomDetectorR	inherited;
public:
					CSimpleDetector				();
	virtual			~CSimpleDetector			();

	virtual void			feel_touch_new				(CObject* O);
	virtual void			feel_touch_delete			(CObject* O);
	virtual BOOL			feel_touch_contact			(CObject* O);
	//virtual BOOL			feel_touch_on_contact		(CObject* O);

	float					DetectorFeel(CObject* object);
	
protected:
//.	virtual void 	UpdateZones					();
	virtual void 	UpdateAf					();
	virtual void 	CreateUI					();
	CUIArtefactDetectorSimple&	ui				();
};
