/*
Gr1ph00n
30/06/12
*/
#pragma once

#include "customzone.h"
#include "../xrScripts/script_export_space.h"

class CZoneMine : public CCustomZone
{
private:
	typedef	CCustomZone	inherited;
public:
	CZoneMine(void);
	virtual ~CZoneMine(void);

	virtual void Load(LPCSTR section);
	virtual void Postprocess(f32 val);
	virtual bool EnableEffector() {return true;}


	virtual void Affect(SZoneObjectInfo* O);

protected:
	virtual bool BlowoutState();
	//для того чтобы blowout обновился один раз
	//после того как зона перключилась в другое состояние
	bool m_bLastBlowoutUpdate;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
