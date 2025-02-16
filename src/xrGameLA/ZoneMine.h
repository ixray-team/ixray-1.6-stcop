/*
Gr1ph00n
30/06/12
*/
#pragma once

#include "customzone.h"
#include "script_export_space.h"

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
add_to_type_list(CZoneMine)
#undef script_type_list
#define script_type_list save_type_list(CZoneMine)