#pragma once

#include "customzone.h"
#include "../xrScripts/script_export_space.h"

class CMosquitoBald : public CCustomZone
{
private:
	typedef	CCustomZone	inherited;
public:
	CMosquitoBald(void);
	virtual ~CMosquitoBald(void);

	virtual void Load(LPCSTR section);
	virtual void Postprocess(f32 val);
	virtual bool EnableEffector() {return true;}


	virtual void Affect(SZoneObjectInfo* O);

protected:
	virtual bool BlowoutState();
			bool ShouldIgnoreObject(CGameObject*) override;

	//для того чтобы blowout обновился один раз
	//после того как зона перключилась в другое состояние
	bool m_bLastBlowoutUpdate;
	bool m_killCarEngine;
	float m_hitKoefCar;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
