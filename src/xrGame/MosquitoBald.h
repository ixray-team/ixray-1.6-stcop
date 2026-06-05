#pragma once

#include "AnomalyZone.h"
#include "../xrScripts/script_export_space.h"

class CMosquitoBald : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	CMosquitoBald();
	virtual ~CMosquitoBald();

	virtual void Load(const char* section);
	virtual void Affect(SZoneObjectInfo* O);

	CMosquitoBald* cast_mosquito_bald_zone() override { return this; }

protected:
	virtual bool BlowoutState();
	virtual void UpdateSecondaryHit();
	//для того чтобы blowout обновился один раз
	//после того как зона перключилась в другое состояние
	bool m_bLastBlowoutUpdate;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
