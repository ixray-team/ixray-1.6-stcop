#pragma once

#include "AnomalyZone.h"

XREFLECT(LuaExport)
class CMosquitoBald : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	XPROPERTY(LuaConstructor)
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
};
