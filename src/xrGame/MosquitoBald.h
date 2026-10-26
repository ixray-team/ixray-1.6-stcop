#pragma once

#include "AnomalyZone.h"

XREFLECT(LuaExport)
class CMosquitoBald : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	XPROPERTY(LuaConstructor)
	CMosquitoBald();
	virtual ~CMosquitoBald() override;

	virtual void Load(const char* section) override;
	virtual void Affect(SZoneObjectInfo* O) override;

	virtual CMosquitoBald* cast_mosquito_bald_zone() override { return this; }

protected:
	virtual bool BlowoutState() override;
	virtual void UpdateSecondaryHit() override;
	//для того чтобы blowout обновился один раз
	//после того как зона перключилась в другое состояние
	bool m_bLastBlowoutUpdate;
};
