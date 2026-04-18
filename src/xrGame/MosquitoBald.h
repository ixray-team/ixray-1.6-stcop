#pragma once

#include "AnomalyZone.h"
#include "../xrScripts/script_export_space.h"

class CMosquitoBald : public CAnomalyZone
{
private:
	typedef	CAnomalyZone	inherited;
public:
					CMosquitoBald				();
	virtual			~CMosquitoBald				();

	virtual void	Load						(const char* section);

	virtual void	Affect						(SZoneObjectInfo* O);

protected:
	virtual bool	BlowoutState				();
	virtual	void	UpdateSecondaryHit			();
	//для того чтобы blowout обновился один раз
	//после того как зона перключилась в другое состояние
	bool			m_bLastBlowoutUpdate;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};