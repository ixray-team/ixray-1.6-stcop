///////////////////////////////////////////////////////////////
// StalkerOutfit.cpp
// StalkerOutfit - защитный костюм сталкера
///////////////////////////////////////////////////////////////


#pragma once

#include "customoutfit.h"
#include "../xrScripts/script_export_space.h"

class CStalkerOutfit : public CCustomOutfit {
private:
    typedef	CCustomOutfit inherited;
public:
	CStalkerOutfit(void);
	virtual ~CStalkerOutfit(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
