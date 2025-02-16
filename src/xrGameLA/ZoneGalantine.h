#pragma once

#include "mincer.h"
#include "../xrScripts/script_export_space.h"

class CZoneGalantine : public CMincer
{
private:
	typedef CMincer inherited;
public:
						CZoneGalantine						();
	virtual				~CZoneGalantine						();
	virtual BOOL		net_Spawn							(CSE_Abstract* DC);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
