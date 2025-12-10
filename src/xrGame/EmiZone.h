#pragma once
#include "AnomalyZone.h"
#include "../xrScripts/script_export_space.h"

class CEmiZone :
	public CAnomalyZone
{
private:
	using inherited = CAnomalyZone;

public:
	void	Load(LPCSTR section) override;

	void	enter_Zone(SZoneObjectInfo& io) override;
	void	exit_Zone(SZoneObjectInfo& io) override;

	DECLARE_SCRIPT_REGISTER_FUNCTION

};

