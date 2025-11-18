///////////////////////////////////////////////////////////////
// StalkerOutfit.cpp
// StalkerOutfit - защитный костюм сталкера
///////////////////////////////////////////////////////////////

#pragma once

#include "CustomOutfit.h"
#include "../xrScripts/script_export_space.h"

class CStalkerOutfit final : public CCustomOutfit
{
public:
	CStalkerOutfit() = default;
	virtual ~CStalkerOutfit() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};