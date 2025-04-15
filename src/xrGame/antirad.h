///////////////////////////////////////////////////////////////
// Antirad.h
// Antirad - таблетки выводящие радиацию
///////////////////////////////////////////////////////////////


#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

class CAntirad: public CEatableItemObject {

public:
				 CAntirad			();
	virtual		 ~CAntirad			();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
