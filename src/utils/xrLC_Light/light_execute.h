#pragma once
 
#include "xrDeflectorDefs.h"
#include "base_lighting.h"
#include "../../xrCore/Collision/xrCDB.h"




class light_execute
{

		HASH			H;
		CDB::COLLIDER	DB;
		base_lighting	LightsSelected;
	 public:
		 void run( CDeflector& D );
};

