#include "stdafx.h"
#include "IGame_Level.h"
#include "Feel_Touch.h"
#include "xr_object.h"
using namespace Feel;

Touch::Touch():pure_relcase(&Touch::feel_touch_relcase)
{
}

Touch::~Touch()
{
}

BOOL Touch::feel_touch_contact(CObject* O)
{ 
	return TRUE; 
}

void Touch::feel_touch_deny(CObject* O, DWORD T)
{
	feel_touch_disable.push_back({ O, Device.dwTimeGlobal + T });
}

void Touch::feel_touch_update(Fvector& C, float R)
{
	// Check if denied objects expire in time
	DWORD dwT = Device.dwTimeGlobal;
	feel_touch_disable.erase(std::remove_if(feel_touch_disable.begin(), feel_touch_disable.end(),
			[dwT](const DenyTouch& D) { return D.Expire < dwT; }), feel_touch_disable.end());

	// Find nearest objects
	q_nearest.reserve(feel_touch.size());
	g_SpatialSpace->q_sphere(q_nearest, 0, ESPATIAL_TYPE::COLLIDEABLE | ESPATIAL_TYPE::SHAPE, C, R);

	for (ISpatialShared& S : q_nearest)
	{
		if(!S.get())
			continue;

		CObject* O = S->dcast_CObject();
		if (!O || O->getDestroy() || !feel_touch_contact(O))
			continue;

		if (std::find(feel_touch.begin(),feel_touch.end(),O) == feel_touch.end())
		{
			// check for deny
			if (std::find_if(feel_touch_disable.begin(), feel_touch_disable.end(),
				[O](const DenyTouch& D) { return D.O == O; }) == feel_touch_disable.end())
			{
				feel_touch.push_back(O);
				feel_touch_new(O);
			}
		}
	}

	// Process results (DELETE)
	feel_touch.erase(std::remove_if(feel_touch.begin(), feel_touch.end(), [this](CObject* O)
		{
			if (O->getDestroy() || !feel_touch_contact(O) || (std::find_if(q_nearest.begin(), q_nearest.end(), [O](const ISpatialShared& S) { return O == S->dcast_CObject(); }) == q_nearest.end()))	// Don't touch candidates for destroy
			{
				feel_touch_delete(O);
				return true;
			}
			return false;
		}), feel_touch.end());

	//. Engine.Sheduler.Slice	();	
}

void Touch::feel_touch_relcase(CObject* O)
{
	xr_vector<CObject*>::iterator I = std::find (feel_touch.begin(),feel_touch.end(),O);
	if (I!=feel_touch.end())
	{
		feel_touch.erase(I);
		feel_touch_delete(O);
	}

	feel_touch_disable.erase(std::remove_if(feel_touch_disable.begin(), feel_touch_disable.end(), [O](const DenyTouch& D)
		{
			if (D.O == O)
				return true;

			return false;
		}), feel_touch_disable.end());
}
