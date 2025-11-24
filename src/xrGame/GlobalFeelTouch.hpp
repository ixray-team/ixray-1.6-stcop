#pragma once
#include "../xrEngine/Feel_Touch.h"

class GlobalFeelTouch final:
	public Feel::Touch
{
public:
							GlobalFeelTouch();
	virtual					~GlobalFeelTouch();

	virtual void			feel_touch_update			(Fvector& P, float	R);

			bool			is_object_denied			(CObject const * O);
};