#include "stdafx.h"
#include "../xrCDB/ISpatial.h"
#include "ICollidable.h"
#include "xr_collide_form.h"

ICollidable::ICollidable()
{
	collidable.model = nullptr;
}

ICollidable::~ICollidable()
{
	xr_delete(collidable.model);
}