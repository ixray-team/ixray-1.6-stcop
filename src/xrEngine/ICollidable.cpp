#include "stdafx.h"
#include "../xrCore/Collision/ISpatial.h"
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