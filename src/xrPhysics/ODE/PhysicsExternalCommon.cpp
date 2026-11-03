#include "StdAfx.h"
#include "PhysicsExternalCommon.h"
#include "ExtendedGeom.h"
#include "MathUtilsOde.h"

void PHCallContactCallback(ContactCallbackFun* Callback, CDB::TRI* T, const dContactGeom* c)
{
	if (Callback == nullptr)
	{
		return;
	}

	SPHShotMarkContact Contact;
	Contact.Position.set(cast_fv(c->pos));
	Contact.Normal.set(cast_fv(c->normal));

	dBodyID b = dGeomGetBody(c->g1);
	dxGeomUserData* data = nullptr;
	if (!b)
	{
		b = dGeomGetBody(c->g2);
		data = dGeomGetUserData(c->g2);
		Contact.InvertNormal = true;
	}
	else
	{
		data = dGeomGetUserData(c->g1);
	}

	if (b)
	{
		dVector3 vel;
		dMass m;
		dBodyGetMass(b, &m);
		dBodyGetPointVel(b, c->pos[0], c->pos[1], c->pos[2], vel);
		Contact.VelCret = std::abs(dDOT(vel, c->normal)) * _sqrt(m.mass);
		Contact.HasEffect = true;
	}

	if (data)
	{
		Contact.HasUserData = true;
		Contact.Object = data->ph_ref_object;
		Contact.Material = data->material;
		Contact.IsCharacter = data->ph_object && data->ph_object->CastType() == CPHObject::tpCharacter;
	}

	Callback(T, Contact);
}
