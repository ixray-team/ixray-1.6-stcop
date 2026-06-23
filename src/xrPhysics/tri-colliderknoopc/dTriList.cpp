#include "StdAfx.h"

#include "dTriCollideK.h"
#include "dxTriList.h"
#include "dcTriListCollider.h"
#include "../ExtendedGeom.h"
#include "dSortTriPrimitive.h"


dcTriListCollider::dcTriListCollider(dxGeom* Geometry)
{
	this->Geometry = Geometry;
	GeomData = (dxTriList*)dGeomGetClassData(Geometry);
}

dcTriListCollider::~dcTriListCollider()
{
}

int dCollideBP(const dxGeom* o1, const dxGeom* o2, int flags, dContactGeom* contact, int skip); // ODE internal function


//#define CONTACT(Ptr, Stride) ((dContactGeom*) (((byte*)Ptr) + (Stride)))
//#define SURFACE(Ptr, Stride) ((dSurfaceParameters*) (((byte*)Ptr) + (Stride-sizeof(dSurfaceParameters))))

int dcTriListCollider::CollideBox(dxGeom* Box, int Flags, dContactGeom* Contacts, int Stride)
{
	Fvector AABB;
	dVector3 BoxSides;
	dGeomBoxGetLengths(Box, BoxSides);
	dReal* R = const_cast<dReal*>(dGeomGetRotation(Box));
	AABB.x = (dFabs(BoxSides[0]*R[0]) + dFabs(BoxSides[1]*R[1]) + dFabs(BoxSides[2]*R[2])) / 2.f + 10.f * EPS_L;
	AABB.y = (dFabs(BoxSides[0]*R[4]) + dFabs(BoxSides[1]*R[5]) + dFabs(BoxSides[2]*R[6])) / 2.f + 10.f * EPS_L;
	AABB.z = (dFabs(BoxSides[0]*R[8]) + dFabs(BoxSides[1]*R[9]) + dFabs(BoxSides[2]*R[10])) / 2.f + 10.f * EPS_L;
	dBodyID box_body = dGeomGetBody(Box);
	if (box_body)
	{
		const dReal* velocity = dBodyGetLinearVel(box_body);
		AABB.x += dFabs(velocity[0]) * 0.04f;
		AABB.y += dFabs(velocity[1]) * 0.04f;
		AABB.z += dFabs(velocity[2]) * 0.04f;
	}


	BoxTri bt(*this);
	return dSortTriPrimitiveCollide(bt,
	                                Box,
	                                Geometry,
	                                Flags,
	                                Contacts,
	                                Stride,
	                                AABB
	);
}


int dcTriListCollider::CollideCylinder(dxGeom* Cylinder, int Flags, dContactGeom* Contacts, int Stride)
{
	Fvector AABB;
	dReal CylinderRadius, CylinderLength;


	dGeomCylinderGetParams(Cylinder, &CylinderRadius, &CylinderLength);

	dReal* R = const_cast<dReal*>(dGeomGetRotation(Cylinder));

	AABB.x = REAL(0.5) * dFabs(R[1] * CylinderLength) + (_sqrt(R[0] * R[0] + R[2] * R[2]) * CylinderRadius);

	AABB.y = REAL(0.5) * dFabs(R[5] * CylinderLength) + (_sqrt(R[4] * R[4] + R[6] * R[6]) * CylinderRadius);

	AABB.z = REAL(0.5) * dFabs(R[9] * CylinderLength) + (_sqrt(R[8] * R[8] + R[10] * R[10]) * CylinderRadius);

	const dReal* velocity = dBodyGetLinearVel(dGeomGetBody(Cylinder));
	AABB.x += dFabs(velocity[0]) * 0.04f;
	AABB.y += dFabs(velocity[1]) * 0.04f;
	AABB.z += dFabs(velocity[2]) * 0.04f;

	CylTri ct(*this);
	return dSortTriPrimitiveCollide(
		ct,
		Cylinder,
		Geometry,
		Flags,
		Contacts,
		Stride,
		AABB
	);
}


////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
int dcTriListCollider::CollideSphere(dxGeom* Sphere, int Flags, dContactGeom* Contacts, int Stride)
{
	const float SphereRadius = dGeomSphereGetRadius(Sphere);
	Fvector AABB;


	// Make AABB 
	AABB.x = SphereRadius;
	AABB.y = SphereRadius;
	AABB.z = SphereRadius;

	const dReal* velocity = dBodyGetLinearVel(dGeomGetBody(Sphere));
	AABB.x += dFabs(velocity[0]) * 0.04f;
	AABB.y += dFabs(velocity[1]) * 0.04f;
	AABB.z += dFabs(velocity[2]) * 0.04f;
	SphereTri st(*this);
	return dSortTriPrimitiveCollide(st,
	                                Sphere,
	                                Geometry,
	                                Flags,
	                                Contacts,
	                                Stride,
	                                AABB);
}


int dTriListClass = -1;

dcTriListCollider* GetData(dxGeom* TriList)
{
	dxTriList* Data = (dxTriList*)dGeomGetClassData(TriList);

	return Data->Collider;
}


inline bool ValidateCollision(dxGeom* o1, dxGeom* o2)
{
	return dGeomGetUserData(o1)->b_static_colide;
	/*
	dxBody* b1 = dGeomGetBody(o1);

	dxBody* b2 = dGeomGetBody(o2);



	if (b1){

		if (!dBodyIsEnabled(b1)){

			b1 = 0;

		}

	}

	if (b2){

		if (!dBodyIsEnabled(b2)){

			b2 = 0;

		}

	}

	return b1 || b2;
	*/
	//return true;
}


int dCollideSTL(dxGeom* TriList, dxGeom* Sphere, int Flags, dContactGeom* Contact, int Stride) throw()
{
	if (ValidateCollision(Sphere, TriList))
	{
		return GetData(TriList)->CollideSphere(Sphere, Flags, Contact, Stride);
	}

	else
		return 0;
}


int dCollideBTL(dxGeom* TriList, dxGeom* Box, int Flags, dContactGeom* Contact, int Stride) throw()
{
	if (ValidateCollision(Box, TriList))
	{
		return GetData(TriList)->CollideBox(Box, Flags, Contact, Stride);
	}

	else
		return 0;
}

int dCollideCTL(dxGeom* TriList, dxGeom* Cyl, int Flags, dContactGeom* Contact, int Stride) throw()
{
	if (ValidateCollision(Cyl, TriList))
	{
		return GetData(TriList)->CollideCylinder(Cyl, Flags, Contact, Stride);
	}

	else
		return 0;
}


dColliderFn* dTriListColliderFn(int num)
{
	//	Log("in dTriListColliderFn ");
	//	Msg("num=%d",num);
	if (num == dBoxClass)
	{
		return (dColliderFn*)&dCollideBTL;
	}
	if (num == dSphereClass)
	{
		return (dColliderFn*)&dCollideSTL;
	}

	if (num == dCylinderClassUser)
		return (dColliderFn*)&dCollideCTL;

	return 0;
}

int dAABBTestTL(dxGeom* TriList, dxGeom* Object, dReal AABB[6]) throw()
{
	return 1;
}

void dDestroyTriList(dGeomID g)
{
	xr_delete(((dxTriList*)dGeomGetClassData(g))->Collider);
}


/* External functions */

void dGeomTriListSetCallback(dGeomID g, dTriCallback* Callback)
{
	dxTriList* Data = (dxTriList*)dGeomGetClassData(g);

	Data->Callback = Callback;
}


dTriCallback* dGeomTriListGetCallback(dGeomID g)
{
	dxTriList* Data = (dxTriList*)dGeomGetClassData(g);

	return Data->Callback;
}


void dGeomTriListSetArrayCallback(dGeomID g, dTriArrayCallback* ArrayCallback)
{
	dxTriList* Data = (dxTriList*)dGeomGetClassData(g);

	Data->ArrayCallback = ArrayCallback;
}


dTriArrayCallback* dGeomTriListGetArrayCallback(dGeomID g)
{
	dxTriList* Data = (dxTriList*)dGeomGetClassData(g);

	return Data->ArrayCallback;
}


dxGeom* dCreateTriList(dSpaceID space, dTriCallback* Callback, dTriArrayCallback* ArrayCallback)
{
	if (dTriListClass == -1)
	{
		dGeomClass c;

		c.bytes = sizeof(dxTriList);

		c.collider = &dTriListColliderFn;

		c.aabb = &dInfiniteAABB;

		c.aabb_test = &dAABBTestTL;

		//	c.aabb_test=nullptr;
		c.dtor = &dDestroyTriList;


		dTriListClass = dCreateGeomClass(&c);
	}


	dxGeom* g = dCreateGeom(dTriListClass);

	if (space)
		dSpaceAdd(space, g);


	dxTriList* Data = (dxTriList*)dGeomGetClassData(g);

	Data->Callback = Callback;

	Data->ArrayCallback = ArrayCallback;

	Data->Collider = new dcTriListCollider(g);


	return g;
}