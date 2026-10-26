#include "StdAfx.h"
#include "PHContact.h"
#include "ExtendedGeom.h"
#include "PHCharacter.h"
#include "CalculateTriangle.h"

static dxGeomUserData* GetUserData(const dContact& Contact, bool First)
{
	return retrieveGeomUserData(First ? Contact.geom.g1 : Contact.geom.g2);
}

CPHContact::CPHContact(dContact& Contact) :
	Contact(Contact)
{
}

bool CPHContact::HasUserData(bool First) const
{
	return GetUserData(Contact, First) != nullptr;
}

IPhysicsShellHolder* CPHContact::GetObject(bool First) const
{
	dxGeomUserData* Data = GetUserData(Contact, First);
	return Data ? Data->ph_ref_object : nullptr;
}

void* CPHContact::GetCallbackData(bool First) const
{
	dxGeomUserData* Data = GetUserData(Contact, First);
	return Data ? Data->callback_data : nullptr;
}

IPHCharacter* CPHContact::GetCharacter(bool First) const
{
	dxGeomUserData* Data = GetUserData(Contact, First);
	if (Data == nullptr || Data->ph_object == nullptr || Data->ph_object->CastType() != CPHObject::tpCharacter)
	{
		return nullptr;
	}

	return static_cast<CPHCharacter*>(Data->ph_object);
}

bool CPHContact::HasObjectCallback(bool First, ObjectContactCallbackFun* Callback) const
{
	dxGeomUserData* Data = GetUserData(Contact, First);
	return Data && Data->object_callbacks && Data->object_callbacks->HasCallback(Callback);
}

bool CPHContact::GetLastPosition(bool First, Fvector& Position) const
{
	dxGeomUserData* Data = GetUserData(Contact, First);
	if (Data == nullptr || Data->last_pos[0] == -dInfinity)
	{
		return false;
	}

	Position.set(cast_fv(Data->last_pos));
	return true;
}

bool CPHContact::GetPushingNegTriangle(bool First, Fvector& Normal, float& Dist) const
{
	dGeomID Geom = First ? Contact.geom.g1 : Contact.geom.g2;
	dxGeomUserData* Data = retrieveGeomUserData(Geom);
	if (Data == nullptr || !Data->pushing_neg)
	{
		return false;
	}

	Triangle NegTri;
	CalculateTriangle(Data->neg_tri, Geom, NegTri);
	Normal.set(cast_fv(NegTri.norm));
	Dist = NegTri.dist;
	return true;
}

const Fvector& CPHContact::GetPosition() const
{
	return cast_fv(Contact.geom.pos);
}

const Fvector& CPHContact::GetNormal() const
{
	return cast_fv(Contact.geom.normal);
}

float CPHContact::GetDepth() const
{
	return Contact.geom.depth;
}

bool CPHContact::IsCylinder() const
{
#ifdef DEBUG
	return IsCyliderContact(Contact);
#else
	return false;
#endif
}

float CPHContact::GetFriction() const
{
	return Contact.surface.mu;
}

void CPHContact::SetFriction(float Mu)
{
	Contact.surface.mu = Mu;
}

void CPHContact::SetBounce(float Bounce, float BounceVel)
{
	Contact.surface.bounce = Bounce;
	Contact.surface.bounce_vel = BounceVel;
	Contact.surface.mode |= dContactBounce;
}

void CPHContact::ScaleSpringDamping(float Spring, float Damping)
{
	// HACK: matches the original car wheel code, which passes soft_cfm as both cfm and erp.
	MulSprDmp(Contact.surface.soft_cfm, Contact.surface.soft_cfm, Spring, Damping);
}
