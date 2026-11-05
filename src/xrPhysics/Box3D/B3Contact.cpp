#include "StdAfx.h"
#include "PHContact.h"
#include "B3Shell.h"

static SB3GeomOwner* GetOwner(const dContact& Contact, bool First)
{
	CODEGeom* Geom = Contact.Geom[First ? 0 : 1];
	return Geom ? Geom->Owner : nullptr;
}

CPHContact::CPHContact(dContact& Contact) :
	Contact(Contact)
{
}

bool CPHContact::HasUserData(bool First) const
{
	return GetOwner(Contact, First) != nullptr;
}

IPhysicsShellHolder* CPHContact::GetObject(bool First) const
{
	SB3GeomOwner* Owner = GetOwner(Contact, First);
	return Owner ? Owner->RefObject : nullptr;
}

void* CPHContact::GetCallbackData(bool First) const
{
	SB3GeomOwner* Owner = GetOwner(Contact, First);
	return Owner ? Owner->CallbackData : nullptr;
}

IPHCharacter* CPHContact::GetCharacter(bool First) const
{
	SB3GeomOwner* Owner = GetOwner(Contact, First);
	return Owner ? Owner->Character : nullptr;
}

bool CPHContact::HasObjectCallback(bool First, ObjectContactCallbackFun* Callback) const
{
	CODEGeom* Geom = Contact.Geom[First ? 0 : 1];
	if (Geom == nullptr)
	{
		return false;
	}

	const auto Has = [Callback](const xr_vector<ObjectContactCallbackFun*>& List)
	{
		return std::find(List.begin(), List.end(), Callback) != List.end();
	};
	return Has(Geom->ObjectCallbacks) || (Geom->Owner && Has(Geom->Owner->ObjectCallbacks));
}

bool CPHContact::GetLastPosition(bool First, Fvector& Position) const
{
	return false;
}

bool CPHContact::GetPushingNegTriangle(bool First, const Fvector* Verts, Fvector& Normal, float& Dist) const
{
	// Box3D has no negative-triangle push-out.
	return false;
}

const Fvector& CPHContact::GetPosition() const
{
	return Contact.Position;
}

const Fvector& CPHContact::GetNormal() const
{
	return Contact.Normal;
}

float CPHContact::GetDepth() const
{
	return Contact.Depth;
}

bool CPHContact::IsCylinder() const
{
	return (Contact.Geom[0] && Contact.Geom[0]->Type == EB3GeomType::Cylinder) ||
		(Contact.Geom[1] && Contact.Geom[1]->Type == EB3GeomType::Cylinder);
}

float CPHContact::GetFriction() const
{
	return Contact.Friction;
}

// Box3D pre-solve can only cancel a contact; surface edits are kept for reading back only.
void CPHContact::SetFriction(float Mu)
{
	Contact.Friction = Mu;
}

void CPHContact::SetBounce(float Bounce, float BounceVel)
{
	Contact.Bounce = Bounce;
	Contact.BounceVel = BounceVel;
}

void CPHContact::ScaleSpringDamping(float Spring, float Damping)
{
}
