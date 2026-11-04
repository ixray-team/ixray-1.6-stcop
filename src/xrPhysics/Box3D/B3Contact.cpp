#include "StdAfx.h"
#include "PHContact.h"
#include "B3Shell.h"
#include "../../xrEngine/GameMtlLib.h"

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

// Callbacks run before the step's state is pulled: CurForm is the pose before the step, as ODE last_pos.
bool CPHContact::GetLastPosition(bool First, Fvector& Position) const
{
	const CODEGeom* Geom = Contact.Geom[First ? 0 : 1];
	if (Geom == nullptr || Geom->Owner == nullptr || Geom->Owner->Shell == nullptr || !b3Shape_IsValid(Geom->Shape))
	{
		return false;
	}

	const CPHElement* Element = static_cast<const CPHElement*>(b3Body_GetUserData(b3Shape_GetBody(Geom->Shape)));
	Element->CurForm.transform_tiny(Position, Geom->LocalForm.c);
	return true;
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
	Contact.FrictionRead = true;
	return Contact.Friction;
}

// Edits persist on the calling side's shape from the next step. A value made from GetFriction is
// a factor (car wheels) and stays relative under product mixing; any other value is absolute.
void CPHContact::SetFriction(float Mu)
{
	const bool Scaled = Contact.FrictionRead && Contact.Friction > EPS_S;
	CODEGeom* Geom = Contact.Geom[Contact.Side];
	if (Geom && Geom->Owner)
	{
		const SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(Geom->Owner->Material);
		const float Base = Mtl ? Mtl->fPHFriction : 1.f;
		Geom->SetSurface(B3MaterialFlag::Friction, Scaled ? Base * Mu / Contact.Friction : Mu, !Scaled);
	}
	Contact.Friction = Mu;
}

// The threshold is global in Box3D: BounceVel only gates the level bounce added by hand (CB3World).
void CPHContact::SetBounce(float Bounce, float BounceVel)
{
	Contact.Bounce = Bounce;
	Contact.BounceVel = BounceVel;
	if (CODEGeom* Geom = Contact.Geom[Contact.Side])
	{
		Geom->SetSurface(B3MaterialFlag::Bounce, Bounce, true);
	}
}

void CPHContact::ScaleSpringDamping(float Spring, float Damping)
{
}
