#pragma once

#include "PhysicsExternalCommon.h"

class IPhysicsShellHolder;
class IPHCharacter;
struct dContact;

// Backend-neutral view of one contact, valid only inside a contact callback.
// "First" selects the first geom of the contact pair (the "bo1" side).
class XRPHYSICS_API CPHContact
{
public:
	explicit CPHContact(dContact& Contact);

	bool HasUserData(bool First) const;
	IPhysicsShellHolder* GetObject(bool First) const;
	void* GetCallbackData(bool First) const;
	IPHCharacter* GetCharacter(bool First) const;
	bool HasObjectCallback(bool First, ObjectContactCallbackFun* Callback) const;
	bool GetLastPosition(bool First, Fvector& Position) const;
	bool GetPushingNegTriangle(bool First, const Fvector* Verts, Fvector& Normal, float& Dist) const;

	const Fvector& GetPosition() const;
	const Fvector& GetNormal() const;
	float GetDepth() const;
	bool IsCylinder() const;

	float GetFriction() const;
	void SetFriction(float Mu);
	void SetBounce(float Bounce, float BounceVel);
	void ScaleSpringDamping(float Spring, float Damping);

	// Backend access for xrPhysics internals only.
	dContact& Native() const
	{
		return Contact;
	}

private:
	dContact& Contact;
};
