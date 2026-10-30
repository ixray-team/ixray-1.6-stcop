#pragma once

#include "PhysicsExternalCommon.h"
#include "../xrEngine/AI/alife_space.h"

class IPhysicsShellHolder;
class IPhysicsElement;
class IClimableObject;
class ICollisionDamageInfo;
class CElevatorState;
struct SPHNetState;

enum EEnvironment
{
	peOnGround,
	peAtWall,
	peInAir
};

// Character controller (actor or AI capsule) as seen outside xrPhysics.
class IPHCharacter
{
public:
	virtual bool Exists() const = 0;
	virtual bool IsActorCharacter() = 0;

	virtual void Create(const Fvector& Sizes) = 0;
	virtual void Destroy() = 0;
	virtual void Reinit() = 0;
	virtual void SetBox(const Fvector& Sizes) = 0;

	virtual void Enable() = 0;
	virtual void Disable() = 0;
	virtual bool IsEnabled() = 0;
	virtual void Freeze() = 0;
	virtual void UnFreeze() = 0;
	virtual void step(float dt) = 0;
	virtual void collision_disable() = 0;
	virtual void collision_enable() = 0;
	virtual void collision_dynamic_disable() = 0;
	virtual void collision_dynamic_enable() = 0;
	virtual void SwitchOFFInitContact() = 0;
	virtual void SwitchInInitContact() = 0;

	virtual void SetRestrictionType(ERestrictionType Type) = 0;
	virtual void ChooseRestrictionType(ERestrictionType MyType, float MyDepth, IPHCharacter* Other) = 0;
	virtual bool UpdateRestrictionType(IPHCharacter* Other) = 0;
	virtual void SetObjectRadius(float Radius) = 0;
	virtual void SetRestrictorRadius(ERestrictionType Type, float Radius) = 0;
	virtual void SetActorMovable(bool Value) = 0;

	virtual void SetPLastMaterialIDX(u16* Material) = 0;
	virtual const u16& LastMaterialIDX() const = 0;
	virtual u16 InjuriousMaterialIDX() const = 0;
	virtual void update_last_material() = 0;
	virtual void SetMaterial(u16 Material) = 0;

	virtual const ICollisionDamageInfo* CollisionDamageInfo() const = 0;
	virtual ICollisionDamageInfo* CollisionDamageInfo() = 0;
	virtual CElevatorState* ElevatorState() = 0;
	virtual void SetHitType(ALife::EHitType Type) = 0;
	virtual void SetCollisionDamageFactor(float Factor) = 0;

	virtual void SetMaximumVelocity(float Velocity) = 0;
	virtual float GetMaximumVelocity() = 0;
	virtual void SetJupmUpVelocity(float Velocity) = 0;
	virtual void SetAirControlFactor(float Factor) = 0;
	virtual void SetForcedPhysicsControl(bool Value) = 0;
	virtual bool ForcedPhysicsControl() = 0;
	virtual void SetCamDir(const Fvector& CamDir) = 0;
	virtual void SetAcceleration(Fvector Accel) = 0;
	virtual void SetApplyGravity(bool Flag) = 0;

	virtual void ApplyImpulse(const Fvector& Dir, const float P) = 0;
	virtual void ApplyForce(const Fvector& Force) = 0;
	virtual void ApplyForce(const Fvector& Dir, float Force) = 0;
	virtual void ApplyForce(float X, float Y, float Z) = 0;
	virtual void setForce(const Fvector& Force) = 0;
	virtual void Jump(const Fvector& JumpVelocity) = 0;
	virtual bool JumpState() = 0;

	virtual EEnvironment CheckInvironment() = 0;
	virtual bool ContactWas() = 0;
	virtual u16 ContactBone() = 0;
	virtual void GroundNormal(Fvector& Normal) = 0;

	virtual void SetObjectContactCallbackData(void* Data) = 0;
	virtual void SetObjectContactCallback(ObjectContactCallbackFun* Callback) = 0;
	virtual void SetWheelContactCallback(ObjectContactCallbackFun* Callback) = 0;
	virtual ObjectContactCallbackFun* ObjectContactCallBack() = 0;
	// Contacts only report depth and do not push the character (used by virtual moves).
	virtual void SetVirtualMoveContactCallback() = 0;

	virtual void GetVelocity(Fvector& Velocity) const = 0;
	virtual void GetSavedVelocity(Fvector& Velocity) = 0;
	virtual void GetSmothedVelocity(Fvector& Velocity) = 0;
	virtual void SetVelocity(Fvector Velocity) = 0;
	virtual void GetPosition(Fvector& Position) = 0;
	virtual void GetBodyPosition(Fvector& Position) = 0;
	virtual const Fvector& BodyPosition() const = 0;
	virtual void get_body_position(Fvector& Position) = 0;
	virtual void GetFootCenter(Fvector& Position) = 0;
	virtual void GetDesiredPosition(Fvector& Position) = 0;
	virtual void IPosition(Fvector& Position) = 0;
	virtual void DeathPosition(Fvector& Position) = 0;
	virtual void SetPosition(const Fvector& Position) = 0;
	virtual bool TryPosition(Fvector Position, bool Exact) = 0;
	virtual void fix_body_rotation() = 0;

	virtual void SetMas(float Mass) = 0;
	virtual float Mass() = 0;
	virtual float FootRadius() = 0;
	virtual float& FrictionFactor() = 0;
	virtual void SetPhysicsRefObject(IPhysicsShellHolder* RefObject) = 0;
	virtual IPhysicsShellHolder* PhysicsRefObject() = 0;
	virtual void SetNonInteractive(bool Value) = 0;
	virtual void NetRelcase(IPhysicsShellHolder* Object) = 0;
	virtual const IPhysicsElement* PhysicsElement() const = 0;

	virtual void get_State(SPHNetState& State) = 0;
	virtual void set_State(const SPHNetState& State) = 0;

	virtual ~IPHCharacter() = default;
};

XRPHYSICS_API IPHCharacter* create_ai_character();
XRPHYSICS_API IPHCharacter* create_actor_character(bool single_game);
