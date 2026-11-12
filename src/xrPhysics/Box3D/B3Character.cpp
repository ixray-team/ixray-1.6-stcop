#include "StdAfx.h"
#include "IPHCharacter.h"
#include "IColisiondamageInfo.h"
#include "IElevatorState.h"
#include "IPhysicsShellHolder.h"
#include "DamageSource.h"
#include "MathUtils.h"
#include "PHUpdateObject.h"
#include "params.h"
#include "../../xrEngine/GameMtlLib.h"
#include "B3World.h"
#include "B3Math.h"
#include "../../xrCore/Collision/xr_area.h"
#include "iclimableobject.h"
#include "../../xrServerEntities/PHNetState.h"
#include "../../xrServerEntities/PHSynchronize.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/xr_object_list.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/bone.h"
#include "../../Include/xrRender/Kinematics.h"

class CB3CollisionDamageInfo : public ICollisionDamageInfo
{
public:
	// HitPos is read as a bone-space point and stays at the bone origin; Point is the world contact.
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector Direction = {0.f, -1.f, 0.f};
	Fvector Point = {0.f, 0.f, 0.f};
	mutable float Velocity = 0.f;
	ALife::EHitType Type = ALife::eHitTypeStrike;
	ALife::_OBJECT_ID Initiator = ALife::INVALID_OBJECT_ID;
	ICollisionHitCallback* Callback = nullptr;
	bool Initiated = false;
	IPhysicsShellHolder* Owner = nullptr;

	void Record(float Speed, const Fvector& Normal, const Fvector& At, IPhysicsShellHolder* Object)
	{
		IDamageSource* Source = Object ? Object->ObjectCastIDamageSource() : nullptr;
		Velocity = Speed;
		Direction.set(Normal);
		Point.set(At);
		Callback = Object ? Object->ObjectGetCollisionHitCallback() : nullptr;
		Initiator = Source ? Source->Initiator() : ALife::INVALID_OBJECT_ID;
	}

	virtual float ContactVelocity() const override
	{
		const float Result = Velocity;
		Velocity = 0.f;
		return Result;
	}
	virtual void HitDir(Fvector& dir) const override
	{
		dir.set(Direction);
	}
	virtual const Fvector& HitPos() const override
	{
		return Position;
	}
	virtual ALife::_OBJECT_ID DamageInitiatorID() const override
	{
		if (Initiator != ALife::INVALID_OBJECT_ID)
		{
			return Initiator;
		}
		return Owner ? Owner->ObjectID() : ALife::INVALID_OBJECT_ID;
	}
	virtual CObject* DamageInitiator() const override
	{
		CObject* Found = Initiator != ALife::INVALID_OBJECT_ID && g_B3World && g_B3World->Objects() ? g_B3World->Objects()->net_Find(Initiator) : nullptr;
		return Found ? Found : smart_cast<CObject*>(Owner);
	}
	virtual ALife::EHitType HitType() const override
	{
		return Type;
	}
	virtual ICollisionHitCallback* HitCallback() const override
	{
		return Callback;
	}
	virtual void Reinit() override
	{
		Velocity = 0.f;
		Initiator = ALife::INVALID_OBJECT_ID;
		Callback = nullptr;
		Initiated = false;
	}
	virtual void SetInitiated() override
	{
		Initiated = true;
	}
	virtual bool IsInitiated() const override
	{
		return Initiated;
	}
	virtual bool GetAndResetInitiated() override
	{
		const bool Result = Initiated;
		Initiated = false;
		return Result;
	}
};

// Character on the Box3D mover (b3World_CollideMover + b3SolvePlanes + b3World_CastMover) driven by the
// CPHSimpleCharacter force model. A kinematic capsule body follows the mover so items and other characters
// collide with it.
// CPHSynchronize: xrGame reaches the net state through smart_cast<CPHSynchronize*> (CPHMovementControl::GetSyncItem).
class CB3Character :
	public IPHCharacter,
	public CPHSynchronize,
	public CPHUpdateObject
{
	static constexpr int PlaneCapacity = 64;
	static constexpr float SqrtHalf = 0.70710678f;
	static constexpr float PullForce = 25.f;
	static constexpr float LoseControlDistance = 0.5f;
	static constexpr float ClambDistance = 0.5f;
	static constexpr u64 ImpulseSteps = 30;
	static constexpr float GettingOnDistance = 0.3f;
	static constexpr float StopClimbingDistance = 0.1f;
	static constexpr float OutDistance = 1.5f;
	static constexpr float LookAngleCosine = 0.9238795f;
	static constexpr float LookupAngleSine = 0.34202014f;

	// CElevatorState data. xrGame reads it only through IElevatorState.
	struct SB3Elevator final : public IElevatorState
	{
		IClimableObject* Ladder = nullptr;
		Estate Value = clbNoLadder;
		Fvector StartPosition = {0.f, 0.f, 0.f};
		u32 StartTime = 0;

		virtual Estate State() override
		{
			return Value;
		}
		virtual void NetRelcase(IPhysicsShellHolder* Object) override
		{
			if (Object && Ladder && Object == Ladder->cast_IPhysicsShellHolder())
			{
				Value = clbNoLadder;
				Ladder = nullptr;
			}
		}
	};

	struct SRay
	{
		CB3Character* Self;
		SB3RayHit* Hit;
		bool Found;
	};

	struct STry
	{
		CB3Character* Self;
		bool Blocked;
	};

	SB3GeomOwner Owner;
	CODEGeom Geom;
	b3BodyId Body = b3_nullBodyId;
	b3CollisionPlane Planes[PlaneCapacity];
	b3ShapeId PlaneShapes[PlaneCapacity];
	Fvector PlanePoints[PlaneCapacity];
	int PlaneCount = 0;
	b3Vec3 QueryOrigin = {0.f, 0.f, 0.f};
	// Dynamic shapes the mover pushes this step; B3CharacterPreSolve cancels their solver contacts.
	b3ShapeId Pushed[PlaneCapacity];
	bool PushedRigid[PlaneCapacity];
	int PushedCount = 0;
	b3ShapeId Ignored[PlaneCapacity];
	int IgnoredCount = 0;

	CB3CollisionDamageInfo DamageInfo;
	collide::rq_results BoneQuery;
	CDB::COLLIDER Collider;
	SB3Elevator Elevator;
	ObjectContactCallbackFun* ObjectCallback = nullptr;
	u16* LastMaterialPtr = nullptr;
	u16 LastMaterial = GAMEMTL_NONE_IDX;
	u16 InjuriousMaterial = GAMEMTL_NONE_IDX;
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector PrevPosition = {0.f, 0.f, 0.f};
	Fvector BodyPos = {0.f, 0.f, 0.f};
	Fvector Velocity = {0.f, 0.f, 0.f};
	Fvector SavedVelocity = {0.f, 0.f, 0.f};
	Fvector LastMove = {0.f, 0.f, 0.f};
	Fvector Accel = {0.f, 0.f, 0.f};
	Fvector PendingForce = {0.f, 0.f, 0.f};
	Fvector ControlForce = {0.f, 0.f, 0.f};
	Fvector JumpVelocity = {0.f, 0.f, 0.f};
	Fvector DepartPosition = {0.f, 0.f, 0.f};
	Fvector JumpDepartPosition = {0.f, 0.f, 0.f};
	Fvector ClambDepartPosition = {0.f, 0.f, 0.f};
	Fvector GroundNormalValue = {0.f, 1.f, 0.f};
	Fvector GroundPoint = {0.f, 0.f, 0.f};
	Fvector GroundVelocity = {0.f, 0.f, 0.f};
	Fvector WallNormal = {0.f, 1.f, 0.f};
	Fvector WallPoint = {0.f, 0.f, 0.f};
	Fvector Size = {0.6f, 1.8f, 0.6f};
	Fvector CamDir = {0.f, 0.f, 1.f};
	u64 ExternalImpulseStopStep = u64(-1);
	float MaxVelocity = 5.f;
	float JumpUpVelocity = 6.f;
	float AirControl = 0.f;
	float MassValue = 70.f;
	float Friction = 1.f;
	float GroundMu = 1.f;
	float CollisionDamageFactor = 1.f;
	int GroundIndex = -1;
	int ContactCount = 0;
	bool Created = false;
	bool Enabled = true;
	bool Frozen = false;
	bool Collision = true;
	bool DynamicCollision = true;
	bool ApplyGravity = true;
	bool ForcedPhysics = false;
	bool NonInteractive = false;
	bool ActorMovable = true;
	// VirtualMoveTo: moves against everything as rigid, pushes nothing and changes no state.
	bool Probe = false;
	bool Contact = false;
	bool WasContact = false;
	bool WasControl = false;
	bool ObjectContact = false;
	bool WasObjectContact = false;
	bool GroundValid = false;
	bool WallValid = false;
	bool SideContact = false;
	bool LoseControl = true;
	// Landing after a jump or fall; read once by ContactWas, as b_meet_control in ODE.
	bool MeetControl = false;
	bool Jumping = false;
	bool JumpPending = false;
	bool ClambJump = false;
	bool ExternalImpulse = false;
	bool Actor;

	float Radius() const
	{
		return std::max(0.1f, std::min(Size.x, Size.z) * 0.5f);
	}

	b3Capsule MakeCapsule() const
	{
		const float R = Radius();
		const float Top = std::max(Size.y - R, R + 0.01f);
		return {{0.f, R, 0.f}, {0.f, Top, 0.f}, R};
	}

	void UpdateBodyPosition()
	{
		BodyPos.set(Position.x, Position.y + Size.y * 0.5f, Position.z);
	}

	bool Climbing() const
	{
		return Elevator.Value == clbClimbingUp || Elevator.Value == clbClimbingDown;
	}

	u64 CollideMask() const
	{
		if (!Collision)
		{
			return 0ull;
		}
		return B3Category::Level | B3Category::Static | B3Category::ActorObstacle | (DynamicCollision ? (B3Category::Dynamic | B3Category::Character) : 0ull);
	}

	u64 CapsuleMask() const
	{
		return Collision && DynamicCollision ? B3Category::Dynamic : 0ull;
	}

	static SGameMtl* MaterialOf(u16 Index)
	{
		return GMLibrary().GetMaterialByIdx(Index);
	}

	static u16 ShapeMaterial(b3ShapeId Shape)
	{
		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
		return Other && Other->Owner ? Other->Owner->Material : u16(b3Shape_GetSurfaceMaterial(Shape).userMaterialId);
	}

	static u16 SlideMaterial()
	{
		static const u16 Index = []
		{
			const auto It = GMLibrary().GetMaterialIt("materials\\earth_slide");
			return It == GMLibrary().GetMaterials().end() ? GAMEMTL_NONE_IDX : u16(It - GMLibrary().FirstMaterial());
		}();
		return Index;
	}

	void StopBody()
	{
		if (b3Body_IsValid(Body))
		{
			B3_SAFE(b3Body_SetLinearVelocity(Body, b3Vec3_zero));
		}
	}

	void CreateBody()
	{
		DestroyBody();
		if (g_B3World == nullptr)
		{
			return;
		}

		b3BodyDef Def = b3DefaultBodyDef();
		Def.type = b3_kinematicBody;
		Def.position = B3Math::ToB3(Position);
		Def.userData = this;
		Def.enableContactRecycling = false;
		Body = b3CreateBody(g_B3World->Id(), &Def);

		b3ShapeDef ShapeDef = b3DefaultShapeDef();
		ShapeDef.userData = &Geom;
		ShapeDef.enablePreSolveEvents = true;
		ShapeDef.filter.categoryBits = B3Category::Character;
		ShapeDef.filter.maskBits = CapsuleMask();
		ShapeDef.baseMaterial.userMaterialId = Owner.Material;
		const b3Capsule Capsule = MakeCapsule();
		Geom.Shape = b3CreateCapsuleShape(Body, &ShapeDef, &Capsule);
	}

	void DestroyBody()
	{
		Geom.Shape = b3_nullShapeId;
		if (b3Body_IsValid(Body))
		{
			b3DestroyBody(Body);
		}
		Body = b3_nullBodyId;
	}

	void Ignore(b3ShapeId Shape)
	{
		if (IgnoredCount < PlaneCapacity)
		{
			Ignored[IgnoredCount++] = Shape;
		}
	}

	int PushedIndex(b3ShapeId Shape) const
	{
		for (int I = 0; I < PushedCount; ++I)
		{
			if (B3_ID_EQUALS(Pushed[I], Shape))
			{
				return I;
			}
		}
		return -1;
	}

	// Skips the capsule, refused shapes, every shape of the same object (bone collision, held items),
	// objects out of character collision, passable materials and ladders the character is not in front of.
	bool Accepts(b3ShapeId Shape)
	{
		if (B3_ID_EQUALS(Shape, Geom.Shape))
		{
			return false;
		}
		for (int I = 0; I < IgnoredCount; ++I)
		{
			if (B3_ID_EQUALS(Shape, Ignored[I]))
			{
				return false;
			}
		}

		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
		if (Other && Other->Owner && ((Owner.RefObject && Other->Owner->RefObject == Owner.RefObject) || (Other->Owner->Climable && !Other->Owner->Climable->BeforeLadder(this, -0.1f))))
		{
			return false;
		}
		const b3Filter Filter = b3Shape_GetFilter(Shape);
		if ((Filter.categoryBits & B3Category::Dynamic) && !(Filter.maskBits & B3Category::Character))
		{
			return false;
		}
		const SGameMtl* Mtl = MaterialOf(ShapeMaterial(Shape));
		return !(Mtl && Mtl->Flags.test(SGameMtl::flPassable) && !Mtl->Flags.test(SGameMtl::flActorObstacle));
	}

	static bool MoverFilter(b3ShapeId Shape, void* Context)
	{
		return static_cast<CB3Character*>(Context)->Accepts(Shape);
	}

	static bool PlaneCallback(b3ShapeId Shape, const b3PlaneResult* Results, int Count, void* Context)
	{
		CB3Character* Self = static_cast<CB3Character*>(Context);
		if (Count <= 0 || !Self->Accepts(Shape))
		{
			return true;
		}

		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
		const CB3Character* Character = Other && Other->Owner ? static_cast<const CB3Character*>(Other->Owner->Character) : nullptr;
		const bool Dynamic = b3Body_GetType(b3Shape_GetBody(Shape)) == b3_dynamicBody;
		const int Index = Self->PushedIndex(Shape);
		// As ODE: object callbacks decide whether the character touches a dynamic object (a rocket ignores
		// its shooter). Pushed shapes passed this step already; B3CharacterPreSolve would now refuse them.
		if (Dynamic && !Self->Probe && Index < 0 && b3Shape_IsValid(Self->Geom.Shape))
		{
			const Fvector Point = B3Math::FromB3(b3Add(Self->QueryOrigin, Results[0].point));
			const Fvector Normal = B3Math::FromB3(b3Neg(Results[0].plane.normal));
			if (!g_B3World->AllowContact(Self->Geom.Shape, Shape, Point, Normal))
			{
				Self->Ignore(Shape);
				return true;
			}
		}

		// Soft planes are pushed by the mover instead of blocking it: dynamic sides until the push saturates,
		// the actor for NPCs, and NPCs the actor may move (CPHActorCharacter restrictors).
		const bool SoftCharacter = Character && !Self->Probe && (Self->Actor ? Character->ActorMovable : Character->Actor);
		const bool SoftObject = Dynamic && !Self->Probe && !(Index >= 0 && Self->PushedRigid[Index]);
		for (int I = 0; I < Count && Self->PlaneCount < PlaneCapacity; ++I)
		{
			b3CollisionPlane& Plane = Self->Planes[Self->PlaneCount];
			Plane.plane = Results[I].plane;
			Plane.push = 0.f;
			const bool Soft = SoftCharacter || (SoftObject && Plane.plane.normal.y <= SqrtHalf);
			Plane.pushLimit = Soft ? 0.02f : FLT_MAX;
			Plane.clipVelocity = !Soft;
			Self->PlaneShapes[Self->PlaneCount] = Shape;
			Self->PlanePoints[Self->PlaneCount] = B3Math::FromB3(b3Add(Self->QueryOrigin, Results[I].point));
			++Self->PlaneCount;
		}
		return true;
	}

	b3QueryFilter MoverQuery() const
	{
		b3QueryFilter Filter = b3DefaultQueryFilter();
		Filter.categoryBits = B3Category::Dynamic | B3Category::ActorObstacleQuery | B3Category::Ladder;
		Filter.maskBits = CollideMask() | B3Category::Ladder;
		return Filter;
	}

	void Collide(const b3Vec3& Origin)
	{
		PlaneCount = 0;
		QueryOrigin = Origin;
		const b3Capsule Capsule = MakeCapsule();
		b3World_CollideMover(g_B3World->Id(), Origin, &Capsule, MoverQuery(), &PlaneCallback, this);
		if (!g_B3World->IsStepping())
		{
			g_B3World->RunDeferred();
		}
	}

	static float RayCallback(b3ShapeId Shape, b3Pos Point, b3Vec3 Normal, float Fraction, uint64_t UserMaterial, int Triangle, int Child, void* Context)
	{
		SRay& Ray = *static_cast<SRay*>(Context);
		if (!Ray.Self->Accepts(Shape))
		{
			return -1.f;
		}

		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
		Ray.Hit->Position = B3Math::FromB3(Point);
		Ray.Hit->Normal = B3Math::FromB3(Normal);
		Ray.Hit->Distance = Fraction;
		Ray.Hit->Material = Other && Other->Owner ? Other->Owner->Material : u16(UserMaterial);
		Ray.Hit->ShapeTriangle = Triangle;
		Ray.Hit->Geom = const_cast<CODEGeom*>(Other);
		Ray.Hit->Shape = Shape;
		Ray.Found = true;
		return Fraction;
	}

	bool CastRay(const Fvector& From, const Fvector& Dir, float Range, u64 Mask, SB3RayHit& Hit)
	{
		Hit = {};
		b3QueryFilter Filter = b3DefaultQueryFilter();
		Filter.categoryBits = B3Category::Dynamic | B3Category::ActorObstacleQuery;
		Filter.maskBits = Mask;
		SRay Ray{this, &Hit, false};
		b3World_CastRay(g_B3World->Id(), B3Math::ToB3(From), B3Math::ToB3(Fvector().mul(Dir, Range)), Filter, &RayCallback, &Ray);
		if (Ray.Found)
		{
			Hit.Distance *= Range;
			g_B3World->ResolveRayHit(Hit);
		}
		return Ray.Found;
	}

	static bool AnyOverlap(b3ShapeId Shape, void* Context)
	{
		*static_cast<bool*>(Context) = true;
		return false;
	}

	// Box with Half along side, up and Forward.
	bool SolidBox(const Fvector& Center, const Fvector& Forward, const Fvector& Half) const
	{
		const Fvector Side = {-Forward.z, 0.f, Forward.x};
		b3Vec3 Points[8];
		for (int I = 0; I < 8; ++I)
		{
			Fvector Point = Center;
			Point.mad(Side, (I & 1) ? Half.x : -Half.x);
			Point.y += (I & 2) ? Half.y : -Half.y;
			Point.mad(Forward, (I & 4) ? Half.z : -Half.z);
			Points[I] = B3Math::ToB3(Point);
		}

		b3ShapeProxy Proxy = {Points, 8, 0.f};
		b3QueryFilter Filter = b3DefaultQueryFilter();
		Filter.categoryBits = B3Category::Dynamic;
		Filter.maskBits = B3Category::Level | B3Category::Static;
		bool Hit = false;
		b3World_OverlapShape(g_B3World->Id(), b3Vec3_zero, &Proxy, Filter, &AnyOverlap, &Hit);
		return Hit;
	}

	// CPHSimpleCharacter::ValidateWalkOnMesh: geometry 0.4 m ahead between the feet and 2R above them,
	// with nothing at 1.5 m, means a clamber. No control counts too, for the x4 standing friction.
	bool ValidateWalkOnMesh()
	{
		Fvector Forward = {Accel.x, 0.f, Accel.z};
		const float Magnitude = Forward.magnitude();
		if (!(Magnitude > 0.f))
		{
			return true;
		}

		Forward.div(Magnitude);
		const float R = Radius();
		Fvector Center;
		Center.mad(Position, Forward, 0.4f);
		Fvector Forbid = Center;
		Forbid.y += 1.5f;
		Center.y += R + 0.05f;
		if (SolidBox(Forbid, Forward, {0.5f * R, 2.f * R, 0.7f * R}))
		{
			SideContact = true;
			return false;
		}
		return SolidBox(Center, Forward, {0.5f * R, R, 0.7f * R});
	}

	void ValidateWalkOn()
	{
		SideContact = false;
		if (Actor && LastMaterialIDX() == SlideMaterial())
		{
			ClambJump = false;
			return;
		}

		const bool OnObject = ObjectContact || WasObjectContact;
		ClambJump = ValidateWalkOnMesh() && (OnObject || Elevator.Value != clbNearDown);
		if (!OnObject)
		{
			return;
		}

		// CPHSimpleCharacter::ValidateWalkOnObject: clamber onto an object touched above the ground contact.
		if (ClambJump && std::abs(Position.y - ClambDepartPosition.y) > ClambDistance)
		{
			ClambJump = false;
		}
		Fvector D;
		D.sub(WallPoint, GroundPoint);
		const bool Walls = WallValid && ContactCount > 1;
		if (!(Elevator.Ladder && Elevator.Value != clbNone) && Walls && WallNormal.y < SqrtHalf && !SideContact)
		{
			ClambJump = D.x * ControlForce.x + D.z * ControlForce.z > 0.05f && D.y > 0.01f;
		}
		if (Walls && ClambJump && std::abs(D.x + D.z) > 0.05f && D.y > 0.01f)
		{
			ClambDepartPosition = Position;
		}
	}

	float ClimbDirection()
	{
		Fvector D;
		Elevator.Ladder->DToPlain(this, D);
		const float Direction = Accel.dotproduct(D);
		return Direction > EPS_L ? Direction * (CamDir.y + LookupAngleSine) : Direction;
	}

	// CElevatorState::SwitchState with StateSwitchInertion: leaving the depart state takes 2 m or 3 s.
	void SwitchElevator(Estate State)
	{
		Fvector Moved;
		GetFootCenter(Moved);
		Moved.sub(Elevator.StartPosition);
		const bool Inert = Elevator.Value == clbDepart && State == clbNoLadder;
		if (!(Moved.magnitude() > (Inert ? 2.f : 0.f) || Device.dwTimeGlobal - Elevator.StartTime > (Inert ? 3000u : 0u)))
		{
			return;
		}
		Elevator.StartTime = Device.dwTimeGlobal;
		GetFootCenter(Elevator.StartPosition);
		Elevator.Value = State;
	}

	void SetElevator(IClimableObject* Climable)
	{
		Fvector D;
		const float Distance = Climable->DDToAxis(this, D);
		if (Elevator.Ladder == Climable || Distance > OutDistance || (Elevator.Ladder && Elevator.Ladder->DDToAxis(this, D) < Distance))
		{
			return;
		}
		SwitchElevator(clbNone);
		Elevator.Ladder = Climable;
	}

	void NearLadderEnd()
	{
		Fvector D;
		const float Lower = Elevator.Ladder->DDLowerP(this, D);
		const float Upper = Elevator.Ladder->DDUpperP(this, D);
		if (GettingOnDistance + Radius() > std::min(Lower, Upper))
		{
			SwitchElevator(Lower < Upper ? clbNearDown : clbNearUp);
		}
	}

	// CElevatorState::PhTune.
	void UpdateElevator()
	{
		IClimableObject* Ladder = Elevator.Ladder;
		if (Ladder == nullptr)
		{
			return;
		}

		const float R = Radius();
		Fvector D;
		switch (Elevator.Value)
		{
		case clbNone:
			Ladder->DToPlain(this, D);
			if (Ladder->BeforeLadder(this) && Ladder->InTouch(this) && dXZDotNormalized(D, CamDir) > LookAngleCosine)
			{
				SwitchElevator(ClimbDirection() > 0.f ? clbClimbingUp : clbClimbingDown);
			}
			else
			{
				NearLadderEnd();
			}
			break;
		case clbNearUp:
			if (Ladder->InTouch(this) && CamDir.y < -PI / 20.f && Ladder->DDToPlain(this, D) > R / 3.f && Ladder->BeforeLadder(this, 0.1f))
			{
				SwitchElevator(clbClimbingDown);
			}
			if (Ladder->DDUpperP(this, D) - R > OutDistance)
			{
				SwitchElevator(clbNoLadder);
			}
			break;
		case clbNearDown:
		{
			const float Distance = Ladder->DDLowerP(this, D);
			if (Ladder->InTouch(this) && dXZDotNormalized(D, CamDir) > LookAngleCosine && D.dotproduct(Accel) > 0.f && ClimbDirection() > 0.f && Ladder->BeforeLadder(this))
			{
				SwitchElevator(clbClimbingUp);
			}
			if (Distance - R > OutDistance)
			{
				SwitchElevator(clbNoLadder);
			}
			break;
		}
		case clbClimbingUp:
		case clbClimbingDown:
		{
			const bool Up = Elevator.Value == clbClimbingUp;
			const float Direction = ClimbDirection();
			if ((Up ? Direction < 0.f : Direction > 0.f) && Ladder->BeforeLadder(this))
			{
				SwitchElevator(Up ? clbClimbingDown : clbClimbingUp);
			}
			const float ToAxis = Ladder->DDToAxis(this, D);
			Fvector Control;
			const float ControlMagnitude = to_mag_and_dir(Accel, Control);
			Fvector Norm = Ladder->Norm();
			Norm.normalize();
			if (!fis_zero(ToAxis) && !fis_zero(ControlMagnitude) && std::abs(Control.dotproduct(Norm)) < SqrtHalf)
			{
				SwitchElevator(clbDepart);
			}
			if (Up ? Ladder->AxDistToUpperP(this) + R < StopClimbingDistance : Ladder->AxDistToLowerP(this) - R < StopClimbingDistance)
			{
				SwitchElevator(Up ? clbNearUp : clbNearDown);
			}
			if (ToAxis - R > OutDistance)
			{
				SwitchElevator(clbNoLadder);
			}
			if (fis_zero(ControlMagnitude) && D.dotproduct(Ladder->Norm()) < 0.f)
			{
				ApplyForce(D, MassValue * g_B3World->Gravity());
			}
			if (!Up && Ladder->AxDistToUpperP(this) < -R)
			{
				SwitchElevator(clbNoLadder);
			}
			if (!Up && Velocity.y > EPS_S)
			{
				ApplyForce(0.f, -MassValue * g_B3World->Gravity(), 0.f);
			}
			break;
		}
		case clbDepart:
			NearLadderEnd();
			SwitchElevator(clbNoLadder);
			break;
		default:
			Elevator.Ladder = nullptr;
			break;
		}
	}

	// CElevatorState::GetControlDir; Dir comes in zeroed for the climbing states.
	bool LadderControlDir(Fvector& Dir)
	{
		IClimableObject* Ladder = Elevator.Ladder;
		Fvector D;
		switch (Elevator.Value)
		{
		case clbNearUp:
		case clbNearDown:
		{
			const float Distance = Elevator.Value == clbNearUp ? Ladder->DDUpperP(this, D) : Ladder->DDLowerP(this, D);
			if (dXZDotNormalized(D, CamDir) > LookAngleCosine && !fis_zero(Distance, EPS_L) && Accel.dotproduct(D) > 0.f)
			{
				Dir.set(D);
			}
			return true;
		}
		case clbClimbingUp:
			Ladder->DDAxis(Dir);
			Ladder->DDToAxis(this, D);
			Dir.add(D).normalize();
			return true;
		case clbClimbingDown:
			Ladder->DDToAxis(this, D);
			if (!Ladder->BeforeLadder(this) && D.dotproduct(Dir) <= 0.f)
			{
				return false;
			}
			Ladder->DDAxis(Dir);
			Dir.invert().add(D).normalize();
			return true;
		default:
			return true;
		}
	}

	// CPHSimpleCharacter::ApplyAcceleration.
	Fvector ControlForceValue()
	{
		Fvector Force = {0.f, 0.f, 0.f};
		if (LoseControl)
		{
			return Force.mul(Accel, MassValue * AirControl);
		}

		Fvector Wanted = {Accel.x, 0.f, Accel.z};
		if (Elevator.Ladder && Elevator.Value != clbNone)
		{
			if (Elevator.Value == clbNearUp || Elevator.Value == clbNearDown)
			{
				LadderControlDir(Wanted);
			}
			if (Climbing() && LadderControlDir(Force))
			{
				return Force.mul(Friction * MassValue * PullForce * 2.f);
			}
		}

		const bool Clamb = ClambJump && WallValid;
		const bool GoodGround = GroundValid && GroundNormalValue.y > SqrtHalf;
		Fvector Side, Dir;
		Side.crossproduct(Fvector().set(0.f, 1.f, 0.f), Wanted);
		if (Clamb)
		{
			Dir.crossproduct(Side, WallNormal);
		}
		else if (GoodGround)
		{
			Dir.crossproduct(Side, GroundNormalValue);
		}
		else
		{
			Dir.set(Wanted);
		}
		Force.mul(Dir.normalize_safe(), MassValue * PullForce * (Clamb || GoodGround ? 1.f : 1.5f));
		if (!Climbing() && ClambJump)
		{
			Force.mul(4.f);
			Force.y = std::abs(Force.y);
			Force.x = Force.x * Wanted.x < 0.f ? -Force.x : Force.x;
			Force.z = Force.z * Wanted.z < 0.f ? -Force.z : Force.z;
		}
		return Force.mul(Friction);
	}

	// CPHSimpleCharacter::InitContact: every contact but another character is a ground candidate and the
	// highest normal wins; the wall is the contact most against the control.
	void Contacts()
	{
		Contact = PlaneCount > 0;
		ContactCount = PlaneCount;
		ObjectContact = false;
		WallValid = false;
		GroundIndex = -1;
		for (int I = 0; I < PlaneCount; ++I)
		{
			const Fvector Normal = B3Math::FromB3(Planes[I].plane.normal);
			const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(PlaneShapes[I]));
			const bool IsCharacter = Other && Other->Owner && Other->Owner->Character;
			ObjectContact |= IsCharacter || b3Body_GetType(b3Shape_GetBody(PlaneShapes[I])) == b3_dynamicBody;
			if (IsCharacter)
			{
				continue;
			}
			if (GroundIndex < 0 || Normal.y > GroundNormalValue.y)
			{
				GroundIndex = I;
				GroundNormalValue = Normal;
				GroundPoint = PlanePoints[I];
			}
			if (!WallValid || Normal.x * Accel.x + Normal.z * Accel.z < WallNormal.x * Accel.x + WallNormal.z * Accel.z)
			{
				WallNormal = Normal;
				WallPoint = PlanePoints[I];
				WallValid = true;
			}
		}
		GroundValid = GroundIndex >= 0;
	}

	// ODE collides the character with passable level triangles too: they set the foot and injurious
	// materials, slow it down, hurt the actor and make shot marks (bush rustle, splashes).
	u16 Passables(float& SlowDown)
	{
		u16 Foot = GAMEMTL_NONE_IDX;
		const float R = Radius();
		Collider.box_options(0);
		Collider.box_query(g_B3World->ObjectSpace(), Fbox().set(Position.x - R, Position.y, Position.z - R, Position.x + R, Position.y + Size.y, Position.z + R));
		ContactCallbackFun* Mark = g_B3World->DefaultCharacterContactShotMark();
		for (const CDB::RESULT& Result : Collider.r_vec())
		{
			const TriabgleCDBData Triangle{Result.ModelWorldTransform, Result.model, Result.tris_id};
			const u16 Index = u16(Triangle.GetTri().material);
			const SGameMtl* Mtl = MaterialOf(Index);
			if (!Mtl->Flags.test(SGameMtl::flPassable) || Mtl->Flags.test(SGameMtl::flActorObstacle))
			{
				continue;
			}

			if (Mtl->Flags.test(SGameMtl::flInjurious))
			{
				InjuriousMaterial = Index;
			}
			else
			{
				Foot = Index;
			}
			if (Mtl->Flags.test(SGameMtl::flSlowDown))
			{
				SlowDown = std::max(SlowDown, 1.f - Mtl->fFlotationFactor);
			}
			Fvector Normal;
			Fvector Verts[3];
			Triangle.GetVerts(Verts);
			Normal.mknormal(Verts[0], Verts[1], Verts[2]);
			const float Speed = Velocity.magnitude() * Mtl->fBounceDamageFactor;
			if (Actor && Speed > DamageInfo.Velocity)
			{
				DamageInfo.Record(Speed, Normal, BodyPos, nullptr);
			}
			if (Mark && Velocity.square_magnitude() > 0.01f)
			{
				SPHShotMarkContact Shot;
				Shot.Position.mad(BodyPos, Normal, -Normal.dotproduct(Fvector().sub(BodyPos, Verts[0])));
				Shot.Normal = Normal;
				Shot.Object = Owner.RefObject;
				Shot.Material = Owner.Material;
				Shot.VelCret = std::abs(Velocity.dotproduct(Normal)) * _sqrt(MassValue);
				Shot.HasEffect = true;
				Shot.HasUserData = true;
				Shot.IsCharacter = true;
				Mark(Triangle.GetTri(), Verts, Shot);
				Mark = nullptr;
			}
		}
		return Foot;
	}

	// CPHSimpleCharacter::PhTune state: meeting and departing contact, the 0.5 m lose-control rule,
	// jump end on good ground, clambering and the pending jump.
	void UpdateControl(bool IsControl)
	{
		const bool Depart = WasContact && !Contact;
		const bool Meet = !WasContact && Contact;
		if (LoseControl && (Contact || Climbing()))
		{
			MeetControl = true;
		}
		const bool OnGround = GroundValid || (Meet && !Depart);
		if (Climbing())
		{
			Friction = 1.f;
			if (WasControl && !IsControl)
			{
				Velocity.set(0.f, 0.f, 0.f);
			}
		}
		if (Depart)
		{
			DepartPosition = Position;
		}
		if (LoseControl && ((OnGround && GroundNormalValue.y > SqrtHalf * 0.5f) || fis_zero(Velocity.square_magnitude()) || Climbing()))
		{
			LoseControl = false;
		}
		if ((Jumping && GroundValid && GroundNormalValue.y > SqrtHalf) || (Climbing() && WallValid))
		{
			Jumping = false;
		}
		if (!OnGround && !Climbing())
		{
			Fvector Moved;
			Moved.sub(Position, DepartPosition);
			if (Moved.square_magnitude() > LoseControlDistance * LoseControlDistance && std::abs(Moved.y) > 0.1f)
			{
				LoseControl = true;
			}
		}

		ValidateWalkOn();
		if (JumpPending)
		{
			LoseControl = true;
			Velocity = JumpVelocity;
			JumpDepartPosition = Position;
			JumpPending = false;
			Jumping = true;
			if (Elevator.Ladder && Climbing())
			{
				SwitchElevator(clbDepart);
			}
		}
		WasContact = Contact;
		WasControl = IsControl;
		WasObjectContact = ObjectContact;
	}

	// Mover pushes the actor out of an NPC that does not let the actor move it, or both share the push.
	void PushCharacter(int Index, CB3Character& Other)
	{
		const Fvector Dir = B3Math::FromB3(b3Neg(Planes[Index].plane.normal));
		const Fvector OtherVelocity = Other.Enabled ? Other.Velocity : Fvector().set(0.f, 0.f, 0.f);
		const float Approach = Fvector().sub(Velocity, OtherVelocity).dotproduct(Dir);
		if (Planes[Index].clipVelocity || Approach <= 0.f)
		{
			return;
		}

		const bool Shared = Actor || ActorMovable;
		const float Impulse = Shared ? Approach / (1.f / MassValue + 1.f / Other.MassValue) : Approach * Other.MassValue;
		Other.ApplyForce(Dir, Impulse / fixed_step);
		if (Shared)
		{
			Velocity.mad(Dir, -Impulse / MassValue);
		}
	}

	// Two-body contact with dynamic objects: the impulse that brings the contact to rest, limited to this
	// step's pull force with the reaction on the character. A saturated push leaves the object a wall.
	// Supports get the character's weight.
	void PushObjects(float Step)
	{
		const float Budget = MassValue * PullForce * Friction * Step;
		for (int I = 0; I < PlaneCount; ++I)
		{
			const b3ShapeId Shape = PlaneShapes[I];
			const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
			if (Other && Other->Owner && Other->Owner->Character)
			{
				PushCharacter(I, *static_cast<CB3Character*>(Other->Owner->Character));
				continue;
			}
			const b3BodyId Object = b3Shape_GetBody(Shape);
			if (b3Body_GetType(Object) != b3_dynamicBody || PushedCount >= PlaneCapacity)
			{
				continue;
			}

			const b3Vec3 Point = B3Math::ToB3(PlanePoints[I]);
			const b3Vec3 Normal = b3Neg(Planes[I].plane.normal);
			bool Rigid = Planes[I].plane.normal.y > SqrtHalf;
			if (Rigid && I == GroundIndex)
			{
				const b3Vec3 Weight = {0.f, -MassValue * g_B3World->Gravity() * Step, 0.f};
				B3_SAFE(b3Body_ApplyLinearImpulse(Object, Weight, Point, true));
			}
			else if (!Rigid)
			{
				const b3Vec3 Arm = b3Cross(b3Sub(Point, b3Body_GetWorldCenterOfMass(Object)), Normal);
				const float K = b3Body_GetInverseMass(Object) + b3Dot(Arm, b3MulMV(b3Body_GetWorldInverseRotationalInertia(Object), Arm)) + 1.f / MassValue;
				const float Approach = b3Dot(b3Sub(b3Body_GetWorldPointVelocity(Object, Point), B3Math::ToB3(Velocity)), Normal);
				float Impulse = std::max(-Approach / K, 0.f);
				if (Impulse > Budget)
				{
					Impulse = Budget;
					Rigid = true;
				}
				if (Impulse > 0.f)
				{
					B3_SAFE(b3Body_ApplyLinearImpulse(Object, b3MulSV(Impulse, Normal), Point, true));
					Velocity.mad(B3Math::FromB3(Normal), -Impulse / MassValue);
				}
			}

			Planes[I].pushLimit = Rigid ? FLT_MAX : 0.02f;
			Planes[I].clipVelocity = Rigid;
			const int Index = PushedIndex(Shape);
			if (Index >= 0)
			{
				PushedRigid[Index] |= Rigid;
				continue;
			}
			Pushed[PushedCount] = Shape;
			PushedRigid[PushedCount++] = Rigid;
		}
	}

	// CPHSimpleCharacterInline.h UpdateDynamicDamage, N from the object to the character.
	void ObjectDamage(float NormalVelocity, float ObjectNormalVelocity, float ObjectMass, const Fvector& Normal, const Fvector& Point, const CODEGeom* Other)
	{
		IPhysicsShellHolder* Object = Other && Other->Owner ? Other->Owner->RefObject : nullptr;
		if (NormalVelocity > ObjectNormalVelocity || Object == nullptr || Object->ObjectGetDestroy())
		{
			return;
		}

		const float Momentum = NormalVelocity * MassValue + ObjectNormalVelocity * ObjectMass;
		const float Energy = NormalVelocity * NormalVelocity * MassValue * 0.5f * CollisionDamageFactor + ObjectNormalVelocity * ObjectNormalVelocity * ObjectMass * 0.5f * object_damage_factor - Momentum * Momentum / (MassValue + ObjectMass) * 0.5f;
		const float Speed = Energy > 0.f ? _sqrt(Energy / MassValue * 2.f) * MaterialOf(Other->Owner->Material)->fBounceDamageFactor : 0.f;
		if (Speed > DamageInfo.Velocity)
		{
			DamageInfo.Record(Speed, Normal, Point, Object);
		}
	}

	// Damage and shot marks from the planes the move ended against, with the velocity before clipping:
	// UpdateStaticDamage (actor only, CPHAICharacter skips it) and UpdateDynamicDamage.
	void Impacts(const Fvector& Before)
	{
		int Best = -1;
		float BestSpeed = 1.f;
		for (int I = 0; I < PlaneCount; ++I)
		{
			const Fvector Normal = B3Math::FromB3(Planes[I].plane.normal);
			const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(PlaneShapes[I]));
			if (Other && Other->Owner && Other->Owner->Character)
			{
				continue;
			}
			const b3BodyId Object = b3Shape_GetBody(PlaneShapes[I]);
			if (b3Body_GetType(Object) == b3_dynamicBody)
			{
				ObjectDamage(Before.dotproduct(Normal), B3Math::FromB3(b3Body_GetLinearVelocity(Object)).dotproduct(Normal), b3Body_GetMass(Object), Normal, PlanePoints[I], Other);
				continue;
			}
			const float Approach = std::abs(Before.dotproduct(Normal));
			const float Speed = std::max(Approach, _sqrt(std::max(Before.square_magnitude() - Approach * Approach, 0.f)));
			if (Speed > BestSpeed && (Actor || -Before.dotproduct(Normal) > 1.f))
			{
				Best = I;
				BestSpeed = Speed;
			}
		}
		if (Best < 0)
		{
			return;
		}

		const Fvector Normal = B3Math::FromB3(Planes[Best].plane.normal);
		Fvector From;
		From.mad(PlanePoints[Best], Normal, 0.05f);
		SB3RayHit Hit;
		if (!g_B3World->RayPick(From, Fvector().invert(Normal), 0.1f, B3Category::Level | B3Category::Static | B3Category::ActorObstacle, Hit))
		{
			return;
		}

		const SGameMtl* Mtl = MaterialOf(Hit.Geom && Hit.Geom->Owner ? Hit.Geom->Owner->Material : Hit.Material);
		const float Approach = std::abs(Before.dotproduct(Normal));
		const float Tangent = _sqrt(std::max(Before.square_magnitude() - Approach * Approach, 0.f));
		const float Speed = (Mtl->Flags.test(SGameMtl::flPassable) ? Before.magnitude() : std::max(Tangent * Mtl->fPHFriction, Approach)) * Mtl->fBounceDamageFactor;
		if (Actor && Speed > DamageInfo.Velocity)
		{
			DamageInfo.Record(Speed, Normal, PlanePoints[Best], nullptr);
		}

		ContactCallbackFun* Mark = g_B3World->DefaultCharacterContactShotMark();
		if (Mark && -Before.dotproduct(Normal) > 1.f && Hit.Geom == nullptr && Hit.Triangle.Valid())
		{
			SPHShotMarkContact Shot;
			Shot.Position = PlanePoints[Best];
			Shot.Normal = Normal;
			Shot.Object = Owner.RefObject;
			Shot.Material = Owner.Material;
			Shot.VelCret = Approach * _sqrt(MassValue);
			Shot.HasEffect = true;
			Shot.HasUserData = true;
			Shot.IsCharacter = true;
			Fvector Verts[3];
			Hit.Triangle.GetVerts(Verts);
			Mark(Hit.Triangle.GetTri(), Verts, Shot);
		}
	}

	// Objects meet the kinematic capsule as an infinite mass in the solver: rebuild their approach from the
	// pre-solve normal velocity for UpdateDynamicDamage, and wake the character as ODE contacts do.
	void ObjectHits()
	{
		if (!b3Shape_IsValid(Geom.Shape))
		{
			return;
		}

		b3ContactData Data[16];
		const int Count = b3Shape_GetContactData(Geom.Shape, Data, 16);
		const Fvector Moving = Enabled ? LastMove : Fvector().set(0.f, 0.f, 0.f);
		for (int I = 0; I < Count; ++I)
		{
			const bool First = B3_ID_EQUALS(Data[I].shapeIdA, Geom.Shape);
			const b3ShapeId Shape = First ? Data[I].shapeIdB : Data[I].shapeIdA;
			const b3BodyId Object = b3Shape_GetBody(Shape);
			if (b3Body_GetType(Object) != b3_dynamicBody)
			{
				continue;
			}

			const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
			for (int M = 0; M < Data[I].manifoldCount; ++M)
			{
				const b3Manifold& Manifold = Data[I].manifolds[M];
				const Fvector Normal = B3Math::FromB3(First ? b3Neg(Manifold.normal) : Manifold.normal);
				for (int P = 0; P < Manifold.pointCount; ++P)
				{
					const b3ManifoldPoint& Point = Manifold.points[P];
					if (Point.totalNormalImpulse <= 0.f || Point.normalVelocity > -0.1f)
					{
						continue;
					}
					const float NormalVelocity = Moving.dotproduct(Normal);
					const Fvector At = B3Math::FromB3(b3OffsetPos(b3Body_GetWorldCenterOfMass(First ? Body : Object), Point.anchorA));
					ObjectDamage(NormalVelocity, NormalVelocity - Point.normalVelocity, b3Body_GetMass(Object), Normal, At, Other);
					Enabled = true;
				}
			}
		}
	}

	void Move(float Step)
	{
		const float R = Radius();
		const u64 Mask = CollideMask();
		IgnoredCount = 0;
		PushedCount = Probe ? PushedCount : 0;
		PlaneCount = 0;
		if (Mask != 0)
		{
			Collide(B3Math::ToB3(Position));
		}

		const bool IsControl = !Probe && Accel.magnitude() > 0.1f;
		const float SnapDrop = _sqrt(Velocity.x * Velocity.x + Velocity.z * Velocity.z) * Step + 0.01f;
		SB3RayHit Ground;
		bool GroundHit = false;
		float SlowDown = 0.f;
		bool SolidSlowDown = false;
		if (!Probe)
		{
			Contacts();
			GroundHit = Mask != 0 && CastRay(Fvector().set(Position.x, Position.y + R, Position.z), Fvector().set(0.f, -1.f, 0.f), R + 0.1f + SnapDrop, Mask & ~B3Category::Character, Ground);
			const u16 GroundMaterial = GroundHit ? Ground.Material : (GroundValid ? ShapeMaterial(PlaneShapes[GroundIndex]) : GAMEMTL_NONE_IDX);
			const float OwnMu = MaterialOf(Owner.Material)->fPHFriction;
			GroundMu = GroundMaterial == GAMEMTL_NONE_IDX ? OwnMu : MaterialOf(GroundMaterial)->fPHFriction * OwnMu;
			Friction = Contact ? std::max(ObjectContact ? 1.f : 0.f, GroundMu) : 0.f;
			const b3BodyId GroundBody = GroundValid ? b3Shape_GetBody(PlaneShapes[GroundIndex]) : b3_nullBodyId;
			GroundVelocity = GroundValid && b3Body_GetType(GroundBody) == b3_dynamicBody ? B3Math::FromB3(b3Body_GetWorldPointVelocity(GroundBody, B3Math::ToB3(GroundPoint))) : Fvector().set(0.f, 0.f, 0.f);

			// CPHSimpleCharacterInline.h foot_material_update: the ladder, else a passable, else the ground.
			InjuriousMaterial = GAMEMTL_NONE_IDX;
			const u16 Passable = Mask != 0 ? Passables(SlowDown) : GAMEMTL_NONE_IDX;
			const u16 Foot = Climbing() ? Elevator.Ladder->Material() : (Passable != GAMEMTL_NONE_IDX ? Passable : (Contact ? GroundMaterial : GAMEMTL_NONE_IDX));
			if (Foot != GAMEMTL_NONE_IDX)
			{
				LastMaterial = Foot;
				if (LastMaterialPtr)
				{
					*LastMaterialPtr = Foot;
				}
			}
			if (InjuriousMaterial == GAMEMTL_NONE_IDX && LastMaterialIDX() != GAMEMTL_NONE_IDX && MaterialOf(LastMaterialIDX())->Flags.test(SGameMtl::flInjurious))
			{
				InjuriousMaterial = LastMaterialIDX();
			}
			if (SlowDown <= 0.f && Contact && GroundMaterial != GAMEMTL_NONE_IDX && MaterialOf(GroundMaterial)->Flags.test(SGameMtl::flSlowDown))
			{
				SlowDown = 1.f - MaterialOf(GroundMaterial)->fFlotationFactor;
				SolidSlowDown = true;
			}

			UpdateElevator();
			UpdateControl(IsControl);
		}

		// Forces of CPHSimpleCharacter::PhTune, cut to 5 m/s per step as BodyCutForce(m_body, 5, 0).
		Fvector Force = PendingForce;
		PendingForce.set(0.f, 0.f, 0.f);
		if (!Probe)
		{
			ControlForce = ControlForceValue();
			if (IsControl)
			{
				Fvector Side;
				Side.crossproduct(ControlForce, Fvector().set(0.f, 1.f, 0.f)).normalize_safe();
				const float Damping = Side.dotproduct(Velocity) * (500.f + 200.f * ClambJump) * Friction;
				Force.add(ControlForce);
				if (!LoseControl || ClambJump)
				{
					Force.x -= Side.x * Damping;
					Force.z -= Side.z * Damping;
					Force.y -= !LoseControl && !Contact ? MassValue * 50.f : 0.f;
				}
			}

			const float AccelXZ = _sqrt(Accel.x * Accel.x + Accel.z * Accel.z);
			if (Jumping && AccelXZ > 0.f)
			{
				const float AirFactor = LoseControl && Actor ? 10.f * AirControl : 1.f;
				Fvector Moved;
				Moved.sub(Position, JumpDepartPosition);
				if ((Moved.x * Accel.x + Moved.z * Accel.z) / AccelXZ < 0.3f)
				{
					Force.x += Accel.x * 1000.f / AccelXZ * AirFactor;
					Force.z += Accel.z * 1000.f / AccelXZ * AirFactor;
				}
				const float Projection = Accel.x * Velocity.x + Accel.z * Velocity.z;
				const float SpeedXZ = Velocity.x * Velocity.x + Velocity.z * Velocity.z;
				if (Projection < 0.f && SpeedXZ > 0.f)
				{
					const float Brake = 3000.f * AirFactor / SpeedXZ / AccelXZ * Projection;
					Force.x += Velocity.x * Brake;
					Force.z += Velocity.z * Brake;
				}
			}

			// CPHContactBodyEffector: liquid drag, along the surface only for solid materials.
			if (SlowDown > 0.f)
			{
				Fvector Drag;
				Drag.mul(Velocity, -std::min(Velocity.magnitude() * 10000.f * SlowDown * SlowDown, MassValue / fixed_step));
				if (SolidSlowDown)
				{
					Drag.mad(GroundNormalValue, -Drag.dotproduct(GroundNormalValue));
				}
				Force.add(Drag);
			}
		}

		const float ForceLimit = 5.f / fixed_step * MassValue;
		const float ForceMagnitude = Force.magnitude();
		if (ForceMagnitude > ForceLimit)
		{
			Force.mul(ForceLimit / ForceMagnitude);
		}
		if (Actor && !Probe && LoseControl && !ExternalImpulse)
		{
			Force.y = std::min(Force.y, 4000.f);
		}
		Velocity.mad(Force, Step / MassValue);
		if (ApplyGravity && !Climbing())
		{
			Velocity.y -= g_B3World->Gravity() * Step;
		}

		if (!Probe)
		{
			// CPHSimpleCharacter::SafeAndLimitVelocity.
			const bool LoseGround = !((GroundValid && GroundNormalValue.y > SqrtHalf) || Climbing()) || LoseControl;
			const float Limit = IsControl && !LoseControl && !ExternalImpulse ? MaxVelocity / phTimefactor : default_l_limit;
			const float Speed = Velocity.magnitude();
			if (Speed > Limit && fis_zero(Limit))
			{
				Velocity.set(0.f, 0.f, 0.f);
			}
			else if (Speed > Limit && LoseGround && Velocity.y < 0.f && Velocity.y > -default_l_limit)
			{
				Velocity.x *= Limit / Speed;
				Velocity.z *= Limit / Speed;
			}
			else if (Speed > Limit)
			{
				Velocity.mul(Limit / Speed);
			}

			// Ground friction: none while controlling (AI also out of control and jumping), x4 while clambering.
			if (GroundValid && !Climbing() && !IsControl && (Actor || !(LoseControl || Jumping)))
			{
				Fvector Relative;
				Relative.sub(Velocity, GroundVelocity);
				const float Normal = Relative.dotproduct(GroundNormalValue);
				Fvector Tangent;
				Tangent.mad(Relative, GroundNormalValue, -Normal);
				const float TangentSpeed = Tangent.magnitude();
				if (Normal < 0.f && TangentSpeed > 0.f)
				{
					Velocity.mad(Tangent, -std::min(1.f, -Normal * GroundMu * Friction * (ClambJump ? 4.f : 1.f) / TangentSpeed));
				}
			}

			if (DynamicCollision)
			{
				PushObjects(Step);
			}
		}

		Fvector Delta;
		Delta.mul(Velocity, Step);
		// Slope-sized drops are followed; bigger ones fall with the stick-down force.
		if (GroundHit && !Contact && !LoseControl && !Jumping && !Climbing() && Velocity.y <= 0.f && Ground.Normal.y > SqrtHalf && Position.y - Ground.Position.y <= SnapDrop)
		{
			Delta.y = Ground.Position.y - Position.y;
		}

		const Fvector Before = Velocity;
		if (Mask == 0)
		{
			Position.add(Delta);
		}
		else
		{
			const b3Capsule Capsule = MakeCapsule();
			const b3QueryFilter Filter = MoverQuery();
			b3Vec3 Current = B3Math::ToB3(Position);
			const b3Vec3 Target = b3Add(Current, B3Math::ToB3(Delta));
			for (int Iteration = 0; Iteration < 5; ++Iteration)
			{
				if (Iteration > 0)
				{
					Collide(Current);
				}
				const b3PlaneSolverResult Result = b3SolvePlanes(b3Sub(Target, Current), Planes, PlaneCount);
				const float Fraction = b3World_CastMover(g_B3World->Id(), Current, &Capsule, Result.delta, Filter, &MoverFilter, this);
				const b3Vec3 Moved = b3MulSV(Fraction, Result.delta);
				Current = b3Add(Current, Moved);
				if (b3LengthSquared(Moved) < 1e-6f)
				{
					break;
				}
			}

			Velocity = B3Math::FromB3(b3ClipVector(B3Math::ToB3(Velocity), Planes, PlaneCount));
			Position = B3Math::FromB3(Current);
			if (!Probe)
			{
				Impacts(Before);
			}
		}

		VERIFY(_valid(Position));
		UpdateBodyPosition();
		if (!Probe)
		{
			SavedVelocity.set(Velocity);
			if (b3Body_IsValid(Body))
			{
				B3_SAFE(b3Body_SetTargetTransform(Body, {B3Math::ToB3(Position), b3Quat_identity}, Step, true));
			}
		}
	}

	static bool AnyAccepted(b3ShapeId Shape, const b3PlaneResult* Results, int Count, void* Context)
	{
		STry& Try = *static_cast<STry*>(Context);
		Try.Blocked = Count > 0 && Try.Self->Accepts(Shape);
		return !Try.Blocked;
	}

	static bool LadderCallback(b3ShapeId ShapeId, void* Context)
	{
		CODEGeom* Other = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeId));
		if (Other && Other->Owner && Other->Owner->Climable)
		{
			*static_cast<IClimableObject**>(Context) = Other->Owner->Climable;
			return false;
		}
		return true;
	}

	IClimableObject* FindLadder() const
	{
		const float R = Radius();
		const b3Vec3 Center = B3Math::ToB3(BodyPos);
		b3AABB Box;
		Box.lowerBound = {Center.x - R, Center.y - Size.y * 0.5f, Center.z - R};
		Box.upperBound = {Center.x + R, Center.y + Size.y * 0.5f, Center.z + R};

		b3QueryFilter Filter = b3DefaultQueryFilter();
		Filter.categoryBits = B3Category::Ladder;
		Filter.maskBits = B3Category::Ladder;
		IClimableObject* Found = nullptr;
		b3World_OverlapAABB(g_B3World->Id(), Box, Filter, &LadderCallback, &Found);
		return Found;
	}

public:
	explicit CB3Character(bool IsActor) :
		Actor(IsActor)
	{
		Geom.Type = EB3GeomType::Sphere;
		Geom.Owner = &Owner;
		Owner.Character = this;
	}

	virtual ~CB3Character() override
	{
		Destroy();
	}

	// B3CharacterPreSolve: the solver must not shove what the mover already pushes.
	static bool Pushes(b3ShapeId Capsule, b3ShapeId Shape)
	{
		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(Capsule));
		const CB3Character* Self = Other && Other->Owner ? static_cast<const CB3Character*>(Other->Owner->Character) : nullptr;
		return Self && B3_ID_EQUALS(Capsule, Self->Geom.Shape) && Self->PushedIndex(Shape) >= 0;
	}

	virtual bool Exists() const override
	{
		return Created;
	}
	virtual bool IsActorCharacter() override
	{
		return Actor;
	}

	virtual void Create(const Fvector& Sizes) override
	{
		if (Created)
		{
			return;
		}
		Size.set(Sizes);
		Created = true;
		Enabled = true;
		Velocity.set(0.f, 0.f, 0.f);
		PrevPosition = Position;
		UpdateBodyPosition();
		CreateBody();
		CPHUpdateObject::Activate();
	}
	virtual void Destroy() override
	{
		CPHUpdateObject::Deactivate();
		DestroyBody();
		Elevator.Value = clbNoLadder;
		Elevator.Ladder = nullptr;
		PushedCount = 0;
		Created = false;
	}
	virtual void Reinit() override
	{
		DamageInfo.Reinit();
	}
	virtual void SetBox(const Fvector& Sizes) override
	{
		Size.set(Sizes);
		UpdateBodyPosition();
		if (Created)
		{
			CreateBody();
		}
	}

	virtual void Enable() override
	{
		Enabled = true;
	}
	virtual void Disable() override
	{
		Enabled = false;
		PrevPosition = Position;
		PushedCount = 0;
		StopBody();
	}
	virtual bool IsEnabled() override
	{
		return Created && Enabled;
	}
	virtual void Freeze() override
	{
		Frozen = true;
		StopBody();
	}
	virtual void UnFreeze() override
	{
		Frozen = false;
	}
	virtual void step(float dt) override
	{
		if (Created && g_B3World)
		{
			Move(dt);
		}
	}
	virtual void collision_disable() override
	{
		Collision = false;
		Geom.SetMask(CapsuleMask());
	}
	virtual void collision_enable() override
	{
		Collision = true;
		Geom.SetMask(CapsuleMask());
	}
	virtual void collision_dynamic_disable() override
	{
		DynamicCollision = false;
		Geom.SetMask(CapsuleMask());
	}
	virtual void collision_dynamic_enable() override
	{
		DynamicCollision = true;
		Geom.SetMask(CapsuleMask());
	}
	virtual void SwitchOFFInitContact() override {}
	virtual void SwitchInInitContact() override {}

	virtual void SetRestrictionType(ERestrictionType Type) override {}
	virtual void ChooseRestrictionType(ERestrictionType MyType, float MyDepth, IPHCharacter* Other) override {}
	virtual bool UpdateRestrictionType(IPHCharacter* Other) override
	{
		return false;
	}
	virtual void SetObjectRadius(float Value) override {}
	virtual void SetRestrictorRadius(ERestrictionType Type, float Value) override {}
	virtual void SetActorMovable(bool Value) override
	{
		ActorMovable = Value;
	}

	virtual void SetPLastMaterialIDX(u16* Material) override
	{
		LastMaterialPtr = Material;
	}
	virtual const u16& LastMaterialIDX() const override
	{
		return LastMaterialPtr ? *LastMaterialPtr : LastMaterial;
	}
	virtual u16 InjuriousMaterialIDX() const override
	{
		return InjuriousMaterial;
	}
	virtual void update_last_material() override {}
	virtual void SetMaterial(u16 Material) override
	{
		Owner.Material = Material;
	}

	virtual const ICollisionDamageInfo* CollisionDamageInfo() const override
	{
		return &DamageInfo;
	}
	virtual ICollisionDamageInfo* CollisionDamageInfo() override
	{
		return &DamageInfo;
	}
	virtual CElevatorState* ElevatorState() override
	{
		// HACK: xrGame uses it only as IElevatorState, the first and only base of CElevatorState.
		return reinterpret_cast<CElevatorState*>(static_cast<IElevatorState*>(&Elevator));
	}
	virtual void SetHitType(ALife::EHitType Type) override
	{
		DamageInfo.Type = Type;
	}
	virtual void SetCollisionDamageFactor(float Factor) override
	{
		CollisionDamageFactor = Factor;
	}

	virtual void SetMaximumVelocity(float Value) override
	{
		MaxVelocity = Value;
	}
	virtual float GetMaximumVelocity() override
	{
		return MaxVelocity;
	}
	virtual void SetJupmUpVelocity(float Value) override
	{
		JumpUpVelocity = Value;
	}
	virtual void SetAirControlFactor(float Factor) override
	{
		AirControl = Factor;
	}
	// As CPHAICharacter: only AI keeps the flag, and only TryPosition and PhysicsOnlyMode read it.
	virtual void SetForcedPhysicsControl(bool Value) override
	{
		ForcedPhysics = !Actor && Value;
	}
	virtual bool ForcedPhysicsControl() override
	{
		return ForcedPhysics;
	}
	virtual void SetCamDir(const Fvector& Value) override
	{
		CamDir.set(Value);
	}
	virtual void SetAcceleration(Fvector Value) override
	{
		if (Created && !Enabled && !fis_zero(Value.magnitude()))
		{
			Enable();
		}
		Accel.set(Value);
	}
	virtual void SetApplyGravity(bool Flag) override
	{
		ApplyGravity = Flag;
	}

	// As CPHSimpleCharacter::ApplyImpulse: one impulse per 30 steps, a one-step force from rest, straight
	// down while out of control or jumping, and the character stops when the window ends.
	virtual void ApplyImpulse(const Fvector& Dir, const float P) override
	{
		if (!Created || ExternalImpulse)
		{
			return;
		}

		const bool Down = LoseControl || Jumping || JumpPending;
		Enable();
		LoseControl = true;
		ExternalImpulse = true;
		ExternalImpulseStopStep = (g_B3World ? g_B3World->StepsNum() : 0) + ImpulseSteps;
		Velocity.set(0.f, 0.f, 0.f);
		PendingForce.mul(Down ? Fvector().set(0.f, -1.f, 0.f) : Dir, P / fixed_step);
	}
	virtual void ApplyForce(const Fvector& Force) override
	{
		ApplyForce(Force.x, Force.y, Force.z);
	}
	virtual void ApplyForce(const Fvector& Dir, float Force) override
	{
		ApplyForce(Dir.x * Force, Dir.y * Force, Dir.z * Force);
	}
	virtual void ApplyForce(float X, float Y, float Z) override
	{
		if (!Created)
		{
			return;
		}
		Enable();
		PendingForce.add(Fvector().set(X, Y, Z));
	}
	virtual void setForce(const Fvector& Force) override
	{
		PendingForce.set(Force);
	}
	// Actor: the argument is the movement acceleration, as in CPHActorCharacter::Jump.
	// AI: the argument is the ready jump velocity, as in CPHAICharacter::Jump.
	virtual void Jump(const Fvector& Value) override
	{
		if (!Actor)
		{
			JumpPending = true;
			JumpVelocity.set(Value);
			return;
		}
		if (!Created || LoseControl || LastMaterialIDX() == SlideMaterial() || !(GroundNormalValue.y > 0.5f || Climbing()))
		{
			return;
		}

		JumpPending = true;
		if (Climbing())
		{
			// CElevatorState::GetJumpDir at half the default jump speed.
			Fvector Side, Wanted;
			Elevator.Ladder->DDNorm(JumpVelocity);
			Elevator.Ladder->DDSide(Side);
			const float SideComponent = Wanted.set(Accel).normalize_safe().dotproduct(Side);
			if (std::abs(SideComponent) > SqrtHalf)
			{
				JumpVelocity.mad(Side, SideComponent < 0.f ? -1.f : 1.f).normalize_safe();
			}
			JumpVelocity.mul(3.f);
		}
		else
		{
			const float Magnitude = std::max(1.f, Accel.magnitude());
			JumpVelocity.set(Velocity.x * 1.2f + Accel.x / Magnitude * 0.2f, JumpUpVelocity, Velocity.z * 1.2f + Accel.z / Magnitude * 0.2f);
		}
		Enable();
	}
	virtual bool JumpState() override
	{
		return Jumping || JumpPending;
	}

	virtual EEnvironment CheckInvironment() override
	{
		if (Climbing())
		{
			LoseControl = false;
			return peAtWall;
		}
		return LoseControl ? peInAir : peOnGround;
	}
	virtual bool ContactWas() override
	{
		const bool Result = MeetControl;
		MeetControl = false;
		return Result;
	}
	// CPHSimpleCharacter::RetriveContactBone.
	virtual u16 ContactBone() override
	{
		IPhysicsShellHolder* Object = Owner.RefObject;
		Fvector Dir;
		DamageInfo.HitDir(Dir);
		if (Object == nullptr || g_B3World == nullptr || fis_zero(Dir.square_magnitude()))
		{
			return 0;
		}

		collide::ray_defs Query(DamageInfo.Point, Dir, Radius(), CDB::OPT_ONLYNEAREST | CDB::OPT_CULL, collide::rqtBoth);
		BoneQuery.r_clear();
		if (g_B3World->ObjectSpace().RayQuery(BoneQuery, Object->ObjectCollisionModel(), Query))
		{
			return u16(BoneQuery.r_results().front().element);
		}

		IKinematics* Kinematics = Object->ObjectKinematics();
		if (Kinematics == nullptr)
		{
			return 0;
		}
		Fvector Local;
		Local.sub(DamageInfo.Point, Object->ObjectPosition());
		Fmatrix Form;
		Form.set(Object->ObjectXFORM());
		Form.transpose();
		Form.transform_dir(Local);
		u16 Bone = 0;
		float Best = flt_max;
		for (u16 I = 0; I < Kinematics->LL_BoneCount(); ++I)
		{
			const float Distance = Kinematics->LL_GetBoneInstance(I).mTransform.c.distance_to_sqr(Local);
			if (Distance < Best)
			{
				Best = Distance;
				Bone = I;
			}
		}
		return Bone;
	}
	virtual void GroundNormal(Fvector& Normal) override
	{
		if (Climbing())
		{
			Elevator.Ladder->DDNorm(Normal);
			return;
		}
		Normal.set(GroundNormalValue);
	}

	virtual void SetObjectContactCallbackData(void* Data) override
	{
		Owner.CallbackData = Data;
	}
	virtual void SetObjectContactCallback(ObjectContactCallbackFun* Callback) override
	{
		Owner.ObjectCallbacks.clear();
		Owner.AddObjectCallback(Callback);
		ObjectCallback = Callback;
		Probe = false;
	}
	virtual void SetWheelContactCallback(ObjectContactCallbackFun* Callback) override {}
	virtual ObjectContactCallbackFun* ObjectContactCallBack() override
	{
		return ObjectCallback;
	}
	virtual void SetVirtualMoveContactCallback() override
	{
		Probe = true;
	}

	virtual void GetVelocity(Fvector& Value) const override
	{
		Value.set(Velocity);
	}
	virtual void GetSavedVelocity(Fvector& Value) override
	{
		Value.set(Enabled ? SavedVelocity : Velocity);
	}
	virtual void GetSmothedVelocity(Fvector& Value) override
	{
		Value.set(LastMove);
	}
	virtual void SetVelocity(Fvector Value) override
	{
		const float Speed = Value.magnitude();
		if (Speed > default_l_limit)
		{
			Value.mul(default_l_limit / Speed);
		}
		Velocity.set(Value);
	}
	virtual void GetPosition(Fvector& Value) override
	{
		Value.set(Position);
	}
	virtual void GetBodyPosition(Fvector& Value) override
	{
		Value.set(BodyPos);
	}
	virtual const Fvector& BodyPosition() const override
	{
		return BodyPos;
	}
	virtual void get_body_position(Fvector& Value) override
	{
		Value.set(BodyPos);
	}
	virtual void GetFootCenter(Fvector& Value) override
	{
		Value.set(Position.x, Position.y + Radius(), Position.z);
	}
	virtual void GetDesiredPosition(Fvector& Value) override
	{
		Value.set(Position);
	}
	virtual void IPosition(Fvector& Value) override
	{
		Value.lerp(PrevPosition, Position, g_B3World ? g_B3World->InterpolationFactor() : 1.f);
	}
	virtual void DeathPosition(Fvector& Value) override
	{
		Value.set(Position);
	}
	virtual void SetPosition(const Fvector& Value) override
	{
		Position.set(Value);
		PrevPosition.set(Value);
		UpdateBodyPosition();
		if (b3Body_IsValid(Body) && !(g_B3World && g_B3World->Processing()))
		{
			B3_SAFE(b3Body_SetTransform(Body, B3Math::ToB3(Position), b3Quat_identity));
			B3_SAFE(b3Body_SetLinearVelocity(Body, b3Vec3_zero));
		}
	}
	// As CPHAICharacter::TryPosition: a pure test against objects (step_single never collides with the
	// level); touching or crossing one hands the move to physics.
	virtual bool TryPosition(Fvector Value, bool Exact) override
	{
		if (!Created || g_B3World == nullptr || ForcedPhysics || JumpState())
		{
			return false;
		}

		Fvector Delta;
		Delta.sub(Value, Position);
		if (fis_zero(Delta.magnitude()) || fis_zero(Device.fTimeDelta))
		{
			return true;
		}

		if (Collision)
		{
			b3QueryFilter Filter = b3DefaultQueryFilter();
			Filter.categoryBits = B3Category::Dynamic;
			Filter.maskBits = B3Category::Static | (DynamicCollision ? (B3Category::Dynamic | B3Category::Character) : 0ull);
			const b3Capsule Capsule = MakeCapsule();
			const b3Vec3 Start = B3Math::ToB3(Position);
			IgnoredCount = 0;
			STry Try{this, false};
			b3World_CollideMover(g_B3World->Id(), Start, &Capsule, Filter, &AnyAccepted, &Try);
			if (Try.Blocked || b3World_CastMover(g_B3World->Id(), Start, &Capsule, B3Math::ToB3(Delta), Filter, &MoverFilter, this) < 1.f)
			{
				return false;
			}
		}

		SetPosition(Value);
		Disable();
		DamageInfo.Velocity = 0.f;
		LastMove.mul(Delta, 1.f / Device.fTimeDelta);
		return true;
	}
	virtual void fix_body_rotation() override {}

	virtual void SetMas(float Value) override
	{
		MassValue = std::max(Value, 1.f);
	}
	virtual float Mass() override
	{
		return MassValue;
	}
	virtual float FootRadius() override
	{
		return Radius();
	}
	virtual float& FrictionFactor() override
	{
		return Friction;
	}
	virtual void SetPhysicsRefObject(IPhysicsShellHolder* Object) override
	{
		Owner.RefObject = Object;
		DamageInfo.Owner = Object;
	}
	virtual IPhysicsShellHolder* PhysicsRefObject() override
	{
		return Owner.RefObject;
	}
	// As ODE: still an obstacle for others, but never moved by physics.
	virtual void SetNonInteractive(bool Value) override
	{
		NonInteractive = Value;
	}
	virtual void NetRelcase(IPhysicsShellHolder* Object) override
	{
		Elevator.NetRelcase(Object);
	}
	virtual const IPhysicsElement* PhysicsElement() const override
	{
		return nullptr;
	}

	virtual void get_State(SPHNetState& State) override
	{
		State.position.set(Position);
		State.previous_position.set(PrevPosition);
		State.linear_vel.set(Velocity);
		State.angular_vel.set(0.f, 0.f, 0.f);
		State.force.set(PendingForce);
		State.torque.set(0.f, 0.f, 0.f);
		State.accel.set(Accel);
		State.max_velocity = MaxVelocity;
		State.enabled = Enabled;
	}
	virtual void set_State(const SPHNetState& State) override
	{
		SetPosition(State.position);
		SetVelocity(State.linear_vel);
		setForce(State.force);
		if (State.enabled)
		{
			Enable();
		}
		else
		{
			Disable();
		}
	}
	virtual void cv2obj_Xfrom(const Fquaternion& Q, const Fvector& Pos, Fmatrix& Xform) override {}
	virtual void cv2bone_Xfrom(const Fquaternion& Q, const Fvector& Pos, Fmatrix& Xform) override {}

	// CPHUpdateObject: move before the Box3D step so the kinematic capsule follows in the same step.
	virtual void PhTune(float Step) override
	{
		if (!Created || Frozen || g_B3World == nullptr || g_B3World->IsFrozen())
		{
			return;
		}
		if (!Enabled || NonInteractive)
		{
			LoseControl = false;
			PushedCount = 0;
			return;
		}

		if (IClimableObject* Found = Collision ? FindLadder() : nullptr)
		{
			SetElevator(Found);
		}
		PrevPosition = Position;
		Move(Step);
		LastMove.sub(Position, PrevPosition).div(Step);
	}

	virtual void PhDataUpdate(float Step) override
	{
		if (!Created || Frozen || g_B3World == nullptr || g_B3World->IsFrozen())
		{
			return;
		}

		ObjectHits();
		if (ExternalImpulse && g_B3World->StepsNum() > ExternalImpulseStopStep)
		{
			ExternalImpulse = false;
			ExternalImpulseStopStep = u64(-1);
			Velocity.set(0.f, 0.f, 0.f);
		}
	}
};

bool B3CharacterPreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB)
{
	return !CB3Character::Pushes(ShapeA, ShapeB) && !CB3Character::Pushes(ShapeB, ShapeA);
}

IPHCharacter* create_ai_character()
{
	return new CB3Character(false);
}

IPHCharacter* create_actor_character(bool single_game)
{
	return new CB3Character(true);
}
