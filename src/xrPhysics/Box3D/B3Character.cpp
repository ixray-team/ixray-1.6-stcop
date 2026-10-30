#include "StdAfx.h"
#include "IPHCharacter.h"
#include "IColisiondamageInfo.h"
#include "IPhysicsShellHolder.h"
#include "PHUpdateObject.h"
#include "../../xrEngine/GameMtlLib.h"
#include "B3World.h"
#include "B3Math.h"
#include "iclimableobject.h"
#include "../../xrServerEntities/PHNetState.h"
#include "../../xrServerEntities/PHSynchronize.h"
#include "../../xrEngine/xr_object.h"

class CB3CollisionDamageInfo : public ICollisionDamageInfo
{
public:
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector Direction = {0.f, -1.f, 0.f};
	float Velocity = 0.f;
	ALife::EHitType Type = ALife::eHitTypeStrike;
	bool Initiated = false;
	// Only static (fall) damage is reported, and ODE names the character itself as its initiator.
	IPhysicsShellHolder* Owner = nullptr;

	virtual float ContactVelocity() const override
	{
		return Velocity;
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
		return Owner ? Owner->ObjectID() : ALife::INVALID_OBJECT_ID;
	}
	virtual CObject* DamageInitiator() const override
	{
		return smart_cast<CObject*>(Owner);
	}
	virtual ALife::EHitType HitType() const override
	{
		return Type;
	}
	virtual ICollisionHitCallback* HitCallback() const override
	{
		return nullptr;
	}
	virtual void Reinit() override
	{
		Velocity = 0.f;
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

// Character on the Box3D mover (b3World_CollideMover + b3SolvePlanes + b3World_CastMover).
// A kinematic capsule body follows the mover so items and other characters collide with it.
// CPHSynchronize: xrGame reaches the net state through smart_cast<CPHSynchronize*> (CPHMovementControl::GetSyncItem).
class CB3Character :
	public IPHCharacter,
	public CPHSynchronize,
	public CPHUpdateObject
{
	static constexpr int PlaneCapacity = 16;
	static constexpr float StepHeight = 0.35f;
	static constexpr float GroundProbe = 0.2f;
	// cos(45 deg): steeper planes are walls.
	static constexpr float WalkableNormalY = 0.7f;

	SB3GeomOwner Owner;
	CODEGeom Geom;
	b3BodyId Body = b3_nullBodyId;
	b3CollisionPlane Planes[PlaneCapacity];
	b3ShapeId PlaneShapes[PlaneCapacity];
	b3Vec3 PlanePoints[PlaneCapacity];
	int PlaneCount = 0;
	// A walkable plane touched by the last mover pass; also counts as standing on the ground.
	bool WalkableContact = false;
	Fvector WalkableNormal = {0.f, 1.f, 0.f};
	u16 WalkableMaterial = 0;

	CB3CollisionDamageInfo DamageInfo;
	ObjectContactCallbackFun* ObjectCallback = nullptr;
	u16* LastMaterialPtr = nullptr;
	u16 LastMaterial = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector BodyPos = {0.f, 0.f, 0.f};
	Fvector Velocity = {0.f, 0.f, 0.f};
	Fvector SavedVelocity = {0.f, 0.f, 0.f};
	Fvector Accel = {0.f, 0.f, 0.f};
	Fvector PendingImpulse = {0.f, 0.f, 0.f};
	u64 ExternalImpulseStopStep = 0; // as ODE: one external impulse per impulse time
	bool ResetVelocity = false;
	Fvector GroundNormalValue = {0.f, 1.f, 0.f};
	Fvector Size = {0.6f, 1.8f, 0.6f};
	IClimableObject* Ladder = nullptr;
	Fvector CamDir = {0.f, 0.f, 1.f};
	float MaxVelocity = 5.f;
	float JumpUpVelocity = 6.f;
	float AirControl = 0.f;
	float MassValue = 70.f;
	float Friction = 1.f;
	EEnvironment Environment = peInAir;
	bool Created = false;
	bool Enabled = true;
	bool Frozen = false;
	bool Collision = true;
	bool DynamicCollision = true;
	bool ApplyGravity = true;
	bool ForcedPhysics = false;
	bool Jumping = false;
	bool Contact = false;
	// Landing after a jump or fall; read once by ContactWas, as b_meet_control in ODE.
	bool MeetControl = false;
	bool NonInteractive = false;
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

	u64 CollideMask() const
	{
		if (!Collision)
		{
			return 0ull;
		}
		return B3Category::Level | B3Category::Static | (DynamicCollision ? (B3Category::Dynamic | B3Category::Character) : 0ull);
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
		Body = b3CreateBody(g_B3World->Id(), &Def);

		b3ShapeDef ShapeDef = b3DefaultShapeDef();
		ShapeDef.userData = &Geom;
		ShapeDef.enablePreSolveEvents = true;
		ShapeDef.filter.categoryBits = B3Category::Character;
		ShapeDef.filter.maskBits = NonInteractive ? 0ull : B3Category::Dynamic;
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

	// Skips the capsule itself and every other shape of the same object (bone collision, held items).
	static bool MoverFilter(b3ShapeId ShapeId, void* Context)
	{
		CB3Character* Self = static_cast<CB3Character*>(Context);
		if (B3_ID_EQUALS(ShapeId, Self->Geom.Shape))
		{
			return false;
		}

		const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(ShapeId));
		return !(Other && Other->Owner && Self->Owner.RefObject && Other->Owner->RefObject == Self->Owner.RefObject);
	}

	static bool PlaneCallback(b3ShapeId ShapeId, const b3PlaneResult* Results, int Count, void* Context)
	{
		CB3Character* Self = static_cast<CB3Character*>(Context);
		if (!MoverFilter(ShapeId, Context))
		{
			return true;
		}

		// Passable materials do not block.
		CODEGeom* Other = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeId));
		const b3SurfaceMaterial Material = b3Shape_GetSurfaceMaterial(ShapeId);
		const u16 MaterialIdx = Other && Other->Owner ? Other->Owner->Material : u16(Material.userMaterialId);
		if (SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(MaterialIdx); Mtl && Mtl->Flags.test(SGameMtl::flPassable))
		{
			return true;
		}

		const bool Dynamic = b3Body_GetType(b3Shape_GetBody(ShapeId)) == b3_dynamicBody;
		// As ODE: object callbacks decide whether the character touches a dynamic object
		// (a rocket ignores its shooter and must not be pushed at launch).
		if (Dynamic && Count > 0 && b3Shape_IsValid(Self->Geom.Shape))
		{
			const Fvector Point = B3Math::FromB3(b3Add(B3Math::ToB3(Self->Position), Results[0].point));
			const Fvector Normal = B3Math::FromB3(b3Neg(Results[0].plane.normal)); // from the character to the object
			if (!g_B3World->AllowContact(Self->Geom.Shape, ShapeId, Point, Normal))
			{
				return true;
			}
		}
		const bool CanStand = !Self->Jumping && Self->Velocity.y <= 0.5f && Self->Ladder == nullptr;
		for (int I = 0; I < Count && Self->PlaneCount < PlaneCapacity; ++I)
		{
			b3CollisionPlane& Plane = Self->Planes[Self->PlaneCount];
			Plane.plane = Results[I].plane;
			if (!Dynamic && CanStand && Plane.plane.normal.y > WalkableNormalY)
			{
				// Walkable ground and step edges push straight up: no sliding on slopes,
				// and thresholds lift the capsule instead of stopping it.
				Self->WalkableContact = true;
				Self->WalkableNormal = B3Math::FromB3(Plane.plane.normal);
				Self->WalkableMaterial = MaterialIdx;
				Plane.plane.offset /= Plane.plane.normal.y;
				Plane.plane.normal = {0.f, 1.f, 0.f};
			}
			// Dynamic objects are pushed rather than blocking.
			Plane.pushLimit = Dynamic ? 0.02f : FLT_MAX;
			Plane.push = 0.f;
			Plane.clipVelocity = !Dynamic;
			Self->PlaneShapes[Self->PlaneCount] = ShapeId;
			Self->PlanePoints[Self->PlaneCount] = Results[I].point;
			++Self->PlaneCount;
		}
		return true;
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

	void PushDynamicBodies()
	{
		const b3Vec3 MoverVelocity = B3Math::ToB3(Velocity);
		for (int I = 0; I < PlaneCount; ++I)
		{
			const b3BodyId Other = b3Shape_GetBody(PlaneShapes[I]);
			if (b3Body_GetType(Other) != b3_dynamicBody)
			{
				continue;
			}

			const b3Vec3 Point = b3Add(B3Math::ToB3(Position), PlanePoints[I]);
			const b3Vec3 Normal = b3Neg(Planes[I].plane.normal);
			const b3Vec3 R = b3Sub(Point, b3Body_GetWorldCenterOfMass(Other));
			const b3Vec3 RN = b3Cross(R, Normal);
			const float K = b3Body_GetInverseMass(Other) + b3Dot(RN, b3MulMV(b3Body_GetWorldInverseRotationalInertia(Other), RN));
			if (K <= 0.f)
			{
				continue;
			}

			const b3Vec3 PointVelocity = b3Add(b3Body_GetLinearVelocity(Other), b3Cross(b3Body_GetAngularVelocity(Other), R));
			const float Vn = b3Dot(b3Sub(PointVelocity, MoverVelocity), Normal);
			const float Impulse = std::max(-Vn / K, 0.f);
			B3_SAFE(b3Body_ApplyLinearImpulse(Other, b3MulSV(Impulse, Normal), Point, true));
		}
	}

	void Move(float Step)
	{
		const bool Climbing = Ladder != nullptr;

		// Control: horizontal velocity follows the acceleration direction.
		Fvector Control;
		Control.set(Accel.x, 0.f, Accel.z);
		const float ControlMagnitude = Control.magnitude();
		if (ControlMagnitude > 0.1f)
		{
			Control.mul(MaxVelocity / ControlMagnitude);
		}
		else
		{
			Control.set(0.f, 0.f, 0.f);
		}

		const float ControlFactor = (Environment == peOnGround || Climbing) ? 1.f : AirControl;
		const float Blend = std::min(1.f, Step * 10.f * ControlFactor * Friction);
		if (!ForcedPhysics)
		{
			Velocity.x += (Control.x - Velocity.x) * Blend;
			Velocity.z += (Control.z - Velocity.z) * Blend;
		}

		if (Climbing)
		{
			// As CElevatorState::ClimbDirection: pushing towards the ladder climbs,
			// looking up goes up and looking down goes down.
			static constexpr float LookupAngleSine = 0.34202014f;
			const float AccelMagnitude = Accel.magnitude();
			Fvector ToPlain;
			Ladder->DToPlain(this, ToPlain);
			ToPlain.normalize_safe();
			float Climb = AccelMagnitude > 0.1f ? Accel.dotproduct(ToPlain) / AccelMagnitude : 0.f;
			Climb = Climb > EPS_L ? clampr(Climb * (CamDir.y + LookupAngleSine) * 2.f, -1.f, 1.f) : 0.f;
			Velocity.y = Climb * MaxVelocity * 0.5f;
		}
		else if (ApplyGravity && Environment != peOnGround)
		{
			Velocity.y -= g_B3World->Gravity() * Step;
		}

		if (ResetVelocity)
		{
			Velocity.set(0.f, 0.f, 0.f);
			ResetVelocity = false;
		}
		Velocity.add(PendingImpulse);
		PendingImpulse.set(0.f, 0.f, 0.f);

		// Ground probe from the foot sphere.
		const float R = Radius();
		const u64 Mask = CollideMask();
		Fvector Foot;
		Foot.set(Position.x, Position.y + R, Position.z);
		SB3RayHit Ground;
		const bool WasOnGround = Environment == peOnGround;
		const bool GroundHit = Mask != 0 && Velocity.y <= 0.5f && !Jumping &&
			g_B3World->RayPick(Foot, Fvector().set(0.f, -1.f, 0.f), R + (WasOnGround ? StepHeight : GroundProbe), Mask & ~(B3Category::Dynamic | B3Category::Character), Ground) &&
			Ground.Normal.y > 0.5f;

		Fvector Delta;
		Delta.mul(Velocity, Step);
		if (GroundHit && !Climbing)
		{
			// Follow the ground down slopes and up steps.
			Delta.y = Ground.Position.y - Position.y;
		}

		Contact = false;
		if (Mask == 0)
		{
			Position.add(Delta);
		}
		else
		{
			const b3Capsule Capsule = MakeCapsule();
			b3QueryFilter Filter = b3DefaultQueryFilter();
			Filter.categoryBits = B3Category::Dynamic;
			Filter.maskBits = Mask;

			const b3Vec3 Start = B3Math::ToB3(Position);
			b3Vec3 Current = Start;
			const b3Vec3 Target = b3Add(Current, B3Math::ToB3(Delta));
			WalkableContact = false;
			for (int Iteration = 0; Iteration < 5; ++Iteration)
			{
				PlaneCount = 0;
				b3World_CollideMover(g_B3World->Id(), Current, &Capsule, Filter, &PlaneCallback, this);
				if (!g_B3World->IsStepping())
				{
					g_B3World->RunDeferred();
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

			// Step up: when blocked on the ground, lift by StepHeight, move, then drop back down.
			const b3Vec3 Wanted = b3Sub(Target, Start);
			const b3Vec3 Done = b3Sub(Current, Start);
			const float WantedXZ = Wanted.x * Wanted.x + Wanted.z * Wanted.z;
			const float DoneXZ = Done.x * Done.x + Done.z * Done.z;
			if (WasOnGround && !Climbing && Velocity.y <= 0.5f && WantedXZ > 1e-6f && DoneXZ < WantedXZ * 0.81f)
			{
				const b3Vec3 Up = {0.f, StepHeight, 0.f};
				const float UpFraction = b3World_CastMover(g_B3World->Id(), Start, &Capsule, Up, Filter, &MoverFilter, this);
				b3Vec3 Raised = b3Add(Start, b3MulSV(UpFraction, Up));
				const b3Vec3 Forward = {Wanted.x, 0.f, Wanted.z};
				const float ForwardFraction = b3World_CastMover(g_B3World->Id(), Raised, &Capsule, Forward, Filter, &MoverFilter, this);
				Raised = b3Add(Raised, b3MulSV(ForwardFraction, Forward));
				const b3Vec3 Down = {0.f, -(StepHeight * UpFraction + 0.05f), 0.f};
				const float DownFraction = b3World_CastMover(g_B3World->Id(), Raised, &Capsule, Down, Filter, &MoverFilter, this);
				Raised = b3Add(Raised, b3MulSV(DownFraction, Down));

				const b3Vec3 Stepped = b3Sub(Raised, Start);
				SB3RayHit StepGround;
				Fvector StepFoot = B3Math::FromB3(Raised);
				StepFoot.y += R;
				if (DownFraction < 1.f && Stepped.x * Stepped.x + Stepped.z * Stepped.z > DoneXZ &&
					g_B3World->RayPick(StepFoot, Fvector().set(0.f, -1.f, 0.f), R + 0.1f, Mask & ~(B3Category::Dynamic | B3Category::Character), StepGround) &&
					StepGround.Normal.y > 0.5f)
				{
					Current = Raised;
				}
			}

			Contact = PlaneCount > 0;
			if (DynamicCollision && !NonInteractive)
			{
				PushDynamicBodies();
			}
			const Fvector Clipped = B3Math::FromB3(b3ClipVector(B3Math::ToB3(Velocity), Planes, PlaneCount));
			Velocity.x = Clipped.x;
			Velocity.z = Clipped.z;
			if (!GroundHit)
			{
				Velocity.y = Clipped.y;
			}
			Position = B3Math::FromB3(Current);
		}

		const bool OnGround = !Climbing && (GroundHit || WalkableContact);
		if (OnGround)
		{
			if (!WasOnGround)
			{
				// Landing speed feeds fall damage.
				DamageInfo.Velocity = std::max(0.f, -SavedVelocity.y);
				Contact = true;
				MeetControl = true;
			}
			Velocity.y = 0.f;
			Environment = peOnGround;
			GroundNormalValue.set(GroundHit ? Ground.Normal : WalkableNormal);
			LastMaterial = GroundHit ? Ground.Material : WalkableMaterial;
			if (LastMaterialPtr)
			{
				*LastMaterialPtr = LastMaterial;
			}
		}
		else
		{
			// xrGame treats peAtWall as climbing (Actor_Movement mcClimb).
			Environment = (Contact || Climbing) ? peAtWall : peInAir;
			if (Velocity.y < 0.f)
			{
				Jumping = false;
			}
		}

		SavedVelocity.set(Velocity);
		VERIFY(_valid(Position));
		UpdateBodyPosition();

		if (b3Body_IsValid(Body))
		{
			B3_SAFE(b3Body_SetTargetTransform(Body, {B3Math::ToB3(Position), b3Quat_identity}, Step, true));
		}
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
		UpdateBodyPosition();
		CreateBody();
		CPHUpdateObject::Activate();
	}
	virtual void Destroy() override
	{
		CPHUpdateObject::Deactivate();
		DestroyBody();
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
	}
	virtual bool IsEnabled() override
	{
		return Created && Enabled;
	}
	virtual void Freeze() override
	{
		Frozen = true;
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
	}
	virtual void collision_enable() override
	{
		Collision = true;
	}
	virtual void collision_dynamic_disable() override
	{
		DynamicCollision = false;
	}
	virtual void collision_dynamic_enable() override
	{
		DynamicCollision = true;
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
	virtual void SetActorMovable(bool Value) override {}

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
		return GAMEMTL_NONE_IDX;
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
		return nullptr;
	}
	virtual void SetHitType(ALife::EHitType Type) override
	{
		DamageInfo.Type = Type;
	}
	virtual void SetCollisionDamageFactor(float Factor) override {}

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
	virtual void SetForcedPhysicsControl(bool Value) override
	{
		ForcedPhysics = Value;
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
		Accel.set(Value);
	}
	virtual void SetApplyGravity(bool Flag) override
	{
		ApplyGravity = Flag;
	}

	// As CPHSimpleCharacter::ApplyImpulse: an explosion hits many bones, but only the first impulse
	// in 30 steps counts, it replaces the velocity, and it goes down while jumping or out of control.
	virtual void ApplyImpulse(const Fvector& Dir, const float P) override
	{
		static constexpr u64 ImpulseTimeConstant = 30;
		const u64 Step = g_B3World ? g_B3World->StepsNum() : 0;
		if (Step < ExternalImpulseStopStep)
		{
			return;
		}
		ExternalImpulseStopStep = Step + ImpulseTimeConstant;

		Fvector ImpulseDir = Dir;
		if (Jumping || Environment == peInAir)
		{
			ImpulseDir.set(0.f, -1.f, 0.f);
		}
		ResetVelocity = true;
		PendingImpulse.mul(ImpulseDir, P / MassValue);
		Environment = peInAir;
	}
	virtual void ApplyForce(const Fvector& Force) override
	{
		PendingImpulse.mad(Force, fixed_step / MassValue);
	}
	virtual void ApplyForce(const Fvector& Dir, float Force) override
	{
		PendingImpulse.mad(Dir, Force * fixed_step / MassValue);
	}
	virtual void ApplyForce(float X, float Y, float Z) override
	{
		ApplyForce(Fvector().set(X, Y, Z));
	}
	virtual void setForce(const Fvector& Force) override
	{
		ApplyForce(Force);
	}
	// Actor: the argument is the movement acceleration, as in CPHActorCharacter::Jump.
	// AI: the argument is the ready jump velocity, as in CPHAICharacter::Jump.
	virtual void Jump(const Fvector& JumpVelocity) override
	{
		if (!Actor)
		{
			Velocity.set(JumpVelocity);
		}
		else if (Environment == peOnGround || Ladder)
		{
			static constexpr float IncreaseVelocityRate = 1.2f;
			const float AccelMagnitude = std::max(1.f, JumpVelocity.magnitude());
			if (Ladder)
			{
				Fvector Dir;
				Dir.set(Ladder->Norm());
				Velocity.mul(Dir, JumpUpVelocity * 0.5f);
			}
			else
			{
				Velocity.set(Velocity.x * IncreaseVelocityRate + JumpVelocity.x / AccelMagnitude * 0.2f, JumpUpVelocity, Velocity.z * IncreaseVelocityRate + JumpVelocity.z / AccelMagnitude * 0.2f);
			}
		}
		else
		{
			return;
		}

		Jumping = true;
		Environment = peInAir;
		Ladder = nullptr;
	}
	virtual bool JumpState() override
	{
		return Jumping;
	}

	virtual EEnvironment CheckInvironment() override
	{
		return Environment;
	}
	virtual bool ContactWas() override
	{
		const bool Result = MeetControl;
		MeetControl = false;
		return Result;
	}
	virtual u16 ContactBone() override
	{
		return 0;
	}
	virtual void GroundNormal(Fvector& Normal) override
	{
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
	}
	virtual void SetWheelContactCallback(ObjectContactCallbackFun* Callback) override {}
	virtual ObjectContactCallbackFun* ObjectContactCallBack() override
	{
		return ObjectCallback;
	}
	virtual void SetVirtualMoveContactCallback() override {}

	virtual void GetVelocity(Fvector& Value) const override
	{
		Value.set(Velocity);
	}
	virtual void GetSavedVelocity(Fvector& Value) override
	{
		Value.set(SavedVelocity);
	}
	virtual void GetSmothedVelocity(Fvector& Value) override
	{
		Value.set(Velocity);
	}
	virtual void SetVelocity(Fvector Value) override
	{
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
		Value.set(Position);
	}
	virtual void DeathPosition(Fvector& Value) override
	{
		Value.set(Position);
	}
	virtual void SetPosition(const Fvector& Value) override
	{
		Position.set(Value);
		UpdateBodyPosition();
		if (b3Body_IsValid(Body) && !(g_B3World && g_B3World->Processing()))
		{
			B3_SAFE(b3Body_SetTransform(Body, B3Math::ToB3(Position), b3Quat_identity));
		}
	}
	virtual bool TryPosition(Fvector Value, bool Exact) override
	{
		if (g_B3World == nullptr || !Created)
		{
			SetPosition(Value);
			return true;
		}

		// Move there with the mover and report whether it got close.
		Fvector Delta;
		Delta.sub(Value, Position);
		if (Delta.magnitude() < EPS_L)
		{
			return true;
		}

		const Fvector SavedVel = Velocity;
		const Fvector SavedAccel = Accel;
		const bool SavedGravity = ApplyGravity;
		Accel.set(0.f, 0.f, 0.f);
		Velocity.div(Delta, fixed_step);
		ApplyGravity = false;
		Move(fixed_step);
		ApplyGravity = SavedGravity;
		Velocity = SavedVel;
		Accel = SavedAccel;
		return Position.distance_to(Value) < (Exact ? EPS_L : 0.05f);
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
	virtual void SetNonInteractive(bool Value) override
	{
		NonInteractive = Value;
		Geom.SetMask(Value ? 0ull : B3Category::Dynamic);
	}
	virtual void NetRelcase(IPhysicsShellHolder* Object) override {}
	virtual const IPhysicsElement* PhysicsElement() const override
	{
		return nullptr;
	}

	virtual void get_State(SPHNetState& State) override
	{
		State.position.set(Position);
		State.previous_position.set(Position);
		State.linear_vel.set(Velocity);
		State.angular_vel.set(0.f, 0.f, 0.f);
		State.force.set(0.f, 0.f, 0.f);
		State.torque.set(0.f, 0.f, 0.f);
		State.accel.set(Accel);
		State.max_velocity = MaxVelocity;
		State.enabled = Enabled;
	}
	virtual void set_State(const SPHNetState& State) override
	{
		SetPosition(State.position);
		Velocity.set(State.linear_vel);
		Enabled = State.enabled;
	}
	virtual void cv2obj_Xfrom(const Fquaternion& Q, const Fvector& Pos, Fmatrix& Xform) override {}
	virtual void cv2bone_Xfrom(const Fquaternion& Q, const Fvector& Pos, Fmatrix& Xform) override {}

	// CPHUpdateObject: move before the Box3D step so the kinematic capsule follows in the same step.
	virtual void PhTune(float Step) override
	{
		if (!Created || !Enabled || Frozen || g_B3World == nullptr)
		{
			return;
		}

		IClimableObject* const WasLadder = Ladder;
		Ladder = Collision ? FindLadder() : nullptr;
		if (Ladder != WasLadder && Actor)
		{
			Msg("* [Box3D] character ladder %s", Ladder ? "found" : "lost");
		}
		if (Ladder && !WasLadder && Environment == peInAir)
		{
			MeetControl = true;
		}
		Move(Step);
	}

	virtual void PhDataUpdate(float Step) override {}
};

IPHCharacter* create_ai_character()
{
	return new CB3Character(false);
}

IPHCharacter* create_actor_character(bool single_game)
{
	return new CB3Character(true);
}
