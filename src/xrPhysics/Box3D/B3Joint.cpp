#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"

static constexpr float MaxB3Angle = 0.99f * PI;

// A joint whose other axes stay within this angle behaves as a hinge (about 11 degrees).
static constexpr float FullControlHingeTolerance = 0.2f;

static b3BodyId BodyOf(CPhysicsElement* Element)
{
	CPHElement* E = static_cast<CPHElement*>(Element);
	return E && b3Body_IsValid(E->Body) ? E->Body : b3_nullBodyId;
}

// ODE body_for_joint: a fixed element joins as the world, which reverses the joint.
bool CPHJoint::FirstIsReversed() const
{
	if (FirstIsGround || First == nullptr)
	{
		return true;
	}
	return static_cast<CPHElement*>(First)->isFixed();
}

static bool IsFreeRange(float Low, float High)
{
	return !(High - Low < PI * 2.f);
}

// An engine angle around an axis is the negative angle around the mirrored axis.
static void ToB3Limits(float Low, float High, float& Lower, float& Upper)
{
	Lower = clampr(-High, -MaxB3Angle, MaxB3Angle);
	Upper = clampr(-Low, -MaxB3Angle, MaxB3Angle);
	if (Lower > Upper)
	{
		std::swap(Lower, Upper);
	}
}

// A wheel steers about the bone's local X axis, which both engines keep as is (unlike the
// revolute, whose frame is Z and needs the mirrored sign), so the engine limits are used
// directly here.
static void ToB3SteeringLimits(float Low, float High, float& Lower, float& Upper)
{
	Lower = clampr(Low, -MaxB3Angle, MaxB3Angle);
	Upper = clampr(High, -MaxB3Angle, MaxB3Angle);
	if (Lower > Upper)
	{
		std::swap(Lower, Upper);
	}
}

// Box3D steers with a spring to a target angle, ODE with a velocity motor (dParamVel).
// The spring stiffness sets the torque available against the tire load: on a stand a
// 10 Hz spring is deflected by ~0.25 rad at 1 kN*m and saturates at 3 kN*m, which is why
// a heavy car barely turned the wheel. 60 Hz is as stiff as the base joint constraints
// and keeps the lag within a few degrees even at 10 kN*m.
static constexpr float WheelSteeringHertz = 60.f;
static constexpr float WheelSteeringDampingRatio = 0.7f;

static void MakeFrame(const Fvector& Origin, const Fvector& AxisZ, const Fvector* AxisX, Fmatrix& Frame)
{
	Frame.identity();
	Frame.k.normalize_safe(Fvector(AxisZ));
	if (Frame.k.square_magnitude() < EPS)
	{
		Frame.k.set(0.f, 0.f, 1.f);
	}

	Fvector X;
	if (AxisX)
	{
		X.set(*AxisX);
	}
	else
	{
		X = std::abs(Frame.k.y) < 0.9f ? Fvector().set(0.f, 1.f, 0.f) : Fvector().set(1.f, 0.f, 0.f);
	}
	// Orthogonalize X against Z.
	X.mad(Frame.k, -X.dotproduct(Frame.k));
	if (X.square_magnitude() < EPS)
	{
		X = std::abs(Frame.k.y) < 0.9f ? Fvector().set(0.f, 1.f, 0.f) : Fvector().set(1.f, 0.f, 0.f);
		X.mad(Frame.k, -X.dotproduct(Frame.k));
	}
	Frame.i.normalize(X);
	Frame.j.crossproduct(Frame.k, Frame.i);
	Frame.c.set(Origin);
}

static Fmatrix ElementForm(CPhysicsElement* Element)
{
	Fmatrix Form;
	Element->GetGlobalTransformDynamic(&Form);
	return Form;
}

void CPHJoint::GlobalAnchor(Fvector& Result) const
{
	Result.set(Anchor);
	CPhysicsElement* Element = AnchorVs == vs_second ? Second : (AnchorVs == vs_first ? First : nullptr);
	if (Element)
	{
		ElementForm(Element).transform_tiny(Result);
	}
}

void CPHJoint::GlobalAxis(int Num, Fvector& Result) const
{
	const SAxis& Axis = Axes[clampr(Num, 0, 2)];
	Result.set(Axis.Direction);
	CPhysicsElement* Element = Axis.Vs == vs_second ? Second : (Axis.Vs == vs_first ? First : nullptr);
	if (Element)
	{
		ElementForm(Element).transform_dir(Result);
	}
	Result.normalize_safe();
}

// ODE keeps axis 0 relative to the first body, axis 2 relative to the second one and
// derives axis 1 as ax[2] x ax[0] (amotorComputeGlobalAxes). The same formula is used
// here, otherwise the middle axis of the joint has no meaningful direction.
void CPHJoint::FullControlAxes(Fvector Dir[3]) const
{
	GlobalAxis(0, Dir[0]);
	GlobalAxis(2, Dir[2]);

	Dir[1].crossproduct(Dir[2], Dir[0]);

	if (Dir[1].square_magnitude() < EPS_S)
	{
		// The two explicit axes are parallel: any perpendicular is as good as another.
		Dir[1] = std::abs(Dir[0].y) < 0.9f ? Fvector().set(0.f, 1.f, 0.f) : Fvector().set(1.f, 0.f, 0.f);
		Dir[1].mad(Dir[0], -Dir[1].dotproduct(Dir[0]));
	}

	Dir[1].normalize_safe();
}

u32 CPHJoint::FullControlTwistAxis() const
{
	u32 Best = 0;
	float BestRange = -1.f;

	for (u32 i = 0; i < 3; ++i)
	{
		if (IsFreeRange(Axes[i].Low, Axes[i].High))
			continue;

		const float Range = Axes[i].High - Axes[i].Low;

		if (Range > BestRange)
		{
			BestRange = Range;
			Best = i;
		}
	}

	if (BestRange <= 1e-4f)
	{
		// Only locked axes are left: a free axis has to carry the twist, otherwise the
		// joint would lose the rotation ODE still allows.
		for (u32 i = 0; i < 3; ++i)
		{
			if (IsFreeRange(Axes[i].Low, Axes[i].High))
				return i;
		}
	}

	return Best;
}

float CPHJoint::FullControlCone(u32 TwistAxis) const
{
	float Cone = 0.f;
	bool Limited = false;

	for (u32 i = 0; i < 3; ++i)
	{
		if (i == TwistAxis || IsFreeRange(Axes[i].Low, Axes[i].High))
			continue;

		Limited = true;

		const float Extent = std::max(std::abs(Axes[i].Low), std::abs(Axes[i].High));

		if (Extent > Cone)
			Cone = Extent;
	}

	return Limited ? clampr(Cone, 0.05f, PI) : -1.f;
}

// The joint moves almost only about its widest axis (a knee, an elbow, a finger): such a
// joint is built as a hinge. The spherical swing limits are degenerate when the joint is
// straight (the swing axis is a cross product of two parallel axes), which never settles.
bool CPHJoint::FullControlIsHinge() const
{
	const u32 Twist = FullControlTwistAxis();

	if (IsFreeRange(Axes[Twist].Low, Axes[Twist].High))
	{
		return false;
	}

	for (u32 i = 0; i < 3; ++i)
	{
		if (i == Twist || IsFreeRange(Axes[i].Low, Axes[i].High))
			continue;

		const float Extent = std::max(std::abs(Axes[i].Low), std::abs(Axes[i].High));

		if (Extent > FullControlHingeTolerance)
			return false;
	}

	return true;
}

void CPHJoint::Activate()
{
	Create();
}

void CPHJoint::Create()
{
	bActive = true;
	if (!b3Joint_IsValid(Joint))
	{
		CreateNative();
	}
}

void CPHJoint::Deactivate()
{
	DestroyNative();
	bActive = false;
}

void CPHJoint::DestroyNative()
{
	if (b3Joint_IsValid(Joint))
	{
		b3DestroyJoint(Joint, true);
	}
	Joint = b3_nullJointId;
}

// A joint friction (the field is not initialized by SJointIKData::Reset) must not turn
// the motor into a source of an infinite torque.
static float SanitizeMotor(float Value)
{
	if (!(Value > 0.f) || Value > 1e6f)
	{
		return 0.f;
	}

	return Value;
}

// ODE damps every ragdoll joint with a velocity motor (dParamFMax, dParamVel = 0). Most
// bones have friction 0, so the motor was off and a joint could resonate forever. The
// torque is scaled by the lighter body: enough to kill a resonance, small enough to let
// the limb swing for a second or two before it settles.
float CPHJoint::DefaultJointDamping() const
{
	const float MassA = First ? First->getMass() : 0.f;
	const float MassB = Second ? Second->getMass() : 0.f;
	const float Gravity = g_B3World ? g_B3World->Gravity() : 9.81f;

	return 0.025f * std::min(MassA, MassB) * Gravity;
}

void CPHJoint::ApplyConstraintTuning()
{
	if (!b3Joint_IsValid(Joint))
	{
		return;
	}

	// ODE makes the stops soft with ERP/CFM from the same spring/damping factors; Box3D
	// has a soft constraint, so the factors are mapped onto its stiffness and damping.
	// Softer than the defaults makes a ball joint pump energy (checked on a test rig), so
	// the tuning only stiffens.
	const float Hertz = clampr(60.f * Spring, 60.f, 120.f);
	const float DampingRatio = clampr(2.f * Damping, 2.f, 4.f);

	B3_SAFE(b3Joint_SetConstraintTuning(Joint, Hertz, DampingRatio));
}

void CPHJoint::CreateNative()
{
	if (g_B3World == nullptr)
	{
		return;
	}

	b3BodyId BodyA = BodyOf(First);
	b3BodyId BodyB = BodyOf(Second);
	if (!b3Body_IsValid(BodyB))
	{
		return;
	}
	FirstIsGround = !b3Body_IsValid(BodyA);
	if (FirstIsGround)
	{
		BodyA = g_B3World->GroundBody();
	}

	Fvector AnchorGlobal;
	GlobalAnchor(AnchorGlobal);
	Fvector Axis0, Axis1;
	GlobalAxis(0, Axis0);
	GlobalAxis(1, Axis1);

	Fmatrix Frame;
	if (eType == hinge2)
	{
		// Wheel: suspension and steering along frame X (axis 0), spin along frame Z (axis 1).
		MakeFrame(AnchorGlobal, Axis1, &Axis0, Frame);
	}
	else if (eType == ball)
	{
		Fmatrix SecondForm = ElementForm(Second);
		MakeFrame(AnchorGlobal, SecondForm.k, &SecondForm.i, Frame);
	}
	else if (eType == full_control)
	{
		// The twist goes to the axis with the widest range, the cone covers the rest.
		Fvector Dir[3];
		FullControlAxes(Dir);
		FullControlTwist = u8(FullControlTwistAxis());
		MakeFrame(AnchorGlobal, Dir[FullControlTwist], nullptr, Frame);
	}
	else
	{
		MakeFrame(AnchorGlobal, Axis0, nullptr, Frame);
	}

	const b3Transform World = B3Math::ToB3(Frame);
	b3JointDef Base = {};
	auto FillBase = [&](b3JointDef& Def)
	{
		Def.bodyIdA = BodyA;
		Def.bodyIdB = BodyB;
		Def.localFrameA = b3InvMulTransforms(b3Body_GetTransform(BodyA), World);
		Def.localFrameB = b3InvMulTransforms(b3Body_GetTransform(BodyB), World);
		Def.collideConnected = false;
		Def.forceThreshold = Base.forceThreshold;
		Def.torqueThreshold = Base.torqueThreshold;
		Def.userData = this;
	};

	const float Friction = SanitizeMotor(Axes[0].Force);
	Base.forceThreshold = BreakForce > 0.f ? BreakForce : FLT_MAX;
	Base.torqueThreshold = BreakTorque > 0.f ? BreakTorque : FLT_MAX;
	switch (eType)
	{
	case hinge:
	{
		b3RevoluteJointDef Def = b3DefaultRevoluteJointDef();
		FillBase(Def.base);
		if (!IsFreeRange(Axes[0].Low, Axes[0].High))
		{
			Def.enableLimit = true;
			ToB3Limits(Axes[0].Low, Axes[0].High, Def.lowerAngle, Def.upperAngle);
		}
		Def.enableMotor = Friction > 0.f;
		Def.maxMotorTorque = Friction;
		Def.motorSpeed = -Axes[0].Velocity;
		Joint = b3CreateRevoluteJoint(g_B3World->Id(), &Def);
		break;
	}
	case slider:
	{
		b3PrismaticJointDef Def = b3DefaultPrismaticJointDef();
		FillBase(Def.base);
		Def.enableLimit = Axes[0].High > Axes[0].Low;
		Def.lowerTranslation = Axes[0].Low;
		Def.upperTranslation = Axes[0].High;
		Def.enableMotor = Friction > 0.f;
		Def.maxMotorForce = Friction;
		Def.motorSpeed = Axes[0].Velocity;
		Joint = b3CreatePrismaticJoint(g_B3World->Id(), &Def);
		break;
	}
	case hinge2:
	{
		b3WheelJointDef Def = b3DefaultWheelJointDef();
		FillBase(Def.base);
		Def.enableSuspensionSpring = true;
		Def.suspensionHertz = clampr(4.f * Spring, 0.5f, 20.f);
		Def.suspensionDampingRatio = clampr(0.7f * Damping, 0.1f, 2.f);
		Def.enableSteering = true;
		Def.enableSteeringLimit = !IsFreeRange(Axes[0].Low, Axes[0].High);
		ToB3SteeringLimits(Axes[0].Low, Axes[0].High, Def.lowerSteeringLimit, Def.upperSteeringLimit);
		Def.steeringHertz = WheelSteeringHertz;
		Def.steeringDampingRatio = WheelSteeringDampingRatio;
		Def.maxSteeringTorque = std::max(Axes[0].Force, 1000.f);
		Def.targetSteeringAngle = 0.f;
		// A new native joint measures the steering angle from the current pose.
		SteerTarget = 0.f;
		Def.enableSpinMotor = Axes[1].Force > 0.f;
		Def.maxSpinTorque = Axes[1].Force;
		Def.spinSpeed = -Axes[1].Velocity;
		Joint = b3CreateWheelJoint(g_B3World->Id(), &Def);
		break;
	}
	case ball:
	{
		b3SphericalJointDef Def = b3DefaultSphericalJointDef();
		FillBase(Def.base);

		// ODE ignores the joint force for a ball joint, so a cloth joint stays limp here too.
		Def.enableMotor = false;

		Joint = b3CreateSphericalJoint(g_B3World->Id(), &Def);
		break;
	}
	case full_control:
	{
		const u32 Twist = FullControlTwist;

		// ODE gives every axis the same maximum motor torque.
		const float Authored = SanitizeMotor(std::max(Axes[0].Force, std::max(Axes[1].Force, Axes[2].Force)));
		const float Motor = std::max(Authored, DefaultJointDamping());

		FullControlAsHinge = FullControlIsHinge();

		if (FullControlAsHinge)
		{
			b3RevoluteJointDef Def = b3DefaultRevoluteJointDef();
			FillBase(Def.base);
			Def.enableLimit = true;
			ToB3Limits(Axes[Twist].Low, Axes[Twist].High, Def.lowerAngle, Def.upperAngle);
			Def.enableMotor = Motor > 0.f;
			Def.maxMotorTorque = Motor;
			Def.motorSpeed = -Axes[Twist].Velocity;
			Joint = b3CreateRevoluteJoint(g_B3World->Id(), &Def);
			break;
		}

		b3SphericalJointDef Def = b3DefaultSphericalJointDef();
		FillBase(Def.base);

		if (!IsFreeRange(Axes[Twist].Low, Axes[Twist].High))
		{
			Def.enableTwistLimit = true;
			ToB3Limits(Axes[Twist].Low, Axes[Twist].High, Def.lowerTwistAngle, Def.upperTwistAngle);
		}

		const float Cone = FullControlCone(Twist);
		Def.enableConeLimit = Cone > 0.f;
		Def.coneAngle = (Cone > 0.f) ? Cone : 0.05f;

		Def.enableMotor = Motor > 0.f;
		Def.maxMotorTorque = Motor;

		Joint = b3CreateSphericalJoint(g_B3World->Id(), &Def);
		break;
	}
	}

	ApplyConstraintTuning();
}

void CPHJoint::UpdateMotor()
{
	if (!b3Joint_IsValid(Joint))
	{
		return;
	}

	// Box3D does not wake sleeping bodies on motor changes (ph_door closes a door that fell asleep open).
	B3_SAFE(b3Joint_WakeBodies(Joint));

	switch (eType)
	{
	case hinge:
		B3_SAFE(b3RevoluteJoint_EnableMotor(Joint, Axes[0].Force > 0.f));
		B3_SAFE(b3RevoluteJoint_SetMaxMotorTorque(Joint, Axes[0].Force));
		B3_SAFE(b3RevoluteJoint_SetMotorSpeed(Joint, -Axes[0].Velocity));
		break;
	case slider:
		B3_SAFE(b3PrismaticJoint_EnableMotor(Joint, Axes[0].Force > 0.f));
		B3_SAFE(b3PrismaticJoint_SetMaxMotorForce(Joint, Axes[0].Force));
		B3_SAFE(b3PrismaticJoint_SetMotorSpeed(Joint, Axes[0].Velocity));
		break;
	case hinge2:
	{
		// Box3D steers with a spring to a target angle; ODE drives the axis with a velocity
		// motor. PhTune moves the target at Axes[0].Velocity, here it is only clamped to the
		// current limits and applied.
		float Lower, Upper;
		ToB3SteeringLimits(Axes[0].Low, Axes[0].High, Lower, Upper);
		SteerTarget = clampr(SteerTarget, Lower, Upper);
		B3_SAFE(b3WheelJoint_SetTargetSteeringAngle(Joint, SteerTarget));
		B3_SAFE(b3WheelJoint_SetMaxSteeringTorque(Joint, std::max(Axes[0].Force, 1000.f)));
		B3_SAFE(b3WheelJoint_SetSteeringHertz(Joint, WheelSteeringHertz));
		B3_SAFE(b3WheelJoint_SetSteeringDampingRatio(Joint, WheelSteeringDampingRatio));
		B3_SAFE(b3WheelJoint_SetSuspensionHertz(Joint, clampr(4.f * Spring, 0.5f, 20.f)));
		B3_SAFE(b3WheelJoint_SetSuspensionDampingRatio(Joint, clampr(0.7f * Damping, 0.1f, 2.f)));
		B3_SAFE(b3WheelJoint_EnableSpinMotor(Joint, Axes[1].Force > 0.f));
		B3_SAFE(b3WheelJoint_SetMaxSpinTorque(Joint, Axes[1].Force));
		B3_SAFE(b3WheelJoint_SetSpinMotorSpeed(Joint, -Axes[1].Velocity));
		break;
	}
	case ball:
		// ODE has no motor on a ball joint.
		break;
	case full_control:
	{
		const float Authored = SanitizeMotor(std::max(Axes[0].Force, std::max(Axes[1].Force, Axes[2].Force)));
		const float Motor = std::max(Authored, DefaultJointDamping());

		if (FullControlAsHinge)
		{
			B3_SAFE(b3RevoluteJoint_EnableMotor(Joint, Motor > 0.f));
			B3_SAFE(b3RevoluteJoint_SetMaxMotorTorque(Joint, Motor));
			B3_SAFE(b3RevoluteJoint_SetMotorSpeed(Joint, -Axes[FullControlTwist].Velocity));
			break;
		}

		B3_SAFE(b3SphericalJoint_EnableMotor(Joint, Motor > 0.f));
		B3_SAFE(b3SphericalJoint_SetMaxMotorTorque(Joint, Motor));
		break;
	}
	}
	B3_SAFE(b3Joint_WakeBodies(Joint));
}

void CPHJoint::PhTune(float Step)
{
	if (eType != hinge2 || !b3Joint_IsValid(Joint))
	{
		return;
	}

	// The ODE hinge2 steering motor moves the angle at dParamVel. Box3D has no steering
	// velocity, only a spring to a target angle, so the target is advanced here at the
	// same rate: the authored steering speed is kept and the spring only has to track it.
	float Lower, Upper;
	ToB3SteeringLimits(Axes[0].Low, Axes[0].High, Lower, Upper);
	SteerTarget = clampr(SteerTarget + Axes[0].Velocity * Step, Lower, Upper);
	B3_SAFE(b3WheelJoint_SetTargetSteeringAngle(Joint, SteerTarget));
}

void CPHJoint::SetBreakable(float force, float torque)
{
	BreakForce = force;
	BreakTorque = torque;
	if (b3Joint_IsValid(Joint))
	{
		B3_SAFE(b3Joint_SetForceThreshold(Joint, force > 0.f ? force : FLT_MAX));
		B3_SAFE(b3Joint_SetTorqueThreshold(Joint, torque > 0.f ? torque : FLT_MAX));
	}
}

void CPHJoint::UpdateLimits()
{
	if (!b3Joint_IsValid(Joint))
	{
		return;
	}

	B3_SAFE(b3Joint_WakeBodies(Joint));

	switch (eType)
	{
	case hinge:
	{
		float Lower, Upper;
		ToB3Limits(Axes[0].Low, Axes[0].High, Lower, Upper);
		B3_SAFE(b3RevoluteJoint_EnableLimit(Joint, !IsFreeRange(Axes[0].Low, Axes[0].High)));
		B3_SAFE(b3RevoluteJoint_SetLimits(Joint, Lower, Upper));
		break;
	}
	case slider:
		B3_SAFE(b3PrismaticJoint_EnableLimit(Joint, Axes[0].High > Axes[0].Low));
		B3_SAFE(b3PrismaticJoint_SetLimits(Joint, Axes[0].Low, Axes[0].High));
		break;
	case hinge2:
	{
		// Cars steer by narrowing the limits around the wanted angle.
		float Lower, Upper;
		ToB3SteeringLimits(Axes[0].Low, Axes[0].High, Lower, Upper);
		B3_SAFE(b3WheelJoint_EnableSteeringLimit(Joint, !IsFreeRange(Axes[0].Low, Axes[0].High)));
		B3_SAFE(b3WheelJoint_SetSteeringLimits(Joint, Lower, Upper));
		// Keep the angle the wheel is going to, do not recenter it: the game narrows one
		// limit to bring the wheel back and drives it with the steer velocity.
		SteerTarget = clampr(SteerTarget, Lower, Upper);
		B3_SAFE(b3WheelJoint_SetTargetSteeringAngle(Joint, SteerTarget));
		break;
	}
	case full_control:
	{
		const u32 Twist = FullControlTwist;

		if (FullControlAsHinge)
		{
			float Lower, Upper;
			ToB3Limits(Axes[Twist].Low, Axes[Twist].High, Lower, Upper);
			B3_SAFE(b3RevoluteJoint_EnableLimit(Joint, !IsFreeRange(Axes[Twist].Low, Axes[Twist].High)));
			B3_SAFE(b3RevoluteJoint_SetLimits(Joint, Lower, Upper));
			break;
		}

		if (!IsFreeRange(Axes[Twist].Low, Axes[Twist].High))
		{
			float Lower, Upper;
			ToB3Limits(Axes[Twist].Low, Axes[Twist].High, Lower, Upper);
			B3_SAFE(b3SphericalJoint_EnableTwistLimit(Joint, true));
			B3_SAFE(b3SphericalJoint_SetTwistLimits(Joint, Lower, Upper));
		}
		else
		{
			B3_SAFE(b3SphericalJoint_EnableTwistLimit(Joint, false));
		}

		const float Cone = FullControlCone(Twist);
		B3_SAFE(b3SphericalJoint_EnableConeLimit(Joint, Cone > 0.f));

		if (Cone > 0.f)
		{
			B3_SAFE(b3SphericalJoint_SetConeLimit(Joint, Cone));
		}
		break;
	}
	default:
		break;
	}
	B3_SAFE(b3Joint_WakeBodies(Joint));
}

float CPHJoint::GetAxisAngle(int axis_num)
{
	if (!b3Joint_IsValid(Joint))
	{
		return 0.f;
	}

	switch (eType)
	{
	case hinge:
		// Stops and motor work on -angle. ODE reports the angle negated when the first element
		// has no body or is fixed (reversed joint), and GetLimits negates the limits to match.
		return FirstIsReversed() ? b3RevoluteJoint_GetAngle(Joint) : -b3RevoluteJoint_GetAngle(Joint);
	case slider:
		return b3PrismaticJoint_GetTranslation(Joint);
	case hinge2:
		// The steering axis is the wheel's local X in both engines: no mirrored sign.
		return axis_num == 0 ? b3WheelJoint_GetSteeringAngle(Joint) : 0.f;
	case full_control:
		// Only the twist axis has an angle, the other two are covered by the cone.
		if (axis_num != int(FullControlTwist))
		{
			return 0.f;
		}
		return FullControlAsHinge ? -b3RevoluteJoint_GetAngle(Joint) : -b3SphericalJoint_GetTwistAngle(Joint);
	default:
		return 0.f;
	}
}

float CPHJoint::GetAxisAngleRate(int axis_num)
{
	if (!b3Joint_IsValid(Joint))
	{
		return 0.f;
	}
	if (eType == slider)
	{
		return b3PrismaticJoint_GetSpeed(Joint);
	}
	if (eType == hinge2 && axis_num == 1)
	{
		return -b3WheelJoint_GetSpinSpeed(Joint);
	}
	if (eType == full_control && FullControlAsHinge)
	{
		if (axis_num != int(FullControlTwist))
		{
			return 0.f;
		}

		Fvector Axis;
		GlobalAxis(int(FullControlTwist), Axis);

		Fvector AngularA, AngularB;
		AngularA.set(0.f, 0.f, 0.f);
		AngularB.set(0.f, 0.f, 0.f);

		if (First)
		{
			First->get_AngularVel(AngularA);
		}
		if (Second)
		{
			Second->get_AngularVel(AngularB);
		}

		// The angle is reported negated for full_control.
		return -Fvector().sub(AngularB, AngularA).dotproduct(Axis);
	}

	Fvector AngularA, AngularB, Axis;
	AngularA.set(0.f, 0.f, 0.f);
	AngularB.set(0.f, 0.f, 0.f);
	if (First)
	{
		First->get_AngularVel(AngularA);
	}
	if (Second)
	{
		Second->get_AngularVel(AngularB);
	}
	GlobalAxis(axis_num, Axis);
	return Fvector().sub(AngularB, AngularA).dotproduct(Axis);
}

void CPHJoint::GetAxisDirDynamic(int num, Fvector& axis)
{
	GlobalAxis(num, axis);
}

void CPHJoint::GetAnchorDynamic(Fvector& anchor)
{
	GlobalAnchor(anchor);
}
