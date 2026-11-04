#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "console_vars.h"
#include "../../xrCore/Kernel/EngineExternal.h"

static constexpr float MaxB3Angle = 0.99f * PI;

// A joint whose other axes stay within this angle behaves as a hinge (about 2 degrees).
static constexpr float FullControlHingeTolerance = 0.03f;

// ODE CPHFracture compares the torque against break_torque times this.
static constexpr float FractureTorqueFactor = 10000000.f;

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

// ODE CalcAxis: a range set at creation is shifted to contain the bind pose.
static void NormalizeOdeRange(float& Low, float& High)
{
	if (Low < -PI)
	{
		High -= Low + PI;
		Low = -PI;
	}
	if (Low > 0.f)
	{
		High -= Low;
		Low = 0.f;
	}
	if (High > PI)
	{
		Low -= High - PI;
		High = PI;
	}
	if (High < 0.f)
	{
		Low -= High;
		High = 0.f;
	}
}

// ODE breaks on |value|^2 > limit^2 / ph_break_common_factor; a zero factor never breaks.
static float BreakThreshold(float Value)
{
	return _valid(Value) ? std::abs(Value) : FLT_MAX;
}

// ODE hinge2 suspension is an absolute spring of 20000*sf N/m and a damper of 1000*df N*s/m;
// Box3D scales its soft spring by the effective mass.
static void WheelSuspension(b3BodyId BodyA, b3BodyId BodyB, float Spring, float Damping, float& Hertz, float& DampingRatio)
{
	const float MassA = b3Body_GetMass(BodyA);
	const float MassB = b3Body_GetMass(BodyB);
	const float Mass = std::max(MassA > 0.f ? MassA * MassB / (MassA + MassB) : MassB, EPS_L);
	const float K = std::max(20000.f * Spring, EPS_L);
	Hertz = clampr(_sqrt(K / Mass) / (2.f * PI), 0.1f, 60.f);
	DampingRatio = clampr(1000.f * Damping / (2.f * _sqrt(K * Mass)), 0.f, 50.f);
}

// Rodrigues in engine numbers, the space ODE measures its Euler angles in.
static void RotateFrame(Fmatrix& Frame, const Fvector& Axis, float Angle)
{
	const float C = std::cos(Angle);
	const float S = std::sin(Angle);
	for (Fvector* V : {&Frame.i, &Frame.j, &Frame.k})
	{
		Fvector Cross;
		Cross.crossproduct(Axis, *V);
		const float Along = Axis.dotproduct(*V) * (1.f - C);
		V->mul(C).mad(Cross, S).mad(Axis, Along);
	}
}

// An engine angle around an axis is the negative angle around the mirrored axis.
static void ToB3Limits(float Low, float High, float& Lower, float& Upper)
{
	Lower = std::clamp(-High, -MaxB3Angle, MaxB3Angle);
	Upper = std::clamp(-Low, -MaxB3Angle, MaxB3Angle);
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
	Lower = std::clamp(Low, -MaxB3Angle, MaxB3Angle);
	Upper = std::clamp(High, -MaxB3Angle, MaxB3Angle);
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

// At creation ODE CalcAxis shifts every range to contain the bind pose; later changes apply raw.
void CPHJoint::AxisRanges(float Low[3], float High[3], bool Creation) const
{
	for (u32 i = 0; i < 3; ++i)
	{
		Low[i] = Axes[i].Low;
		High[i] = Axes[i].High;
		if (Creation)
		{
			NormalizeOdeRange(Low[i], High[i]);
		}
	}
}

// The widest range carries the twist: a free axis when there is one, so the cone covers the
// limited ones, otherwise the main bend, whose asymmetric limits the twist keeps exactly.
static u32 FullControlTwistAxis(const float Low[3], const float High[3])
{
	u32 Best = 0;
	for (u32 i = 1; i < 3; ++i)
	{
		if (High[i] - Low[i] > High[Best] - Low[Best])
		{
			Best = i;
		}
	}
	return Best;
}

// One circular cone stands for the two other ODE Euler stops: half of the wider range around
// the centre of both. A free axis must not be capped by the other one, so it disables the cone.
static float FullControlCone(u32 Twist, const float Low[3], const float High[3])
{
	float Cone = 0.f;
	for (u32 i = 0; i < 3; ++i)
	{
		if (i == Twist)
		{
			continue;
		}
		if (IsFreeRange(Low[i], High[i]))
		{
			return -1.f;
		}
		Cone = std::max(Cone, 0.5f * (High[i] - Low[i]));
	}
	return clampr(Cone, 0.05f, 0.5f * PI);
}

// The joint moves almost only about its widest axis (a knee, an elbow, a finger): such a
// joint is built as a hinge. The spherical swing limits are degenerate when the joint is
// straight (the swing axis is a cross product of two parallel axes), which never settles.
static bool FullControlIsHinge(u32 Twist, const float Low[3], const float High[3])
{
	if (IsFreeRange(Low[Twist], High[Twist]))
	{
		return false;
	}
	for (u32 i = 0; i < 3; ++i)
	{
		if (i != Twist && (IsFreeRange(Low[i], High[i]) || std::max(std::abs(Low[i]), std::abs(High[i])) > FullControlHingeTolerance))
		{
			return false;
		}
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

	float Low[3], High[3];
	AxisRanges(Low, High, true);

	Fvector AnchorGlobal;
	GlobalAnchor(AnchorGlobal);
	Fvector Axis0, Axis1;
	GlobalAxis(0, Axis0);
	GlobalAxis(1, Axis1);

	Fmatrix Frame;
	Fvector Dir[3];
	float Cone = -1.f;
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
	else if (eType == slider)
	{
		// Box3D slides along frame X. ODE CreateSlider: the axis of the first body, or of the second
		// when the first is fixed (the position is then measured the other way), inverted in SoC.
		Axes[0].Vs = Axes[1].Vs = FirstIsReversed() ? vs_second : vs_first;
		GlobalAxis(0, Axis0);
		if (EngineExternal().ShadowOfChernobylMode() != FirstIsReversed())
		{
			Axis0.invert();
		}
		Fvector Normal;
		Normal.crossproduct(Axis0, std::abs(Axis0.y) < 0.9f ? Fvector().set(0.f, 1.f, 0.f) : Fvector().set(1.f, 0.f, 0.f));
		MakeFrame(AnchorGlobal, Normal, &Axis0, Frame);
	}
	else if (eType == full_control)
	{
		FullControlAxes(Dir);
		FullControlTwist = u8(FullControlTwistAxis(Low, High));
		FullControlAsHinge = FullControlIsHinge(FullControlTwist, Low, High);
		Cone = FullControlAsHinge ? -1.f : FullControlCone(FullControlTwist, Low, High);
		MakeFrame(AnchorGlobal, Dir[FullControlTwist], nullptr, Frame);
	}
	else
	{
		MakeFrame(AnchorGlobal, Axis0, nullptr, Frame);
	}

	// The cone is centred on the middle of the two other ODE ranges: frame A turns to the child
	// pose there, the Euler rotation R0 * R1 * R2 about the bind axes by minus each centre (ODE
	// angles are the parent relative to the child); frame B stays at the bind pose.
	Fmatrix FrameA = Frame;
	for (int i = 2; Cone > 0.f && i >= 0; --i)
	{
		if (i != FullControlTwist)
		{
			RotateFrame(FrameA, Dir[i], -0.5f * (Low[i] + High[i]));
		}
	}

	const b3Transform World = B3Math::ToB3(Frame);
	const b3Transform WorldA = B3Math::ToB3(FrameA);
	auto FillBase = [&](b3JointDef& Def)
	{
		Def.bodyIdA = BodyA;
		Def.bodyIdB = BodyB;
		Def.localFrameA = b3InvMulTransforms(b3Body_GetTransform(BodyA), WorldA);
		Def.localFrameB = b3InvMulTransforms(b3Body_GetTransform(BodyB), World);
		Def.collideConnected = false;
		Def.forceThreshold = ForceThreshold;
		Def.torqueThreshold = TorqueThreshold;
		Def.userData = this;
	};

	const float Friction = SanitizeMotor(Axes[0].Force);
	switch (eType)
	{
	case hinge:
	{
		b3RevoluteJointDef Def = b3DefaultRevoluteJointDef();
		FillBase(Def.base);
		if (!IsFreeRange(Low[0], High[0]))
		{
			Def.enableLimit = true;
			ToB3Limits(Low[0], High[0], Def.lowerAngle, Def.upperAngle);
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
		WheelSuspension(BodyA, BodyB, Spring, Damping, Def.suspensionHertz, Def.suspensionDampingRatio);
		Def.enableSteering = true;
		Def.enableSteeringLimit = !IsFreeRange(Low[0], High[0]);
		ToB3SteeringLimits(Low[0], High[0], Def.lowerSteeringLimit, Def.upperSteeringLimit);
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
		if (Weld)
		{
			b3WeldJointDef Def = b3DefaultWeldJointDef();
			FillBase(Def.base);
			Joint = b3CreateWeldJoint(g_B3World->Id(), &Def);
			break;
		}

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
		const float Motor = SanitizeMotor(std::max(Axes[0].Force, std::max(Axes[1].Force, Axes[2].Force)));

		if (FullControlAsHinge)
		{
			b3RevoluteJointDef Def = b3DefaultRevoluteJointDef();
			FillBase(Def.base);
			Def.enableLimit = true;
			ToB3Limits(Low[Twist], High[Twist], Def.lowerAngle, Def.upperAngle);
			Def.enableMotor = Motor > 0.f;
			Def.maxMotorTorque = Motor;
			Def.motorSpeed = -Axes[Twist].Velocity;
			Joint = b3CreateRevoluteJoint(g_B3World->Id(), &Def);
			break;
		}

		b3SphericalJointDef Def = b3DefaultSphericalJointDef();
		FillBase(Def.base);

		if (!IsFreeRange(Low[Twist], High[Twist]))
		{
			Def.enableTwistLimit = true;
			ToB3Limits(Low[Twist], High[Twist], Def.lowerTwistAngle, Def.upperTwistAngle);
		}

		Def.enableConeLimit = Cone > 0.f;
		Def.coneAngle = (Cone > 0.f) ? Cone : 0.05f;

		Def.enableMotor = Motor > 0.f;
		Def.maxMotorTorque = Motor;

		Joint = b3CreateSphericalJoint(g_B3World->Id(), &Def);
		break;
	}
	}
}

void CPHJoint::UpdateMotor(bool Wake)
{
	if (!b3Joint_IsValid(Joint))
	{
		return;
	}

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
		float Hertz, DampingRatio;
		WheelSuspension(b3Joint_GetBodyA(Joint), b3Joint_GetBodyB(Joint), Spring, Damping, Hertz, DampingRatio);
		B3_SAFE(b3WheelJoint_SetSuspensionHertz(Joint, Hertz));
		B3_SAFE(b3WheelJoint_SetSuspensionDampingRatio(Joint, DampingRatio));
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
		const float Motor = SanitizeMotor(std::max(Axes[0].Force, std::max(Axes[1].Force, Axes[2].Force)));

		if (FullControlAsHinge)
		{
			B3_SAFE(b3RevoluteJoint_EnableMotor(Joint, Motor > 0.f));
			B3_SAFE(b3RevoluteJoint_SetMaxMotorTorque(Joint, Motor));
			B3_SAFE(b3RevoluteJoint_SetMotorSpeed(Joint, -Axes[FullControlTwist].Velocity));
			break;
		}

		B3_SAFE(b3SphericalJoint_EnableMotor(Joint, Motor > 0.f));
		B3_SAFE(b3SphericalJoint_SetMaxMotorTorque(Joint, Motor));
		B3_SAFE(b3SphericalJoint_SetMotorVelocity(Joint, b3Vec3_zero));
		break;
	}
	}

	// Box3D does not wake sleeping bodies on motor changes (ph_door closes a door that fell asleep open).
	if (Wake)
	{
		B3_SAFE(b3Joint_WakeBodies(Joint));
	}
}

// ODE set_JointResistance: SetForce and SetVelocity(0) on every axis, which never enable the
// shell. Corpses call it every frame, so only a real change reaches the native motor.
void CPHJoint::SetResistance(float Force)
{
	if (eType == ball || (std::abs(Force - Axes[0].Force) < 1e-3f && Axes[0].Velocity == 0.f))
	{
		return;
	}
	for (SAxis& Axis : Axes)
	{
		Axis.Force = Force;
		Axis.Velocity = 0.f;
	}
	UpdateMotor(false);
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

// ODE CPHJointDestroyInfo checks the forces and the torques against break_force alone;
// break_torque is never compared.
void CPHJoint::SetBreakable(float force, float torque)
{
	Breakable = true;
	ForceThreshold = TorqueThreshold = BreakThreshold(force / _sqrt(ph_console::phBreakCommonFactor));
	if (b3Joint_IsValid(Joint))
	{
		B3_SAFE(b3Joint_SetForceThreshold(Joint, ForceThreshold));
		B3_SAFE(b3Joint_SetTorqueThreshold(Joint, TorqueThreshold));
	}
}

// ODE CPHFracture: breaks at force * ph_break_common_factor > break_force, and at torque *
// ph_break_common_factor > break_torque * FractureTorqueFactor. Set before activation.
void CPHJoint::SetFracture(float Force, float Torque)
{
	Weld = true;
	Breakable = true;
	ForceThreshold = BreakThreshold(Force / ph_console::phBreakCommonFactor);
	TorqueThreshold = BreakThreshold(Torque * FractureTorqueFactor / ph_console::phBreakCommonFactor);
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

		float Low[3], High[3];
		AxisRanges(Low, High, false);
		const float Cone = FullControlCone(Twist, Low, High);
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
	const float Rate = Fvector().sub(AngularB, AngularA).dotproduct(Axis);
	// ODE dJointGetHingeAngleRate: axis.(w1 - w2), negated for a reversed joint.
	return eType == hinge && !FirstIsReversed() ? -Rate : Rate;
}

void CPHJoint::GetAxisDirDynamic(int num, Fvector& axis)
{
	GlobalAxis(num, axis);
}

void CPHJoint::GetAnchorDynamic(Fvector& anchor)
{
	GlobalAnchor(anchor);
}
