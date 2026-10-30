#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"

static constexpr float MaxB3Angle = 0.99f * PI;

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

	const float Friction = Axes[0].Force;
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
		ToB3Limits(Axes[0].Low, Axes[0].High, Def.lowerSteeringLimit, Def.upperSteeringLimit);
		Def.maxSteeringTorque = std::max(Axes[0].Force, 1000.f);
		Def.targetSteeringAngle = 0.f;
		Def.enableSpinMotor = Axes[1].Force > 0.f;
		Def.maxSpinTorque = Axes[1].Force;
		Def.spinSpeed = -Axes[1].Velocity;
		Joint = b3CreateWheelJoint(g_B3World->Id(), &Def);
		break;
	}
	case ball:
	case full_control:
	{
		b3SphericalJointDef Def = b3DefaultSphericalJointDef();
		FillBase(Def.base);
		if (eType == full_control)
		{
			if (!IsFreeRange(Axes[0].Low, Axes[0].High))
			{
				Def.enableTwistLimit = true;
				ToB3Limits(Axes[0].Low, Axes[0].High, Def.lowerTwistAngle, Def.upperTwistAngle);
			}

			float Cone = 0.f;
			bool ConeLimited = false;
			for (int I = 1; I < 3; ++I)
			{
				if (!IsFreeRange(Axes[I].Low, Axes[I].High))
				{
					ConeLimited = true;
					Cone = std::max(Cone, std::max(std::abs(Axes[I].Low), std::abs(Axes[I].High)));
				}
			}
			Def.enableConeLimit = ConeLimited;
			Def.coneAngle = clampr(Cone, 0.05f, PI);
		}
		Def.enableMotor = Friction > 0.f;
		Def.maxMotorTorque = Friction;
		Joint = b3CreateSphericalJoint(g_B3World->Id(), &Def);
		break;
	}
	}
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
		// Box3D steers with a spring to a target angle: the steer velocity picks the limit to turn to.
		float Lower, Upper;
		ToB3Limits(Axes[0].Low, Axes[0].High, Lower, Upper);
		float Target = clampr(b3WheelJoint_GetSteeringAngle(Joint), Lower, Upper);
		if (Axes[0].Velocity > EPS)
		{
			Target = Lower;
		}
		else if (Axes[0].Velocity < -EPS)
		{
			Target = Upper;
		}
		B3_SAFE(b3WheelJoint_SetTargetSteeringAngle(Joint, Target));
		B3_SAFE(b3WheelJoint_SetMaxSteeringTorque(Joint, std::max(Axes[0].Force, 1000.f)));
		B3_SAFE(b3WheelJoint_SetSuspensionHertz(Joint, clampr(4.f * Spring, 0.5f, 20.f)));
		B3_SAFE(b3WheelJoint_SetSuspensionDampingRatio(Joint, clampr(0.7f * Damping, 0.1f, 2.f)));
		B3_SAFE(b3WheelJoint_EnableSpinMotor(Joint, Axes[1].Force > 0.f));
		B3_SAFE(b3WheelJoint_SetMaxSpinTorque(Joint, Axes[1].Force));
		B3_SAFE(b3WheelJoint_SetSpinMotorSpeed(Joint, -Axes[1].Velocity));
		break;
	}
	case ball:
	case full_control:
		B3_SAFE(b3SphericalJoint_EnableMotor(Joint, Axes[0].Force > 0.f));
		B3_SAFE(b3SphericalJoint_SetMaxMotorTorque(Joint, Axes[0].Force));
		break;
	}
	B3_SAFE(b3Joint_WakeBodies(Joint));
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

	float Lower, Upper;
	ToB3Limits(Axes[0].Low, Axes[0].High, Lower, Upper);
	switch (eType)
	{
	case hinge:
		B3_SAFE(b3RevoluteJoint_EnableLimit(Joint, !IsFreeRange(Axes[0].Low, Axes[0].High)));
		B3_SAFE(b3RevoluteJoint_SetLimits(Joint, Lower, Upper));
		break;
	case slider:
		B3_SAFE(b3PrismaticJoint_EnableLimit(Joint, Axes[0].High > Axes[0].Low));
		B3_SAFE(b3PrismaticJoint_SetLimits(Joint, Axes[0].Low, Axes[0].High));
		break;
	case hinge2:
		// Cars steer by narrowing the limits around the wanted angle.
		B3_SAFE(b3WheelJoint_EnableSteeringLimit(Joint, !IsFreeRange(Axes[0].Low, Axes[0].High)));
		B3_SAFE(b3WheelJoint_SetSteeringLimits(Joint, Lower, Upper));
		B3_SAFE(b3WheelJoint_SetTargetSteeringAngle(Joint, (Lower + Upper) * 0.5f));
		break;
	case full_control:
		B3_SAFE(b3SphericalJoint_EnableTwistLimit(Joint, !IsFreeRange(Axes[0].Low, Axes[0].High)));
		B3_SAFE(b3SphericalJoint_SetTwistLimits(Joint, Lower, Upper));
		break;
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
		return axis_num == 0 ? -b3WheelJoint_GetSteeringAngle(Joint) : 0.f;
	case full_control:
		return axis_num == 0 ? -b3SphericalJoint_GetTwistAngle(Joint) : 0.f;
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
