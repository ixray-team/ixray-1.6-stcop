#include "stdafx.h"

#include "HairDynamics.h"

static constexpr float LinearStiffness = 90.0f;
static constexpr float LinearDamping = 11.0f;
static constexpr float AngularStiffness = 70.0f;
static constexpr float AngularDamping = 9.0f;
static constexpr float MaxLagDistance = 0.06f;
static constexpr float MaxLagAngle = 0.35f;
static constexpr float TeleportDistance = 1.5f;
static constexpr float MaxStep = 1.0f / 120.0f;
static constexpr float MaxDelta = 0.1f;
static constexpr float Strength = 1.0f;

static constexpr float HeadCrownOffset = 0.2f;

static Fvector Rotate(const Fvector& V, const Fvector& W)
{
	const float Angle = W.magnitude();
	if (Angle < EPS_S)
	{
		return V;
	}

	Fvector Axis = W;
	Axis.div(Angle);

	const float Cos = std::cos(Angle);
	const float Sin = std::sin(Angle);

	Fvector Cross;
	Cross.crossproduct(Axis, V);

	Fvector Result;
	Result.mul(V, Cos);
	Result.mad(Cross, Sin);
	Result.mad(Axis, Axis.dotproduct(V) * (1.0f - Cos));
	return Result;
}

static void RotateBasis(Fmatrix& M, const Fvector& W)
{
	M.i = Rotate(M.i, W);
	M.j = Rotate(M.j, W);
	M.k = Rotate(M.k, W);
}

static void Orthonormalize(Fmatrix& M)
{
	M.i.normalize_safe();
	M.k.crossproduct(M.i, M.j);
	M.k.normalize_safe();
	M.j.crossproduct(M.k, M.i);
	M._14 = 0.0f;
	M._24 = 0.0f;
	M._34 = 0.0f;
	M._44 = 1.0f;
}

static Fvector RotationVector(const Fmatrix& M)
{
	Fvector Raw;
	Raw.set(M.j.z - M.k.y, M.k.x - M.i.z, M.i.y - M.j.x);

	const float SinTwice = Raw.magnitude();
	if (SinTwice < EPS_S)
	{
		return Fvector().set(0.0f, 0.0f, 0.0f);
	}

	const float Cos = (M._11 + M._22 + M._33 - 1.0f) * 0.5f;
	const float Angle = std::atan2(SinTwice * 0.5f, Cos);
	Raw.mul(Angle / SinTwice);
	return Raw;
}

static Fvector RotationBetween(const Fmatrix& From, const Fmatrix& To)
{
	Fmatrix FromRotation = From;
	FromRotation.c.set(0.0f, 0.0f, 0.0f);
	Fmatrix FromInverse;
	FromInverse.transpose(FromRotation);
	Fmatrix Delta;
	Delta.mul_43(To, FromInverse);
	return RotationVector(Delta);
}

static void Store3x4(Fvector4 Dst[3], const Fmatrix& M)
{
	Dst[0].set(M._11, M._21, M._31, M._41);
	Dst[1].set(M._12, M._22, M._32, M._42);
	Dst[2].set(M._13, M._23, M._33, M._43);
}

static void Store(Fvector4& Dst, const Fvector& V, float W)
{
	Dst.set(V.x, V.y, V.z, W);
}

void HairDynamics::Update(const Fmatrix& World, const CBoneInstance* Bones, u16 BoneCount, bool Reset)
{
	if (!IsValidBone(Head, BoneCount))
	{
		SetDisabled();
		return;
	}

	Fmatrix Target;
	Target.mul_43(World, Bones[Head].mTransform);
	Orthonormalize(Target);

	if (Reset || !HasHistory || LagPosition.distance_to(Target.c) > TeleportDistance)
	{
		ResetSprings(Target);
	}

	StepSprings(Target, std::clamp(Device.fTimeDelta, 0.0f, MaxDelta));
	StoreLag(World, Target);
	StoreBody(Bones, BoneCount);
	Data.Params.set(Strength, 0.0f, 0.0f, 1.0f);
}

void HairDynamics::SetDisabled()
{
	HasHistory = false;
	Data = {};
	Store3x4(Data.Lag, Fidentity);
	Store3x4(Data.LagOld, Fidentity);
}

void HairDynamics::ResetSprings(const Fmatrix& Target)
{
	PrevTarget = Target;
	LagRotation = Target;
	LagRotation.c.set(0.0f, 0.0f, 0.0f);
	LagPosition = Target.c;
	LinearVelocity.set(0.0f, 0.0f, 0.0f);
	AngularVelocity.set(0.0f, 0.0f, 0.0f);
	HasHistory = false;
}

void HairDynamics::StepSprings(const Fmatrix& Target, float Delta)
{

	Fvector TargetVelocity = {0.0f, 0.0f, 0.0f};
	Fvector TargetAngularVelocity = {0.0f, 0.0f, 0.0f};
	if (Delta > EPS_S)
	{
		TargetVelocity.sub(Target.c, PrevTarget.c).div(Delta);
		TargetAngularVelocity = RotationBetween(PrevTarget, Target).div(Delta);
	}

	const u32 Steps = std::max(1u, u32(std::ceil(Delta / MaxStep)));
	const float Step = Delta / float(Steps);

	for (u32 Index = 0; Index < Steps; ++Index)
	{
		Fvector StepTarget = PrevTarget.c;
		StepTarget.mad(TargetVelocity, Step * float(Index + 1));

		Fvector Acceleration;
		Acceleration.sub(StepTarget, LagPosition).mul(LinearStiffness);
		Acceleration.mad(Fvector().sub(LinearVelocity, TargetVelocity), -LinearDamping);
		LinearVelocity.mad(Acceleration, Step);
		LagPosition.mad(LinearVelocity, Step);

		Acceleration.mul(RotationBetween(LagRotation, Target), AngularStiffness);
		Acceleration.mad(Fvector().sub(AngularVelocity, TargetAngularVelocity), -AngularDamping);
		AngularVelocity.mad(Acceleration, Step);
		RotateBasis(LagRotation, Fvector().mul(AngularVelocity, Step));
		Orthonormalize(LagRotation);
	}

	PrevTarget = Target;

	Fvector Offset;
	Offset.sub(LagPosition, Target.c);
	const float Distance = Offset.magnitude();
	if (Distance > MaxLagDistance)
	{
		LagPosition.mad(Target.c, Offset, MaxLagDistance / Distance);
		LinearVelocity.lerp(LinearVelocity, TargetVelocity, 0.5f);
	}

	const Fvector Error = RotationBetween(LagRotation, Target);
	const float Angle = Error.magnitude();
	if (Angle > MaxLagAngle)
	{
		RotateBasis(LagRotation, Fvector().mul(Error, (Angle - MaxLagAngle) / Angle));
		Orthonormalize(LagRotation);
		AngularVelocity.lerp(AngularVelocity, TargetAngularVelocity, 0.5f);
	}
}

void HairDynamics::StoreLag(const Fmatrix& World, const Fmatrix& Target)
{

	Fmatrix Lagged = LagRotation;
	Lagged.c = LagPosition;
	Fmatrix TargetInverse;
	TargetInverse.invert_b(Target);
	Fmatrix LagWorld;
	LagWorld.mul_43(Lagged, TargetInverse);
	Fmatrix WorldInverse;
	WorldInverse.invert_b(World);
	Fmatrix LagModel;
	LagModel.mul_43(WorldInverse, Fmatrix().mul_43(LagWorld, World));

	if (HasHistory)
	{
		std::memcpy(Data.LagOld, Data.Lag, sizeof(Data.Lag));
	}

	Store3x4(Data.Lag, LagModel);

	if (!HasHistory)
	{
		std::memcpy(Data.LagOld, Data.Lag, sizeof(Data.Lag));
		HasHistory = true;
	}
}

void HairDynamics::StoreBody(const CBoneInstance* Bones, u16 BoneCount)
{
	const Fvector& HeadPosition = Bones[Head].mTransform.c;

	if (IsValidBone(Neck, BoneCount))
	{
		const Fvector& NeckPosition = Bones[Neck].mTransform.c;
		Fvector Crown;
		Crown.sub(HeadPosition, NeckPosition).normalize_safe();
		Crown.mad(HeadPosition, Crown, HeadCrownOffset);

		Store(Data.HeadBone, HeadPosition, 1.0f);
		Store(Data.HeadLow, NeckPosition, 0.0f);
		Store(Data.HeadHigh, Crown, 0.0f);
	}
	else
	{
		Store(Data.HeadBone, HeadPosition, 0.0f);
		Store(Data.HeadLow, HeadPosition, 0.0f);
		Store(Data.HeadHigh, HeadPosition, 0.0f);
	}

	for (u32 Index = 0; Index < BoxCount; ++Index)
	{
		Fvector4* Box = &Data.Boxes[Index * 4];
		const Fvector& HalfSize = BoxHalfSizes[Index];

		if (!IsValidBone(BodyBones[Index], BoneCount) || HalfSize.magnitude() < EPS_L)
		{
			Store3x4(Box, Fidentity);
			Box[3].set(0.0f, 0.0f, 0.0f, 0.0f);
			continue;
		}

		Fmatrix BoxModel;
		BoxModel.mul_43(Bones[BodyBones[Index]].mTransform, BoxTransforms[Index]);
		Fmatrix ModelToBox;
		ModelToBox.invert_b(BoxModel);

		Store3x4(Box, ModelToBox);
		Store(Box[3], HalfSize, 1.0f);
	}
}
