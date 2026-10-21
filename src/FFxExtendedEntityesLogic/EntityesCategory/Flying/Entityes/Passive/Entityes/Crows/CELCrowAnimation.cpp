#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowAnimation.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "Include/xrRender/Kinematics.h"
#include "xrEngine/bone.h"
#include "xrGame/Level.h"

// Связывает позы, клевание и костные поправки вороны с необходимыми службами и состоянием владельца.
CELCrowAnimation::CELCrowAnimation(CBaseMonster& OwnerMonster) : Owner(OwnerMonster)
{
	BodyMotion.identity();
	BodyInverse.identity();
	NeckParent.identity();
}
// Завершает позы, клевание и костные поправки вороны и освобождает связанные ресурсы.
CELCrowAnimation::~CELCrowAnimation()
{
	Detach();
}

// Подключает владельца к службам расширения и создаёт начальное состояние.
void CELCrowAnimation::Attach()
{
	Detach();
	Settings = &CELCrowSettings::Read(Owner.cNameSect().c_str());
	Skeleton = Owner.Visual() ? Owner.Visual()->dcast_PKinematics() : nullptr;
	if (!Skeleton || !Owner.g_Alive())
	{
		return;
	}
	Bones.resize(Skeleton->LL_BoneCount());
	u16 BoneIdentifier;
	SBone* CurrentBone;
	CBoneInstance* Instance;
	for (BoneIdentifier = 0; BoneIdentifier < Skeleton->LL_BoneCount(); ++BoneIdentifier)
	{
		CurrentBone = &Bones[BoneIdentifier];
		CurrentBone->Animation = this;
		CurrentBone->Identifier = BoneIdentifier;
		CurrentBone->Parent = Skeleton->GetBoneData(BoneIdentifier).GetParentID();
		CurrentBone->Resting = Skeleton->GetBoneData(BoneIdentifier).get_bind_transform();
		CurrentBone->Solved = CurrentBone->Resting;
		CurrentBone->Unscaled.identity();
	}
	ConfigureWings();
	NeckIdentifier = Skeleton->LL_BoneID(Settings->Pose.NeckName);
	HeadIdentifier = Skeleton->LL_BoneID(Settings->Pose.HeadName);
	BodyIdentifier = Skeleton->LL_BoneID(Settings->Pose.BodyName);
	TailIdentifier = Skeleton->LL_BoneID(Settings->Pose.TailName);
	u32 LegIndex;
	u16 LegIdentifier;
	bool HasFootAnchors = BodyIdentifier != u16(-1);
	for (LegIndex = 0; LegIndex < 2; ++LegIndex)
	{
		LegIdentifier = Skeleton->LL_BoneID(Settings->Pose.LegNames[LegIndex]);
		if (LegIdentifier == u16(-1) || Bones[LegIdentifier].Parent != BodyIdentifier || Skeleton->LL_GetBoneInstance(LegIdentifier).callback())
		{
			HasFootAnchors = false;
		}
		else
		{
			Bones[LegIdentifier].IsFootAnchor = true;
		}
	}
	if (!HasFootAnchors)
	{
		BodyIdentifier = u16(-1);
	}
	for (BoneIdentifier = 0; BoneIdentifier < Skeleton->LL_BoneCount(); ++BoneIdentifier)
	{
		Instance = &Skeleton->LL_GetBoneInstance(BoneIdentifier);
		if (!Instance->callback())
		{
			Instance->set_callback(bctCustom, BoneCallback, &Bones[BoneIdentifier]);
		}
	}
	IdlePhase = Random.randF(0.f, PI_MUL_2);
	LookRemaining = Settings->Pose.LookInterval.Sample();
	LookHoldRemaining = 0.f;
	RuffleRemaining = Settings->Pose.RuffleInterval.Sample();
	Skeleton->CalculateBones_Invalidate();
}
// Находит кости крыльев и задаёт параметры их визуальных поправок.
void CELCrowAnimation::ConfigureWings()
{
	u32 WingIndex;
	u16 BoneIdentifier;
	SBone* CurrentBone;
	Fvector Angles;
	Fvector Translation;
	for (WingIndex = 0; WingIndex < 8; ++WingIndex)
	{
		if (!Settings->Pose.WingNames[WingIndex].size())
		{
			continue;
		}
		BoneIdentifier = Skeleton->LL_BoneID(Settings->Pose.WingNames[WingIndex]);
		if (BoneIdentifier == u16(-1))
		{
			continue;
		}
		CurrentBone = &Bones[BoneIdentifier];
		CurrentBone->IsWing = true;
		CurrentBone->WingStage = WingIndex % 4;
		CurrentBone->WingSide = WingIndex < 4 ? -1 : 1;
		Angles = Settings->Pose.WingAngles[WingIndex % 4];
		if (WingIndex >= 4)
		{
			Angles.x = -Angles.x;
			Angles.z = -Angles.z;
		}
		Translation = CurrentBone->Resting.c;
		CurrentBone->Resting.setXYZi(Angles);
		CurrentBone->Resting.c = Translation;
	}
}
// Снимает управление расширения и освобождает регистрации владельца.
void CELCrowAnimation::Detach()
{
	xr_vector<SBone>::iterator Bone;
	CBoneInstance* Instance;
	if (Skeleton)
	{
		for (Bone = Bones.begin(); Bone != Bones.end(); ++Bone)
		{
			Instance = &Skeleton->LL_GetBoneInstance(Bone->Identifier);
			if (Instance->callback_param() == &*Bone)
			{
				Instance->reset_callback();
			}
		}
	}
	Bones.clear();
	Skeleton = nullptr;
	PoseWeight = 0.f;
	PeckPhase = -1.f;
	HasContact = HasSolvedPeck = IsGrounded = false;
}
// Переключает наземное состояние и связанные визуальные поправки.
void CELCrowAnimation::SetGrounded(bool Grounded, float CurrentHopHeight)
{
	if (IsGrounded != Grounded)
	{
		CELFlyingEntityBase* Entity = smart_cast<CELFlyingEntityBase*>(&Owner);
		if (Entity)
		{
			Entity->SetFlightGrounded(Grounded);
		}
	}
	if (Grounded && !IsGrounded && Settings)
	{
		SettleRemaining = Settings->Pose.SettleDuration;
	}
	IsGrounded = Grounded;
	HopHeight = CurrentHopHeight;
	if (!Grounded)
	{
		PeckPhase = -1.f;
		HasSolvedPeck = HasContact = false;
	}
}
// Рассчитывает текущую интенсивность позы клевания.
float CELCrowAnimation::PeckWeight() const
{
	if (PeckPhase < 0.f)
	{
		return 0.f;
	}
	float Weight = std::min(PeckPhase, 1.f - PeckPhase) / .35f;
	clamp(Weight, 0.f, 1.f);
	return .5f - .5f * std::cos(PI * Weight);
}
// Возвращает направление выноса клюва в текущей позе.
void CELCrowAnimation::PeckForward(Fmatrix& NeckModel, Fmatrix& HeadModel) const
{
	NeckModel.mul_43(NeckParent, Bones[NeckIdentifier].Solved);
	HeadModel.mul_43(NeckModel, Bones[HeadIdentifier].Solved);
}
// Рассчитывает костные поправки для достижения пищи клювом.
bool CELCrowAnimation::SolvePeck(const Fvector& Contact)
{
	if (NeckIdentifier == u16(-1) || HeadIdentifier == u16(-1) || Bones[HeadIdentifier].Parent != NeckIdentifier)
	{
		return false;
	}
	SBone& Neck = Bones[NeckIdentifier];
	SBone& Head = Bones[HeadIdentifier];
	NeckParent.identity();
	u16 ParentIdentifier;
	Fmatrix Combined;
	for (ParentIdentifier = Neck.Parent; ParentIdentifier != u16(-1); ParentIdentifier = Bones[ParentIdentifier].Parent)
	{
		Combined.mul_43(Bones[ParentIdentifier].Resting, NeckParent);
		NeckParent = Combined;
	}
	Neck.Solved = Neck.Resting;
	Head.Solved = Head.Resting;
	Fmatrix Inverse;
	Inverse.invert(Owner.XFORM());
	Fvector Target;
	Inverse.transform_tiny(Target, Contact);
	float UsedAngle[2] = {0.f, 0.f};
	Fmatrix NeckModel;
	Fmatrix HeadModel;
	Fmatrix InverseParent;
	Fmatrix Rotation;
	Fmatrix Rotated;
	Fvector Tip;
	Fvector LocalTip;
	Fvector LocalTarget;
	Fvector CurrentDirection;
	Fvector TargetDirection;
	Fvector Axis;
	float Cosine;
	float Angle;
	Fmatrix* Local;
	u32 Iteration;
	u32 JointIndex;
	for (Iteration = 0; Iteration < 12; ++Iteration)
	{
		for (JointIndex = 0; JointIndex < 2; ++JointIndex)
		{
			PeckForward(NeckModel, HeadModel);
			HeadModel.transform_tiny(Tip, Settings->Pose.BeakOffset);
			if (Tip.distance_to_sqr(Target) < .0001f)
			{
				SolvedTip = Tip;
				return true;
			}
			InverseParent.invert(JointIndex == 0 ? NeckModel : NeckParent);
			InverseParent.transform_tiny(LocalTip, Tip);
			InverseParent.transform_tiny(LocalTarget, Target);
			Local = JointIndex == 0 ? &Head.Solved : &Neck.Solved;
			CurrentDirection.sub(LocalTip, Local->c).normalize_safe();
			TargetDirection.sub(LocalTarget, Local->c).normalize_safe();
			Cosine = CurrentDirection.dotproduct(TargetDirection);
			clamp(Cosine, -1.f, 1.f);
			Angle = std::min(acosf(Cosine), std::max(0.f, Settings->Pose.PeckMaxAngle - UsedAngle[JointIndex]));
			Axis.crossproduct(CurrentDirection, TargetDirection);
			if (Angle < EPS_L || Axis.square_magnitude() < EPS_L)
			{
				continue;
			}
			Axis.normalize_safe();
			Rotation.rotation(Axis, Angle);
			Rotated.mul_43(Rotation, *Local);
			Rotated.c = Local->c;
			*Local = Rotated;
			UsedAngle[JointIndex] += Angle;
		}
	}
	PeckForward(NeckModel, HeadModel);
	HeadModel.transform_tiny(Tip, Settings->Pose.BeakOffset);
	if (Tip.distance_to(Target) <= Settings->Pose.NeckExtension)
	{
		// A bounded translation of the neck joint, without scaling bones or moving the feet.
		InverseParent.invert(NeckParent);
		CurrentDirection.sub(Target, Tip);
		InverseParent.transform_dir(LocalTarget, CurrentDirection);
		Neck.Solved.c.add(LocalTarget);
		PeckForward(NeckModel, HeadModel);
		HeadModel.transform_tiny(Tip, Settings->Pose.BeakOffset);
	}
	SolvedTip = Tip;
	return Tip.distance_to_sqr(Target) < .0001f;
}
// Вычисляет положение клюва в спокойной наземной позе.
bool CELCrowAnimation::RestingBeak(Fvector& Point) const
{
	if (!Skeleton || HeadIdentifier == u16(-1))
	{
		return false;
	}
	Fmatrix Model = Bones[HeadIdentifier].Resting;
	Fmatrix Combined;
	u16 ParentIdentifier;
	for (ParentIdentifier = Bones[HeadIdentifier].Parent; ParentIdentifier != u16(-1); ParentIdentifier = Bones[ParentIdentifier].Parent)
	{
		Combined.mul_43(Bones[ParentIdentifier].Resting, Model);
		Model = Combined;
	}
	Fvector LocalTip;
	Model.transform_tiny(LocalTip, Settings->Pose.BeakOffset);
	Owner.XFORM().transform_tiny(Point, LocalTip);
	return true;
}

// Начинает движение клевания к выбранной точке пищи.
bool CELCrowAnimation::BeginPeck(const Fvector& Contact)
{
	if (!Settings || !IsGrounded || IsPecking())
	{
		return false;
	}
	HasSolvedPeck = Settings->Pose.HasInverseKinematics && SolvePeck(Contact);
	if (!HasSolvedPeck && Settings->Pose.HasInverseKinematics)
	{
		return false;
	}
	PeckContact = Contact;
	HasContact = false;
	PeckPhase = 0.f;
	return true;
}
// Однократно забирает подтверждение контакта клюва.
bool CELCrowAnimation::ConsumeContact(Fvector& Contact)
{
	const bool Result = HasContact;
	Contact = ConfirmedContact;
	HasContact = false;
	return Result;
}
// Обновляет осмотр и периодические движения спокойной вороны.
void CELCrowAnimation::UpdateIdle(float DeltaTime)
{
	const SELCrowPoseSettings& Pose = Settings->Pose;
	IdlePhase += DeltaTime * PI;
	if (LookHoldRemaining > 0.f)
	{
		LookHoldRemaining = std::max(0.f, LookHoldRemaining - DeltaTime);
		if (LookHoldRemaining <= 0.f)
		{
			LookTarget.set(0.f, 0.f, 0.f);
		}
	}
	else
	{
		LookRemaining -= DeltaTime;
		if (LookRemaining <= 0.f)
		{
			LookTarget.set(Random.randF(-Pose.LookAngle, Pose.LookAngle), Random.randF(-Pose.PitchRange, Pose.PitchRange), 0.f);
			LookHoldRemaining = Pose.LookDuration;
			LookRemaining = Pose.LookInterval.Sample();
		}
	}
	Look.lerp(Look, LookTarget, 1.f - expf(-Pose.LookSpeed * DeltaTime));
	RuffleRemaining -= DeltaTime;
	if (RuffleRemaining < -.65f)
	{
		RuffleRemaining = Pose.RuffleInterval.Sample();
	}
	SettleRemaining = std::max(0.f, SettleRemaining - DeltaTime);
	const float Settle = SettleRemaining / Pose.SettleDuration;
	Fvector Angles = {0.f, .006f * std::sin(IdlePhase), .008f * std::sin(IdlePhase) - .025f * Settle};
	Angles.mul(Pose.IdleAmount * PoseWeight);
	BodyMotion.setXYZi(Angles);
	BodyMotion.c.set(0.f, (.002f * std::sin(IdlePhase) - .018f * Settle) * Pose.IdleAmount * PoseWeight, 0.f);
	BodyInverse.invert(BodyMotion);
}
// Проверяет, нужна ли подробная костная анимация с учётом камеры.
bool CELCrowAnimation::HasDetailedView() const
{
	Fvector Direction;
	Direction.sub(Owner.Position(), Device.vCameraPosition);
	const float Distance = Direction.magnitude();
	if (Distance <= Settings->Pose.DetailDistance)
	{
		return true;
	}
	const float ZoomScale = std::min(10.f, std::max(1.f, g_fov / std::max(1.f, Device.fFOV)));
	if (Distance > Settings->Pose.FocusDistance * ZoomScale || Distance < EPS_L)
	{
		return false;
	}
	Direction.div(Distance);
	return Direction.dotproduct(Device.vCameraDirection) >= std::cos(deg2rad(Device.fFOV) * .5f * Settings->Pose.FocusFraction);
}

// Обновляет позы, клевание и костные поправки вороны и обрабатывает текущий результат выполнения.
void CELCrowAnimation::Update(float DeltaTime)
{
	if (!Skeleton || !Settings || LastFrame == Device.dwFrame)
	{
		return;
	}
	LastFrame = Device.dwFrame;
	clamp(DeltaTime, 0.f, .1f);
	const SELCrowPoseSettings& Pose = Settings->Pose;
	PoseWeight += ((IsGrounded ? 1.f : 0.f) - PoseWeight) * std::min(1.f, (IsGrounded ? Pose.FoldSpeed : Pose.OpenSpeed) * DeltaTime);
	if (PeckPhase >= 0.f)
	{
		PeckPhase += DeltaTime / Settings->PeckDuration;
		if (PeckPhase >= 1.f)
		{
			PeckPhase = -1.f;
		}
	}
	if (HasDetailedView() || IsPecking())
	{
		UpdateIdle(DeltaTime);
	}
	Skeleton->CalculateBones_Invalidate();
	if (IsPecking() && PeckWeight() >= .99f && HeadIdentifier != u16(-1))
	{
		// Validate the actual blended skeleton, including the non-IK fallback pose.
		Skeleton->CalculateBones(TRUE);
		Fvector LocalTip;
		Fvector WorldTip;
		Skeleton->LL_GetBoneInstance(HeadIdentifier).mTransform.transform_tiny(LocalTip, Pose.BeakOffset);
		Owner.XFORM().transform_tiny(WorldTip, LocalTip);
		if (WorldTip.distance_to_sqr(PeckContact) <= Pose.ContactDistance * Pose.ContactDistance)
		{
			HasContact = true;
			ConfirmedContact = WorldTip;
		}
	}
}
// Применяет рассчитанную поправку к соответствующей кости модели.
void _BCL CELCrowAnimation::BoneCallback(CBoneInstance* Instance)
{
	SBone* Bone = static_cast<SBone*>(Instance->callback_param());
	if (!Bone || !Bone->Animation || Bone->Animation->PoseWeight <= EPS_L)
	{
		return;
	}
	CELCrowAnimation& Animation = *Bone->Animation;
	const SELCrowPoseSettings& Settings = Animation.Settings->Pose;
	const Fmatrix& AnimatedParent = Bone->Parent == u16(-1) ? Fidentity : Animation.Skeleton->LL_GetBoneInstance(Bone->Parent).mTransform;
	const bool OwnParent = Bone->Parent != u16(-1) && Animation.Skeleton->LL_GetBoneInstance(Bone->Parent).callback_param() == &Animation.Bones[Bone->Parent];
	const Fmatrix& Parent = OwnParent ? Animation.Bones[Bone->Parent].Unscaled : AnimatedParent;
	Fmatrix Resting = Bone->Resting;
	const float Peck = Animation.PeckWeight();
	Fquaternion OriginalRotation;
	Fquaternion TargetRotation;
	Fquaternion Rotation;
	Fvector Translation;
	if (Animation.HasSolvedPeck && (Bone->Identifier == Animation.NeckIdentifier || Bone->Identifier == Animation.HeadIdentifier))
	{
		OriginalRotation.set(Resting);
		TargetRotation.set(Bone->Solved);
		Rotation.slerp(OriginalRotation, TargetRotation, Peck);
		Translation.lerp(Resting.c, Bone->Solved.c, Peck);
		Resting.mk_xform(Rotation, Translation);
	}
	float Weight = Animation.PoseWeight;
	if (Bone->IsWing)
	{
		Weight *= 1.f - Settings.HopWingAmount * std::min(1.f, Animation.HopHeight / std::max(.01f, Animation.Settings->HopHeight));
		const float Delay = Bone->WingStage == 0 ? .2f : Bone->WingStage == 1 ? .1f
																			  : 0.f;
		Weight = std::max(0.f, (Weight - Delay) / (1.f - Delay));
		Weight = Weight * Weight * (3.f - 2.f * Weight);
	}
	Fmatrix InverseParent;
	Fmatrix Animated;
	Fmatrix Blended;
	InverseParent.invert(AnimatedParent);
	Animated.mul_43(InverseParent, Instance->mTransform);
	OriginalRotation.set(Animated);
	TargetRotation.set(Resting);
	Rotation.slerp(OriginalRotation, TargetRotation, Weight);
	Translation.lerp(Animated.c, Resting.c, Weight);
	Blended.mk_xform(Rotation, Translation);
	Fmatrix Motion;
	Fmatrix Combined;
	if (Bone->Parent == u16(-1))
	{
		Blended.c.y += Animation.HopHeight;
	}
	if (Bone->Identifier == Animation.BodyIdentifier)
	{
		Combined.mul_43(Blended, Animation.BodyMotion);
		Blended = Combined;
	}
	else if (Bone->IsFootAnchor && Animation.BodyIdentifier != u16(-1))
	{
		Combined.mul_43(Animation.BodyInverse, Blended);
		Blended = Combined;
	}
	Fvector Angles = {0.f, 0.f, 0.f};
	if (Bone->Identifier == Animation.NeckIdentifier || Bone->Identifier == Animation.HeadIdentifier)
	{
		Angles.mul(Settings.LookYawAxis, Animation.Look.x);
		Angles.mad(Settings.LookPitchAxis, Animation.Look.y);
		Angles.mul((Bone->Identifier == Animation.NeckIdentifier ? Settings.NeckShare : 1.f - Settings.NeckShare) * Weight * (1.f - Peck));
		if (!Settings.HasInverseKinematics)
		{
			Angles.mad(Settings.PeckAngles[Bone->Identifier == Animation.NeckIdentifier ? 0 : 1], Peck);
		}
	}
	if (Bone->IsWing && Animation.RuffleRemaining < 0.f)
	{
		Angles.z += .08f * float(Bone->WingSide) * std::sin(-Animation.RuffleRemaining * PI / .65f) * Weight;
	}
	if (Bone->Identifier == Animation.TailIdentifier)
	{
		Angles.y += .025f * std::sin(Animation.IdlePhase) * Weight;
	}
	Motion.setXYZi(Angles);
	Combined.mul_43(Blended, Motion);
	Combined.c = Blended.c;
	Instance->mTransform.mul_43(Parent, Combined);
	Bone->Unscaled = Instance->mTransform;
	if (Bone->IsWing)
	{
		Instance->mTransform.i.mul(1.f + (Settings.FeatherLength - 1.f) * Weight);
		Instance->mTransform.j.mul(1.f + (Settings.FeatherWidth - 1.f) * Weight);
	}
}
