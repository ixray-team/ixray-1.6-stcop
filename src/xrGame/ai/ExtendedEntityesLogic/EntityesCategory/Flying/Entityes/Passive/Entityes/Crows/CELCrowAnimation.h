#pragma once
#include "CELCrowSettings.h"
class CBaseMonster;
class CBoneInstance;
class IKinematics;

// Owned by the native bird, not a relocatable/removable ECS component.
class CELCrowAnimation
{
public:
	explicit CELCrowAnimation(CBaseMonster& OwnerMonster);
	~CELCrowAnimation();
	void Attach();
	void Detach();
	void SetGrounded(bool Grounded, float HopHeight = 0.f);
	bool BeginPeck(const Fvector& Contact);
	bool RestingBeak(Fvector& Point) const;
	bool IsPecking() const { return PeckPhase >= 0.f; }
	bool ConsumeContact(Fvector& Contact);
	void Update(float DeltaTime);

private:
	struct SBone
	{
		CELCrowAnimation* Animation = nullptr;
		u16 Identifier = u16(-1);
		u16 Parent = u16(-1);
		Fmatrix Resting;
		Fmatrix Solved;
		Fmatrix Unscaled;
		bool IsWing = false;
		bool IsFootAnchor = false;
		u32 WingStage = 0;
		int WingSide = 0;
	};
	static void _BCL BoneCallback(CBoneInstance* Bone);
	void ConfigureWings();
	void UpdateIdle(float DeltaTime);
	bool HasDetailedView() const;
	bool SolvePeck(const Fvector& Contact);
	void PeckForward(Fmatrix& NeckModel, Fmatrix& HeadModel) const;
	float PeckWeight() const;
	CBaseMonster& Owner;
	const SELCrowSettings* Settings = nullptr;
	IKinematics* Skeleton = nullptr;
	xr_vector<SBone> Bones;
	u16 NeckIdentifier = u16(-1);
	u16 HeadIdentifier = u16(-1);
	u16 BodyIdentifier = u16(-1);
	u16 TailIdentifier = u16(-1);
	Fmatrix NeckParent;
	Fmatrix BodyMotion;
	Fmatrix BodyInverse;
	Fvector SolvedTip = {0.f, 0.f, 0.f};
	Fvector PeckContact = {0.f, 0.f, 0.f};
	Fvector ConfirmedContact = {0.f, 0.f, 0.f};
	Fvector Look = {0.f, 0.f, 0.f};
	Fvector LookTarget = {0.f, 0.f, 0.f};
	float PoseWeight = 0.f;
	float PeckPhase = -1.f;
	float HopHeight = 0.f;
	float IdlePhase = 0.f;
	float LookRemaining = 0.f;
	float LookHoldRemaining = 0.f;
	float RuffleRemaining = 0.f;
	float SettleRemaining = 0.f;
	bool IsGrounded = false;
	bool HasSolvedPeck = false;
	bool HasContact = false;
	u32 LastFrame = u32(-1);
};
