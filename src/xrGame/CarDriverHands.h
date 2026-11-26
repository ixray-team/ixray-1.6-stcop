#pragma once
#include "../xrEngine/bone.h"

class IRenderVisual;
class IKinematics;
class CActor;

// First-person driver hands of a car. The visual is anchored to the driver seat and rendered in world space while the local actor drives in the first-person view.
// The steering-wheel bone only supplies the two IK grip targets. It deliberately bypasses player_hud/CHudItem.
class CCarDriverHands
{
public:
	struct SLoadParams
	{
		const char* CarSection = nullptr;
		IKinematics* CarKinematics = nullptr;
		u16 DefaultSteerBone = BI_NONE;
	};

	struct SFrameParams
	{
		const char* CarSection = nullptr;
		IKinematics* CarKinematics = nullptr;
		Fmatrix CarXform;
		Fmatrix SeatXform;
		float SteerAngle = 0.f;
	};

	CCarDriverHands();
	~CCarDriverHands();

	CCarDriverHands(const CCarDriverHands&) = delete;
	CCarDriverHands& operator=(const CCarDriverHands&) = delete;

	// HUD hands section of the actor: outfit, then the current player HUD, then actor.player_hud_default.
	static shared_str ResolveSection(CActor* Actor);

	void Load(const SLoadParams& Params, const shared_str& HandsSection);
	void Destroy();

	void Hide();
	void Update(const SFrameParams& Params);
	void Render() const;

	const shared_str& SourceSection() const
	{
		return SourceSectionName;
	}

private:
	// One hand's home grip on the rim. The regrip path is derived from the
	// steering wheel angle every frame; the configurable lag only smooths how
	// fast the hand follows it.
	struct SHandState
	{
		float HomeAngle = 0.f;
		float Radius = 0.f;
		float Z = 0.f;
	};

	// One finger group's bend state. All bones of the group use the same
	// angle; Axis is in the proximal (chain root) bone's local space. The
	// other segments rotate around the same model-space axis, otherwise the
	// mirrored finger bones bend in the opposite direction.
	struct SFingerBend
	{
		Fvector Axis = {0.f, 0.f, -1.f};
		float Angle = 0.f;
	};

	enum : u8
	{
		HandNone = 0,
		HandLeft = 1,
		HandRight = 2,
	};

	static constexpr u8 FingerGroupCount = 5;
	static constexpr u8 NoFingerGroup = 0xFF;

	static s8 SignOf(float Value);
	u8 GetLowerHand(float WheelAngle) const;
	void ReadConfig(const char* Section);
	void SetupFingers();
	void BuildFingerAxes();
	void ResetStates();
	void UpdateFingerStates(float Dt);
	void ApplyBindPose();
	void ApplyFingerBend();
	void ApplyShoulderWidth();
	void CalibrateHandOffsets(const Fmatrix& WheelNeutralWorld, const Fmatrix& HandsInverse, const char* CarSection);
	void UpdateRegrip(float WheelAngle, const char* CarSection);
	void SolveArms(const Fmatrix& WheelWorld, float WheelAngle, const Fmatrix& HandsInverse);

	IRenderVisual* Visual = nullptr;
	IKinematics* Kinematics = nullptr;
	shared_str SourceSectionName;
	xr_vector<Fmatrix> BindPose;
	xr_vector<Fmatrix> BindRenderPose;
	Fmatrix Xform = Fidentity;
	Fmatrix LeftHandOffset = Fidentity;
	Fmatrix RightHandOffset = Fidentity;

	// Config, see driver_hands_* in the car section.
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector Rotation = {0.f, 0.f, 0.f};
	Fvector LeftGrip = {0.f, 0.f, 0.f};
	Fvector RightGrip = {0.f, 0.f, 0.f};
	Fvector LeftHandRotation = {0.f, 0.f, 0.f};
	Fvector RightHandRotation = {0.f, 0.f, 0.f};
	bool LeftHandRotationExplicit = false;
	bool RightHandRotationExplicit = false;
	Fvector LeftElbow = {0.f, 0.f, 0.f};
	Fvector RightElbow = {0.f, 0.f, 0.f};
	float Scale = 1.f;
	float ShoulderWidth = -1.f;
	float RegripTrigger = 15.f;
	float RegripEnd = 165.f;
	float RegripTravel = 90.f;
	float RegripLag = 0.08f;
	float RegripLift = 0.07f;
	float RegripOverRadial = 0.04f;
	float SteerTwist = 0.f;
	float SteerTwistStart = 90.f;
	Fvector LeftSteerTwistAxis = {0.f, 0.f, 1.f};
	Fvector RightSteerTwistAxis = {0.f, 0.f, 1.f};
	bool LeftSteerTwistAxisExplicit = false;
	bool RightSteerTwistAxisExplicit = false;
	float SteerFactor = 1.f;
	bool SteerFactored = false;
	bool RegripEnabled = false;
	bool RegripDebug = false;
	u16 LeftThumbRoot = BI_NONE;
	u16 RightThumbRoot = BI_NONE;
	float FingerAngles[FingerGroupCount] = {};
	float FingerAirAngles[FingerGroupCount] = {};
	float FingerSpeed = 12.f;

	// Bones.
	u16 SteerBone = BI_NONE;
	u16 LeftClavicle = BI_NONE;
	u16 RightClavicle = BI_NONE;
	u16 LeftIK[3] = {BI_NONE, BI_NONE, BI_NONE};
	u16 RightIK[3] = {BI_NONE, BI_NONE, BI_NONE};

	u8 ActiveHand = HandNone;
	s8 ActiveSign = 0;
	u8 DebugHand = HandNone;
	bool IKReady = false;
	bool IKInitialized = false;
	bool Ready = false;
	xr_vector<u8> FingerBoneGroup;
	xr_vector<u16> FingerBoneRoot;
	xr_vector<Fvector> FingerBoneAxis;
	SHandState LeftState;
	SHandState RightState;
	SFingerBend FingerState[2][FingerGroupCount];
	float Progress = 0.f;
	u32 CarBonesFrame = 0;
};
