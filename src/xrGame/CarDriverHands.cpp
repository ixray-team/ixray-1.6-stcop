#include "StdAfx.h"
#include "CarDriverHands.h"
#include "Actor.h"
#include "CameraRig.h"
#include "CustomOutfit.h"
#include "player_hud.h"
#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../xrEngine/Render.h"

extern float rs_car_steer_factor;

CCarDriverHands::CCarDriverHands()
{
	ResetStates();
}

CCarDriverHands::~CCarDriverHands()
{
	Destroy();
}

shared_str CCarDriverHands::ResolveSection(CActor* Actor)
{
	if (pSettings == nullptr)
	{
		return shared_str();
	}

	if (Actor != nullptr)
	{
		if (CCustomOutfit* Outfit = Actor->GetOutfit())
		{
			const shared_str& Section = Outfit->OutfitDesc().PlayerHudSection;
			if (Section.size() != 0 && pSettings->line_exist(Section, "visual"))
			{
				return Section;
			}
		}
	}

	if (g_player_hud != nullptr)
	{
		const shared_str& Section = g_player_hud->section_name();
		if (Section.size() != 0 && pSettings->line_exist(Section, "visual"))
		{
			return Section;
		}
	}

	const char* DefaultSection = pSettings->read_if_exists<str_c>("actor", "player_hud_default", "actor_hud");
	if (DefaultSection != nullptr && pSettings->line_exist(DefaultSection, "visual"))
	{
		return shared_str(DefaultSection);
	}

	return shared_str();
}

void CCarDriverHands::Load(const SLoadParams& Params, const shared_str& HandsSection)
{
	Destroy();
	SourceSectionName = HandsSection;

	if (g_dedicated_server || pSettings == nullptr)
	{
		return;
	}

	const char* Section = Params.CarSection;
	if (Section == nullptr || !pSettings->section_exist(Section))
	{
		return;
	}

	if (!pSettings->read_if_exists<bool>(Section, "driver_hands", false))
	{
		return;
	}

	IKinematics* CarKinematics = Params.CarKinematics;
	if (CarKinematics == nullptr)
	{
		return;
	}

	const char* BoneName = pSettings->read_if_exists<str_c>(Section, "driver_hands_steer_bone", "");
	if (BoneName == nullptr || BoneName[0] == '\0')
	{
		BoneName = pSettings->read_if_exists<str_c>(Section, "driver_hands_bone", "stearing_wheel");
	}
	u16 WheelBone = BoneName != nullptr ? CarKinematics->LL_BoneID(BoneName) : BI_NONE;
	if (WheelBone == BI_NONE || WheelBone >= CarKinematics->LL_BoneCount())
	{
		WheelBone = Params.DefaultSteerBone;
	}
	if (WheelBone == BI_NONE || WheelBone >= CarKinematics->LL_BoneCount())
	{
		return;
	}

	// The wheel bone callback rotates the visual wheel by the steer angle times
	// the per-bone factor. The hand-over-hand target must use exactly the same
	// value, otherwise the grips drift against the visible rim.
	SteerFactored = false;
	SteerFactor = 1.f;
	if (CInifile* CarIni = CarKinematics->LL_UserData())
	{
		const char* FactorBone = (BoneName != nullptr && BoneName[0] != '\0') ? BoneName : CarKinematics->LL_BoneName_dbg(WheelBone);
		string128 FactorKey;
		xr_sprintf(FactorKey, "%s_factor", FactorBone);
		if (CarIni->line_exist("car_definition", FactorKey))
		{
			SteerFactor = CarIni->r_float("car_definition", FactorKey);
			SteerFactored = true;
		}
	}

	const char* VisualName = nullptr;
	if (HandsSection.size() != 0)
	{
		VisualName = pSettings->read_if_exists<str_c>(HandsSection, "visual", nullptr);
	}
	if (VisualName == nullptr || VisualName[0] == '\0')
	{
		VisualName = pSettings->read_if_exists<str_c>(Section, "driver_hands_visual", "dynamics\\weapons\\wpn_hand\\wpn_hand_01");
	}
	if (VisualName == nullptr || VisualName[0] == '\0')
	{
		return;
	}

	string_path VisualFile;
	xr_strconcat(VisualFile, VisualName, strext(VisualName) ? "" : ".ogf");
	if (!FS.exist(_game_meshes_, VisualFile))
	{
		Msg("! [%s] driver hands visual not found: %s", Section, VisualName);
		return;
	}

	IRenderVisual* NewVisual = ::Render->model_Create(VisualName);
	IKinematics* HandsKinematics = NewVisual != nullptr ? NewVisual->dcast_PKinematics() : nullptr;
	if (HandsKinematics == nullptr)
	{
		if (NewVisual != nullptr)
		{
			::Render->model_Delete(NewVisual);
		}
		return;
	}

	Visual = NewVisual;
	Kinematics = HandsKinematics;
	SteerBone = WheelBone;
	ReadConfig(Section);

	LeftClavicle = HandsKinematics->LL_BoneID("l_clavicle");
	RightClavicle = HandsKinematics->LL_BoneID("r_clavicle");
	LeftIK[0] = HandsKinematics->LL_BoneID("l_upperarm");
	LeftIK[1] = HandsKinematics->LL_BoneID("l_forearm");
	LeftIK[2] = HandsKinematics->LL_BoneID("l_hand");
	RightIK[0] = HandsKinematics->LL_BoneID("r_upperarm");
	RightIK[1] = HandsKinematics->LL_BoneID("r_forearm");
	RightIK[2] = HandsKinematics->LL_BoneID("r_hand");
	IKReady =
		LeftIK[0] != BI_NONE && LeftIK[1] != BI_NONE && LeftIK[2] != BI_NONE &&
		RightIK[0] != BI_NONE && RightIK[1] != BI_NONE && RightIK[2] != BI_NONE;

	if (!IKReady)
	{
		Msg("! [%s] driver hands: IK arm bones not found", Section);
		Destroy();
		return;
	}

	SetupFingers();

	LeftHandOffset.identity();
	RightHandOffset.identity();
	Ready = false;
	IKInitialized = false;
	CarBonesFrame = 0;
	Xform.identity();
	ResetStates();

	HandsKinematics->CalculateBones_Invalidate();
	HandsKinematics->CalculateBones(TRUE);
	HandsKinematics->LL_GetBindTransform(BindPose);
	if (BindPose.size() != HandsKinematics->LL_BoneCount())
	{
		Msg("! [%s] driver hands: invalid bind pose", Section);
		Destroy();
		return;
	}

	Msg("* [%s] driver hands: %s (hud %s, ik %s/%s/%s, %s/%s/%s)", Section, VisualName, HandsSection.size() != 0 ? HandsSection.c_str() : "-", HandsKinematics->LL_BoneName_dbg(LeftIK[0]), HandsKinematics->LL_BoneName_dbg(LeftIK[1]), HandsKinematics->LL_BoneName_dbg(LeftIK[2]), HandsKinematics->LL_BoneName_dbg(RightIK[0]), HandsKinematics->LL_BoneName_dbg(RightIK[1]), HandsKinematics->LL_BoneName_dbg(RightIK[2]));

	if (RegripDebug)
	{
		Msg("* [%s] driver hands regrip: enabled=%d trigger=%.1f end=%.1f travel=%.1f lift=%.2f over=%.2f twist=%.1f start=%.1f wheel_factor=%.2f%s",
			Section,
			RegripEnabled ? 1 : 0,
			RegripTrigger,
			RegripEnd,
			RegripTravel,
			RegripLift,
			RegripOverRadial,
			SteerTwist,
			SteerTwistStart,
			SteerFactor,
			SteerFactored ? "" : " (unfactored)");
	}
}

void CCarDriverHands::ReadConfig(const char* Section)
{
	Position = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_position", Fvector().set(0.f, 0.45f, 0.10f));
	Rotation = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_rotation", Fvector().set(0.f, 0.f, 0.f));
	LeftGrip = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_left_grip", Fvector().set(-0.16f, 0.f, 0.f));
	RightGrip = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_right_grip", Fvector().set(0.16f, 0.f, 0.f));
	LeftHandRotation = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_left_hand_rotation", Fvector().set(0.f, 0.f, 0.f));
	RightHandRotation = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_right_hand_rotation", Fvector().set(0.f, 0.f, 0.f));
	LeftElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_left_elbow", Fvector().set(-0.15f, -0.35f, -0.10f));
	RightElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_right_elbow", Fvector().set(0.15f, -0.35f, -0.10f));
	Scale = pSettings->read_if_exists<float>(Section, "driver_hands_scale", 1.f);
	clamp(Scale, 0.05f, 10.f);
	ReachShiftSpeed = pSettings->read_if_exists<float>(Section, "driver_hands_reach_shift_speed", 15.f);
	clamp(ReachShiftSpeed, 0.f, 120.f);
	ShoulderWidth = pSettings->read_if_exists<float>(Section, "driver_hands_shoulder_width", -1.f);
	if (ShoulderWidth > 0.f)
	{
		clamp(ShoulderWidth, 0.05f, 2.f);
	}
	RegripEnabled = pSettings->read_if_exists<bool>(Section, "driver_hands_regrip", false);
	RegripTrigger = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_trigger", 15.f);
	clamp(RegripTrigger, 0.f, 179.f);
	RegripEnd = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_end", 165.f);
	clamp(RegripEnd, 1.f, 180.f);
	if (RegripEnd <= RegripTrigger)
	{
		RegripEnd = std::min(180.f, RegripTrigger + 1.f);
	}
	RegripTravel = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_travel", 90.f);
	clamp(RegripTravel, 0.f, 180.f);
	RegripLift = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_lift", 0.07f);
	clamp(RegripLift, -0.2f, 0.2f);
	RegripOverRadial = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_over_radial", 0.04f);
	clamp(RegripOverRadial, 0.f, 0.15f);
	RegripDebug = pSettings->read_if_exists<bool>(Section, "driver_hands_regrip_debug", false);
	SteerTwist = pSettings->read_if_exists<float>(Section, "driver_hands_steer_twist", 0.f);
	clamp(SteerTwist, -90.f, 90.f);
	SteerTwistStart = pSettings->read_if_exists<float>(Section, "driver_hands_steer_twist_start", 90.f);
	clamp(SteerTwistStart, 0.f, 179.f);
	SteerTwistAxis = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_steer_twist_axis", Fvector().set(0.f, 0.f, 1.f));
	if (SteerTwistAxis.magnitude() < EPS_L)
	{
		SteerTwistAxis.set(0.f, 0.f, 1.f);
	}
	SteerTwistAxis.normalize();
	MirrorHandRotation = pSettings->read_if_exists<bool>(Section, "driver_hands_mirror_hand_rotation", false);

	FingerAngles[0] = pSettings->read_if_exists<float>(Section, "driver_hands_finger_thumb", 30.f);
	FingerAngles[1] = pSettings->read_if_exists<float>(Section, "driver_hands_finger_index", 40.f);
	FingerAngles[2] = pSettings->read_if_exists<float>(Section, "driver_hands_finger_middle", 35.f);
	FingerAngles[3] = pSettings->read_if_exists<float>(Section, "driver_hands_finger_others", 35.f);
	for (float& Angle : FingerAngles)
	{
		clamp(Angle, -90.f, 90.f);
	}

	FingerSpeed = pSettings->read_if_exists<float>(Section, "driver_hands_finger_speed", 12.f);
	clamp(FingerSpeed, 0.f, 120.f);
}

void CCarDriverHands::SetupFingers()
{
	// Index and remaining fingers use the same local curl axis on both hands.
	// The thumb has its own local axis. Values are fixed in the engine.
	const Fvector CurlAxis = {0.f, 0.f, -1.f};
	const Fvector ThumbAxis[2] = {{0.f, -1.f, 0.f}, {0.f, 0.f, 1.f}};

	for (u8 Hand = 0; Hand < 2; ++Hand)
	{
		FingerState[Hand][0].Axis = ThumbAxis[Hand];
		for (u8 Group = 1; Group < FingerGroupCount; ++Group)
		{
			FingerState[Hand][Group].Axis = CurlAxis;
		}
		for (SFingerBend& State : FingerState[Hand])
		{
			State.Angle = 0.f;
		}
	}

	FingerBoneGroup.assign(Kinematics->LL_BoneCount(), NoFingerGroup);
	const auto AddFinger = [this](u8 Hand, u8 Group, std::initializer_list<const char*> Names)
	{
		for (const char* Name : Names)
		{
			const u16 Bone = Kinematics->LL_BoneID(Name);
			if (Bone != BI_NONE && Bone < FingerBoneGroup.size())
			{
				FingerBoneGroup[Bone] = u8(Hand * FingerGroupCount + Group);
			}
		}
	};

	AddFinger(0, 0, {"l_finger0", "l_finger01", "l_finger02"});
	AddFinger(0, 1, {"bip01_l_finger1", "bip01_l_finger11", "bip01_l_finger12"});
	AddFinger(0, 2, {"bip01_l_finger2", "bip01_l_finger21", "bip01_l_finger22"});
	AddFinger(0, 3, {"bip01_l_finger3", "bip01_l_finger31", "bip01_l_finger32", "bip01_l_finger4", "bip01_l_finger41", "bip01_l_finger42"});
	AddFinger(1, 0, {"r_finger0", "r_finger01", "r_finger02"});
	AddFinger(1, 1, {"bip01_r_finger1", "bip01_r_finger11", "bip01_r_finger12"});
	AddFinger(1, 2, {"bip01_r_finger2", "bip01_r_finger21", "bip01_r_finger22"});
	AddFinger(1, 3, {"bip01_r_finger3", "bip01_r_finger31", "bip01_r_finger32", "bip01_r_finger4", "bip01_r_finger41", "bip01_r_finger42"});
}

void CCarDriverHands::ResetStates()
{
	const auto ResetHand = [](SHandState& Hand, const Fvector& Grip, float DefaultAngle)
	{
		const float Radius = sqrtf(Grip.x * Grip.x + Grip.y * Grip.y);
		if (Radius > EPS_L)
		{
			Hand.HomeAngle = atan2f(Grip.y, Grip.x);
			Hand.Radius = Radius;
		}
		else
		{
			Hand.HomeAngle = DefaultAngle;
			Hand.Radius = 0.16f;
		}

		Hand.Z = Grip.z;
	};

	ResetHand(LeftState, LeftGrip, PI);
	ResetHand(RightState, RightGrip, 0.f);
	ActiveHand = HandNone;
	ActiveSign = 0;
	DebugHand = HandNone;
	Progress = 0.f;

	for (auto& Hand : FingerState)
	{
		for (SFingerBend& State : Hand)
		{
			State.Angle = 0.f;
		}
	}
}

void CCarDriverHands::Destroy()
{
	if (Visual != nullptr)
	{
		::Render->model_Delete(Visual);
		Visual = nullptr;
	}

	ResetStates();

	Kinematics = nullptr;
	BindPose.clear();
	SteerBone = BI_NONE;
	LeftClavicle = BI_NONE;
	RightClavicle = BI_NONE;
	for (u16& Bone : LeftIK)
	{
		Bone = BI_NONE;
	}
	for (u16& Bone : RightIK)
	{
		Bone = BI_NONE;
	}
	LeftHandOffset.identity();
	RightHandOffset.identity();
	LeftReachShift.set(0.f, 0.f, 0.f);
	RightReachShift.set(0.f, 0.f, 0.f);
	LeftGrip.set(0.f, 0.f, 0.f);
	RightGrip.set(0.f, 0.f, 0.f);
	IKReady = false;
	Ready = false;
	IKInitialized = false;
	CarBonesFrame = 0;
}

void CCarDriverHands::Hide()
{
	Ready = false;
	LeftReachShift.set(0.f, 0.f, 0.f);
	RightReachShift.set(0.f, 0.f, 0.f);
	ResetStates();
}

void CCarDriverHands::UpdateFingerStates(float Dt)
{
	const float Alpha = (FingerSpeed > 0.f) ? (1.f - expf(-FingerSpeed * Dt)) : 1.f;
	for (u8 Hand = 0; Hand < 2; ++Hand)
	{
		const bool Active = (ActiveHand == u8(Hand + 1));
		// Fingers open only while the regripping hand is actually in the air.
		// At progress 0 and 1 the hand is on the rim and must grip again.
		const bool InAir = Active && Progress > EPS_L && Progress < 1.f - EPS_L;
		for (u8 Group = 0; Group < FingerGroupCount; ++Group)
		{
			SFingerBend& State = FingerState[Hand][Group];
			const float Target = deg2rad(InAir ? 0.f : FingerAngles[Group]);
			State.Angle += (Target - State.Angle) * Alpha;
			if (fabsf(State.Angle) < 1e-5f)
			{
				State.Angle = 0.f;
			}
		}
	}
}

void CCarDriverHands::ApplyBindPose()
{
	// An IKinematicsAnimated without a motion does not apply the bind pose, so
	// every bone stays at the model origin and the hands collapse into a blob.
	// Restore the bind skeleton explicitly before solving the arms.
	const u16 BoneCount = Kinematics->LL_BoneCount();
	for (u16 Bone = 0; Bone < BoneCount; ++Bone)
	{
		CBoneInstance& Instance = Kinematics->LL_GetBoneInstance(Bone);
		Instance.mTransform.set(BindPose[Bone]);
		Instance.mRenderTransform.mul_43(Instance.mTransform, Kinematics->LL_GetData(Bone).m2b_transform);
	}
}

void CCarDriverHands::ApplyFingerBend()
{
	// Bend the fingers after the whole bind pose is restored. The delta is
	// built around the bone's current model-space origin and its local axis,
	// and TransformBone applies it to the whole child chain so the segments
	// follow the parent instead of rotating in place.
	const u16 BoneCount = std::min<u16>(Kinematics->LL_BoneCount(), u16(FingerBoneGroup.size()));
	for (u16 Bone = 0; Bone < BoneCount; ++Bone)
	{
		const u8 Group = FingerBoneGroup[Bone];
		if (Group == NoFingerGroup)
		{
			continue;
		}

		const SFingerBend& State = FingerState[Group / FingerGroupCount][Group % FingerGroupCount];
		if (fabsf(State.Angle) <= EPS_L)
		{
			continue;
		}

		const Fmatrix& BoneXform = Kinematics->LL_GetBoneInstance(Bone).mTransform;

		Fvector AxisModel;
		BoneXform.transform_dir(AxisModel, State.Axis);
		if (AxisModel.magnitude() <= EPS_L)
		{
			continue;
		}
		AxisModel.normalize();

		Fmatrix Delta;
		Delta.rotation(AxisModel, State.Angle);
		Fvector RotatedOrigin;
		Delta.transform_dir(RotatedOrigin, BoneXform.c);
		Delta.c.sub(BoneXform.c, RotatedOrigin);

		CCameraRig::TransformBone(Kinematics, Bone, Delta);
	}
}

void CCarDriverHands::ApplyShoulderWidth()
{
	if (ShoulderWidth <= 0.f)
	{
		return;
	}

	// Shoulder width is a model-space X distance between the upper-arm
	// roots. Translate the clavicles (their children carry the arms) so
	// the shoulders can be narrowed or widened independently of the hand
	// grips and the arm scale.
	const Fvector LeftRoot = Kinematics->LL_GetTransform(LeftIK[0]).c;
	const Fvector RightRoot = Kinematics->LL_GetTransform(RightIK[0]).c;
	const float CenterX = (LeftRoot.x + RightRoot.x) * 0.5f;
	const float HalfWidth = ShoulderWidth * 0.5f;
	const u16 LeftBone = LeftClavicle != BI_NONE ? LeftClavicle : LeftIK[0];
	const u16 RightBone = RightClavicle != BI_NONE ? RightClavicle : RightIK[0];

	Fmatrix Delta;
	Delta.identity();
	Delta.c.set((CenterX - HalfWidth) - LeftRoot.x, 0.f, 0.f);
	CCameraRig::TransformBone(Kinematics, LeftBone, Delta);

	Delta.identity();
	Delta.c.set((CenterX + HalfWidth) - RightRoot.x, 0.f, 0.f);
	CCameraRig::TransformBone(Kinematics, RightBone, Delta);
}

void CCarDriverHands::CalibrateHandOffsets(const Fmatrix& WheelNeutralWorld, const Fmatrix& HandsInverse)
{
	// Preserve the authored bind orientation of each hand. The stored
	// offset is wheel-local, so the palms follow the wheel while the arms
	// are solved independently.
	const auto Calibrate = [this, &WheelNeutralWorld](u16 HandBone, const Fmatrix& ConfigRot, Fmatrix& Offset)
	{
		Fmatrix HandRot = Kinematics->LL_GetTransform(HandBone);
		HandRot.c.set(0.f, 0.f, 0.f);
		HandRot.mulB_43(ConfigRot);

		Fmatrix HandsRot = Xform;
		HandsRot.c.set(0.f, 0.f, 0.f);

		Fmatrix HandWorldRot;
		HandWorldRot.mul_43(HandsRot, HandRot);

		Fmatrix WheelRot = WheelNeutralWorld;
		WheelRot.c.set(0.f, 0.f, 0.f);
		WheelRot.invert();

		Offset.mul_43(WheelRot, HandWorldRot);
		Offset.c.set(0.f, 0.f, 0.f);
	};

	Fmatrix LeftConfigRot;
	LeftConfigRot.setXYZ(deg2rad(LeftHandRotation.x), deg2rad(LeftHandRotation.y), deg2rad(LeftHandRotation.z));

	Fmatrix RightConfigRot;
	if (MirrorHandRotation)
	{
		// Mirror the left hand's deviation from its bind pose in model
		// space:
		// C_right = B_right * M * B_left^-1 * C_left * B_left * M * B_right^-1.
		// Fmatrix::mul_43(A, B) computes B * A, so the operands are
		// passed in the reverse order.
		Fmatrix HandRotLeft = Kinematics->LL_GetTransform(LeftIK[2]);
		HandRotLeft.c.set(0.f, 0.f, 0.f);
		Fmatrix HandRotRight = Kinematics->LL_GetTransform(RightIK[2]);
		HandRotRight.c.set(0.f, 0.f, 0.f);

		Fmatrix Mirror;
		Mirror.identity();
		Mirror.i.x = -1.f;

		// Mirror about the wheel's neutral left-right plane, not the
		// hands model X axis: on an offset or tilted wheel those differ.
		Fvector Normal;
		HandsInverse.transform_dir(Normal, WheelNeutralWorld.i);
		if (Normal.magnitude() > EPS_L)
		{
			Normal.normalize();
			const float Nx = Normal.x;
			const float Ny = Normal.y;
			const float Nz = Normal.z;
			Mirror._11 = 1.f - 2.f * Nx * Nx;
			Mirror._12 = -2.f * Nx * Ny;
			Mirror._13 = -2.f * Nx * Nz;
			Mirror._21 = -2.f * Ny * Nx;
			Mirror._22 = 1.f - 2.f * Ny * Ny;
			Mirror._23 = -2.f * Ny * Nz;
			Mirror._31 = -2.f * Nz * Nx;
			Mirror._32 = -2.f * Nz * Ny;
			Mirror._33 = 1.f - 2.f * Nz * Nz;
		}

		Fmatrix LeftInv;
		LeftInv.invert(HandRotLeft);
		Fmatrix RightInv;
		RightInv.invert(HandRotRight);

		Fmatrix Temp;
		Temp.mul_43(Mirror, HandRotRight); // B_right * M
		Fmatrix Temp2;
		Temp2.mul_43(LeftInv, Temp); // Temp * B_left^-1
		Fmatrix Temp3;
		Temp3.mul_43(LeftConfigRot, Temp2); // Temp2 * C_left
		Temp.mul_43(HandRotLeft, Temp3);	// Temp3 * B_left
		Fmatrix Temp4;
		Temp4.mul_43(Mirror, Temp);				// Temp * M
		RightConfigRot.mul_43(RightInv, Temp4); // Temp4 * B_right^-1
	}
	else
	{
		RightConfigRot.setXYZ(deg2rad(RightHandRotation.x), deg2rad(RightHandRotation.y), deg2rad(RightHandRotation.z));
	}

	Calibrate(LeftIK[2], LeftConfigRot, LeftHandOffset);
	Calibrate(RightIK[2], RightConfigRot, RightHandOffset);
	IKInitialized = true;
}

s8 CCarDriverHands::SignOf(float Value)
{
	return (Value > 0.f) ? 1 : ((Value < 0.f) ? -1 : 0);
}

u8 CCarDriverHands::GetLowerHand(float WheelAngle) const
{
	// The wheel's local Y axis is its up direction. Use it instead of the
	// world Y so a tilted or offset wheel does not swap the lower hand.
	const float LeftHomeY = sinf(LeftState.HomeAngle) * LeftState.Radius;
	const float RightHomeY = sinf(RightState.HomeAngle) * RightState.Radius;
	if (fabsf(LeftHomeY - RightHomeY) > EPS_L)
	{
		return (LeftHomeY < RightHomeY) ? HandLeft : HandRight;
	}

	return (WheelAngle > 0.f) ? HandLeft : HandRight;
}

void CCarDriverHands::UpdateRegrip(float WheelAngle, const char* CarSection)
{
	const float AbsWheelAngle = fabsf(WheelAngle);
	const float TriggerRad = deg2rad(RegripTrigger);
	const float EndRad = deg2rad(RegripEnd);

	// Regrip is a reversible path parameter driven by the wheel angle, not a
	// timed animation. If the wheel stops the hand stops; if the wheel returns
	// the hand goes back along the same path. EndRad is both the point where
	// the forward move finishes and where the return move starts.
	float NewProgress = 0.f;
	if (RegripEnabled && EndRad > TriggerRad)
	{
		NewProgress = std::clamp((AbsWheelAngle - TriggerRad) / (EndRad - TriggerRad), 0.f, 1.f);
	}

	const s8 WheelSign = SignOf(WheelAngle);

	const u8 LowerHand = GetLowerHand(WheelAngle);

	if (ActiveHand != HandNone && WheelSign != 0 && ActiveSign != 0 && WheelSign != ActiveSign)
	{
		// The wheel crossed the centre. The previous hand must be back home
		// before the other hand can start.
		NewProgress = 0.f;
		ActiveHand = HandNone;
		ActiveSign = 0;
	}

	if (NewProgress <= 0.f || WheelSign == 0)
	{
		NewProgress = 0.f;
		ActiveHand = HandNone;
		ActiveSign = 0;
	}
	else if (ActiveHand == HandNone)
	{
		ActiveHand = LowerHand;
		ActiveSign = WheelSign;
	}

	Progress = NewProgress;

	if (RegripDebug && ActiveHand != DebugHand)
	{
		DebugHand = ActiveHand;
		Msg("* [%s] driver hands: active=%s W=%.1f p=%.2f", CarSection, ActiveHand == HandLeft ? "left" : (ActiveHand == HandRight ? "right" : "none"), rad2deg(WheelAngle), Progress);
	}
}

void CCarDriverHands::SolveArms(const Fmatrix& WheelWorld, float WheelAngle, const Fmatrix& HandsInverse)
{
	const float AbsWheelAngle = fabsf(WheelAngle);
	const float EndRad = deg2rad(RegripEnd);
	const s8 WheelSign = SignOf(WheelAngle);


	Fvector WheelNormal = WheelWorld.k;
	WheelNormal.normalize();

	// Lift must always go towards the driver, not into the dashboard. The
	// hands origin is behind the wheel, so its side of the wheel plane gives
	// the sign without relying on the wheel bone's local Z direction.
	Fvector ToDriver;
	ToDriver.sub(Xform.c, WheelWorld.c);
	Fvector LiftDir = WheelNormal;
	if (ToDriver.dotproduct(WheelNormal) < 0.f)
	{
		LiftDir.mul(-1.f);
	}

	Fmatrix WheelRot = WheelWorld;
	WheelRot.c.set(0.f, 0.f, 0.f);

	// The moving hand goes over the top of the wheel, not under it. At
	// progress 1 the wheel has turned by EndRad, so the hand's local angle
	// must change by travel + end: it cancels the wheel turn and adds its
	// own short arc over the top. Left hand moves clockwise (negative),
	// right hand counter-clockwise.
	const float PathLift = sinf(PI * Progress);
	const float OverTravel = deg2rad(RegripTravel) + EndRad;

	// Move the hand over the top of the wheel, which may not be at the
	// canonical 9/3 o'clock positions on a custom/tilted wheel.
	const auto ToTopDirection = [](float HomeAngle)
	{
		return (angle_normalize_signed(PI * 0.5f - HomeAngle) >= 0.f) ? 1.f : -1.f;
	};

	float LeftLocal = LeftState.HomeAngle;
	float RightLocal = RightState.HomeAngle;
	if (ActiveHand == HandLeft)
	{
		LeftLocal += ToTopDirection(LeftState.HomeAngle) * OverTravel * Progress;
	}
	else if (ActiveHand == HandRight)
	{
		RightLocal += ToTopDirection(RightState.HomeAngle) * OverTravel * Progress;
	}

	// Small passive wrist twist for the hand that stays on the rim (the one
	// that is not regripping). It starts only after the configured wheel
	// angle and grows to the full value at full lock.
	const float TwistStartRad = deg2rad(SteerTwistStart);
	float TwistScale = 0.f;
	if (fabsf(SteerTwist) > 0.f && AbsWheelAngle > TwistStartRad && PI > TwistStartRad)
	{
		TwistScale = std::clamp((AbsWheelAngle - TwistStartRad) / (PI - TwistStartRad), 0.f, 1.f);
	}

	const float TwistAngle = -WheelSign * deg2rad(SteerTwist) * TwistScale;
	const u8 UpperHand = (ActiveHand != HandNone) ? u8(3 - ActiveHand) : u8(3 - GetLowerHand(WheelAngle));
	const float LeftTwist = (UpperHand == HandLeft) ? TwistAngle : 0.f;
	const float RightTwist = (UpperHand == HandRight) ? TwistAngle : 0.f;

	const auto BuildTarget = [&](const SHandState& Hand, float LocalAngle, float HandTwist, const Fmatrix& BaseOffset, float LiftScale, Fmatrix& Result)
	{
		const float Radius = Hand.Radius + RegripOverRadial * LiftScale;
		const Fvector Local = Fvector().set(cosf(LocalAngle) * Radius, sinf(LocalAngle) * Radius, Hand.Z);
		Fvector World;
		WheelWorld.transform_tiny(World, Local);
		if (LiftScale > 0.f)
		{
			World.mad(LiftDir, RegripLift * LiftScale);
		}

		Fmatrix AngleRot;
		AngleRot.rotateZ(LocalAngle - Hand.HomeAngle);

		if (fabsf(HandTwist) > EPS_L)
		{
			Fmatrix TwistRot;
			TwistRot.rotation(SteerTwistAxis, HandTwist);
			Fmatrix Rotated;
			Rotated.mul_43(TwistRot, AngleRot); // standard AngleRot * TwistRot
			AngleRot = Rotated;
		}

		Fmatrix Offset;
		Offset.mul_43(AngleRot, BaseOffset);

		Fmatrix HandRotation;
		HandRotation.mul_43(WheelRot, Offset);

		Result = HandRotation;
		Result.c = World;
	};

	Fmatrix LeftTarget;
	Fmatrix RightTarget;
	BuildTarget(LeftState, LeftLocal, LeftTwist, LeftHandOffset, ActiveHand == HandLeft ? PathLift : 0.f, LeftTarget);
	BuildTarget(RightState, RightLocal, RightTwist, RightHandOffset, ActiveHand == HandRight ? PathLift : 0.f, RightTarget);

	const auto Solve = [this, &HandsInverse](const u16* Chain, u16 Clavicle, const Fmatrix& TargetWorld, const Fvector& Elbow, Fvector& CurrentShift)
	{
		Fmatrix Target;
		Target.mul_43(HandsInverse, TargetWorld);

		// Fixed-root IK cannot move the shoulder. If the wheel grip is farther
		// than the arm can reach, move the clavicle toward the target instead
		// of letting SolveLimb clamp the wrist short.
		const Fvector Shoulder = Kinematics->LL_GetTransform(Chain[0]).c;
		const Fvector ElbowPos = Kinematics->LL_GetTransform(Chain[1]).c;
		const Fvector WristPos = Kinematics->LL_GetTransform(Chain[2]).c;
		const float UpperLength = Shoulder.distance_to(ElbowPos);
		const float LowerLength = ElbowPos.distance_to(WristPos);

		Fvector DesiredShift;
		DesiredShift.set(0.f, 0.f, 0.f);
		if (UpperLength > EPS_L && LowerLength > EPS_L)
		{
			constexpr float ReachMargin = 0.005f;
			const float MaxReach = UpperLength + LowerLength - ReachMargin;
			if (MaxReach > EPS_L)
			{
				Fvector ToTarget;
				ToTarget.sub(Target.c, Shoulder);
				const float Distance = ToTarget.magnitude();
				if (Distance > EPS_L && Distance > MaxReach)
				{
					ToTarget.div(Distance);
					DesiredShift.mul(ToTarget, Distance - MaxReach);
				}
			}
		}

		if (ReachShiftSpeed > 0.f)
		{
			const float Alpha = 1.f - expf(-ReachShiftSpeed * Device.fTimeDelta);
			Fvector Smoothed;
			Smoothed.lerp(CurrentShift, DesiredShift, Alpha);
			CurrentShift = Smoothed;
		}
		else
		{
			CurrentShift = DesiredShift;
		}

		if (CurrentShift.magnitude() > EPS_L)
		{
			Fmatrix Shift;
			Shift.identity();
			Shift.c.set(CurrentShift);
			const u16 ShiftBone = Clavicle != BI_NONE ? Clavicle : Chain[0];
			CCameraRig::TransformBone(Kinematics, ShiftBone, Shift);
		}

		CCameraRig::SolveLimb(Kinematics, Chain, Target, 1.f, Elbow);
	};

	Solve(LeftIK, LeftClavicle, LeftTarget, LeftElbow, LeftReachShift);
	Solve(RightIK, RightClavicle, RightTarget, RightElbow, RightReachShift);
}

void CCarDriverHands::Update(const SFrameParams& Params)
{
	if (Visual == nullptr || Kinematics == nullptr || !IKReady)
	{
		Ready = false;
		return;
	}

	Fmatrix Setup;
	Setup.scale(Scale, Scale, Scale);

	Fmatrix HandRot;
	HandRot.setXYZ(deg2rad(Rotation.x), deg2rad(Rotation.y), deg2rad(Rotation.z));
	Setup.mulB_43(HandRot);

	Setup.c.set(Position);
	Xform.mul_43(Params.SeatXform, Setup);

	Kinematics->CalculateBones_Invalidate();
	Kinematics->CalculateBones(TRUE);

	UpdateFingerStates(std::clamp(Device.fTimeDelta, 0.f, 0.1f));
	ApplyBindPose();
	ApplyFingerBend();
	ApplyShoulderWidth();

	IKinematics* CarKinematics = Params.CarKinematics;
	if (CarKinematics == nullptr || SteerBone >= CarKinematics->LL_BoneCount())
	{
		Ready = false;
		return;
	}

	if (CarBonesFrame != Device.dwFrame)
	{
		CarKinematics->CalculateBones_Invalidate();
		CarKinematics->CalculateBones(TRUE);
		CarBonesFrame = Device.dwFrame;
	}

	Fmatrix WheelWorld;
	CarKinematics->LL_GetBoneWorldTransform(SteerBone, Params.CarXform, WheelWorld);

	Fmatrix HandsInverse = Xform;
	HandsInverse.invert();

	if (!IKInitialized)
	{
		xr_vector<Fmatrix> CarBind;
		CarKinematics->LL_GetBindTransform(CarBind);
		Fmatrix WheelNeutralWorld = WheelWorld;
		if (SteerBone < CarBind.size())
		{
			WheelNeutralWorld.mul_43(Params.CarXform, CarBind[SteerBone]);
		}

		CalibrateHandOffsets(WheelNeutralWorld, HandsInverse);
	}

	const float WheelAngle = Params.SteerAngle * (SteerFactored ? SteerFactor * rs_car_steer_factor : 1.f);
	UpdateRegrip(WheelAngle, Params.CarSection);
	SolveArms(WheelWorld, WheelAngle, HandsInverse);

	Ready = true;
}

void CCarDriverHands::Render() const
{
	if (!Ready || Visual == nullptr)
	{
		return;
	}

	if (::Render->phase != IRender_interface::PHASE_NORMAL)
	{
		return;
	}

	// set_Transform takes a non-const reference.
	Fmatrix Transform = Xform;
	::Render->set_Transform(Transform);
	::Render->add_Visual(Visual, true);
}
