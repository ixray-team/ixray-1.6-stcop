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

namespace
{
	// Phase of a hand-release motion. It drives the finger grip, the active-hand
	// state and the regrip path. A sharp input must not snap the fingers or the
	// hand state, so the phase follows its target with a first-order lag. The
	// regrip path is built from the phase wheel transform, so this lag does not
	// move the grip points with the wheel.
	float SmoothHandReleasePhase(float Current, float Target, float Lag, float Dt)
	{
		if (Lag <= 0.f)
		{
			return Target;
		}

		const float Alpha = 1.f - expf(-Dt / Lag);
		float Phase = Current + (Target - Current) * Alpha;

		// The exponential approach never exactly reaches the target. Snap the
		// tail so the hand lands on the rim and the fingers can close.
		constexpr float SnapDistance = 0.01f;
		if (fabsf(Target - Phase) < SnapDistance)
		{
			Phase = Target;
		}

		return Phase;
	}

	// Finger bend keys accept the legacy single grip angle, the "grip, air"
	// pair, or the "grip, air, gear" triple. The gear angle is used by the
	// right hand on the gear lever; if it is omitted, the grip angle is used.
	// Negative values unbend the finger past the bind pose.
	void ReadDriverFingerAngles(const CInifile& Ini, const char* Section, const char* Key, float DefaultGrip, float& Grip, float& Air, float& Gear)
	{
		Grip = DefaultGrip;
		Air = 0.f;
		Gear = DefaultGrip;

		const char* Value = Ini.read_if_exists<str_c>(Section, Key, nullptr);
		if (Value == nullptr || Value[0] == '\0')
		{
			return;
		}

		float ParsedGrip = DefaultGrip;
		float ParsedAir = 0.f;
		float ParsedGear = DefaultGrip;
		const int Count = sscanf(Value, "%f , %f , %f", &ParsedGrip, &ParsedAir, &ParsedGear);
		if (Count >= 1)
		{
			Grip = ParsedGrip;
			Gear = ParsedGrip;
		}
		if (Count >= 2)
		{
			Air = ParsedAir;
		}
		if (Count >= 3)
		{
			Gear = ParsedGear;
		}
	}

	// A hand target is a rigid transform, so the gear transition interpolates
	// its position and orientation separately. The slerp keeps the wrist from
	// rolling through an arbitrary intermediate rotation.
	void BlendHandTargets(const Fmatrix& From, const Fmatrix& To, float T, Fmatrix& Result)
	{
		if (T <= 0.f)
		{
			Result = From;
			return;
		}
		if (T >= 1.f)
		{
			Result = To;
			return;
		}

		Fquaternion FromRotation;
		Fquaternion ToRotation;
		Fquaternion BlendRotation;
		FromRotation.set(From);
		ToRotation.set(To);
		BlendRotation.slerp(FromRotation, ToRotation, T);

		Result.rotation(BlendRotation);
		Result.c.lerp(From.c, To.c, T);
	}
}

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

	GearBone = BI_NONE;
	if (GearBoneName.size() != 0)
	{
		GearBone = CarKinematics->LL_BoneID(GearBoneName.c_str());
		if (GearBone == BI_NONE || GearBone >= CarKinematics->LL_BoneCount())
		{
			// The default bone name is inherited by every car, including models
			// without a gear lever. Only report explicitly configured names.
			if (GearDebug || xr_strcmp(GearBoneName.c_str(), "gear") != 0)
			{
				Msg("! [%s] driver hands: gear bone not found: %s", Section, GearBoneName.c_str());
			}
			GearBone = BI_NONE;
		}
	}

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

	BindRenderPose.resize(BindPose.size());
	for (u16 Bone = 0; Bone < BindPose.size(); ++Bone)
	{
		BindRenderPose[Bone].mul_43(BindPose[Bone], HandsKinematics->LL_GetData(Bone).m2b_transform);
	}

	BuildFingerAxes();

	Msg("* [%s] driver hands: %s (hud %s, ik %s/%s/%s, %s/%s/%s)", Section, VisualName, HandsSection.size() != 0 ? HandsSection.c_str() : "-", HandsKinematics->LL_BoneName_dbg(LeftIK[0]), HandsKinematics->LL_BoneName_dbg(LeftIK[1]), HandsKinematics->LL_BoneName_dbg(LeftIK[2]), HandsKinematics->LL_BoneName_dbg(RightIK[0]), HandsKinematics->LL_BoneName_dbg(RightIK[1]), HandsKinematics->LL_BoneName_dbg(RightIK[2]));

	if (RegripDebug)
	{
		Msg("* [%s] driver hands regrip: enabled=%d trigger=%.1f end=%.1f travel=%.1f lag=%.2f lift=%.2f over=%.2f twist=%.1f start=%.1f wheel_factor=%.2f%s",
			Section,
			RegripEnabled ? 1 : 0,
			RegripTrigger,
			RegripEnd,
			RegripTravel,
			RegripLag,
			RegripLift,
			RegripOverRadial,
			SteerTwist,
			SteerTwistStart,
			SteerFactor,
			SteerFactored ? "" : " (unfactored)");
		Msg("* [%s] driver hands fingers: thumb=%.1f/%.1f/%.1f index=%.1f/%.1f/%.1f middle=%.1f/%.1f/%.1f ring=%.1f/%.1f/%.1f pinky=%.1f/%.1f/%.1f speed=%.1f",
			Section,
			FingerAngles[0], FingerAirAngles[0], FingerGearAngles[0],
			FingerAngles[1], FingerAirAngles[1], FingerGearAngles[1],
			FingerAngles[2], FingerAirAngles[2], FingerGearAngles[2],
			FingerAngles[3], FingerAirAngles[3], FingerGearAngles[3],
			FingerAngles[4], FingerAirAngles[4], FingerGearAngles[4],
			FingerSpeed);
	}

	if (GearDebug)
	{
		if (GearBone != BI_NONE)
		{
			Msg("* [%s] driver hands gear: bone=%s hand=%s auto=%d offset=(%.3f, %.3f, %.3f) wrist=(%.1f, %.1f, %.1f) speed=%.2f path=(%.3f, %.3f, %.3f) hold=%.2f",
				Section,
				GearBoneName.c_str(),
				GearLeftHand ? "left" : "right",
				GearAuto ? 1 : 0,
				GearOffset.x, GearOffset.y, GearOffset.z,
				GearRotation.x, GearRotation.y, GearRotation.z,
				GearSpeed,
				GearPath.x, GearPath.y, GearPath.z,
				GearHoldExtra);
		}
		else
		{
			Msg("* [%s] driver hands gear: disabled", Section);
		}
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
	LeftHandRotationExplicit = pSettings->line_exist(Section, "driver_hands_left_hand_rotation");
	RightHandRotationExplicit = pSettings->line_exist(Section, "driver_hands_right_hand_rotation");
	LeftElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_left_elbow", Fvector().set(-0.15f, -0.35f, -0.10f));
	RightElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_right_elbow", Fvector().set(0.15f, -0.35f, -0.10f));

	// Optional gear shift. The bone belongs to the car visual; the offset
	// positions the wrist in that bone's local space. GearRotation is a wrist
	// offset in the selected gear hand's local frame. GearLeftHand selects the
	// side; GearAuto limits the hand to forward <-> reverse shifts.
	GearLeftHand = pSettings->read_if_exists<bool>(Section, "driver_hands_gear_left_hand", false);
	GearAuto = pSettings->read_if_exists<bool>(Section, "driver_hands_gear_auto", false);
	GearBoneName = pSettings->read_if_exists<str_c>(Section, "driver_hands_gear_bone", "");
	GearOffset = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_offset", Fvector().set(0.f, 0.f, 0.f));
	GearRotation = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_rotation", Fvector().set(0.f, 0.f, 0.f));
	if (GearLeftHand)
	{
		GearElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_left_elbow", LeftElbow);
	}
	else
	{
		// driver_hands_gear_elbow is the legacy right-hand name.
		const Fvector3 LegacyElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_elbow", RightElbow);
		GearElbow = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_right_elbow", LegacyElbow);
	}
	GearSpeed = pSettings->read_if_exists<float>(Section, "driver_hands_gear_speed", 8.f);
	clamp(GearSpeed, 0.1f, 20.f);
	GearPath = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_gear_path", Fvector().set(0.f, 0.05f, 0.f));
	GearHoldExtra = pSettings->read_if_exists<float>(Section, "driver_hands_gear_hold_extra", 0.f);
	clamp(GearHoldExtra, 0.f, 2.f);
	GearDebug = pSettings->read_if_exists<bool>(Section, "driver_hands_gear_debug", false);
	Scale = pSettings->read_if_exists<float>(Section, "driver_hands_scale", 1.f);
	clamp(Scale, 0.05f, 10.f);
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
	RegripLag = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_lag", 0.08f);
	clamp(RegripLag, 0.f, 1.f);
	RegripLift = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_lift", 0.07f);
	clamp(RegripLift, -0.2f, 0.2f);
	RegripOverRadial = pSettings->read_if_exists<float>(Section, "driver_hands_regrip_over_radial", 0.04f);
	clamp(RegripOverRadial, 0.f, 0.15f);
	RegripDebug = pSettings->read_if_exists<bool>(Section, "driver_hands_regrip_debug", false);
	SteerTwist = pSettings->read_if_exists<float>(Section, "driver_hands_steer_twist", 0.f);
	clamp(SteerTwist, -90.f, 90.f);
	SteerTwistStart = pSettings->read_if_exists<float>(Section, "driver_hands_steer_twist_start", 90.f);
	clamp(SteerTwistStart, 0.f, 179.f);
	const bool LegacyTwistAxisExists = pSettings->line_exist(Section, "driver_hands_steer_twist_axis");
	const bool LeftTwistAxisExists = pSettings->line_exist(Section, "driver_hands_left_steer_twist_axis");
	const bool RightTwistAxisExists = pSettings->line_exist(Section, "driver_hands_right_steer_twist_axis");
	const Fvector LegacyTwistAxis = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_steer_twist_axis", Fvector().set(0.f, 0.f, 1.f));

	LeftSteerTwistAxis = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_left_steer_twist_axis", Fvector().set(0.f, 0.f, 0.f));
	RightSteerTwistAxis = pSettings->read_if_exists<Fvector3>(Section, "driver_hands_right_steer_twist_axis", LegacyTwistAxis);
	LeftSteerTwistAxisExplicit = LeftTwistAxisExists;
	RightSteerTwistAxisExplicit = RightTwistAxisExists || LegacyTwistAxisExists;

	if (LeftSteerTwistAxis.magnitude() < EPS_L)
	{
		LeftSteerTwistAxis = RightSteerTwistAxis;
	}
	if (RightSteerTwistAxis.magnitude() < EPS_L)
	{
		RightSteerTwistAxis.set(0.f, 0.f, 1.f);
	}
	LeftSteerTwistAxis.normalize();
	RightSteerTwistAxis.normalize();

	const float FingerOthers = pSettings->read_if_exists<float>(Section, "driver_hands_finger_others", 35.f);
	ReadDriverFingerAngles(*pSettings, Section, "driver_hands_finger_thumb", 30.f, FingerAngles[0], FingerAirAngles[0], FingerGearAngles[0]);
	ReadDriverFingerAngles(*pSettings, Section, "driver_hands_finger_index", 40.f, FingerAngles[1], FingerAirAngles[1], FingerGearAngles[1]);
	ReadDriverFingerAngles(*pSettings, Section, "driver_hands_finger_middle", 35.f, FingerAngles[2], FingerAirAngles[2], FingerGearAngles[2]);
	ReadDriverFingerAngles(*pSettings, Section, "driver_hands_finger_ring", FingerOthers, FingerAngles[3], FingerAirAngles[3], FingerGearAngles[3]);
	ReadDriverFingerAngles(*pSettings, Section, "driver_hands_finger_pinky", FingerOthers, FingerAngles[4], FingerAirAngles[4], FingerGearAngles[4]);

	for (u8 Group = 0; Group < FingerGroupCount; ++Group)
	{
		clamp(FingerAngles[Group], -90.f, 90.f);
		clamp(FingerAirAngles[Group], -90.f, 90.f);
		clamp(FingerGearAngles[Group], -90.f, 90.f);
	}

	FingerSpeed = pSettings->read_if_exists<float>(Section, "driver_hands_finger_speed", 12.f);
	clamp(FingerSpeed, 0.f, 120.f);
}

void CCarDriverHands::SetupFingers()
{
	// Curl axes are in the proximal (chain root) bone's local space. The local
	// axis is baked into the engine and the same for both hands; the geometric
	// hinge for the remaining fingers is rebuilt from the index finger at
	// calibration.
	const Fvector CurlAxis = {0.f, 0.f, -1.f};
	const Fvector ThumbAxis[2] = {{0.f, 0.f, -1.f}, {0.f, 0.f, 1.f}};

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
	FingerBoneRoot.assign(Kinematics->LL_BoneCount(), BI_NONE);
	const auto AddFinger = [this](u8 Hand, u8 Group, std::initializer_list<const char*> Names)
	{
		u16 Root = BI_NONE;
		for (const char* Name : Names)
		{
			const u16 Bone = Kinematics->LL_BoneID(Name);
			if (Bone != BI_NONE && Bone < FingerBoneGroup.size())
			{
				if (Root == BI_NONE)
				{
					Root = Bone;
				}

				FingerBoneGroup[Bone] = u8(Hand * FingerGroupCount + Group);
				FingerBoneRoot[Bone] = Root;
			}
		}
	};

	AddFinger(0, 0, {"l_finger0", "l_finger01", "l_finger02"});
	AddFinger(0, 1, {"bip01_l_finger1", "bip01_l_finger11", "bip01_l_finger12"});
	AddFinger(0, 2, {"bip01_l_finger2", "bip01_l_finger21", "bip01_l_finger22"});
	AddFinger(0, 3, {"bip01_l_finger3", "bip01_l_finger31", "bip01_l_finger32"});
	AddFinger(0, 4, {"bip01_l_finger4", "bip01_l_finger41", "bip01_l_finger42"});
	AddFinger(1, 0, {"r_finger0", "r_finger01", "r_finger02"});
	AddFinger(1, 1, {"bip01_r_finger1", "bip01_r_finger11", "bip01_r_finger12"});
	AddFinger(1, 2, {"bip01_r_finger2", "bip01_r_finger21", "bip01_r_finger22"});
	AddFinger(1, 3, {"bip01_r_finger3", "bip01_r_finger31", "bip01_r_finger32"});
	AddFinger(1, 4, {"bip01_r_finger4", "bip01_r_finger41", "bip01_r_finger42"});

	LeftThumbRoot = Kinematics->LL_BoneID("l_finger0");
	RightThumbRoot = Kinematics->LL_BoneID("r_finger0");
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
	GearActive = false;
	GearReturning = false;
	GearStartPending = false;
	GearBlend = 0.f;
	GearStartLocal.identity();
	LastGearLocal.identity();
	HasLastGearLocal = false;

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
	BindRenderPose.clear();
	FingerBoneAxis.clear();
	SteerBone = BI_NONE;
	GearBone = BI_NONE;
	GearBoneName = shared_str();
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
	ResetStates();
}

void CCarDriverHands::UpdateFingerStates(float Dt)
{
	const float Alpha = (FingerSpeed > 0.f) ? (1.f - expf(-FingerSpeed * Dt)) : 1.f;
	const u8 GearHandIndex = GearLeftHand ? u8(HandLeft - 1) : u8(HandRight - 1);
	for (u8 Hand = 0; Hand < 2; ++Hand)
	{
		const bool Active = (ActiveHand == u8(Hand + 1));
		// The selected gear hand uses its dedicated gear angle. The grip angle
		// is used on the rim, the air angle while the regripping hand is off
		// the wheel.
		const bool GearHand = (Hand == GearHandIndex) && GearActive &&
			(!GearReturning || GearBlend > 0.5f);
		const bool InAir = Active && Progress > EPS_L && Progress < 1.f - EPS_L;
		for (u8 Group = 0; Group < FingerGroupCount; ++Group)
		{
			SFingerBend& State = FingerState[Hand][Group];
			const float TargetDeg = GearHand ? FingerGearAngles[Group] : (InAir ? FingerAirAngles[Group] : FingerAngles[Group]);
			const float Target = deg2rad(TargetDeg);
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
		Instance.mRenderTransform.set(BindRenderPose[Bone]);
	}
}

void CCarDriverHands::ApplyFingerBend()
{
	// Bend the fingers using the per-bone local curl axes built once at load.
	// The delta is built around the bone's current model-space origin and the
	// chain axis; TransformBone applies it to the whole child chain so the
	// segments follow the parent instead of rotating in place.
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

		if (Bone >= FingerBoneAxis.size())
		{
			continue;
		}

		const Fvector& AxisLocal = FingerBoneAxis[Bone];
		if (AxisLocal.magnitude() <= EPS_L)
		{
			continue;
		}

		const Fmatrix& BoneXform = Kinematics->LL_GetBoneInstance(Bone).mTransform;

		Fvector AxisModel;
		BoneXform.transform_dir(AxisModel, AxisLocal);
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

void CCarDriverHands::BuildFingerAxes()
{
	FingerBoneAxis.clear();
	if (Kinematics == nullptr)
	{
		return;
	}

	const u16 BoneCount = Kinematics->LL_BoneCount();
	if (BindPose.size() < BoneCount)
	{
		return;
	}

	FingerBoneAxis.resize(BoneCount);
	for (Fvector& Axis : FingerBoneAxis)
	{
		Axis.set(0.f, 0.f, 0.f);
	}

	xr_vector<Fvector> FingerRootAxis(BoneCount);
	for (Fvector& Axis : FingerRootAxis)
	{
		Axis.set(0.f, 0.f, 0.f);
	}

	for (u16 Bone = 0; Bone < BoneCount; ++Bone)
	{
		if (Bone >= FingerBoneRoot.size())
		{
			break;
		}

		const u16 Root = FingerBoneRoot[Bone];
		if (Root == BI_NONE || Root >= FingerRootAxis.size() || Root >= FingerBoneGroup.size())
		{
			continue;
		}
		if (FingerRootAxis[Root].magnitude() > EPS_L)
		{
			continue;
		}

		const u8 RootState = FingerBoneGroup[Root];
		if (RootState == NoFingerGroup)
		{
			continue;
		}

		const SFingerBend& State = FingerState[RootState / FingerGroupCount][RootState % FingerGroupCount];
		const Fmatrix& RootXform = BindPose[Root];
		RootXform.transform_dir(FingerRootAxis[Root], State.Axis);
		if (FingerRootAxis[Root].magnitude() > EPS_L)
		{
			FingerRootAxis[Root].normalize();
		}
		else
		{
			FingerRootAxis[Root].set(0.f, 0.f, 0.f);
		}
	}

	// The middle and little fingers have their own root rolls in the model, so
	// using the same local axis on every finger tilts them sideways. Derive a
	// common palm normal from the index finger, then rebuild each finger's
	// hinge as palm_normal x finger_direction.
	for (u8 Hand = 0; Hand < 2; ++Hand)
	{
		u16 RootByGroup[FingerGroupCount] = {};
		for (u16 Bone = 0; Bone < BoneCount; ++Bone)
		{
			if (Bone >= FingerBoneGroup.size() || Bone >= FingerBoneRoot.size())
			{
				break;
			}

			const u8 FingerStateIdx = FingerBoneGroup[Bone];
			if (FingerStateIdx == NoFingerGroup || FingerStateIdx / FingerGroupCount != Hand)
			{
				continue;
			}

			const u8 Group = FingerStateIdx % FingerGroupCount;
			if (Group == 0)
			{
				continue;
			}

			const u16 Root = FingerBoneRoot[Bone];
			if (Root == Bone)
			{
				RootByGroup[Group] = Root;
			}
		}

		const u16 IndexRoot = RootByGroup[1];
		if (IndexRoot == BI_NONE || IndexRoot >= BindPose.size())
		{
			continue;
		}

		const auto BoneDirection = [&](u16 Root, Fvector& Dir)
		{
			if (Root == BI_NONE || Root >= BindPose.size())
			{
				return false;
			}

			const CBoneData& Data = Kinematics->LL_GetData(Root);
			if (Data.children.empty())
			{
				return false;
			}

			const u16 Child = Data.children.front()->GetSelfID();
			if (Child == BI_NONE || Child >= BindPose.size())
			{
				return false;
			}

			Dir.sub(BindPose[Child].c, BindPose[Root].c);
			if (Dir.magnitude() <= EPS_L)
			{
				return false;
			}
			Dir.normalize();
			return true;
		};

		Fvector IndexDir;
		Fvector IndexHinge = FingerRootAxis[IndexRoot];
		if (!BoneDirection(IndexRoot, IndexDir) || IndexHinge.magnitude() <= EPS_L)
		{
			continue;
		}

		IndexHinge.mad(IndexDir, -IndexHinge.dotproduct(IndexDir));
		if (IndexHinge.magnitude() <= EPS_L)
		{
			continue;
		}
		IndexHinge.normalize();

		Fvector PalmNormal;
		PalmNormal.crossproduct(IndexDir, IndexHinge);
		if (PalmNormal.magnitude() <= EPS_L)
		{
			continue;
		}
		PalmNormal.normalize();

		for (u8 Group = 1; Group < FingerGroupCount; ++Group)
		{
			const u16 Root = RootByGroup[Group];
			Fvector Dir;
			if (Root == BI_NONE || !BoneDirection(Root, Dir))
			{
				continue;
			}

			Fvector Hinge;
			Hinge.crossproduct(PalmNormal, Dir);
			if (Hinge.magnitude() <= EPS_L)
			{
				continue;
			}
			Hinge.normalize();
			if (Hinge.dotproduct(IndexHinge) < 0.f)
			{
				Hinge.mul(-1.f);
			}
			FingerRootAxis[Root] = Hinge;
		}
	}

	// Express the chain's model-space curl axis in each segment's own bind
	// frame. At runtime it follows the parent bend instead of staying fixed in
	// model space.
	for (u16 Bone = 0; Bone < BoneCount; ++Bone)
	{
		if (Bone >= FingerBoneRoot.size() || Bone >= BindPose.size())
		{
			break;
		}

		const u16 Root = FingerBoneRoot[Bone];
		if (Root == BI_NONE || Root >= FingerRootAxis.size())
		{
			continue;
		}
		if (FingerRootAxis[Root].magnitude() <= EPS_L)
		{
			continue;
		}

		Fmatrix BindInv;
		BindInv.invert(BindPose[Bone]);
		BindInv.transform_dir(FingerBoneAxis[Bone], FingerRootAxis[Root]);
		if (FingerBoneAxis[Bone].magnitude() > EPS_L)
		{
			FingerBoneAxis[Bone].normalize();
		}
		else
		{
			FingerBoneAxis[Bone].set(0.f, 0.f, 0.f);
		}
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

void CCarDriverHands::CalibrateHandOffsets(const Fmatrix& WheelNeutralWorld, const Fmatrix& HandsInverse, const char* CarSection)
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

	// Hand rotation offsets are authored in one common coordinate system:
	// the left hand's local frame. The right hand's local frame is mirrored,
	// so its local rotation is derived from the same common angles below.
	Fmatrix LeftCommonRot;
	if (LeftHandRotationExplicit)
	{
		LeftCommonRot.setXYZ(deg2rad(LeftHandRotation.x), deg2rad(LeftHandRotation.y), deg2rad(LeftHandRotation.z));
	}
	else if (RightHandRotationExplicit)
	{
		LeftCommonRot.setXYZ(deg2rad(RightHandRotation.x), deg2rad(RightHandRotation.y), deg2rad(RightHandRotation.z));
	}
	else
	{
		LeftCommonRot.identity();
	}

	Fmatrix RightCommonRot;
	if (RightHandRotationExplicit)
	{
		RightCommonRot.setXYZ(deg2rad(RightHandRotation.x), deg2rad(RightHandRotation.y), deg2rad(RightHandRotation.z));
	}
	else
	{
		RightCommonRot = LeftCommonRot;
	}

	Fmatrix LeftConfigRot = LeftCommonRot;

	Fmatrix HandRotLeft = Kinematics->LL_GetTransform(LeftIK[2]);
	HandRotLeft.c.set(0.f, 0.f, 0.f);
	Fmatrix HandRotRight = Kinematics->LL_GetTransform(RightIK[2]);
	HandRotRight.c.set(0.f, 0.f, 0.f);

	Fmatrix Mirror;
	Mirror.identity();
	Mirror.i.x = -1.f;

	// Mirror about the wheel's neutral left-right plane, not the hands
	// model X axis: on an offset or tilted wheel those differ.
	Fvector Normal;
	HandsInverse.transform_dir(Normal, WheelNeutralWorld.i);
	if (Normal.magnitude() > EPS_L)
	{
		Normal.normalize();
		const float nx = Normal.x;
		const float ny = Normal.y;
		const float nz = Normal.z;
		Mirror._11 = 1.f - 2.f * nx * nx;
		Mirror._12 = -2.f * nx * ny;
		Mirror._13 = -2.f * nx * nz;
		Mirror._21 = -2.f * ny * nx;
		Mirror._22 = 1.f - 2.f * ny * ny;
		Mirror._23 = -2.f * ny * nz;
		Mirror._31 = -2.f * nz * nx;
		Mirror._32 = -2.f * nz * ny;
		Mirror._33 = 1.f - 2.f * nz * nz;
	}

	Fmatrix LeftInv;
	LeftInv.invert(HandRotLeft);
	Fmatrix RightInv;
	RightInv.invert(HandRotRight);

	Fmatrix RightConfigRot;

	// Convert the common rotation into the right hand's local frame:
	// C_right = B_right * M * B_left^-1 * C_common * B_left * M * B_right^-1.
	// Fmatrix::mul_43(A, B) computes B * A, so the operands are passed in
	// the reverse order.
	Fmatrix Temp;
	Temp.mul_43(Mirror, HandRotRight);           // B_right * M
	Fmatrix Temp2;
	Temp2.mul_43(LeftInv, Temp);                 // Temp * B_left^-1
	Fmatrix Temp3;
	Temp3.mul_43(RightCommonRot, Temp2);         // Temp2 * C_common
	Temp.mul_43(HandRotLeft, Temp3);             // Temp3 * B_left
	Fmatrix Temp4;
	Temp4.mul_43(Mirror, Temp);                  // Temp * M
	RightConfigRot.mul_43(RightInv, Temp4);      // Temp4 * B_right^-1

	// The gear lever keeps the selected hand's authored local orientation
	// instead of taking the lever bone's arbitrary axes. This is the same
	// local rotation the hand has on the wheel at its neutral position.
	LeftNeutralLocalRot = Kinematics->LL_GetTransform(LeftIK[2]);
	LeftNeutralLocalRot.c.set(0.f, 0.f, 0.f);
	LeftNeutralLocalRot.mulB_43(LeftConfigRot);

	RightNeutralLocalRot = Kinematics->LL_GetTransform(RightIK[2]);
	RightNeutralLocalRot.c.set(0.f, 0.f, 0.f);
	RightNeutralLocalRot.mulB_43(RightConfigRot);

	// The twist axis is applied in the wheel's local space, where the left
	// and right axes are mirror images: only the local X component changes
	// sign. Do not convert it through the hand bones; that produced a wrong
	// direction for the derived hand.
	if (RightSteerTwistAxisExplicit && !LeftSteerTwistAxisExplicit)
	{
		LeftSteerTwistAxis = RightSteerTwistAxis;
		LeftSteerTwistAxis.x = -LeftSteerTwistAxis.x;
	}
	else if (LeftSteerTwistAxisExplicit && !RightSteerTwistAxisExplicit)
	{
		RightSteerTwistAxis = LeftSteerTwistAxis;
		RightSteerTwistAxis.x = -RightSteerTwistAxis.x;
	}

	// The thumb bones use a different local Z direction on the left and
	// right hand. A raw local vector therefore bends the left thumb
	// sideways. Mirror the right thumb's model-space hinge axis through
	// the same bind-plane transform and convert it back to the left root's
	// local space.
	if (LeftThumbRoot != BI_NONE &&
		RightThumbRoot != BI_NONE &&
		LeftThumbRoot < BindPose.size() &&
		RightThumbRoot < BindPose.size())
	{
		Fmatrix ThumbLeft = BindPose[LeftThumbRoot];
		ThumbLeft.c.set(0.f, 0.f, 0.f);
		Fmatrix ThumbRight = BindPose[RightThumbRoot];
		ThumbRight.c.set(0.f, 0.f, 0.f);
		Fmatrix ThumbLeftInv;
		ThumbLeftInv.invert(ThumbLeft);

		Fvector RightModel;
		ThumbRight.transform_dir(RightModel, FingerState[1][0].Axis);
		Fvector Mirrored;
		Mirror.transform_dir(Mirrored, RightModel);
		Mirrored.mul(-1.f);

		Fvector LeftLocal;
		ThumbLeftInv.transform_dir(LeftLocal, Mirrored);
		if (LeftLocal.magnitude() > EPS_L)
		{
			LeftLocal.normalize();
			FingerState[0][0].Axis = LeftLocal;
		}
	}

	// The thumb axis above may have been mirrored during calibration, so
	// rebuild the cached finger hinges with the final axes.
	BuildFingerAxes();

	if (RegripDebug)
	{
		Msg("* [%s] driver hands twist axes: left=(%.3f, %.3f, %.3f) right=(%.3f, %.3f, %.3f)",
			CarSection,
			LeftSteerTwistAxis.x, LeftSteerTwistAxis.y, LeftSteerTwistAxis.z,
			RightSteerTwistAxis.x, RightSteerTwistAxis.y, RightSteerTwistAxis.z);
		Msg("* [%s] driver hands thumb axes: left=(%.3f, %.3f, %.3f) right=(%.3f, %.3f, %.3f)",
			CarSection,
			FingerState[0][0].Axis.x, FingerState[0][0].Axis.y, FingerState[0][0].Axis.z,
			FingerState[1][0].Axis.x, FingerState[1][0].Axis.y, FingerState[1][0].Axis.z);
		Fvector LeftHandEuler;
		LeftCommonRot.getXYZ(LeftHandEuler);
		Fvector RightHandEuler;
		RightCommonRot.getXYZ(RightHandEuler);
		Msg("* [%s] driver hands rotation (common): left=(%.2f, %.2f, %.2f) right=(%.2f, %.2f, %.2f)",
			CarSection,
			rad2deg(LeftHandEuler.x), rad2deg(LeftHandEuler.y), rad2deg(LeftHandEuler.z),
			rad2deg(RightHandEuler.x), rad2deg(RightHandEuler.y), rad2deg(RightHandEuler.z));
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
	float TargetProgress = 0.f;
	if (RegripEnabled && EndRad > TriggerRad)
	{
		TargetProgress = std::clamp((AbsWheelAngle - TriggerRad) / (EndRad - TriggerRad), 0.f, 1.f);
	}

	const s8 WheelSign = SignOf(WheelAngle);

	const u8 LowerHand = GetLowerHand(WheelAngle);

	// The wheel crossed the centre. The previous hand must return to its home
	// grip before the other hand can start; keep it active until the lag
	// brings the phase back to zero.
	if (ActiveHand != HandNone && WheelSign != 0 && ActiveSign != 0 && WheelSign != ActiveSign)
	{
		TargetProgress = 0.f;
	}

	// The gear lever has priority over the gear hand's regrip while the hand is
	// actually on the lever. A hand that was already moving is released
	// immediately, and the gear blend takes over from its last solved pose. An
	// already active other-hand regrip may finish its path.
	const u8 GearHand = GearLeftHand ? HandLeft : HandRight;
	const u8 OtherHand = GearLeftHand ? HandRight : HandLeft;
	const bool GearHold = GearActive && !GearReturning;
	if (GearHold)
	{
		if (ActiveHand == GearHand)
		{
			ActiveHand = HandNone;
			ActiveSign = 0;
			Progress = 0.f;
		}

		if (ActiveHand != OtherHand)
		{
			TargetProgress = 0.f;
		}
	}

	// On the way back from the lever the hand should land on the grip it would
	// have on the turned wheel, not on its home grip followed by a second
	// regrip. If the gear hand is the lower hand, select it and snap its phase
	// to the wheel-angle target for the return blend.
	bool SnapGearReturn = false;
	if (GearActive && GearReturning &&
		(ActiveHand == HandNone || ActiveHand == GearHand) &&
		LowerHand == GearHand && WheelSign != 0 && TargetProgress > 0.f)
	{
		ActiveHand = GearHand;
		ActiveSign = WheelSign;
		SnapGearReturn = true;
	}

	// The phase lag drives the hand-over-hand move. The active hand is built
	// from a phase wheel transform below, so its grip points stay fixed in
	// world space even when the real wheel keeps turning under the hand.
	const float Dt = std::clamp(Device.fTimeDelta, 0.f, 0.1f);
	if (SnapGearReturn)
	{
		Progress = TargetProgress;
	}
	else
	{
		Progress = SmoothHandReleasePhase(Progress, TargetProgress, RegripLag, Dt);
	}

	if (Progress <= 0.f)
	{
		Progress = 0.f;
		ActiveHand = HandNone;
		ActiveSign = 0;
	}
	else if (ActiveHand == HandNone)
	{
		ActiveHand = LowerHand;
		ActiveSign = WheelSign;
	}

	if (RegripDebug && ActiveHand != DebugHand)
	{
		DebugHand = ActiveHand;
		Msg("* [%s] driver hands: active=%s W=%.1f p=%.2f", CarSection, ActiveHand == HandLeft ? "left" : (ActiveHand == HandRight ? "right" : "none"), rad2deg(WheelAngle), Progress);
	}
}

void CCarDriverHands::UpdateGearState(const char* CarSection, bool Switching, bool ReverseSwitching, float SwitchEnd, float Dt)
{
	if (GearBone == BI_NONE)
	{
		GearActive = false;
		GearReturning = false;
		GearStartPending = false;
		GearBlend = 0.f;
		return;
	}

	// In an automatic gearbox the hand only takes part in forward <-> reverse
	// changes; ordinary automatic up/down shifts keep it on the wheel.
	const bool GearSwitching = Switching && (!GearAuto || ReverseSwitching);

	// GearSwitching is the start/hold trigger: it is lowered by the car after
	// transmission_switch_delay. The reported switch end already includes the
	// gear lever's start delay; GearHoldExtra adds the tail after the lever has
	// started.
	const bool ExtraHold =
		!GearSwitching &&
		GearActive &&
		!GearReturning &&
		SwitchEnd > 0.f &&
		Device.fTimeGlobal < SwitchEnd + GearHoldExtra;
	const bool Hold = GearSwitching || ExtraHold;

	if (Hold)
	{
		if (!GearActive)
		{
			GearActive = true;
			GearReturning = false;
			GearStartPending = true;
			GearBlend = 0.f;
			if (GearDebug)
			{
				Msg("* [%s] driver hands gear: start (%s)", CarSection, GearBoneName.c_str());
			}
			return;
		}
		else if (GearReturning && GearSwitching)
		{
			// A new shift interrupted the return. Start again from the pose
			// the hand has at this moment, without snapping back to the wheel.
			GearReturning = false;
			GearStartPending = true;
			GearBlend = 0.f;
			if (GearDebug)
			{
				Msg("* [%s] driver hands gear: restart", CarSection);
			}
			return;
		}
	}
	else if (GearActive && !GearReturning)
	{
		GearReturning = true;
		if (GearDebug)
		{
			Msg("* [%s] driver hands gear: return", CarSection);
		}
		return;
	}

	if (!GearActive)
	{
		return;
	}

	const float Direction = GearReturning ? -1.f : 1.f;
	GearBlend = std::clamp(GearBlend + Direction * GearSpeed * Dt, 0.f, 1.f);
}

void CCarDriverHands::SolveArms(const Fmatrix& WheelWorld, const Fmatrix& GearWorld, float WheelAngle, const Fmatrix& HandsInverse)
{
	const float AbsWheelAngle = fabsf(WheelAngle);
	const float TriggerRad = deg2rad(RegripTrigger);
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

	// The active hand follows the fixed phase path while it travels between the
	// grip points. The blend returns it to the real wheel at both ends: the hand
	// starts on the rim and, once it has regripped, keeps moving with the wheel.
	Fmatrix PhaseWheel = WheelWorld;
	if (ActiveHand != HandNone && ActiveSign != 0)
	{
		constexpr float StartBlend = 0.1f;
		constexpr float EndBlend = 0.2f;
		const float Blend = std::min(
			std::clamp(Progress / StartBlend, 0.f, 1.f),
			std::clamp((1.f - Progress) / EndBlend, 0.f, 1.f));
		const float PhaseAngle = float(ActiveSign) * (TriggerRad + Progress * (EndRad - TriggerRad));
		Fmatrix PhaseRot;
		PhaseRot.rotateZ((PhaseAngle - WheelAngle) * Blend);
		PhaseWheel.mulB_43(PhaseRot);
	}

	const Fmatrix& LeftWheel = (ActiveHand == HandLeft) ? PhaseWheel : WheelWorld;
	const Fmatrix& RightWheel = (ActiveHand == HandRight) ? PhaseWheel : WheelWorld;

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

	const auto BuildTarget = [&](const Fmatrix& WheelXform, const SHandState& Hand, float LocalAngle, float HandTwist, const Fvector& TwistAxis, const Fmatrix& BaseOffset, float LiftScale, Fmatrix& Result)
	{
		const float Radius = Hand.Radius + RegripOverRadial * LiftScale;
		const Fvector Local = Fvector().set(cosf(LocalAngle) * Radius, sinf(LocalAngle) * Radius, Hand.Z);
		Fvector World;
		WheelXform.transform_tiny(World, Local);
		if (LiftScale > 0.f)
		{
			World.mad(LiftDir, RegripLift * LiftScale);
		}

		Fmatrix AngleRot;
		AngleRot.rotateZ(LocalAngle - Hand.HomeAngle);

		if (fabsf(HandTwist) > EPS_L)
		{
			Fmatrix TwistRot;
			TwistRot.rotation(TwistAxis, HandTwist);
			Fmatrix Rotated;
			Rotated.mul_43(TwistRot, AngleRot); // standard AngleRot * TwistRot
			AngleRot = Rotated;
		}

		Fmatrix Offset;
		Offset.mul_43(AngleRot, BaseOffset);

		Fmatrix WheelRotation = WheelXform;
		WheelRotation.c.set(0.f, 0.f, 0.f);
		Fmatrix HandRotation;
		HandRotation.mul_43(WheelRotation, Offset);

		Result = HandRotation;
		Result.c = World;
	};

	Fmatrix LeftTarget;
	Fmatrix RightTarget;
	BuildTarget(LeftWheel, LeftState, LeftLocal, LeftTwist, LeftSteerTwistAxis, LeftHandOffset, ActiveHand == HandLeft ? PathLift : 0.f, LeftTarget);
	BuildTarget(RightWheel, RightState, RightLocal, RightTwist, RightSteerTwistAxis, RightHandOffset, ActiveHand == HandRight ? PathLift : 0.f, RightTarget);

	if (GearActive)
	{
		// The selected hand keeps its authored local orientation on the lever.
		// The lever bone supplies only the grip position, so its local axes
		// cannot twist the palm; GearRotation is the wrist offset in the hand's
		// local frame.
		Fmatrix GearTargetLocal = GearLeftHand ? LeftNeutralLocalRot : RightNeutralLocalRot;
		Fmatrix WristRot;
		WristRot.setXYZ(deg2rad(GearRotation.x), deg2rad(GearRotation.y), deg2rad(GearRotation.z));
		GearTargetLocal.mulB_43(WristRot);

		Fvector GearWorldPos;
		GearWorld.transform_tiny(GearWorldPos, GearOffset);
		HandsInverse.transform_tiny(GearTargetLocal.c, GearWorldPos);

		Fmatrix& GearWheelTarget = GearLeftHand ? LeftTarget : RightTarget;

		// Blend in hands-local space. The seat and the lever move with the car,
		// so a world-space start pose would lag behind while the car moves.
		Fmatrix GearWheelTargetLocal;
		GearWheelTargetLocal.mul_43(HandsInverse, GearWheelTarget);

		if (GearStartPending)
		{
			GearStartLocal = HasLastGearLocal ? LastGearLocal : GearWheelTargetLocal;
			GearStartPending = false;
		}

		const float T = std::clamp(GearBlend, 0.f, 1.f);
		const Fmatrix& Source = GearReturning ? GearWheelTargetLocal : GearStartLocal;
		Fmatrix FinalGearLocal;
		BlendHandTargets(Source, GearTargetLocal, T, FinalGearLocal);

		Fvector Arc;
		Arc.mul(GearPath, sinf(PI * T));
		FinalGearLocal.c.add(Arc);

		GearWheelTarget.mul_43(Xform, FinalGearLocal);
	}

	const auto Solve = [this, &HandsInverse](const u16* Chain, u16 Clavicle, const Fmatrix& TargetWorld, const Fvector& Elbow)
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

		if (DesiredShift.magnitude() > EPS_L)
		{
			Fmatrix Shift;
			Shift.identity();
			Shift.c.set(DesiredShift);
			const u16 ShiftBone = Clavicle != BI_NONE ? Clavicle : Chain[0];
			CCameraRig::TransformBone(Kinematics, ShiftBone, Shift);
		}

		CCameraRig::SolveLimb(Kinematics, Chain, Target, 1.f, Elbow);
	};

	Fvector LeftPole = LeftElbow;
	Fvector RightPole = RightElbow;
	if (GearActive)
	{
		const float T = std::clamp(GearBlend, 0.f, 1.f);
		if (GearLeftHand)
		{
			LeftPole.lerp(LeftElbow, GearElbow, T);
		}
		else
		{
			RightPole.lerp(RightElbow, GearElbow, T);
		}
	}

	Solve(LeftIK, LeftClavicle, LeftTarget, LeftPole);
	Solve(RightIK, RightClavicle, RightTarget, RightPole);

	// Store the selected hand's actual solved transform, not only the requested
	// target: if the arm could not reach the target, the next gear blend must
	// start from where the hand really is.
	LastGearLocal = Kinematics->LL_GetTransform(GearLeftHand ? LeftIK[2] : RightIK[2]);
	HasLastGearLocal = true;
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

	ApplyBindPose();
	ApplyShoulderWidth();

	const float Dt = std::clamp(Device.fTimeDelta, 0.f, 0.1f);

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

	Fmatrix GearWorld = WheelWorld;
	if (GearBone != BI_NONE && GearBone < CarKinematics->LL_BoneCount())
	{
		CarKinematics->LL_GetBoneWorldTransform(GearBone, Params.CarXform, GearWorld);
	}
	else if (GearBone != BI_NONE)
	{
		GearBone = BI_NONE;
	}

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

		CalibrateHandOffsets(WheelNeutralWorld, HandsInverse, Params.CarSection);
	}

	const float WheelAngle = Params.SteerAngle * (SteerFactored ? SteerFactor * rs_car_steer_factor : 1.f);
	UpdateGearState(Params.CarSection, Params.TransmissionSwitching, Params.ReverseSwitching, Params.TransmissionSwitchEnd, Dt);
	UpdateRegrip(WheelAngle, Params.CarSection);
	UpdateFingerStates(Dt);
	ApplyFingerBend();
	SolveArms(WheelWorld, GearWorld, WheelAngle, HandsInverse);

	if (GearActive && GearReturning && GearBlend <= 0.f)
	{
		GearActive = false;
		GearReturning = false;
		GearStartPending = false;
		if (GearDebug)
		{
			Msg("* [%s] driver hands gear: done", Params.CarSection);
		}
	}

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
