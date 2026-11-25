#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead)
struct SAIStalkerQueue
{
	XPROPERTY(IniReadExist, Name="min_queue_size_far")
	u32 MinSizeFar = 0;
	XPROPERTY(IniReadExist, Name="max_queue_size_far")
	u32 MaxSizeFar = 0;
	XPROPERTY(IniReadExist, Name="min_queue_interval_far")
	u32 MinIntervalFar = 0;
	XPROPERTY(IniReadExist, Name="max_queue_interval_far")
	u32 MaxIntervalFar = 0;
	XPROPERTY(IniReadExist, Name="min_queue_size_medium")
	u32 MinSizeMedium = 0;
	XPROPERTY(IniReadExist, Name="max_queue_size_medium")
	u32 MaxSizeMedium = 0;
	XPROPERTY(IniReadExist, Name="min_queue_interval_medium")
	u32 MinIntervalMedium = 0;
	XPROPERTY(IniReadExist, Name="max_queue_interval_medium")
	u32 MaxIntervalMedium = 0;
	XPROPERTY(IniReadExist, Name="min_queue_size_close")
	u32 MinSizeClose = 0;
	XPROPERTY(IniReadExist, Name="max_queue_size_close")
	u32 MaxSizeClose = 0;
	XPROPERTY(IniReadExist, Name="min_queue_interval_close")
	u32 MinIntervalClose = 0;
	XPROPERTY(IniReadExist, Name="max_queue_interval_close")
	u32 MaxIntervalClose = 0;
	XPROPERTY(IniReadExist, Name="queue_fire_dist_med")
	float FireDistMed = 15.0f;
	XPROPERTY(IniReadExist, Name="queue_fire_dist_far")
	float FireDistFar = 30.0f;
};

XREFLECT(IniRead)
struct SAIStalkerQueues
{
	XPROPERTY(IniRead, Prefix="pstl_")
	SAIStalkerQueue Pistol = { 1, 1, 1000, 1250, 2, 4, 750, 1000, 3, 5, 500, 750, 15.0f, 30.0f };
	XPROPERTY(IniRead, Prefix="shtg_")
	SAIStalkerQueue Shotgun = { 1, 1, 1250, 1500, 1, 1, 750, 1250, 1, 1, 500, 1000, 15.0f, 30.0f };
	XPROPERTY(IniRead, Prefix="snp_")
	SAIStalkerQueue Sniper = { 1, 1, 3000, 4000, 1, 1, 3000, 4000, 1, 1, 3000, 4000, 15.0f, 30.0f };
	XPROPERTY(IniRead, Prefix="mchg_")
	SAIStalkerQueue MachineGun = { 1, 6, 500, 1000, 4, 6, 500, 750, 4, 10, 300, 500, 15.0f, 30.0f };
	XPROPERTY(IniRead, Prefix="auto_")
	SAIStalkerQueue Auto = { 1, 6, 500, 1000, 4, 6, 500, 750, 4, 10, 300, 500, 15.0f, 30.0f };
};

XREFLECT(IniRead, Owner="CAI_Stalker")
struct SAIStalker final
{
	using Registry = TDescRegistry<SAIStalker>;

	XPROPERTY(IniRead, Name="disp_walk_stand")
	float DispWalkStand = 0.0f;
	XPROPERTY(IniRead, Name="disp_walk_crouch")
	float DispWalkCrouch = 0.0f;
	XPROPERTY(IniRead, Name="disp_run_stand")
	float DispRunStand = 0.0f;
	XPROPERTY(IniRead, Name="disp_run_crouch")
	float DispRunCrouch = 0.0f;
	XPROPERTY(IniRead, Name="disp_stand_stand")
	float DispStandStand = 0.0f;
	XPROPERTY(IniRead, Name="disp_stand_crouch")
	float DispStandCrouch = 0.0f;
	XPROPERTY(IniRead, Name="disp_stand_stand_zoom")
	float DispStandStandZoom = 0.0f;
	XPROPERTY(IniRead, Name="disp_stand_crouch_zoom")
	float DispStandCrouchZoom = 0.0f;

	XPROPERTY(IniRead, Name="power_fx_factor")
	float PowerFxFactor = 0.0f;
	XPROPERTY(IniRead, Name="can_select_items")
	bool CanSelectItems = false;

	XPROPERTY(IniReadExist, Name="max_left_torso_angle")
	float MaxLeftTorsoAngle = 90.0f;
	XPROPERTY(IniReadExist, Name="max_right_torso_angle")
	float MaxRightTorsoAngle = 60.0f;

	XPROPERTY(IniRead, Name="bone_spin")
	shared_str BoneSpine;
	XPROPERTY(IniRead, Name="bone_shoulder")
	shared_str BoneShoulder;
	XPROPERTY(IniRead, Name="bone_head")
	shared_str BoneHead;
	XPROPERTY(IniRead, Name="weapon_bone0")
	shared_str WeaponBone0;
	XPROPERTY(IniRead, Name="weapon_bone2")
	shared_str WeaponBone2;

	XPROPERTY(IniReadExist, Name="fire_queue_section")
	shared_str FireQueueSection;
	SAIStalkerQueues Queues;

	void Load(const shared_str& Section);
};
