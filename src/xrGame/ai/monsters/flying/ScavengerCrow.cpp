#include "StdAfx.h"
#include "ScavengerCrow.h"
#include "CrowSharedMemory.h"
#include "../../../Level.h"
#include "../../../../Include/xrRender/Kinematics.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"

bool CScavengerCrow::net_Spawn(CSE_Abstract* data)
{
    detach_ground_pose_callbacks();
    if (!CFlyingMonster::net_Spawn(data))
        return false;
    start_crow_behavior();
    if (!RestoreOfflineCrow()) return false;
	EnsureAirMotion();
    CCrowSharedMemory::Get().RegisterBird(ID());
	OfflineSyncInterval = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_offline_sync_interval", .25f);
	R_ASSERT2(_valid(OfflineSyncInterval) && OfflineSyncInterval >= .05f && OfflineSyncInterval <= 5.f, "Invalid crow offline sync interval");
	OfflineSyncTick = float(ID() % 101u) * OfflineSyncInterval / 101.f;
    m_ground_pose_weight = flight().grounded() ? 1.f : 0.f;
    m_ground_pose_speed = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_fold_speed", 4.f);
    clamp(m_ground_pose_speed, .1f, 20.f);
    PerchLookAngles.set(0,0,0);
    PreviousLookAngles.set(0,0,0);
    PreviousHopHeight = 0.f;
    IdleBodyMotion.identity();
    IdleBodyInverse.identity();
    IdleBodyBone = IdleTailBone = BI_NONE;
    IdlePhase = Random.randF(0.f, PI_MUL_2);
    IdleFrequency = Random.randF(.8f, 1.2f);
    IdleTick = IdleHeadPitch = RuffleRemaining = LandingSettleRemaining = 0.f;
    WasGrounded = flight().grounded();
    VisualPoseTick = 0.f;
    AnimationLodTick = float(ID() % 101u) * .2f / 101.f;
    NativeAnimationTick = 0.f;
    DetailedAnimation = true;
	AnimationVisibleFrame = u32(-1);
	AnimationWasVisible = false;
    DetailDistance = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_animation_detail_distance", 30.f);
    FocusDistance = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_animation_focus_distance", 100.f);
    FocusFraction = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_animation_focus_fraction", .3f);
    R_ASSERT2(_valid(DetailDistance) && DetailDistance > 0.f && _valid(FocusDistance) && FocusDistance >= DetailDistance &&
        _valid(FocusFraction) && FocusFraction > 0.f && FocusFraction <= 1.f, "Invalid crow animation LOD settings");
    CachedGroundPose = false;
    PeckSolutionValid = PeckCanReach = false;
    FoldFeatherWidth = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_fold_feather_width", .35f);
    FoldFeatherLength = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_fold_feather_length", .9f);
    R_ASSERT2(_valid(FoldFeatherWidth) && FoldFeatherWidth >= .2f && FoldFeatherWidth <= 1.f &&
        _valid(FoldFeatherLength) && FoldFeatherLength >= .7f && FoldFeatherLength <= 1.f, "Invalid crow feather folding settings");
    IdleAmount = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_idle_amount", 1.f);
    RuffleIntervalMin = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_idle_ruffle_interval_min", 4.f);
    RuffleIntervalMax = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_idle_ruffle_interval_max", 10.f);
    LandingSettleDuration = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_landing_settle_duration", .55f);
    R_ASSERT2(_valid(IdleAmount) && IdleAmount >= 0.f && IdleAmount <= 2.f &&
        _valid(RuffleIntervalMin) && _valid(RuffleIntervalMax) && RuffleIntervalMin > 0.f && RuffleIntervalMax >= RuffleIntervalMin &&
        _valid(LandingSettleDuration) && LandingSettleDuration > 0.f && LandingSettleDuration <= 2.f, "Invalid crow idle settings");
    RuffleTimer = Random.randF(0.f, RuffleIntervalMax);
    LookYawAxis = READ_IF_EXISTS(pSettings, r_fvector3, cNameSect().c_str(), "crow_look_yaw_axis", Fvector().set(0,1,0));
    LookPitchAxis = READ_IF_EXISTS(pSettings, r_fvector3, cNameSect().c_str(), "crow_look_pitch_axis", Fvector().set(0,0,1));
    HeadLookSpeed = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_head_look_speed", 4.f);
    HeadPitchRange = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_head_pitch_range", 12.f) * PI / 180.f;
    NeckLookShare = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_neck_look_share", .35f);
    LookIntervalMin = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_look_interval_min", 2.f);
    LookIntervalMax = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_look_interval_max", 5.f);
    LookDuration = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_look_duration", 1.f);
    LookAngle = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_look_angle", 30.f) * PI / 180.f;
    HopWingAmount = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_hop_wing_weight", .65f);
    WingOpenSpeed = READ_IF_EXISTS(pSettings, r_float, cNameSect().c_str(), "crow_fold_open_speed", 10.f);
    R_ASSERT2(_valid(LookIntervalMin) && _valid(LookIntervalMax) && LookIntervalMin > 0.f && LookIntervalMax >= LookIntervalMin &&
        _valid(LookDuration) && LookDuration > 0.f && _valid(LookAngle) && LookAngle > 0.f && LookAngle <= PI_DIV_2 &&
        _valid(HopWingAmount) && HopWingAmount >= 0.f && HopWingAmount <= 1.f &&
        _valid(WingOpenSpeed) && WingOpenSpeed > 0.f && WingOpenSpeed <= 20.f, "Invalid crow perch motion settings");
    PerchLookTimer = Random.randF(0.f, LookIntervalMin);
    LookRemaining = 0.f;
    R_ASSERT2(_valid(LookYawAxis) && _valid(LookPitchAxis) && LookYawAxis.square_magnitude() > EPS_L &&
        LookPitchAxis.square_magnitude() > EPS_L && _valid(HeadLookSpeed) && HeadLookSpeed > 0.f &&
        _valid(HeadPitchRange) && HeadPitchRange >= 0.f && HeadPitchRange <= PI_DIV_2 &&
        _valid(NeckLookShare) && NeckLookShare >= 0.f && NeckLookShare <= 1.f, "Invalid crow head look settings");
    LookYawAxis.normalize();
    LookPitchAxis.normalize();
    if (!g_Alive() || PPhysicsShell())
        return true;
    auto* skeleton = Visual()->dcast_PKinematics();
    // fly_idle is still an airborne pose: folding the wings alone can leave
    // its body and feet in flight posture. Blend the rig to the standing bind pose,
    // then replace the wing rotations. Keep the userdata storage stable after
    // callbacks have been installed.
    m_ground_pose_bones.resize(skeleton->LL_BoneCount());
    for (u16 id = 0; id < skeleton->LL_BoneCount(); ++id)
    {
        auto& pose = m_ground_pose_bones[id];
        pose.owner = this;
        pose.id = id;
        const auto& data = skeleton->GetBoneData(id);
        pose.parent = data.GetParentID();
        pose.target = data.get_bind_transform();
        pose.wing = false;
        pose.WingSide = 0;
        pose.FootAnchor = false;
        pose.IdleAngles.set(0,0,0);
        pose.peck_angles.set(0,0,0);
        pose.ik_active = false;
    }
    const char* bone_settings[] = {"crow_left_shoulder_bone", "crow_left_elbow_bone", "crow_left_wrist_bone",
        "crow_left_finger_bone", "crow_right_shoulder_bone", "crow_right_elbow_bone", "crow_right_wrist_bone", "crow_right_finger_bone"};
    const char* settings[] = {"crow_fold_shoulder", "crow_fold_elbow", "crow_fold_wrist", "crow_fold_finger"};
    for (u32 i = 0; i < 8; ++i)
    {
        const char* name = READ_IF_EXISTS(pSettings, r_string, cNameSect().c_str(), bone_settings[i], "");
        if (!name[0])
            continue;
        const u16 id = skeleton->LL_BoneID(name);
        if (id == BI_NONE)
        {
            Msg("! [scavenger crow] wing bone missing: %s (%s)", name, bone_settings[i]);
            continue;
        }
        if (!pSettings->line_exist(cNameSect().c_str(), settings[i % 4]))
            continue;
        Fvector rotation = pSettings->r_fvector3(cNameSect().c_str(), settings[i % 4]);
        rotation.mul(PI / 180.f);
        if (i >= 4)
        {
            rotation.x = -rotation.x;
            rotation.z = -rotation.z;
        }
        auto& pose = m_ground_pose_bones[id];
        pose.wing = true;
        pose.WingSide = i < 4 ? -1 : 1;
        pose.WingStage = i % 4;
        const Fvector position = pose.target.c;
        pose.target.setXYZi(rotation);
        pose.target.c = position;
    }
    IdleBodyBone = skeleton->LL_BoneID(READ_IF_EXISTS(pSettings, r_string, cNameSect().c_str(), "crow_idle_body_bone", "bip01_pelvis"));
    IdleTailBone = skeleton->LL_BoneID(READ_IF_EXISTS(pSettings, r_string, cNameSect().c_str(), "crow_idle_tail_bone", "bip01_tail"));
    // Counter the body's visual motion at the leg roots, leaving both feet planted.
    const char* LegSettings[] = {"crow_idle_left_leg_bone", "crow_idle_right_leg_bone"};
    const char* LegDefaults[] = {"bip01_l_thigh", "bip01_r_thigh"};
    bool FeetAnchored = IdleBodyBone != BI_NONE && !skeleton->LL_GetBoneInstance(IdleBodyBone).callback();
    for (u32 Index = 0; Index < 2; ++Index)
    {
        const u16 Bone = skeleton->LL_BoneID(READ_IF_EXISTS(pSettings, r_string, cNameSect().c_str(), LegSettings[Index], LegDefaults[Index]));
        if (Bone == BI_NONE || m_ground_pose_bones[Bone].parent != IdleBodyBone || skeleton->LL_GetBoneInstance(Bone).callback())
        {
            FeetAnchored = false;
            continue;
        }
        m_ground_pose_bones[Bone].FootAnchor = true;
    }
    if (!FeetAnchored)
    {
        IdleBodyBone = BI_NONE;
        for (auto& Pose : m_ground_pose_bones) Pose.FootAnchor = false;
    }
    const char* peck_bones[] = {"crow_peck_neck_bone", "crow_peck_head_bone"};
    const char* peck_settings[] = {"crow_peck_neck_angles", "crow_peck_head_angles"};
    for (u32 i = 0; i < 2; ++i)
    {
        const char* name = READ_IF_EXISTS(pSettings, r_string, cNameSect().c_str(), peck_bones[i], "");
        if (!name[0] || !pSettings->line_exist(cNameSect().c_str(), peck_settings[i])) continue;
        const u16 id = skeleton->LL_BoneID(name);
        if (id == BI_NONE) { Msg("! [scavenger crow] peck bone missing: %s", name); continue; }
        m_ground_pose_bones[id].peck_angles = pSettings->r_fvector3(cNameSect().c_str(), peck_settings[i]);
        m_ground_pose_bones[id].peck_angles.mul(PI / 180.f);
    }
    m_peck_neck = skeleton->LL_BoneID(READ_IF_EXISTS(pSettings,r_string,cNameSect().c_str(),"crow_peck_neck_bone",""));
    m_peck_head = skeleton->LL_BoneID(READ_IF_EXISTS(pSettings,r_string,cNameSect().c_str(),"crow_peck_head_bone",""));
    m_peck_ik_enabled = READ_IF_EXISTS(pSettings,r_bool,cNameSect().c_str(),"crow_peck_ik_enabled",true);
    m_peck_beak_offset = READ_IF_EXISTS(pSettings,r_fvector3,cNameSect().c_str(),"crow_peck_beak_offset",Fvector().set(.12f,0,0));
    m_peck_ik_angle = READ_IF_EXISTS(pSettings,r_float,cNameSect().c_str(),"crow_peck_ik_max_angle",90.f)*PI/180.f;
    m_peck_neck_extension = READ_IF_EXISTS(pSettings,r_float,cNameSect().c_str(),"crow_peck_ik_neck_extension",.12f);
    R_ASSERT2(_valid(m_peck_beak_offset) && _valid(m_peck_ik_angle) && m_peck_ik_angle>0.f && m_peck_ik_angle<=PI &&
        _valid(m_peck_neck_extension) && m_peck_neck_extension>=0.f && m_peck_neck_extension<=.5f,"Invalid crow peck IK settings");
    if (m_peck_neck == BI_NONE || m_peck_head == BI_NONE ||
        m_ground_pose_bones[m_peck_head].parent != m_peck_neck)
    {
        if (m_peck_ik_enabled) Msg("! [scavenger crow] peck IK requires a neck -> head bone chain");
        m_peck_ik_enabled=false;
    }
    for (auto& pose : m_ground_pose_bones)
    {
        auto& bone = skeleton->LL_GetBoneInstance(pose.id);
        if (!bone.callback())
            bone.set_callback(bctCustom, ground_pose_callback, &pose);
    }
	if (auto* Animated = Visual()->dcast_PKinematicsAnimated(); Animated && !Animated->GetUpdateTracksCalback())
	{
		VisibleTracksCallback.Owner = this;
		Animated->SetUpdateTracksCalback(&VisibleTracksCallback);
	}
    skeleton->CalculateBones_Invalidate();
    return true;
}

void CScavengerCrow::ground_pose_callback(CBoneInstance* bone)
{
    auto* pose = static_cast<SGroundPoseBone*>(bone->callback_param());
    if (!pose || !pose->owner || pose->owner->m_ground_pose_weight <= 0.f)
        return;
    const auto* crow = pose->owner;
    if (crow->CachedGroundPose)
    {
        bone->mTransform = pose->CachedModel;
        return;
    }
    auto* skeleton = crow->Visual()->dcast_PKinematics();
    const Fmatrix& AnimatedParent = pose->parent == BI_NONE ? Fidentity :
        skeleton->LL_GetBoneInstance(pose->parent).mTransform;
    const bool OwnParent = pose->parent != BI_NONE &&
        skeleton->LL_GetBoneInstance(pose->parent).callback_param() == &crow->m_ground_pose_bones[pose->parent];
    const Fmatrix& parent = OwnParent ? crow->m_ground_pose_bones[pose->parent].UnscaledModel : AnimatedParent;
    Fmatrix blended;
    Fmatrix resting = pose->target;
    if (pose->ik_active)
    {
        Fquaternion neutral, solved, blend;
        neutral.set(pose->target); solved.set(pose->ik_target);
        const float peck = crow->peck_weight();
        blend.slerp(neutral,solved,peck);
        Fvector translation; translation.lerp(pose->target.c,pose->ik_target.c,peck);
        resting.mk_xform(blend,translation);
    }
    // Body and feet keep the standing pose; only the wing chain unfolds.
    float weight = crow->m_ground_pose_weight *
        (pose->wing ? 1.f - crow->GroundHopWingWeight() : 1.f);
    if (pose->wing)
    {
        // Close the feather tips before bringing the shoulder against the flank.
        const float Delay = pose->WingStage == 0 ? .2f : pose->WingStage == 1 ? .1f : 0.f;
        weight = std::max(0.f, (weight - Delay) / (1.f - Delay));
        weight = weight * weight * (3.f - 2.f * weight);
    }
    if (weight >= 1.f)
    {
        blended = resting;
    }
    else
    {
        Fmatrix Inverse, Animated;
        Inverse.invert(AnimatedParent);
        Animated.mul_43(Inverse, bone->mTransform);
        Fquaternion Original, Target, Rotation;
        Original.set(Animated);
        Target.set(resting);
        Rotation.slerp(Original, Target, weight);
        Fvector Position;
        Position.lerp(Animated.c, resting.c, weight);
        blended.mk_xform(Rotation, Position);
    }
    // The navigation root remains on the support while the mesh performs a hop.
    if (pose->parent == BI_NONE)
        blended.c.y += crow->flight().ground_hop_height();
    if (pose->id == crow->IdleBodyBone)
    {
        Fmatrix Local;
        Local.mul_43(blended, crow->IdleBodyMotion);
        blended = Local;
    }
    else if (pose->FootAnchor)
    {
        Fmatrix Local;
        Local.mul_43(crow->IdleBodyInverse, blended);
        blended = Local;
    }
    if (pose->IdleAngles.square_magnitude() > EPS_S * EPS_S)
    {
        Fmatrix Motion, Local;
        Motion.setXYZi(pose->IdleAngles);
        Local.mul_43(blended, Motion);
        Local.c = blended.c;
        blended = Local;
    }
    const float peck = crow->peck_weight();
    if (pose->id == crow->m_peck_neck || pose->id == crow->m_peck_head)
    {
        const float Share = pose->id == crow->m_peck_neck ? crow->NeckLookShare : 1.f - crow->NeckLookShare;
        Fvector Angles;
        Angles.mul(crow->LookYawAxis, crow->PerchLookAngles.x);
        Angles.mad(crow->LookPitchAxis, crow->PerchLookAngles.y + crow->IdleHeadPitch);
        Angles.mul(Share * crow->m_ground_pose_weight * (1.f - peck));
        Fmatrix Look, Local;
        Look.setXYZi(Angles);
        Local.mul_43(blended, Look);
        Local.c = blended.c;
        blended = Local;
    }
    if (!crow->m_peck_ik_enabled && peck > 0.f && pose->peck_angles.square_magnitude() > EPS_S)
    {
        Fmatrix bend, local;
        Fvector angles = pose->peck_angles;
        angles.mul(peck);
        bend.setXYZi(angles);
        local.mul_43(blended, bend);
        local.c = blended.c;
        blended = local;
    }
    bone->mTransform.mul_43(parent, blended);
    pose->UnscaledModel = bone->mTransform;
    if (pose->wing)
    {
        // This flight rig has no separate folding feather bones. Contract the
        // fan in its surface plane, without scaling child links or moving pivots.
        // Children use UnscaledModel; flight weight zero restores the original mesh.
        bone->mTransform.i.mul(1.f + (crow->FoldFeatherLength - 1.f) * weight);
        bone->mTransform.j.mul(1.f + (crow->FoldFeatherWidth - 1.f) * weight);
    }
}

void CScavengerCrow::update_peck_ik()
{
    if (!m_peck_ik_enabled || peck_weight() <= 0.f || m_ground_pose_bones.empty())
    {
        if (PeckSolutionValid)
        {
            m_ground_pose_bones[m_peck_neck].ik_active = false;
            m_ground_pose_bones[m_peck_head].ik_active = false;
        }
        PeckSolutionValid = PeckCanReach = false;
        return;
    }
    if (PeckSolutionValid)
    {
        if (PeckCanReach && peck_weight() >= .99f && !m_peck_contact_reached)
        {
            Fvector Tip, Surface;
            XFORM().transform_tiny(Tip, PeckSolvedTip);
            if (peck_surface(Tip, Surface) && Tip.distance_to_sqr(Surface) < .0001f) m_peck_contact_reached = true;
        }
        return;
    }
    auto& neck=m_ground_pose_bones[m_peck_neck];
    auto& head=m_ground_pose_bones[m_peck_head];
    // Reconstruct the resting chain without reading last frame's animated
    // transforms. Bone callbacks will apply the solved local rotations below.
    Fmatrix parent; parent.identity();
    for (u16 id=neck.parent; id!=BI_NONE; id=m_ground_pose_bones[id].parent)
    {
        Fmatrix next; next.mul_43(m_ground_pose_bones[id].target,parent); parent=next;
    }
    neck.ik_target=neck.target; head.ik_target=head.target;
    Fmatrix neck_model, head_model;
    auto forward=[&]()
    {
        neck_model.mul_43(parent,neck.ik_target);
        head_model.mul_43(neck_model,head.ik_target);
    };
    forward();
    Fvector neutral_beak, world_beak, world_contact, contact;
    head_model.transform_tiny(neutral_beak,m_peck_beak_offset);
    XFORM().transform_tiny(world_beak,neutral_beak);
    if (!peck_surface(world_beak,world_contact))
    {
        PeckSolutionValid = true;
        PeckCanReach = false;
        return;
    }
    Fmatrix inverse; inverse.invert(XFORM()); inverse.transform_tiny(contact,world_contact);
    float used_angle[2]={0.f,0.f};
    auto solve=[&]()
    {
        // Bounded two-joint CCD, head then neck. No bone scaling, no movement
        // of the navigation root or feet, and no uncontrolled recursive solve.
        for (u32 iteration=0; iteration<12; ++iteration)
        {
            for (u32 joint=0; joint<2; ++joint)
            {
                forward();
                Fvector tip; head_model.transform_tiny(tip,m_peck_beak_offset);
                if (tip.distance_to_sqr(contact)<.000001f) return;
                Fmatrix inverse_parent;
                inverse_parent.invert(joint==0 ? neck_model : parent);
                Fvector local_tip, local_goal, a, b;
                inverse_parent.transform_tiny(local_tip,tip); inverse_parent.transform_tiny(local_goal,contact);
                auto& local=joint==0 ? head.ik_target : neck.ik_target;
                a.sub(local_tip,local.c); b.sub(local_goal,local.c);
                if (a.square_magnitude()<EPS_S || b.square_magnitude()<EPS_S) continue;
                a.normalize(); b.normalize();
                float dot=a.dotproduct(b); clamp(dot,-1.f,1.f);
                const float angle=std::min(acosf(dot),std::max(0.f,m_peck_ik_angle-used_angle[joint]));
                Fvector axis; axis.crossproduct(a,b);
                if (angle<EPS_L || axis.square_magnitude()<EPS_S) continue;
                axis.normalize();
                Fmatrix rotation, next; rotation.rotation(axis,angle); next.mul_43(rotation,local);
                next.c=local.c; local=next; used_angle[joint]+=angle;
            }
        }
    };
    solve(); forward();
    Fvector tip, error; head_model.transform_tiny(tip,m_peck_beak_offset); error.sub(contact,tip);
    const float length=error.magnitude();
    if (length>EPS_L && m_peck_neck_extension>0.f)
    {
        // A bird's neck is flexible. Allow a small configured extension when
        // rotations alone cannot reach, keeping the rest of the rig planted.
        error.mul(std::min(length,m_peck_neck_extension)/length);
        Fmatrix inverse_parent; inverse_parent.invert(parent);
        Fvector local_extension; inverse_parent.transform_dir(local_extension,error);
        neck.ik_target.c.add(local_extension);
        solve();
    }
    forward(); head_model.transform_tiny(tip,m_peck_beak_offset);
    PeckSolutionValid = true;
    PeckCanReach = tip.distance_to_sqr(contact) <= .0001f;
    PeckSolvedTip = tip;
    neck.ik_active=true; head.ik_active=true;
}

float CScavengerCrow::GroundHopWingWeight() const
{
    if (flight().ground_movement() != CFlyingMovementController::EGroundMovement::Hop ||
        flight().ground_hop_height() <= EPS_S)
    {
        return 0.f;
    }
    const float Phase = flight().ground_hop_phase();
    float Open = (Phase - .15f) / .3f;
    float Close = (.95f - Phase) / .35f;
    clamp(Open, 0.f, 1.f);
    clamp(Close, 0.f, 1.f);
    Open = Open * Open * (3.f - 2.f * Open);
    Close = Close * Close * (3.f - 2.f * Close);
    const Fvector& Destination = flight().destination();
    const float Remaining = sqrtf(_sqr(Destination.x - Position().x) + _sqr(Destination.z - Position().z));
    return Open * Close * std::min(1.f, Remaining / .1f) * HopWingAmount;
}

void CScavengerCrow::UpdatePerchMotion(float Dt)
{
    if (!flight().grounded())
    {
        return;
    }
    PerchLookTimer -= Dt;
    if (PerchLookTimer <= 0.f)
    {
        PerchLookTimer = LookIntervalMax > LookIntervalMin ? Random.randF(LookIntervalMin, LookIntervalMax) : LookIntervalMin;
        LookRemaining = LookDuration;
        HeadTargetYaw = Random.randF(-LookAngle, LookAngle);
        HeadTargetPitch = HeadPitchRange > 0.f ? Random.randF(-HeadPitchRange, HeadPitchRange) : 0.f;
    }
    LookRemaining = std::max(0.f, LookRemaining - Dt);
    const bool IsLooking = LookRemaining > 0.f && peck_weight() <= EPS_S;
    const float Blend = 1.f - expf(-Dt * HeadLookSpeed);
    PerchLookAngles.x += ((IsLooking ? HeadTargetYaw : 0.f) - PerchLookAngles.x) * Blend;
    PerchLookAngles.y += ((IsLooking ? HeadTargetPitch : 0.f) - PerchLookAngles.y) * Blend;
}

bool CScavengerCrow::GetRestingBeak(Fvector& Beak, Fvector& Neck, float& Reach) const
{
    if (m_peck_neck == BI_NONE || m_peck_head == BI_NONE || m_ground_pose_bones.empty()) return false;
    Fmatrix Parent;
    Parent.identity();
    for (u16 Bone = m_ground_pose_bones[m_peck_neck].parent; Bone != BI_NONE; Bone = m_ground_pose_bones[Bone].parent)
    {
        Fmatrix Next;
        Next.mul_43(m_ground_pose_bones[Bone].target, Parent);
        Parent = Next;
    }
    Fmatrix NeckModel, HeadModel;
    NeckModel.mul_43(Parent, m_ground_pose_bones[m_peck_neck].target);
    HeadModel.mul_43(NeckModel, m_ground_pose_bones[m_peck_head].target);
    Fvector Tip;
    HeadModel.transform_tiny(Tip, m_peck_beak_offset);
    XFORM().transform_tiny(Beak, Tip);
    XFORM().transform_tiny(Neck, NeckModel.c);
    Reach = m_ground_pose_bones[m_peck_head].target.c.magnitude() + m_peck_beak_offset.magnitude() + m_peck_neck_extension;
    return true;
}

bool CScavengerCrow::UpdateIdlePose(float Dt)
{
    const bool Grounded = flight().grounded();
    const bool Contact = Grounded && !WasGrounded;
    const bool Departure = !Grounded && WasGrounded;
    WasGrounded = Grounded;
    if (Contact) LandingSettleRemaining = LandingSettleDuration;
    if (Departure)
    {
        // Airborne animation takes over immediately; no stale crouch on takeoff.
        IdleBodyMotion.identity();
        IdleBodyInverse.identity();
        IdleHeadPitch = RuffleRemaining = LandingSettleRemaining = IdleTick = 0.f;
        for (auto& Pose : m_ground_pose_bones) Pose.IdleAngles.set(0,0,0);
        return true;
    }
    if (!Grounded || m_ground_pose_bones.empty()) return false;
    IdleTick += std::max(0.f, Dt);
    if (!Contact && IdleTick < 1.f / 30.f) return false;
    const float Step = IdleTick;
    IdleTick = 0.f;
    IdlePhase = fmodf(IdlePhase + Step * IdleFrequency * PI_MUL_2, PI_MUL_2);
    LandingSettleRemaining = std::max(0.f, LandingSettleRemaining - Step);
    RuffleRemaining = std::max(0.f, RuffleRemaining - Step);
    const bool Resting = flight().status() == CFlyingMovementController::EStatus::Landed && peck_weight() <= EPS_S;
    if (Resting)
    {
        RuffleTimer -= Step;
        if (RuffleTimer <= 0.f)
        {
            RuffleTimer = RuffleIntervalMax > RuffleIntervalMin ? Random.randF(RuffleIntervalMin, RuffleIntervalMax) : RuffleIntervalMin;
            RuffleRemaining = RuffleDuration;
            RuffleSide = Random.randI(0,2) == 0 ? -1 : 1;
        }
    }
    else RuffleRemaining = 0.f;
    const float QuietWeight = IdleAmount * m_ground_pose_weight * (1.f - peck_weight()) * (1.f - GroundHopWingWeight());
    const float Settle = sinf(PI * LandingSettleRemaining / LandingSettleDuration);
    const float Ruffle = _sqr(sinf(PI * RuffleRemaining / RuffleDuration));
    IdleHeadPitch = (.015f * sinf(IdlePhase * 2.f) - .055f * Settle) * QuietWeight;
    Fvector BodyAngles = {0.f, .006f * sinf(IdlePhase), .008f * sinf(IdlePhase) - .025f * Settle};
    BodyAngles.mul(QuietWeight);
    IdleBodyMotion.setXYZi(BodyAngles);
    if (IdleBodyBone != BI_NONE)
    {
        // Express a model-space vertical displacement in the body's local frame.
        Fmatrix Inverse;
        Inverse.invert(m_ground_pose_bones[IdleBodyBone].target);
        Fvector Displacement = {0.f, (.002f * sinf(IdlePhase) - .018f * Settle) * QuietWeight, 0.f};
        Inverse.transform_dir(IdleBodyMotion.c, Displacement);
    }
    IdleBodyInverse.invert(IdleBodyMotion);
    for (auto& Pose : m_ground_pose_bones)
    {
        Pose.IdleAngles.set(0,0,0);
        if (Pose.wing && Pose.WingStage == 0)
        {
            const float SideWeight = Pose.WingSide == RuffleSide ? 1.f : .35f;
            Pose.IdleAngles.set(Pose.WingSide * .018f, .008f, Pose.WingSide * .022f);
            Pose.IdleAngles.mul((Ruffle * SideWeight + Settle * .35f) * QuietWeight);
        }
        else if (Pose.id == IdleTailBone)
        {
            Pose.IdleAngles.set(0.f, .025f * sinf(IdlePhase * .5f), .035f * Settle);
            Pose.IdleAngles.mul(QuietWeight);
        }
    }
    return true;
}

void CScavengerCrow::SetPoseOverwrite(bool Enabled)
{
    if (m_ground_pose_bones.empty()) return;
    auto* Skeleton = Visual()->dcast_PKinematics();
    for (const auto& Pose : m_ground_pose_bones)
    {
        auto& Bone = Skeleton->LL_GetBoneInstance(Pose.id);
        if (Bone.callback_param() == &Pose) Bone.set_callback_overwrite(Enabled);
    }
}

void CScavengerCrow::CacheGroundPose()
{
    CachedGroundPose = false;
    // At full standing weight every local matrix is known. Skip native key
    // evaluation even while refreshing the procedural pose at its own cadence.
    SetPoseOverwrite(true);
    auto* Skeleton = Visual()->dcast_PKinematics();
    Skeleton->CalculateBones_Invalidate();
    Skeleton->CalculateBones(true);
    for (auto& Pose : m_ground_pose_bones)
        Pose.CachedModel = Skeleton->LL_GetBoneInstance(Pose.id).mTransform;
    CachedGroundPose = true;
}

bool CScavengerCrow::AnimationVisible() const
{
	return AnimationVisibleFrame != u32(-1) &&
		(Device.dwFrame == AnimationVisibleFrame || Device.dwFrame - AnimationVisibleFrame == 1u);
}

bool CScavengerCrow::SVisibleTracksCallback::operator()(float Dt, IKinematicsAnimated& Skeleton)
{
	// Bone calculation precedes renderable_Render for some visual sizes.
	// Main-view submission can therefore be the first visibility notification.
	const bool WasVisible = Owner->AnimationVisible();
	const bool MainView = !WasVisible && ::Render->phase == IRender_interface::PHASE_NORMAL &&
		::Render->ViewBase.testSphere_dirty(Owner->SpatialComponent->sphere.P, Owner->SpatialComponent->sphere.R);
	if (MainView)
	{
		Owner->AnimationVisibleFrame = Device.dwFrame;
	}
	if (MainView || WasVisible)
	{
		Skeleton.LL_UpdateTracks(std::min(.066f, std::max(0.f, Dt)), false, false);
	}
	// Consume elapsed time even while hidden; never replay an offscreen backlog.
	return true;
}

void CScavengerCrow::renderable_Render()
{
	// Shadow/reflection submission must not wake animations outside the main view.
	if (::Render->phase == IRender_interface::PHASE_NORMAL)
	{
		AnimationVisibleFrame = Device.dwFrame;
	}
	CFlyingMonster::renderable_Render();
}

bool CScavengerCrow::UpdateAnimationLod(float Dt)
{
    AnimationLodTick += Dt;
    if (AnimationLodTick < .2f)
    {
        return false;
    }
    AnimationLodTick = 0.f;
    Fvector Direction;
    Direction.sub(Position(), Device.vCameraPosition);
    const float DistanceSq = Direction.square_magnitude();
    const float Fov = std::max(1.f, std::min(179.f, Device.fFOV)) * PI / 180.f;
    const float FocusAngle = atanf(tanf(Fov * .5f) * FocusFraction);
    const float Zoom = std::max(1.f, std::min(10.f, tanf(75.f * PI / 360.f) / tanf(Fov * .5f)));
    const float Forward = Direction.dotproduct(Device.vCameraDirection);
    // A central cone follows the actual camera FOV, including binocular/weapon zoom.
    // No ray casts or bone queries: visibility affects animation only.
    const bool Focused = Forward > 0.f && Forward * Forward >= DistanceSq * _sqr(cosf(FocusAngle));
    const bool Visible = Forward > 0.f && Forward * Forward >= DistanceSq * _sqr(cosf(Fov * .5f));
    const bool Detailed = (Visible && DistanceSq <= _sqr(DetailDistance)) ||
        (Focused && DistanceSq <= _sqr(FocusDistance * Zoom));
    if (Detailed == DetailedAnimation)
    {
        return false;
    }
    DetailedAnimation = Detailed;
    PeckSolutionValid = PeckCanReach = false;
    PerchLookAngles.set(0,0,0);
    IdleBodyMotion.identity();
    IdleBodyInverse.identity();
    IdleHeadPitch = 0.f;
    for (auto& Pose : m_ground_pose_bones)
    {
        Pose.ik_active = false;
        Pose.IdleAngles.set(0,0,0);
    }
    CachedGroundPose = false;
    SetPoseOverwrite(false);
    return true;
}

void CScavengerCrow::UpdateSimplePeck()
{
    const float Weight = peck_weight();
    if (Weight <= 0.f)
    {
        PeckSolutionValid = false;
        return;
    }
    if (Weight < .99f || PeckSolutionValid)
    {
        return;
    }
    // Resolve gameplay contact once per bite without solving an invisible skeleton.
    PeckSolutionValid = true;
    Fvector Beak, Neck, Surface;
    float Reach = 0.f;
    if (GetRestingBeak(Beak, Neck, Reach) && peck_surface(Beak, Surface) &&
        Neck.distance_to_sqr(Surface) <= _sqr(Reach + m_peck_neck_extension))
    {
        m_peck_contact_reached = true;
    }
}

void CScavengerCrow::update_flight_animation(float dt)
{
	// UpdateCL precedes rendering: reuse last frame's actual renderer acceptance.
	const bool Visible = AnimationVisible();
	if (!Visible)
	{
		// Feeding state depends on this scalar, not on evaluating an unseen skeleton.
		const float Target = flight().landing_pose_weight(Position());
		const float Step = std::max(0.f, dt) * (Target < m_ground_pose_weight ? WingOpenSpeed : m_ground_pose_speed);
		m_ground_pose_weight += std::max(-Step, std::min(Step, Target - m_ground_pose_weight));
		UpdateSimplePeck();
		NativeAnimationTick = VisualPoseTick = 0.f;
		AnimationWasVisible = false;
		return;
	}
	const bool BecameVisible = !AnimationWasVisible;
	AnimationWasVisible = true;
	if (BecameVisible)
	{
		AnimationLodTick = .2f;
		CachedGroundPose = false;
		SetPoseOverwrite(false);
	}
	const bool LodChanged = UpdateAnimationLod(dt) || BecameVisible;
    const float PoseTarget = flight().landing_pose_weight(Position());
    const bool Transition = std::abs(PoseTarget - m_ground_pose_weight) > EPS_S;
    const bool CanCache = flight().grounded() && m_ground_pose_weight >= 1.f && GroundHopWingWeight() <= EPS_S;
    if (CachedGroundPose && !CanCache)
    {
        CachedGroundPose = false;
        SetPoseOverwrite(false);
    }
    NativeAnimationTick += dt;
    if (DetailedAnimation || Transition || LodChanged || NativeAnimationTick >= .15f)
    {
        CFlyingMonster::update_flight_animation(NativeAnimationTick);
        NativeAnimationTick = 0.f;
    }
    if (DetailedAnimation)
    {
        update_peck_ik();
    }
    else
    {
        UpdateSimplePeck();
    }
    VisualPoseTick += std::max(0.f, dt);
    const float PoseInterval = DetailedAnimation ? 1.f / 30.f : .2f;
    const bool HopChanged = std::abs(GroundHopWingWeight() - m_previous_hop_wings) > EPS_S ||
        std::abs(flight().ground_hop_height() - PreviousHopHeight) > EPS_S;
    if (!Transition && !LodChanged && !HopChanged &&
        ((!DetailedAnimation && CanCache && CachedGroundPose) || VisualPoseTick < PoseInterval))
    {
        return;
    }
    const float PoseDt = VisualPoseTick;
    VisualPoseTick = 0.f;
    if (DetailedAnimation)
    {
        UpdatePerchMotion(PoseDt);
    }
    const float Step = std::max(0.f, PoseDt) * (PoseTarget < m_ground_pose_weight ? WingOpenSpeed : m_ground_pose_speed);
    const float PreviousWeight = m_ground_pose_weight;
    m_ground_pose_weight += std::max(-Step, std::min(Step, PoseTarget - m_ground_pose_weight));
    const bool IdleChanged = DetailedAnimation && UpdateIdlePose(PoseDt);
    if (!DetailedAnimation)
    {
        WasGrounded = flight().grounded();
    }
    const float Peck = peck_weight(), Hop = GroundHopWingWeight();
    const float HopHeight = flight().ground_hop_height();
    const bool PoseChanged = LodChanged || std::abs(PreviousWeight - m_ground_pose_weight) > EPS_S ||
        (DetailedAnimation && (Peck > 0.f || m_previous_peck_weight > 0.f || IdleChanged ||
            PerchLookAngles.distance_to_sqr(PreviousLookAngles) > _sqr(EPS_S))) || HopChanged;
    if (PoseChanged)
    {
        Visual()->dcast_PKinematics()->CalculateBones_Invalidate();
    }
    m_previous_peck_weight = Peck;
    m_previous_hop_wings = Hop;
    PreviousLookAngles = PerchLookAngles;
    PreviousHopHeight = HopHeight;
    if (flight().grounded() && m_ground_pose_weight >= 1.f && Hop <= EPS_S &&
        !m_ground_pose_bones.empty() && (PoseChanged || !CachedGroundPose))
    {
        CacheGroundPose();
    }
}

void CScavengerCrow::detach_ground_pose_callbacks()
{
    if (!Visual())
        return;
	if (auto* Animated = Visual()->dcast_PKinematicsAnimated(); Animated && Animated->GetUpdateTracksCalback() == &VisibleTracksCallback)
	{
		Animated->SetUpdateTracksCalback(nullptr);
	}
    auto* skeleton = Visual()->dcast_PKinematics();
    if (!skeleton)
        return;
    for (auto& pose : m_ground_pose_bones)
    {
        if (pose.id == BI_NONE)
            continue;
        auto& bone = skeleton->LL_GetBoneInstance(pose.id);
        if (bone.callback_param() == &pose)
            bone.reset_callback();
        pose.id = BI_NONE;
    }
    CachedGroundPose = false;
    m_ground_pose_bones.clear();
}

void CScavengerCrow::Die(CObject* who)
{
    stop_crow_behavior();
    CCrowSharedMemory::Get().UnregisterBird(ID());
    detach_ground_pose_callbacks();
    CFlyingMonster::Die(who);
}

void CScavengerCrow::net_Relcase(CObject* Object)
{
    CCrowSharedMemory::Get().InvalidateObject(Object->ID());
    CFlyingMonster::net_Relcase(Object);
}

void CScavengerCrow::net_Destroy()
{
	// Capture the final online state before releasing its food and movement state.
	SyncOfflineCrow();
    CCrowSharedMemory::Get().UnregisterBird(ID());
    stop_crow_behavior();
    detach_ground_pose_callbacks();
    CFlyingMonster::net_Destroy();
}
