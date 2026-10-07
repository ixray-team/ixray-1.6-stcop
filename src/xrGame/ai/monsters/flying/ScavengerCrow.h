#pragma once
#include "flying_monster.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"
class CSE_ALifeScavengerCrow;

// Crow decisions and animations are separate from reusable flyer locomotion.
class CScavengerCrow final : public CFlyingMonster
{
public:
    CScavengerCrow();
    ~CScavengerCrow() override;
    void Load(const char* section) override;
    void reinit() override;
    void Serialize(ISaveObject& object) override;
    void feel_sound_new(CObject* who, int type, CSound_UserDataPtr data,
        const Fvector& position, float power) override;
    bool net_Spawn(CSE_Abstract* data) override;
    void net_Destroy() override;
	void renderable_Render() override;
    void net_Relcase(CObject* Object) override;
    void Die(CObject* who) override;
    char* get_monster_class_name() override { return (char*)"scavenger_crow"; }

protected:
    float SpeciesBehaviorStep(float Dt) override;
    void update_species_behavior(float dt) override;
    void save_species(NET_Packet& packet) override;
    void load_species(IReader& reader, u8 version) override;
    void update_flight_animation(float dt) override;

private:
    struct SBehavior;
    std::unique_ptr<SBehavior> m_behavior;
    void start_crow_behavior();
	void EnsureAirMotion();
    void stop_crow_behavior();
    CSE_ALifeScavengerCrow* GetOfflineServer() const;
    bool RestoreOfflineCrow();
    void SyncOfflineCrow();
	float OfflineSyncTick = 0.f, OfflineSyncInterval = .25f;
    float peck_weight() const;
    bool peck_surface(const Fvector& beak, Fvector& point);
    bool GetRestingBeak(Fvector& Beak, Fvector& Neck, float& Reach) const;
    void update_peck_ik();
    u16 m_peck_neck = BI_NONE, m_peck_head = BI_NONE;
    bool m_peck_ik_enabled = true;
    Fvector m_peck_beak_offset = { .12f, 0.f, 0.f };
    float m_peck_ik_angle = 1.57f, m_peck_neck_extension = .12f;
    bool m_peck_contact_reached = false;
    bool PeckSolutionValid = false, PeckCanReach = false;
    Fvector PeckSolvedTip = {0,0,0};
    struct SGroundPoseBone
    {
        CScavengerCrow* owner = nullptr;
        u16 id = u16(-1), parent = u16(-1);
        Fmatrix target;
        bool wing = false;
        int WingSide = 0;
        u32 WingStage = 0;
        Fmatrix UnscaledModel;
        Fmatrix CachedModel;
        bool FootAnchor = false;
        Fvector IdleAngles = {0,0,0};
        Fvector peck_angles = {0,0,0};
        Fmatrix ik_target;
        bool ik_active = false;
    };
    xr_vector<SGroundPoseBone> m_ground_pose_bones;
    float m_ground_pose_weight = 0.f;
    float m_previous_peck_weight = 0.f, m_previous_hop_wings = 0.f;
    float PreviousHopHeight = 0.f;
    float m_ground_pose_speed = 4.f;
    Fvector PerchLookAngles = {0,0,0};
    Fvector LookYawAxis = {0,1,0}, LookPitchAxis = {0,0,1};
    float HeadLookSpeed = 4.f, HeadPitchRange = .2f, NeckLookShare = .35f;
    Fvector PreviousLookAngles = {0,0,0};
    float PerchLookTimer = 0.f, LookRemaining = 0.f;
    float LookIntervalMin = 2.f, LookIntervalMax = 5.f, LookDuration = 1.f, LookAngle = .52f;
    float HeadTargetYaw = 0.f, HeadTargetPitch = 0.f;
    float HopWingAmount = .65f, WingOpenSpeed = 10.f;
    float FoldFeatherWidth = .35f, FoldFeatherLength = .9f;
    float VisualPoseTick = 0.f;
    float AnimationLodTick = 0.f, NativeAnimationTick = 0.f;
    float DetailDistance = 30.f, FocusDistance = 100.f, FocusFraction = .3f;
    bool DetailedAnimation = true;
	struct SVisibleTracksCallback final : IUpdateTracksCallback
	{
		CScavengerCrow* Owner = nullptr;
		bool operator()(float Dt, IKinematicsAnimated& Skeleton) override;
	};
	SVisibleTracksCallback VisibleTracksCallback;
	bool AnimationVisible() const;
	u32 AnimationVisibleFrame = u32(-1);
	bool AnimationWasVisible = false;
    bool UpdateAnimationLod(float Dt);
    void UpdateSimplePeck();
    bool CachedGroundPose = false;
    void SetPoseOverwrite(bool Enabled);
    void CacheGroundPose();
    u16 IdleBodyBone = BI_NONE, IdleTailBone = BI_NONE;
    Fmatrix IdleBodyMotion, IdleBodyInverse;
    float IdlePhase = 0.f, IdleFrequency = 1.f, IdleTick = 0.f;
    float IdleAmount = 1.f, IdleHeadPitch = 0.f;
    float RuffleTimer = 0.f, RuffleRemaining = 0.f, RuffleDuration = .65f;
    float RuffleIntervalMin = 4.f, RuffleIntervalMax = 10.f;
    float LandingSettleRemaining = 0.f, LandingSettleDuration = .55f;
    int RuffleSide = 1;
    bool WasGrounded = false;
    bool UpdateIdlePose(float Dt);
    void UpdatePerchMotion(float Dt);
    float GroundHopWingWeight() const;
    static void _BCL ground_pose_callback(CBoneInstance* bone);
    void detach_ground_pose_callbacks();
};
