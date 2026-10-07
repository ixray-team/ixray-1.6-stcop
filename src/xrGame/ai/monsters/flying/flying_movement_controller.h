#pragma once
#include <algorithm>
#include <memory>

class CGameObject;

// Reusable locomotion only: species AI owns destinations and motivations.
class CFlyingMovementController
{
public:
    enum class EGroundMovement : u8 { Walk, Hop };
    void set_ground_movement(EGroundMovement mode)
    {
        if (m_ground_movement == mode)
            return;
        m_ground_movement = mode;
        m_hop_phase = m_hop_lift = 0.f;
    }
    EGroundMovement ground_movement() const { return m_ground_movement; }
    enum class EStrategy : u8 { Auto, GroundOnly, AirOnly };
    void set_strategy(EStrategy strategy) { m_strategy = strategy; }
    EStrategy strategy() const { return m_strategy; }
    enum class EStatus : u8 { Idle, Planning, Flying, Landing, Arrived, Landed, Failed, Walking, GroundBlocked };
    CFlyingMovementController();
    ~CFlyingMovementController();
    void Load(const char* section);
    void set_model_bounds(const Fbox& bounds);
    void reset();
    bool fly_to(CGameObject& object, const Fvector& destination, bool land, float landing_spacing = 0.f,
        bool PreserveCurrentOnFailure = false);
    void update(CGameObject& object, float dt);
    void RecoverFlight(CGameObject& Object, float Dt);
    bool BeginCachedTakeoff(CGameObject& Object);
    Fbox FlightBounds() const;
    bool InsideFlightBounds(const Fvector& Point) const;
	bool BeginAmbientCruise(CGameObject& Object, const Fvector& Direction, float Distance);
	bool NeedsNextCruise(const Fvector& Position, float Progress) const;
	bool HasNextCruise() const { return !NextCruise.Path.empty(); }
	Fvector CruiseEndpoint() const { return m_path.empty() ? m_destination : m_path.back(); }
	bool PrepareNextCruise(CGameObject& Object, const Fvector& Destination);
	void SetCruiseContinuity(bool Enabled, float LegDistance = 40.f)
	{
		CruiseContinuity = Enabled;
		if (_valid(LegDistance) && LegDistance > 0.f)
		{
			CruiseLegDistance = std::min(LegDistance, m_max_distance * .8f);
		}
	}
    void stop();
    bool restore_landed(CGameObject& object);
    bool walk(CGameObject& object, const Fvector& target, float dt, float speed);
    bool walk_to(CGameObject& object, const Fvector& target, float speed);
    bool clear_path(CGameObject& object, const Fvector& from, const Fvector& to, bool landing = false) const;
    bool find_landing_point(CGameObject& object, float radius, float range,
        bool ground_only, Fvector& point) const;
    bool landing_footprint(CGameObject& object, const Fvector& contact, const Fvector& normal) const;
    float ground_offset() const { return m_ground_offset; }
    float max_command_distance() const { return m_max_distance; }
    bool debug_enabled() const { return m_debug; }
    void set_speed_scale(float scale) { m_speed_scale = std::max(.25f,std::min(2.f,scale)); }
    float landing_pose_weight(const Fvector& position) const
    {
        return grounded() ? 1.f : m_status == EStatus::Landing ?
            std::max(0.f, 1.f - position.distance_to(m_destination) / m_landing_height) : 0.f;
    }
    float ground_hop_height() const { return m_status == EStatus::Walking ? m_hop_lift : 0.f; }
    float ground_hop_phase() const { return m_status == EStatus::Walking ? m_hop_phase : 0.f; }
    const Fvector& velocity() const { return m_velocity; }
    float planning_time() const { return m_planning_time; }
    float stalled_time() const { return m_progress_time; }
    const xr_vector<Fvector>& path() const { return m_path; }
    const Fvector& destination() const { return PendingCommand ? PendingRoot : fallback_pending() ? m_resume_destination : m_destination; }
    EStatus status() const { return CommandPending() ? EStatus::Planning : PreparationFailed ? EStatus::Failed : m_status; }
    bool CommandPending() const { return PendingCommand || RouteStartPending || LandingRetargetPending; }
    bool wants_landing() const { return PendingCommand ? PendingLanding : fallback_pending() ? m_resume_landing : m_land; }
    bool active() const { return CommandPending() || m_status == EStatus::Planning || m_status == EStatus::Flying || m_status == EStatus::Landing || m_status == EStatus::Walking; }
    bool grounded() const { return m_status == EStatus::Landed || m_status == EStatus::Walking || m_status == EStatus::GroundBlocked; }
    bool NeedsGroundCollision(const Fvector& Position) const
    {
        return grounded() || m_taking_off || DepartureLiftPending || m_status == EStatus::Landing ||
            (m_land && !m_path.empty() && m_path_index + 1 == m_path.size() &&
                Position.distance_to_sqr(m_approach) <= _sqr(m_landing_height * 2.f));
    }
    bool SpawnSpaceFree(CGameObject& Object, const Fvector& Root) const { return landing_space_free(Object, Root); }
    bool fallback_pending() const { return m_fallback_stage != EFallback::None; }

    static bool support(CGameObject& object, const Fvector& from, float range,
        Fvector& point, Fvector& normal);

private:
    EGroundMovement m_ground_movement = EGroundMovement::Walk;
    EStrategy m_strategy = EStrategy::Auto;
    enum class EFallback : u8 { None, Descending, Walking };
    EFallback m_fallback_stage = EFallback::None;
    bool m_fallback_used = false;
    Fvector m_resume_destination = {0,0,0};
    Fvector m_ground_destination = {0,0,0};
    bool m_resume_landing = false;
    bool try_ground_fallback(CGameObject& object);
    void resume_air_command(CGameObject& object);
    bool direct_flight(CGameObject& object, const Fvector& target, bool land, float& length) const;
    float ground_path_length(const Fvector& start) const;
    float m_air_probe_timer = 0.f;
    struct SSearch;
    std::unique_ptr<SSearch> m_search;
    xr_vector<Fvector> m_path;
    Fvector m_destination = {0,0,0};
    Fvector m_approach = {0,0,0};
    EStatus m_status = EStatus::Idle;
    size_t m_path_index = 0;
    bool m_land = false;
    u32 m_replans = 0;
    float m_stuck_time = 0.f;
    float m_corridor_check_timer = 0.f;
    float m_corridor_check_interval = .2f;
    float m_progress_time = 0.f, m_planning_time = 0.f, m_best_waypoint_distance = 0.f;
    float m_progress_timeout = 8.f, m_speed_scale = 1.f;
    float m_takeoff_speed_factor = 1.f, m_landing_speed_factor = 1.f;
    bool m_progress_valid = false, m_taking_off = false;
    bool DepartureLiftPending = false;
    bool RecoveryActive = false;
    float RecoveryRetry = 0.f;
    u32 RecoveryProbe = 0;
    Fvector m_progress_target = {0,0,0}, m_departure_origin = {0,0,0};
    float m_cell_size = 2.f;
    float m_min_cell_size = .4f;
    float m_search_cell = 2.f;
    float m_search_margin = 24.f;
    float m_max_distance = 300.f;
    u32 m_search_budget = 24;
    u32 m_search_limit = 8192;
    float m_landing_height = 1.5f;
    float m_ground_offset = .03f;
    float m_landing_footprint_radius = .01f;
    float LandingMinNormalY = .42261826f;
    float m_landing_search_radius = 3.f;
    float m_landing_search_max_radius = 6.f;
    float m_landing_spacing = .4f;
    float m_command_landing_spacing = .4f;
    float m_landing_body_radius = 0.f;
    Fvector m_requested_landing = {0,0,0};
    u32 m_landing_retargets = 0;
    xr_vector<Fvector> m_rejected_landings;
    bool landing_space_free(CGameObject& object, const Fvector& root) const;
    void reserve_landing();
    void release_landing();
    bool retarget_landing(CGameObject& object);
    bool resolve_landing_point(CGameObject& object, const Fvector& requested, Fvector& root) const;
    bool StartGroundCommand(CGameObject& Object, const Fvector& Target, float Speed);
    bool StartFlight(CGameObject& Object, const Fvector& Destination, bool Land, float Spacing, bool Preserve);
    void ProcessPendingCommand(CGameObject& Object);
    bool ValidateMovement(CGameObject& Object, float Dt);
    bool RecoverAirMotion(CGameObject& Object);
    bool MovementSegmentFree(CGameObject& Object, const Fvector& From, const Fvector& To) const;
    bool move(CGameObject& object, const Fvector& target, float dt);
    void begin_search(CGameObject& object, bool CheckedDirect = false);
    void search_step(CGameObject& object);
    void separate(CGameObject& object, float dt);
    Fvector m_velocity = {0.f, 0.f, 0.f};
    float m_speed = 6.f;
    float m_walk_speed = .5f;
    float m_hop_height = 0.f;
    float m_hop_length = .35f;
    float HopSpeedScale = 1.f;
    float m_hop_phase = 0.f;
    float m_hop_lift = 0.f;
    u32 m_walk_vertex = u32(-1);
    float m_acceleration = 8.f;
    float m_radius = .18f;
    bool m_use_model_bounds = true;
    bool m_have_model_bounds = false;
    Fvector m_body_center = {0,0,0};
    Fvector m_body_half_size = {0,0,0};
    float m_turn_speed = 2.f;
    float m_turn_acceleration = 2.5f;
    float m_orientation_response = 4.f;
    float m_direction_sync_speed = 6.f;
    float m_max_direction_lag = .2617994f;
    float TurnForwardFactor = 0.f;
    bool m_turning = false;
    float m_yaw_rate = 0.f;
    float m_separation_distance = .8f;
    float m_separation_speed = 1.5f;
    float m_separation_timer = 0.f;
    float m_separation_interval = .1f;
    Fvector m_separation_force = {0,0,0};
    // Only species opting in use the coarse, globally budgeted locomotion path.
	struct SPreparedCruise
	{
		xr_vector<Fvector> Path;
		Fvector Origin = {0,0,0}, Destination = {0,0,0};
		void Clear() { Path.clear(); }
	};
	SPreparedCruise NextCruise;
	bool ActivateNextCruise(CGameObject& Object);
	bool ResumeCachedCruise(CGameObject& Object);
	bool CruiseContinuity = false, CruiseSegmentValid = false;
	bool AmbientLeg = false;
	float CruiseLegDistance = 40.f;
	Fvector CruiseSegmentStart = {0,0,0};
    bool BudgetedFlight = false;
    float BoundaryMargin = 40.f;
    float MovementCheckInterval = .25f, MovementCheckTimer = 0.f;
	bool AirValidationWaiting = false, LandingProbeWaiting = false;
    u32 AirChecksPerFrame = 8, RouteStartsPerFrame = 4, LandingProbesPerFrame = 8;
    u32 LandingProbeCount = 32, SearchExpansionsPerFrame = 64;
    bool PendingCommand = false, PendingLanding = false, PendingGround = false, PendingPreserve = false;
    u32 CommandVersion = 0;
    float PendingWalkSpeed = .5f;
    bool RouteStartPending = false, RouteJobActive = false, LandingRetargetPending = false, PreparationFailed = false;
    Fvector PendingTarget = {0,0,0}, PendingRoot = {0,0,0};
    float PendingSpacing = 0.f;
    mutable bool WorkDeferred = false, LandingRequestValid = false;
    mutable Fvector LandingRequest = {0,0,0};
    mutable u32 LandingProbeCursor = 0;
    bool CachedDepartureValid = false;
    Fvector CachedDepartureRoot = {0,0,0}, CachedDepartureAir = {0,0,0};
    bool MoveBudgeted(CGameObject& Object, const Fvector& Target, float Dt);
    float FlightPoseTick = 0.f;
    bool FlightPoseValid = false;
    float FlightHeading = 0.f, FlightPitch = 0.f, FlightBank = 0.f;
    bool SafePositionValid = false;
    Fvector SafePosition = {0,0,0};
    mutable bool AirVolumeValid = false;
    mutable Fbox AirVolume;
    mutable bool CertificateValid = false;
    mutable Fvector CertifiedFrom = {0,0,0}, CertifiedTo = {0,0,0};
    mutable Fmatrix CertifiedOrientation;
    bool m_debug = false;
};
