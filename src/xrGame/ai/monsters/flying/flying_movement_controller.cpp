#include "StdAfx.h"
#include "FlyingWorkBudget.h"
#include "flying_movement_controller.h"
#include "flying_monster.h"
#include "../../../GameObject.h"
#include "../../../Level.h"
#include "../../../ai_space.h"
#include "../../../level_graph.h"
#include "../../../../xrCore/Collision/xr_area.h"
#include "../../../../xrEngine/GameMtlLib.h"
#include "../../../../xrEngine/xr_collide_form.h"
#include "air_path_search.h"

namespace
{
struct SLandingReservation
{
    const CFlyingMovementController* owner;
    Fvector root;
    float spacing;
};
xr_vector<SLandingReservation> landing_reservations;
u32 active_air_searches = 0;
u32 g_RecoveryFrame = u32(-1), g_RecoveryBudget = 0;

struct SFlyingRayContext
{
    collide::rq_result& nearest;
    CGameObject& mover;
    bool support_query;
};

bool flying_solid_hit(collide::rq_result& hit, LPVOID data)
{
    auto& context = *static_cast<SFlyingRayContext*>(data);
    // Airborne birds ignore one another. Grounded birds and final approaches
    // retain collision so occupied perches are not treated as free.
    if (hit.O)
    {
        const auto* flyer = smart_cast<CFlyingMonster*>(hit.O);
        if (flyer && flyer->g_Alive() && !flyer->flight().NeedsGroundCollision(flyer->Position()))
            return true;
        // A test spawn at the player's feet starts inside their collision
        // volume. Let it leave that overlap; outside it the actor blocks rays
        // normally. World geometry is never ignored.
        if (hit.O == Level().CurrentEntity() &&
            context.mover.Position().distance_to_sqr(hit.O->Position()) <
                _sqr(context.mover.Radius() + hit.O->Radius() + .1f))
            return true;
    }
    if (!hit.O)
    {
        const auto* material=GMLib.GetMaterialByIdx(Level().ObjectSpace.GetStaticTris()[hit.element].material);
        if (material->Flags.is(SGameMtl::flPassable) &&
            !(context.support_query && material->Flags.is(SGameMtl::flLiquid))) return true;
    }
    auto& nearest = context.nearest;
    if (nearest.element < 0 || hit.range < nearest.range)
        nearest = hit;
    return true;
}

bool FlyingCollisionCandidate(const collide::ray_defs&, CObject* Object, LPVOID)
{
    const auto* Flyer = smart_cast<CFlyingMonster*>(Object);
    return !Flyer || !Flyer->g_Alive() || Flyer->flight().NeedsGroundCollision(Flyer->Position());
}

bool flying_pick(CGameObject& object, const Fvector& from, const Fvector& direction,
    float range, collide::rq_result& hit, collide::rq_target target = collide::rqtBoth, bool support_query = false, const xr_vector<CObject*>* Candidates = nullptr)
{
    hit.set(nullptr, range, -1);
    collide::rq_results results;
    collide::ray_defs ray(from, direction, range, CDB::OPT_FULL_TEST, target);
    SFlyingRayContext context{hit, object, support_query};
    if (Candidates)
    {
        if (target & collide::rqtStatic)
        {
            ray.tgt = collide::rqtStatic;
            Level().ObjectSpace.RayQuery(results, ray, flying_solid_hit, &context, nullptr, &object);
        }
        if (target & collide::rqtObject)
        {
            ray.tgt = collide::rqtObject;
            for (CObject* Other : *Candidates)
            {
                // Pointers are local to this synchronous hull check, never cached.
                results.r_clear();
                Level().ObjectSpace.RayQuery(results, Other->collidable.model, ray);
                for (auto& Result : results.r_results())
                {
                    flying_solid_hit(Result, &context);
                }
            }
        }
    }
    else
    {
        Level().ObjectSpace.RayQuery(results, ray, flying_solid_hit, &context, FlyingCollisionCandidate, &object);
    }
    return hit.element >= 0;
}
}

struct CFlyingMovementController::SSearch
{
    static xr_vector<SSearch*>& Registry()
    {
        static xr_vector<SSearch*> Searches;
        return Searches;
    }
    bool SharedBudget = false;
    explicit SSearch(bool Shared = false) : SharedBudget(Shared)
    {
        ++active_air_searches;
        if (!SharedBudget) Registry().push_back(this);
    }
    ~SSearch()
    {
        --active_air_searches;
        if (SharedBudget)
        {
            CFlyingWorkBudget::Cancel(this);
            return;
        }
        auto& Searches = Registry();
        Searches.erase(std::remove(Searches.begin(), Searches.end(), this), Searches.end());
    }
    u32 Granted = 0, GrantedFrame = u32(-1);
    u32 Budget(u32 GlobalLimit)
    {
        if (SharedBudget)
        {
            return CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::SearchExpansion, GlobalLimit) ? 1u : 0u;
        }
        static u32 Frame = u32(-1), Cursor = 0;
        auto& Searches = Registry();
        if (Frame != Device.dwFrame)
        {
            Frame = Device.dwFrame;
            // Stamp only granted clients; never reset the entire search registry.
            const u32 Count = std::min(GlobalLimit, u32(Searches.size()));
            u32 Remaining = GlobalLimit;
            for (u32 Index = 0; Index < Count; ++Index)
            {
                Cursor %= u32(Searches.size());
                auto* Search = Searches[Cursor++];
                const u32 Share = Remaining / (Count - Index);
                Search->Granted = Share;
                Search->GrantedFrame = Frame;
                Remaining -= Share;
            }
        }
        const u32 Result = GrantedFrame == Device.dwFrame ? Granted : 0;
        Granted = 0;
        return Result;
    }
    FlyingPath::Search search;
};

CFlyingMovementController::CFlyingMovementController() = default;
CFlyingMovementController::~CFlyingMovementController()
{
    release_landing();
    CFlyingWorkBudget::Cancel(this);
}

void CFlyingMovementController::Load(const char* section)
{
    m_speed = READ_IF_EXISTS(pSettings, r_float, section, "flight_speed", 6.f);
    m_takeoff_speed_factor = READ_IF_EXISTS(pSettings,r_float,section,"flight_takeoff_speed_factor",1.f);
    m_landing_speed_factor = READ_IF_EXISTS(pSettings,r_float,section,"flight_landing_speed_factor",1.f);
    m_progress_timeout = READ_IF_EXISTS(pSettings,r_float,section,"flight_progress_timeout",8.f);
    R_ASSERT2(_valid(m_takeoff_speed_factor) && m_takeoff_speed_factor>0.f && m_takeoff_speed_factor<=2.f &&
        _valid(m_landing_speed_factor) && m_landing_speed_factor>0.f && m_landing_speed_factor<=2.f &&
        _valid(m_progress_timeout) && m_progress_timeout>=1.f,"Invalid flyer speed/progress settings");
    m_acceleration = READ_IF_EXISTS(pSettings, r_float, section, "flight_acceleration", 8.f);
    m_radius = READ_IF_EXISTS(pSettings, r_float, section, "flight_collision_radius", .18f);
    m_use_model_bounds = READ_IF_EXISTS(pSettings, r_bool, section, "flight_use_model_bounds", true);
    m_have_model_bounds = false;
    m_turn_speed = READ_IF_EXISTS(pSettings, r_float, section, "flight_turn_speed", 2.f);
    m_turn_acceleration = READ_IF_EXISTS(pSettings, r_float, section, "flight_turn_acceleration", 2.5f);
    m_orientation_response = READ_IF_EXISTS(pSettings, r_float, section, "flight_orientation_response", 4.f);
    m_direction_sync_speed = READ_IF_EXISTS(pSettings, r_float, section, "flight_direction_sync_speed", 6.f);
    m_max_direction_lag = READ_IF_EXISTS(pSettings, r_float, section, "flight_max_direction_lag", 15.f);
    clamp(m_max_direction_lag, 1.f, 45.f);
    m_max_direction_lag *= PI / 180.f;
    TurnForwardFactor = READ_IF_EXISTS(pSettings, r_float, section, "flight_turn_forward_factor", 0.f);
    R_ASSERT2(_valid(TurnForwardFactor) && TurnForwardFactor >= 0.f && TurnForwardFactor <= 1.f,
        "Invalid flight turn forward factor");
    m_cell_size = READ_IF_EXISTS(pSettings, r_float, section, "flight_path_cell", 2.f);
    m_min_cell_size = READ_IF_EXISTS(pSettings, r_float, section, "flight_path_min_cell", .4f);
    m_search_margin = READ_IF_EXISTS(pSettings, r_float, section, "flight_path_margin", 24.f);
    m_max_distance = READ_IF_EXISTS(pSettings, r_float, section, "flight_max_distance", 300.f);
    m_search_budget = READ_IF_EXISTS(pSettings, r_u32, section, "flight_search_budget", 24);
    m_corridor_check_interval=READ_IF_EXISTS(pSettings,r_float,section,"flight_corridor_check_interval",.2f);
    m_separation_interval=READ_IF_EXISTS(pSettings,r_float,section,"flight_separation_interval",.1f);
    R_ASSERT2(_valid(m_corridor_check_interval) && _valid(m_separation_interval) &&
        m_corridor_check_interval>0.f && m_separation_interval>0.f,"Invalid flying query interval");
    clamp(m_corridor_check_interval,.02f,.5f); clamp(m_separation_interval,.02f,.25f);
    m_search_limit = READ_IF_EXISTS(pSettings, r_u32, section, "flight_search_limit", 8192);
    m_landing_height = READ_IF_EXISTS(pSettings, r_float, section, "flight_landing_height", 1.5f);
    m_hop_height = READ_IF_EXISTS(pSettings, r_float, section, "flight_ground_hop_height", .12f);
    const char* ground_mode = READ_IF_EXISTS(pSettings, r_string, section, "flight_ground_movement", "walk");
    R_ASSERT2(xr_strcmp(ground_mode, "walk") == 0 || xr_strcmp(ground_mode, "hop") == 0,
        "flight_ground_movement must be walk or hop");
    set_ground_movement(xr_strcmp(ground_mode, "hop") == 0 ? EGroundMovement::Hop : EGroundMovement::Walk);
    m_hop_length = READ_IF_EXISTS(pSettings, r_float, section, "flight_ground_hop_length", .35f);
    HopSpeedScale = READ_IF_EXISTS(pSettings, r_float, section, "flight_ground_hop_speed_scale", 1.f);
    R_ASSERT2(_valid(HopSpeedScale) && HopSpeedScale > 0.f && HopSpeedScale <= 8.f, "Invalid ground hop settings");
    m_ground_offset = READ_IF_EXISTS(pSettings, r_float, section, "flight_ground_offset", .03f);
    m_landing_footprint_radius = READ_IF_EXISTS(pSettings, r_float, section,
        "flight_landing_footprint_radius", .01f);
    const float LandingMaxSlope = READ_IF_EXISTS(pSettings, r_float, section, "flight_landing_max_slope", 65.f);
    R_ASSERT2(_valid(LandingMaxSlope) && LandingMaxSlope >= 0.f && LandingMaxSlope <= 65.f,
        "flight_landing_max_slope must be between 0 and 65 degrees");
    LandingMinNormalY = cosf(LandingMaxSlope * PI / 180.f);
    m_landing_search_radius = READ_IF_EXISTS(pSettings, r_float, section, "flight_landing_search_radius", 3.f);
    m_landing_search_max_radius = READ_IF_EXISTS(pSettings, r_float, section, "flight_landing_search_max_radius", 6.f);
    m_landing_body_radius = READ_IF_EXISTS(pSettings,r_float,section,"flight_landing_body_radius",0.f);
    m_landing_spacing = READ_IF_EXISTS(pSettings, r_float, section, "flight_landing_spacing", .4f);
    m_separation_distance = READ_IF_EXISTS(pSettings, r_float, section, "flight_separation_distance", .8f);
    m_separation_speed = READ_IF_EXISTS(pSettings, r_float, section, "flight_separation_speed", 1.5f);
    m_debug = READ_IF_EXISTS(pSettings,r_bool,section,"flight_debug",false);
    clamp(m_speed, .1f, 50.f);
    clamp(m_acceleration, .1f, 100.f);
    clamp(m_radius, .05f, 5.f);
    clamp(m_turn_speed, .1f, 10.f);
    clamp(m_turn_acceleration, .1f, 30.f);
    clamp(m_orientation_response, .1f, 20.f);
    clamp(m_direction_sync_speed, .1f, 30.f);
    clamp(m_cell_size, m_radius * 2.f, 10.f);
    clamp(m_min_cell_size, .1f, m_cell_size);
    clamp(m_search_margin, m_cell_size * 2.f, 100.f);
    clamp(m_max_distance, 10.f, 2000.f);
    clamp(m_search_budget, 1u, 128u);
    clamp(m_search_limit, 64u, 65536u);
    clamp(m_landing_height, m_radius * 2.f, 10.f);
    clamp(m_ground_offset, 0.f, 10.f);
    clamp(m_hop_height, 0.f, 1.f);
    clamp(m_hop_length, .05f, 2.f);
    clamp(m_landing_footprint_radius, .001f, 2.f);
    clamp(m_landing_search_radius, 0.f, 10.f);
    clamp(m_landing_search_max_radius, m_landing_search_radius, 15.f);
    clamp(m_landing_spacing, .02f, 10.f);
    clamp(m_landing_body_radius,0.f,5.f);
    clamp(m_separation_distance, m_radius * 2.f, 10.f);
    clamp(m_separation_speed, .1f, 5.f);
    BudgetedFlight = READ_IF_EXISTS(pSettings, r_bool, section, "flight_budgeted_updates", false);
    BoundaryMargin = READ_IF_EXISTS(pSettings, r_float, section, "flight_boundary_margin", 40.f);
    clamp(BoundaryMargin, 1.f, 1000.f);
    MovementCheckInterval = READ_IF_EXISTS(pSettings, r_float, section, "flight_movement_check_interval", .25f);
    AirChecksPerFrame = READ_IF_EXISTS(pSettings, r_u32, section, "flight_air_checks_per_frame", 8);
    RouteStartsPerFrame = READ_IF_EXISTS(pSettings, r_u32, section, "flight_route_starts_per_frame", 4);
    LandingProbesPerFrame = READ_IF_EXISTS(pSettings, r_u32, section, "flight_landing_probes_per_frame", 8);
    LandingProbeCount = READ_IF_EXISTS(pSettings, r_u32, section, "flight_landing_probe_count", 32);
    SearchExpansionsPerFrame = READ_IF_EXISTS(pSettings, r_u32, section, "flight_search_expansions_per_frame", 64);
    clamp(MovementCheckInterval, .1f, 2.f);
    clamp(AirChecksPerFrame, 1u, 64u);
    clamp(RouteStartsPerFrame, 1u, 64u);
    clamp(LandingProbesPerFrame, 1u, 64u);
    clamp(LandingProbeCount, 1u, 1024u);
    clamp(SearchExpansionsPerFrame, 1u, 128u);
    if (BudgetedFlight)
    {
        CFlyingWorkBudget::Configure(CFlyingWorkBudget::EKind::AirValidation, AirChecksPerFrame);
        CFlyingWorkBudget::Configure(CFlyingWorkBudget::EKind::RouteStart, RouteStartsPerFrame);
        CFlyingWorkBudget::Configure(CFlyingWorkBudget::EKind::LandingProbe, LandingProbesPerFrame);
    }
    reset();
}

void CFlyingMovementController::set_model_bounds(const Fbox& bounds)
{
    if (!m_use_model_bounds || !_valid(bounds.min) || !_valid(bounds.max) ||
        bounds.max.x <= bounds.min.x || bounds.max.y <= bounds.min.y || bounds.max.z <= bounds.min.z)
        return;
    AirVolumeValid = CertificateValid = SafePositionValid = CachedDepartureValid = false;
    m_body_center.add(bounds.min, bounds.max).mul(.5f);
    m_body_half_size.sub(bounds.max, bounds.min).mul(.5f);
    m_radius = std::max(m_body_half_size.x, std::max(m_body_half_size.y, m_body_half_size.z));
    clamp(m_radius, .05f, 5.f);
    // Load() ran before the model bounds were available. Wing span must also
    // determine the separation range, rather than the small fallback radius.
    m_separation_distance = std::max(m_separation_distance, m_radius * 2.f + .05f);
    m_have_model_bounds = true;
}

void CFlyingMovementController::reset()
{
    stop();
    SafePositionValid = false;
    CachedDepartureValid = false;
    m_status = EStatus::Idle;
    m_destination.set(0,0,0);
    m_approach.set(0,0,0);
    m_land = false;
}

void CFlyingMovementController::stop()
{
    CFlyingWorkBudget::Cancel(this);
    AirValidationWaiting = LandingProbeWaiting = false;
    PendingCommand = RouteStartPending = LandingRetargetPending = PreparationFailed = false;
	CruiseSegmentValid = false;
	AmbientLeg = false;
	NextCruise.Clear();
    LandingRequestValid = WorkDeferred = false;
    AirVolumeValid = false;
    release_landing();
    m_command_landing_spacing = m_landing_spacing;
    m_landing_retargets = 0;
    m_rejected_landings.clear();
    const bool on_ground = grounded();
    m_progress_valid=false; m_progress_time=m_planning_time=0.f; m_taking_off=false;
    DepartureLiftPending = false;
    RecoveryActive = false;
    RecoveryRetry = 0.f;
    RecoveryProbe = 0;
    m_fallback_stage = EFallback::None;
    m_fallback_used = false;
    m_air_probe_timer = 0.f;
    m_search.reset();
    m_path.clear();
    m_path_index = 0;
    m_walk_vertex = u32(-1);
    m_hop_phase = m_hop_lift = 0.f;
    m_velocity.set(0,0,0);
    m_status = on_ground ? EStatus::Landed : EStatus::Idle;
    CertificateValid = false;
    FlightPoseValid = false;
    FlightPoseTick = 0.f;
    m_stuck_time = 0.f;
    m_corridor_check_timer = 0.f;
    m_separation_timer=0.f; m_separation_force.set(0,0,0);
    m_replans = 0;
    m_yaw_rate = 0.f;
    m_turning = false;
    m_search_cell = m_cell_size;
}

bool CFlyingMovementController::clear_path(CGameObject& object, const Fvector& from,
    const Fvector& to, bool landing) const
{
    Fvector direction;
    direction.sub(to, from);
    const float range = direction.magnitude();
    if (range < EPS_L)
        return true;
    direction.div(range);
    const bool Cacheable = !landing && !grounded() && !m_taking_off && !DepartureLiftPending && m_status == EStatus::Flying;
    const float Envelope = std::max(m_ground_offset + .02f, m_have_model_bounds ?
        m_body_center.magnitude() + m_body_half_size.magnitude() + .02f : m_radius * 2.f + .1f);
    bool VolumeCertified = Cacheable && AirVolumeValid && AirVolume.contains(from) && AirVolume.contains(to);
    if (Cacheable && !VolumeCertified && range <= 32.f)
    {
        Fbox Roots;
        Roots.set(from, from);
        Roots.modify(to);
        Roots.grow(2.f);
        Fbox Expanded = Roots;
        Expanded.grow(Envelope);
        CDB::COLLIDER Collider;
        Collider.box_options(CDB::OPT_ONLYFIRST);
        Collider.r_clear();
        Collider.box_query(Level().ObjectSpace.GetStaticModel(), Expanded);
        if (!Collider.r_count())
        {
            AirVolume = Roots;
            AirVolumeValid = VolumeCertified = true;
        }
    }
    const auto OnCertifiedSegment = [&](const Fvector& Point)
    {
        Fvector Axis; Axis.sub(CertifiedTo, CertifiedFrom);
        const float LengthSq = Axis.square_magnitude();
        if (LengthSq < EPS_S) return false;
        Fvector Relative; Relative.sub(Point, CertifiedFrom);
        const float Along = Relative.dotproduct(Axis) / LengthSq;
        if (Along < 0.f || Along > 1.f) return false;
        Fvector Projected; Projected.mad(CertifiedFrom, Axis, Along);
        return Projected.distance_to_sqr(Point) < .000001f;
    };
    const bool StaticCertified = VolumeCertified || (Cacheable && CertificateValid &&
        CertifiedOrientation.i.distance_to_sqr(object.XFORM().i) < .000001f &&
        CertifiedOrientation.j.distance_to_sqr(object.XFORM().j) < .000001f &&
        CertifiedOrientation.k.distance_to_sqr(object.XFORM().k) < .000001f &&
        OnCertifiedSegment(from) && OnCertifiedSegment(to));
    Fvector QueryCentre;
    QueryCentre.add(from, to).mul(.5f);
    xr_vector<CObject*> DynamicCandidates;
    // Cruising birds only need the static world. Dynamic hulls matter during
    // ground contact, takeoff and descent, never along an airborne cruise leg.
    if (!BudgetedFlight || landing || NeedsGroundCollision(from))
    {
        xr_vector<ISpatialShared> Candidates;
        g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, QueryCentre, range * .5f + Envelope);
        for (const auto& Spatial : Candidates)
        {
            if (!Spatial) continue;
            auto* Other = Spatial->dcast_CObject();
            if (!Other || Other == &object || Other->getDestroy() || !Other->collidable.model) continue;
            if (!FlyingCollisionCandidate(collide::ray_defs(from, direction, range, 0, collide::rqtObject), Other, nullptr)) continue;
            if (Other->collidable.model->Type() != cftObject) continue;
            DynamicCandidates.push_back(Other);
        }
    }
    const bool DynamicObstacles = !DynamicCandidates.empty();
    if (StaticCertified && !DynamicObstacles) return true;
    const collide::rq_target Target = StaticCertified ? collide::rqtObject :
        DynamicObstacles ? collide::rqtBoth : collide::rqtStatic;
    // Support clipping below is only for the body envelope. The feet must
    // follow the actual segment, otherwise a slope can swallow the root.
    Fvector FeetStart = from;
    FeetStart.y += .02f - m_ground_offset;
    collide::rq_result FeetHit;
    if (flying_pick(object, FeetStart, direction, range, FeetHit, Target, true, &DynamicCandidates))
    {
        return false;
    }
    // Sweep the model volume; the fallback sphere is lifted above its feet.
    Fmatrix orientation;
    if (m_have_model_bounds)
    {
        orientation = object.XFORM();
        orientation.c.set(0,0,0);
    }
    static const Fvector offsets[] = {{0,0,0},{1,0,0},{-1,0,0},{0,1,0},
        {0,-1,0},{0,0,1},{0,0,-1},
        {-1,-1,-1},{-1,-1,1},{-1,1,-1},{-1,1,1},
        {1,-1,-1},{1,-1,1},{1,1,-1},{1,1,1}};
    Fvector support_from, normal_from, support_to, normal_to;
    auto near_support = [&](const Fvector& root, Fvector& contact, Fvector& normal)
    {
        Fvector probe = root;
        probe.y += .1f;
        return support(object, probe, m_ground_offset + .25f, contact, normal) &&
            root.y >= contact.y - .02f && root.y - contact.y <= m_ground_offset + .15f;
    };
    const bool check_support=landing || grounded() || m_taking_off || m_status==EStatus::Landing ||
        m_status==EStatus::Idle || m_status==EStatus::Failed;
    const bool supported_from = check_support && near_support(from, support_from, normal_from);
    const bool supported_to = check_support && near_support(to, support_to, normal_to);
    auto keep_above_support = [](Fvector& sample, const Fvector& contact, const Fvector& normal)
    {
        const float surface_y = contact.y -
            (normal.x * (sample.x - contact.x) + normal.z * (sample.z - contact.z)) / normal.y;
        sample.y = std::max(sample.y, surface_y + .02f);
    };
    Fvector half_size = m_body_half_size;
    if (m_landing_body_radius > 0.f && (landing || m_status == EStatus::Landing || grounded() || m_taking_off || DepartureLiftPending))
    {
        // Folded wings use the smaller envelope on descent and grounded steps.
        half_size.x = std::min(half_size.x, m_landing_body_radius);
        half_size.z = std::min(half_size.z, m_landing_body_radius);
    }
    for (const Fvector& offset : offsets)
    {
        Fvector start;
        if (m_have_model_bounds)
        {
            Fvector local;
            local.set(m_body_center.x + offset.x * half_size.x,
                m_body_center.y + offset.y * half_size.y,
                m_body_center.z + offset.z * half_size.z);
            // An oriented box retains the model's real height and width;
            // using its largest radius as a sphere would bury it in support.
            orientation.transform_dir(start, local);
            start.add(from);
            start.y += .02f;
        }
        else
        {
            start.mad(from, offset, m_radius);
            start.y += m_radius + .06f;
        }
        Fvector end;
        end.mad(start, direction, range);
        if (supported_from)
            keep_above_support(start, support_from, normal_from);
        if (supported_to)
            keep_above_support(end, support_to, normal_to);
        Fvector sample_direction;
        sample_direction.sub(end, start);
        const float sample_range = sample_direction.magnitude();
        if (sample_range < EPS_L)
            continue;
        sample_direction.div(sample_range);
        collide::rq_result hit;
        if (flying_pick(object, start, sample_direction, sample_range, hit, Target, false, &DynamicCandidates))
            return false;
    }
    if (Cacheable && !StaticCertified)
    {
        CertifiedFrom = from; CertifiedTo = to;
        CertifiedOrientation = object.XFORM();
        CertificateValid = true;
    }
    return true;
}

bool CFlyingMovementController::support(CGameObject& object, const Fvector& from,
    float range, Fvector& point, Fvector& normal)
{
    collide::rq_result hit;
    if (!flying_pick(object, from, Fvector().set(0,-1,0), range, hit, collide::rqtStatic, true))
        return false;
    const auto& triangle = Level().ObjectSpace.GetStaticTris()[hit.element];
    if (GMLib.GetMaterialByIdx(triangle.material)->Flags.is(SGameMtl::flLiquid)) return false;
    const auto& vertices = Level().ObjectSpace.GetStaticVerts();
    Fvector a, b;
    a.sub(vertices[triangle.verts[1]], vertices[triangle.verts[0]]);
    b.sub(vertices[triangle.verts[2]], vertices[triangle.verts[0]]);
    normal.crossproduct(a, b).normalize_safe();
    if (normal.y < cosf(65.f * PI / 180.f))
        return false;
    point.mad(from, Fvector().set(0,-1,0), hit.range);
    return true;
}

bool CFlyingMovementController::landing_footprint(CGameObject& object,
    const Fvector& contact, const Fvector& normal) const
{
    if (normal.y < LandingMinNormalY)
        return false;
    // The circular foot disk lies in the triangle's tangent plane. Probe
    // along its normal, so steep slopes do not stretch it into an ellipse.
    Fvector tangent, bitangent;
    Fvector::generate_orthonormal_basis(normal, tangent, bitangent);
    const float tolerance = std::max(.01f, m_landing_footprint_radius * .1f);
    Fvector ray_direction = normal;
    ray_direction.invert();
    // Most terrain/roof perches fit wholly inside a single triangle. Prove
    // that geometrically with one ray, instead of tracing 24 footprint samples.
    Fvector centre_probe; centre_probe.mad(contact,normal,tolerance*2.f);
    collide::rq_result centre_hit;
    if (!flying_pick(object,centre_probe,ray_direction,tolerance*4.f,centre_hit,collide::rqtStatic,true) ||
        std::abs(centre_hit.range-tolerance*2.f)>tolerance) return false;
    const auto& tri=Level().ObjectSpace.GetStaticTris()[centre_hit.element];
    if (GMLib.GetMaterialByIdx(tri.material)->Flags.is(SGameMtl::flLiquid)) return false;
    const auto& verts=Level().ObjectSpace.GetStaticVerts();
    Fvector ab, ac, triangle_normal;
    ab.sub(verts[tri.verts[1]],verts[tri.verts[0]]);
    ac.sub(verts[tri.verts[2]],verts[tri.verts[0]]);
    triangle_normal.crossproduct(ab,ac).normalize_safe();
    bool inside=triangle_normal.dotproduct(normal)>.9999f;
    for (u32 edge=0; edge<3; ++edge)
    {
        Fvector along, offset, inward;
        along.sub(verts[tri.verts[(edge+1)%3]],verts[tri.verts[edge]]);
        offset.sub(contact,verts[tri.verts[edge]]);
        inward.crossproduct(normal,along);
        const float length=inward.magnitude();
        if (length<EPS_L || offset.dotproduct(inward)<(m_landing_footprint_radius+.001f)*length) inside=false;
    }
    if (inside) return true;
    for (u32 ring = 1; ring <= 2; ++ring)
    {
        const u32 samples = ring == 1 ? 8 : 16;
        const float radius = m_landing_footprint_radius * (ring == 1 ? .5f : 1.f);
        for (u32 i = 0; i < samples; ++i)
        {
            const float angle = PI_MUL_2 * float(i) / float(samples);
            Fvector probe = contact;
            probe.mad(tangent, cosf(angle) * radius);
            probe.mad(bitangent, sinf(angle) * radius);
            probe.mad(normal, tolerance * 2.f);
            collide::rq_result hit;
            if (!flying_pick(object, probe, ray_direction, tolerance * 4.f, hit, collide::rqtStatic, true) ||
                std::abs(hit.range - tolerance * 2.f) > tolerance)
                return false;
            if (GMLib.GetMaterialByIdx(Level().ObjectSpace.GetStaticTris()[hit.element].material)->Flags.is(SGameMtl::flLiquid)) return false;
        }
    }
    return true;
}

bool CFlyingMovementController::find_landing_point(CGameObject& object, float radius,
    float range, bool ground_only, Fvector& point) const
{
    if (!_valid(radius) || !_valid(range) || radius <= 0.f || radius > 100.f ||
        range <= 0.f || range > m_max_distance)
        return false;
    xr_vector<Fvector> candidates;
    const bool walking_goal = ground_only && m_strategy == EStrategy::GroundOnly;
    const auto& graph = ai().level_graph();
    u32 ground_start = u32(-1);
    if (walking_goal)
    {
        if (!graph.valid_vertex_position(object.Position()))
            return false;
        ground_start = graph.vertex_id(object.Position());
        if (!graph.valid_vertex_id(ground_start) || !graph.is_accessible(ground_start))
            return false;
    }
    for (u32 i = 0; i < 48; ++i)
    {
        const float angle = Random.randF(0.f, PI_MUL_2);
        const float distance = i ? radius * sqrtf(Random.randF(0.f, 1.f)) : 0.f;
        Fvector from = object.Position(), contact, normal;
        from.x += cosf(angle) * distance;
        from.z += sinf(angle) * distance;
        from.y += .5f;
        if (!support(object, from, range, contact, normal))
            continue;
        if (ground_only)
        {
            // Ground graph validates support only; it never determines the air route.
            if (!graph.valid_vertex_position(contact))
                continue;
            const u32 id = graph.vertex_id(contact);
            if (!graph.valid_vertex_id(id))
                continue;
            const Fvector ground = graph.vertex_position(id);
            if (std::abs(ground.y - contact.y) >= .5f || ground.distance_to_sqr(contact) >= 4.f)
                continue;
            if (walking_goal)
            {
                const float dx = contact.x - object.Position().x;
                const float dz = contact.z - object.Position().z;
                if (!graph.is_accessible(id) || id == ground_start || dx * dx + dz * dz < 1.f ||
                    contact.distance_to(object.Position()) > 10.f)
                    continue;
                xr_vector<u32> ground_path;
                if (!graph.Search(ground_start, id, ground_path, 40.f, 8192, 8192))
                    continue;
            }
        }
        const bool supported = landing_footprint(object, contact, normal);
        Fvector feet = contact, approach;
        feet.y += m_ground_offset;
        approach = feet;
        approach.y += m_landing_height;
        Fvector above = approach;
        above.y += m_radius * 2.f;
        Fvector standing = feet;
        standing.y += .02f;
        const bool clearance = walking_goal ? clear_path(object, feet, standing) :
            (clear_path(object, approach, above) && clear_path(object, approach, feet, true));
        if (supported && clearance)
            candidates.push_back(contact);
    }
    if (candidates.empty())
        return false;
    std::sort(candidates.begin(), candidates.end(), [](const Fvector& a, const Fvector& b)
        { return a.y > b.y; });
    const size_t upper_quarter = (candidates.size() + 3) / 4;
    size_t count = 1;
    while (count < upper_quarter && candidates[count].y >= candidates.front().y - 1.5f)
        ++count;
    point = candidates[Random.randI(0, int(count))];
    return true;
}

void CFlyingMovementController::release_landing()
{
    landing_reservations.erase(std::remove_if(landing_reservations.begin(), landing_reservations.end(),
        [&](const SLandingReservation& entry) { return entry.owner == this; }), landing_reservations.end());
}

void CFlyingMovementController::reserve_landing()
{
    release_landing();
    landing_reservations.push_back({this, m_destination, m_command_landing_spacing});
}

bool CFlyingMovementController::landing_space_free(CGameObject& object, const Fvector& root) const
{
    // Reserve destinations immediately, even while their birds are far away.
    for (const auto& entry : landing_reservations)
        if (entry.owner != this && root.distance_to_sqr(entry.root) <
            _sqr(std::max(m_command_landing_spacing, entry.spacing)))
            return false;
    xr_vector<ISpatialShared> neighbours;
    g_SpatialSpace->q_sphere(neighbours, 0, ESPATIAL_TYPE::RENDERABLE, root, m_command_landing_spacing);
    for (const auto& spatial : neighbours)
    {
        if (!spatial) continue;
        auto* other = smart_cast<CFlyingMonster*>(spatial->dcast_CObject());
        if (!other || other == &object || other->getDestroy() || !other->g_Alive()) continue;
        if (root.distance_to_sqr(other->Position()) <
            _sqr(std::max(m_command_landing_spacing, other->flight().m_landing_spacing)))
            return false;
    }
    return true;
}

bool CFlyingMovementController::retarget_landing(CGameObject& object)
{
    if (!m_land || (!LandingRetargetPending && m_landing_retargets >= 4) || m_strategy == EStrategy::GroundOnly)
        return false;
    if (!LandingRetargetPending)
    {
        ++m_landing_retargets;
        m_rejected_landings.push_back(m_destination);
        LandingRequestValid = false;
    }
    WorkDeferred = false;
    Fvector alternative;
    if (!resolve_landing_point(object, m_requested_landing, alternative) ||
        alternative.distance_to_sqr(m_destination) < .0001f)
    {
        if (WorkDeferred)
        {
            LandingRetargetPending = true;
            return true;
        }
        LandingRetargetPending = false;
        { if (m_debug) Msg("! [flying] landing relocation failed: id=%u attempt=%u max_radius=%.2f",
            u32(object.ID()), m_landing_retargets, m_landing_search_max_radius); }
        return false;
    }
    LandingRetargetPending = false;
    m_destination = alternative;
    m_approach = alternative;
    m_approach.y += m_landing_height;
    reserve_landing();
    m_replans = 0;
    m_search_cell = m_cell_size;
    begin_search(object);
    return true;
}

bool CFlyingMovementController::resolve_landing_point(CGameObject& object,
    const Fvector& requested, Fvector& root) const
{
    auto usable = [&](const Fvector& contact, const Fvector& normal, Fvector& feet)
    {
        feet = contact;
        feet.y += m_ground_offset;
        if (feet.distance_to(object.Position()) > m_max_distance)
            return false;
        // Reject the failed body region, not just the same mathematical point.
        // Adjacent samples on a stone flank can share the same blocked descent.
        const float RejectedRadius = std::max(.25f, m_landing_body_radius * 2.f);
        for (const auto& rejected : m_rejected_landings)
            if (feet.distance_to_sqr(rejected) < _sqr(RejectedRadius))
                return false;
        if (!landing_space_free(object, feet) || !landing_footprint(object, contact, normal))
            return false;
        Fvector approach = feet, above;
        approach.y += m_landing_height;
        above = approach;
        above.y += m_radius * 2.f;
        return clear_path(object, approach, above) && clear_path(object, approach, feet, true);
    };
    if (BudgetedFlight)
    {
        if (!LandingRequestValid || LandingRequest.distance_to_sqr(requested) > .0001f)
        {
            LandingRequest = requested;
            LandingRequestValid = true;
            LandingProbeCursor = 0;
        }
        if (!RouteJobActive && !CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::LandingProbe, LandingProbesPerFrame))
        {
            WorkDeferred = true;
            return false;
        }
        const u32 Index = LandingProbeCursor++;
        Fvector Probe = requested, Contact, Normal;
        float Range = 1.f;
        if (Index && m_landing_search_radius > 0.f)
        {
            const float Fraction = float(Index) / float(LandingProbeCount);
            const float Radius = m_landing_search_max_radius * sqrtf(Fraction);
            const float Angle = float(Index) * 2.39996323f + float(object.ID()) * .61803399f;
            Probe.x += cosf(Angle) * Radius;
            Probe.z += sinf(Angle) * Radius;
            Probe.y += m_landing_search_max_radius;
            Range = m_landing_search_max_radius * 2.f;
        }
        else
        {
            Probe.y += .5f;
        }
        if (support(object, Probe, Range, Contact, Normal) && usable(Contact, Normal, root))
        {
            LandingRequestValid = false;
            return true;
        }
        WorkDeferred = m_landing_search_radius > 0.f && LandingProbeCursor <= LandingProbeCount;
        if (!WorkDeferred)
        {
            LandingRequestValid = false;
        }
        return false;
    }
    Fvector probe = requested, contact, normal;
    probe.y += .5f;
    if (support(object, probe, 1.f, contact, normal) && usable(contact, normal, root))
        return true;
    struct SCandidate { Fvector contact; Fvector normal; float distance; };
    xr_vector<SCandidate> candidates;
    // Search the local zone first, and expand once if it has no free perch.
    for (u32 expansion = 0; expansion < 2 && m_landing_search_radius > 0.f; ++expansion)
    {
        const float search_radius = expansion ? m_landing_search_max_radius : m_landing_search_radius;
        if (expansion && search_radius <= m_landing_search_radius + EPS_L)
            break;
        candidates.clear();
        for (u32 ring = 0; ring <= 8; ++ring)
        {
            const u32 samples = ring ? 24u : 1u;
            const float radius = search_radius * float(ring) / 8.f;
            for (u32 i = 0; i < samples; ++i)
            {
                const float angle = PI_MUL_2 * float(i) / float(samples);
                probe = requested;
                probe.x += cosf(angle) * radius;
                probe.z += sinf(angle) * radius;
                probe.y += search_radius;
                if (!support(object, probe, search_radius * 2.f, contact, normal))
                    continue;
                const float distance = contact.distance_to_sqr(requested);
                if (distance <= _sqr(search_radius))
                    candidates.push_back({contact, normal, distance});
            }
        }
        std::sort(candidates.begin(), candidates.end(), [](const SCandidate& a, const SCandidate& b)
            { return a.distance < b.distance; });
        for (const auto& candidate : candidates)
            if (usable(candidate.contact, candidate.normal, root))
            {
                { if (m_debug) Msg("* [flying] landing adjusted: id=%u search_radius=%.2f distance=%.3f at (%.3f, %.3f, %.3f)",
                    u32(object.ID()), search_radius, sqrtf(candidate.distance), root.x, root.y, root.z); }
                return true;
            }
    }
    return false;
}

bool CFlyingMovementController::fly_to(CGameObject& object, const Fvector& destination, bool land, float landing_spacing,
    bool PreserveCurrentOnFailure)
{
    if (!BudgetedFlight)
    {
        WorkDeferred = false;
        return StartFlight(object, destination, land, landing_spacing, PreserveCurrentOnFailure);
    }
    if (m_strategy == EStrategy::GroundOnly || !_valid(destination) || destination.distance_to(object.Position()) > m_max_distance)
    {
        if (!PreserveCurrentOnFailure)
        {
            stop();
            m_status = EStatus::Failed;
        }
        return false;
    }
	NextCruise.Clear();
    PreparationFailed = false;
    PendingTarget = PendingRoot = destination;
    if (land)
    {
        PendingRoot.y += m_ground_offset;
    }
    ++CommandVersion;
    PendingGround = false;
    PendingLanding = land;
    PendingSpacing = landing_spacing;
    PendingPreserve = PreserveCurrentOnFailure;
    PendingCommand = true;
    if (!land)
    {
        BeginCachedTakeoff(object);
    }
    LandingRetargetPending = false;
    LandingRequestValid = false;
    return true;
}

bool CFlyingMovementController::BeginCachedTakeoff(CGameObject& Object)
{
    if (!BudgetedFlight || !grounded() || !CachedDepartureValid ||
        Object.Position().distance_to_sqr(CachedDepartureRoot) >= .0025f)
    {
        return false;
    }
    // Reverse the previously checked final descent. Danger can start this lift
    // immediately without requesting new geometry or waiting for goal discovery.
    release_landing();
    m_search.reset();
    m_path.clear();
    m_path.push_back(CachedDepartureAir);
    m_path_index = 0;
    m_land = false;
    m_destination = m_approach = CachedDepartureAir;
    m_taking_off = DepartureLiftPending = true;
    m_departure_origin = Object.Position();
    SafePosition = Object.Position();
    SafePositionValid = true;
    RouteStartPending = LandingRetargetPending = false;
    m_progress_valid = false;
    m_status = EStatus::Flying;
    return true;
}

void CFlyingMovementController::ProcessPendingCommand(CGameObject& Object)
{
    if (!PendingCommand || !CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::RouteStart, RouteStartsPerFrame))
    {
        return;
    }
    const Fvector Target = PendingTarget;
    const bool Land = PendingLanding, Preserve = PendingPreserve;
    const float Spacing = PendingSpacing;
    const bool Ground = PendingGround;
    const u32 Version = CommandVersion;
    WorkDeferred = false;
    RouteJobActive = true;
    const bool Accepted = Ground ? StartGroundCommand(Object, Target, PendingWalkSpeed) :
        StartFlight(Object, Target, Land, Spacing, Preserve);
    RouteJobActive = false;
    if (CommandVersion != Version)
    {
        // Auto ground movement may queue an air replacement. Keep that new
        // command instead of clearing it with the previous job's result.
        return;
    }
    PendingCommand = WorkDeferred;
    PreparationFailed = !Accepted && !WorkDeferred;
    if (!Accepted && !WorkDeferred && !Preserve)
    {
        m_status = EStatus::Failed;
    }
}

bool CFlyingMovementController::StartFlight(CGameObject& object, const Fvector& destination, bool land, float landing_spacing,
    bool PreserveCurrentOnFailure)
{
    if (m_strategy == EStrategy::GroundOnly)
        return false;
    const bool redirecting = m_status == EStatus::Flying;
    bool taking_off = grounded() || m_taking_off;
    if (!taking_off && m_status == EStatus::Failed)
    {
        // Failed is a command result, not evidence that the feet left support.
        Fvector Probe = object.Position(), Contact, Normal;
        Probe.y += .1f;
        taking_off = support(object, Probe, m_ground_offset + .25f, Contact, Normal) &&
            object.Position().y >= Contact.y - .02f && object.Position().y - Contact.y <= m_ground_offset + .15f;
    }
    const Fvector previous_velocity = m_velocity;
    const float previous_yaw_rate = m_yaw_rate;
    // Species AI may try several candidates. A rejected candidate must not
    // discard the route that is already keeping the bird airborne.
    const float PreviousSpacing = m_command_landing_spacing;
    xr_vector<Fvector> PreviousRejected;
    PreviousRejected.swap(m_rejected_landings);
    m_command_landing_spacing = _valid(landing_spacing) && landing_spacing > 0.f ?
        std::max(m_landing_spacing, landing_spacing) : m_landing_spacing;
    auto RejectLambda = [&]()
    {
        m_command_landing_spacing = PreviousSpacing;
        PreviousRejected.swap(m_rejected_landings);
        if (!PreserveCurrentOnFailure && !WorkDeferred)
        {
            stop();
            m_status = EStatus::Failed;
        }
        return false;
    };
    if (!_valid(destination) || destination.distance_to(object.Position()) > m_max_distance ||
        (BudgetedFlight && CruiseContinuity && !InsideFlightBounds(destination)))
    {
        return RejectLambda();
    }
    Fvector Destination = destination;
    if (land && !resolve_landing_point(object, destination, Destination))
    {
        { if (m_debug && !WorkDeferred) Msg("! [flying] landing rejected: id=%u no usable support within %.2fm of (%.3f, %.3f, %.3f)",
            u32(object.ID()), m_landing_search_radius > 0.f ? m_landing_search_max_radius : 0.f,
            destination.x, destination.y, destination.z); }
        return RejectLambda();
    }
    if (BudgetedFlight && CruiseContinuity && !InsideFlightBounds(Destination))
    {
        return RejectLambda();
    }
    Fvector Approach = Destination;
    if (land)
        Approach.y += m_landing_height;
    // A short upward probe ensures the arrival volume is free.
    Fvector above = Approach;
    above.y += m_radius * 2.f;
    const bool approach_clear = clear_path(object, Approach, above);
    const bool descent_clear = !land || clear_path(object, Approach, Destination, true);
    if (!approach_clear || !descent_clear)
    {
        { if (m_debug) Msg("! [flying] arrival rejected: id=%u approach=%s descent=%s at (%.3f, %.3f, %.3f)",
            u32(object.ID()), approach_clear ? "clear" : "blocked", descent_clear ? "clear" : "blocked",
            Destination.x, Destination.y, Destination.z); }
        return RejectLambda();
    }
    // Autonomous airborne birds keep their current route when a new corridor
    // is blocked. Another candidate is cheaper than stopping for an A* search.
    const bool CheckedDirect = BudgetedFlight && CruiseContinuity && redirecting && !taking_off;
    if (CheckedDirect && !clear_path(object, object.Position(), Approach))
    {
        return RejectLambda();
    }
    const float NewSpacing = m_command_landing_spacing;
    stop();
    m_command_landing_spacing = NewSpacing;
    m_destination = Destination;
    m_requested_landing = destination;
    m_approach = Approach;
    m_land = land;
    m_taking_off = taking_off;
    m_departure_origin = object.Position();
    if (land)
        reserve_landing();
    begin_search(object, CheckedDirect);
    if (redirecting && m_status == EStatus::Flying)
    {
        m_velocity = previous_velocity;
        m_yaw_rate = previous_yaw_rate;
    }
    return m_status != EStatus::Failed;
}

bool CFlyingMovementController::restore_landed(CGameObject& object)
{
    stop();
    Fvector probe = object.Position(), contact, normal;
    probe.y += .5f;
    if (!support(object, probe, m_ground_offset + .8f, contact, normal) ||
        std::abs(contact.y + m_ground_offset - object.Position().y) > .5f)
    {
        m_status = EStatus::Failed;
        return false;
    }
    contact.y += m_ground_offset;
    // Ground initialization corrects a small spawn-height mismatch. A model
    // spawned with its root at the actor's feet can have its own feet below
    // the surface; air body rays must not prevent this upward correction.
    if (!landing_footprint(object, Fvector().set(contact.x, contact.y - m_ground_offset, contact.z), normal))
    {
        // An unsuitable perch must not leave a newly spawned root below its
        // feet. Correct existing penetration upwards, without seating here.
        object.Position().y = std::max(object.Position().y, contact.y);
        m_status = EStatus::Failed;
        return false;
    }
    object.Position().set(contact);
    m_destination = contact;
    m_approach = m_destination;
    m_land = true;
    m_status = EStatus::Landed;
    return true;
}

void CFlyingMovementController::begin_search(CGameObject& object, bool CheckedDirect)
{
    if (BudgetedFlight && !RouteJobActive &&
        !CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::RouteStart, RouteStartsPerFrame))
    {
        RouteStartPending = true;
        m_status = EStatus::Planning;
        return;
    }
    RouteStartPending = false;
	AmbientLeg = false;
    m_path.clear();
    m_path_index = 0;
    m_velocity.set(0,0,0);
    m_stuck_time = 0.f;
    m_search.reset();
    DepartureLiftPending = false;
    if (m_taking_off)
    {
        // Clear the support before steering towards a distant escape/cruise
        // goal. Try short checked departures under overhangs before pathfinding.
        Fvector Lift = m_departure_origin;
        Lift.y += std::max(m_landing_height, m_radius * 2.f + .1f);
        Fvector Direction;
        Direction.sub(m_approach, m_departure_origin);
        const float Heading = Direction.x * Direction.x + Direction.z * Direction.z > EPS_L ?
            Direction.getH() : object.XFORM().k.getH();
        const Fbox& DepartureBounds = Level().ObjectSpace.GetBoundingVolume();
        for (u32 Attempt = 0; Attempt < 5u; ++Attempt)
        {
            Fvector Departure = Lift;
            if (Attempt > 0)
            {
                Fvector Offset;
                Offset.setHP(Heading + float(Attempt - 1u) * PI_DIV_2, 0.f);
                Departure.mad(Offset, std::max(.5f, m_radius * 2.f));
            }
            if (Departure.x < DepartureBounds.min.x || Departure.x > DepartureBounds.max.x ||
                Departure.z < DepartureBounds.min.z || Departure.z > DepartureBounds.max.z)
            {
                continue;
            }
            if (clear_path(object, object.Position(), Departure))
            {
                m_path.push_back(Departure);
                DepartureLiftPending = true;
                SafePosition = object.Position();
                SafePositionValid = true;
                m_status = EStatus::Flying;
                return;
            }
        }
    }
    if (CheckedDirect || clear_path(object, object.Position(), m_approach))
    {
        m_path.push_back(m_approach);
        SafePosition = object.Position();
        SafePositionValid = true;
        m_status = EStatus::Flying;
        MovementCheckTimer = MovementCheckInterval * float(u32(object.ID()) * 137u % 1024u) / 1024.f;
        return;
    }
    // Autonomous cruise does not need a stationary obstacle search. Escape
    // the blocked corridor cheaply and let the species prepare its next goal.
    if (BudgetedFlight && CruiseContinuity && !m_land && RecoverAirMotion(object))
    {
        return;
    }
    m_search = std::make_unique<SSearch>(BudgetedFlight);
    Fbox bounds;
    bounds.set(object.Position(), object.Position());
    bounds.modify(m_approach);
    bounds.grow(m_search_margin);
    const Fbox level_bounds = FlightBounds();
    bounds.min.max(level_bounds.min);
    // Static geometry bounds describe the map, not an air ceiling. Keep the
    // requested sky altitude and search margin above the tallest geometry.
    bounds.max.x = std::min(bounds.max.x, level_bounds.max.x);
    bounds.max.z = std::min(bounds.max.z, level_bounds.max.z);
    const auto point = [](const Fvector& p) { return FlyingPath::Point{p.x,p.y,p.z}; };
    const u32 refinement = std::min(4u, u32(ceilf(m_cell_size / m_search_cell)));
    m_search->search.begin(point(object.Position()), point(m_approach),
        {point(bounds.min), point(bounds.max)}, m_search_cell,
        std::min(65536u, m_search_limit * refinement));
    m_status = EStatus::Planning;
    { if (m_debug) Msg("* [flying] planning: id=%u cell=%.2f from=(%.2f, %.2f, %.2f) to=(%.2f, %.2f, %.2f)",
        u32(object.ID()),m_search_cell,object.Position().x,object.Position().y,object.Position().z,
        m_approach.x,m_approach.y,m_approach.z); }
}

void CFlyingMovementController::search_step(CGameObject& object)
{
    // Share expansion work between simultaneous planners. One scripted flyer
    // retains its configured budget; a large flock does not multiply it by N.
    const u32 budget = std::min(m_search_budget, m_search->Budget(SearchExpansionsPerFrame));
    if (!budget) return;
    m_planning_time += std::min(Device.fTimeDelta, .1f);
    const auto result = m_search->search.step(budget,
        [&](const FlyingPath::Point& from, const FlyingPath::Point& to)
        {
            return clear_path(object, Fvector().set(from.x,from.y,from.z), Fvector().set(to.x,to.y,to.z));
        });
    if (result == FlyingPath::Search::Status::Found)
    {
        { if (m_debug) Msg("* [flying] route found: id=%u nodes=%u",u32(object.ID()),m_search->search.expanded()); }
        for (const auto& point : m_search->search.path())
            m_path.push_back(Fvector().set(point.x,point.y,point.z));
        m_search.reset();
        // Separation may have moved the start while the incremental search ran.
        if (!m_path.empty() && !clear_path(object, object.Position(), m_path.front()))
        {
            begin_search(object);
            return;
        }
        SafePosition = object.Position();
        SafePositionValid = true;
        m_status = EStatus::Flying;
    }
    else if (result == FlyingPath::Search::Status::Failed)
    {
        { if (m_debug) Msg("! [flying] route search failed: id=%u nodes=%u cell=%.2f",u32(object.ID()),m_search->search.expanded(),m_search_cell); }
        if (m_search_cell > m_min_cell_size + EPS_L)
        {
            m_search_cell = std::max(m_min_cell_size, m_search_cell * .5f);
            begin_search(object);
        }
        else
        {
            m_search.reset();
            if (retarget_landing(object))
                return;
            if (!try_ground_fallback(object))
                m_status = EStatus::Failed;
        }
    }
}

bool CFlyingMovementController::try_ground_fallback(CGameObject& object)
{
    if (m_strategy != EStrategy::Auto || m_fallback_used || m_fallback_stage != EFallback::None)
        return false;
    m_fallback_used = true;
    Fvector from = object.Position(), ground, normal;
    from.y += .5f;
    if (!support(object, from, 100.f, ground, normal))
        return false;
    Fvector feet = ground;
    feet.y += m_ground_offset;
    if (!clear_path(object, object.Position(), feet))
        return false;
    auto& graph = ai().level_graph();
    if (!graph.valid_vertex_position(ground))
        return false;
    const u32 start = graph.vertex_id(ground);
    if (!graph.valid_vertex_id(start) || !graph.is_accessible(start) ||
        graph.vertex_position(start).distance_to_sqr(ground) > 4.f)
        return false;
    Fvector direction;
    direction.sub(m_destination, ground);
    direction.y = 0.f;
    if (direction.square_magnitude() < .01f)
        direction.set(1,0,0);
    const float heading = direction.getH();
    bool found = false;
    float best = type_max(float);
    // A finite fan of local detours. No flight/walk command calls back into this search.
    for (u32 i = 0; i < 9; ++i)
    {
        const float angle = heading + (int(i) - 4) * (PI / 8.f);
        Fvector probe = from, contact;
        probe.x += -sinf(angle) * 8.f;
        probe.z += cosf(angle) * 8.f;
        if (!support(object, probe, 100.f, contact, normal) ||
            !graph.valid_vertex_position(contact))
            continue;
        const u32 finish = graph.vertex_id(contact);
        if (!graph.valid_vertex_id(finish) || !graph.is_accessible(finish) ||
            graph.vertex_position(finish).distance_to_sqr(contact) > 4.f ||
            ground.distance_to(contact) > 10.f)
            continue;
        xr_vector<u32> vertices;
        if (!graph.Search(start, finish, vertices, 40.f, 8192, 8192))
            continue;
        const float score = contact.distance_to_sqr(m_destination);
        if (!found || score < best)
        {
            found = true;
            best = score;
            m_ground_destination = contact;
        }
    }
    if (!found)
        return false;
    m_resume_destination = m_destination;
    m_resume_landing = m_land;
    m_fallback_stage = EFallback::Descending;
    m_search.reset();
    m_path.clear();
    m_path_index = 0;
    m_walk_vertex = u32(-1);
    m_destination = feet;
    m_requested_landing = ground;
    m_landing_retargets = 0;
    m_rejected_landings.clear();
    reserve_landing();
    m_land = true;
    m_velocity.set(0,0,0);
    m_status = EStatus::Landing;
    return true;
}

void CFlyingMovementController::resume_air_command(CGameObject& object)
{
    const Fvector destination = m_resume_destination;
    const bool land = m_resume_landing;
    fly_to(object, destination, land);
    // fly_to resets command state. This retry has already consumed its ground detour.
    m_fallback_used = true;
}

bool CFlyingMovementController::direct_flight(CGameObject& object, const Fvector& target,
    bool land, float& length) const
{
    Fvector departure = object.Position(), arrival = target, feet = target;
    if (grounded())
        departure.y += m_landing_height;
    if (land)
    {
        Fvector probe = target, normal;
        probe.y += .5f;
        Fvector contact;
        if (!support(object, probe, 1.f, contact, normal))
            return false;
        if (!landing_footprint(object, contact, normal))
            return false;
        feet = contact;
        feet.y += m_ground_offset;
        arrival = feet;
        arrival.y += m_landing_height;
    }
    if (!clear_path(object, object.Position(), departure) ||
        !clear_path(object, departure, arrival) || (land && !clear_path(object, arrival, feet, true)))
        return false;
    length = object.Position().distance_to(departure) + departure.distance_to(arrival) +
        (land ? arrival.distance_to(feet) : 0.f);
    return true;
}

float CFlyingMovementController::ground_path_length(const Fvector& start) const
{
    float length = 0.f;
    Fvector previous = start;
    for (size_t i = m_path_index; i < m_path.size(); ++i)
    {
        length += previous.distance_to(m_path[i]);
        previous = m_path[i];
    }
    return length;
}

void CFlyingMovementController::separate(CGameObject& object, float dt)
{
    if (dt <= 0.f || grounded() || m_status == EStatus::Landing || DepartureLiftPending)
        return;
    float separation_weight = 1.f;
    if (m_land && m_status == EStatus::Flying && !m_path.empty() &&
        m_path_index + 1 == m_path.size())
    {
        // Separation must not keep close, explicitly assigned perches from
        // reaching their approach points. Fade on the final segment, then
        // let landing own the position. World/support checks remain active.
        const float arrival_distance = object.Position().distance_to(m_approach);
        separation_weight = std::max(0.f, std::min(1.f,
            (arrival_distance - .25f) / m_separation_distance));
        if (separation_weight <= 0.f)
            return;
    }
    m_separation_timer-=dt;
    Fvector push=m_separation_force;
    if (m_separation_timer<=0.f)
    {
        m_separation_timer=m_separation_interval;
        xr_vector<ISpatialShared> neighbours;
        g_SpatialSpace->q_sphere(neighbours, 0, ESPATIAL_TYPE::RENDERABLE,
            object.Position(), m_separation_distance);
        push.set(0,0,0);
        bool crowded = false;
        for (const ISpatialShared& spatial : neighbours)
        {
            if (!spatial)
                continue;
            auto* other = smart_cast<CFlyingMonster*>(spatial->dcast_CObject());
            if (!other || other == &object || other->getDestroy() || !other->g_Alive())
                continue;
            Fvector away;
            away.sub(object.Position(), other->Position());
            const float distance = away.magnitude();
            if (distance >= m_separation_distance)
                continue;
            crowded = true;
            if (distance > EPS_L)
                away.div(distance);
            else
            {
                // The pair chooses opposite directions even for exactly coincident roots.
                const u32 low = std::min(u32(object.ID()), u32(other->ID()));
                const u32 high = std::max(u32(object.ID()), u32(other->ID()));
                const u32 hash = low * 73856093u ^ high * 19349663u;
                const float angle = float(hash % 4096u) * (PI_MUL_2 / 4096.f);
                away.set(cosf(angle), .35f * sinf(angle * 3.f), sinf(angle));
                away.normalize_safe();
                if (object.ID() > other->ID())
                    away.invert();
            }
            push.mad(away, 1.f - distance / m_separation_distance);
        }
        if (!crowded) { m_separation_force.set(0,0,0); return; }
        if (push.square_magnitude() < EPS_L)
        {
            const float angle = float(u32(object.ID()) * 137u % 4096u) * (PI_MUL_2 / 4096.f);
            push.set(cosf(angle), 0.f, sinf(angle));
        }
        const float strength = std::min(1.f, push.magnitude());
        push.normalize_safe().mul(strength);
        m_separation_force=push;
    }
    if (push.square_magnitude()<EPS_L) return;
    push.mul(m_separation_speed * dt * separation_weight);
    const int steps = std::max(1, int(ceilf(push.magnitude() / (m_radius * .5f))));
    push.div(float(steps));
    for (int step = 0; step < steps; ++step)
    {
        Fvector next;
        next.add(object.Position(), push);
        if (clear_path(object, object.Position(), next))
            object.Position().set(next);
        else
        {
            // Slide away along a free axis instead of pushing through world geometry.
            for (u32 axis = 0; axis < 3; ++axis)
            {
                next = object.Position();
                next[axis] += push[axis];
                if (std::abs(push[axis]) > EPS_L && clear_path(object, object.Position(), next))
                    object.Position().set(next);
            }
            break;
        }
    }
}

bool CFlyingMovementController::MovementSegmentFree(CGameObject& Object, const Fvector& From, const Fvector& To) const
{
    // Every new segment is checked by preparation. Runtime validation is shared
    // and staggered; coarse species tolerate brief clipping until correction.
    return BudgetedFlight || clear_path(Object, From, To);
}

bool CFlyingMovementController::ValidateMovement(CGameObject& Object, float Dt)
{
    if (!BudgetedFlight)
    {
        return true;
    }
    MovementCheckTimer -= Dt;
    if (MovementCheckTimer > 0.f)
    {
        return true;
    }
    if (!CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::AirValidation, AirChecksPerFrame))
    {
        AirValidationWaiting = true;
        return true;
    }
    AirValidationWaiting = false;
    MovementCheckTimer = MovementCheckInterval;
    const Fvector From = SafePositionValid ? SafePosition : Object.Position();
    Fvector Ahead = Object.Position();
    const Fvector Target = m_status == EStatus::Landing ? m_destination : m_path[m_path_index];
    Fvector Direction;
    Direction.sub(Target, Ahead);
    const float Length = Direction.magnitude();
    if (Length > EPS_L)
    {
        Ahead.mad(Direction, std::min(1.f, (m_speed * MovementCheckInterval + .1f) / Length));
    }
    const auto SegmentValid = [&](const Fvector& Start, const Fvector& Finish)
    {
        // The route already passed the hull sweep. Coarse maintenance only
        // checks the feet/root line for world penetration, without spatial
        // queries or repeating all wing-envelope rays.
        if (AirVolumeValid && AirVolume.contains(Start) && AirVolume.contains(Finish))
        {
            return true;
        }
        Fvector Direction;
        Direction.sub(Finish, Start);
        const float Range = Direction.magnitude();
        if (Range < EPS_L)
        {
            return true;
        }
        Direction.div(Range);
        Fvector Feet = Start;
        Feet.y += .02f - m_ground_offset;
        collide::rq_result Hit;
        return !flying_pick(Object, Feet, Direction, Range, Hit, collide::rqtStatic, true);
    };
    if (SegmentValid(From, Object.Position()) && SegmentValid(Object.Position(), Ahead))
    {
        SafePosition = Object.Position();
        SafePositionValid = true;
        return true;
    }
    // This is a previously checked root on the route, never the unchecked goal
    // or a height read from the two-dimensional navigation graph.
    if (SafePositionValid)
    {
        Object.Position().set(SafePosition);
    }
    m_velocity.set(0,0,0);
    AirVolumeValid = CertificateValid = false;
	CruiseSegmentValid = false;
	NextCruise.Clear();
    if (m_status == EStatus::Landing && retarget_landing(Object))
    {
        return false;
    }
    if (!m_land && CruiseContinuity && RecoverAirMotion(Object))
    {
        return false;
    }
    begin_search(Object);
    return false;
}

bool CFlyingMovementController::RecoverAirMotion(CGameObject& Object)
{
    if (!BudgetedFlight || !CruiseContinuity || m_land || grounded() || PendingCommand)
    {
        return false;
    }
    Fvector Root = SafePositionValid ? SafePosition : Object.Position();
    Fvector Away;
    Away.sub(Root, m_approach);
    Away.y = 0.f;
    if (Away.square_magnitude() < EPS_L)
    {
        Away = Object.XFORM().k;
        Away.mul(-1.f);
    }
    const float Lift = std::max(m_landing_height * 2.f, CruiseLegDistance * .5f);
    Fvector Probe = Root;
    Probe.y += Lift;
    collide::rq_result Hit;
    // One static ray only after a blocked corridor, under the validation or
    // route-start quota. No dynamic candidates, hull sweep or graph search.
    if (flying_pick(Object, Probe, Fvector().set(0,-1,0), Lift * 2.f, Hit, collide::rqtStatic, true))
    {
        Root.y = std::max(Root.y, Probe.y - Hit.range + m_ground_offset + m_landing_height);
    }
    stop();
    Object.Position().set(Root);
    SafePosition = Root;
    SafePositionValid = true;
    if (!BeginAmbientCruise(Object, Away, CruiseLegDistance))
    {
        m_status = EStatus::Failed;
        return false;
    }
    // The escape rises instead of retrying the same horizontal terrain cut.
    m_path.back().y += Lift;
    m_destination = m_approach = m_path.back();
    Fvector Direction;
    Direction.sub(m_destination, Root).normalize_safe();
    m_velocity.mul(Direction, m_speed * m_speed_scale);
    return true;
}

bool CFlyingMovementController::move(CGameObject& object, const Fvector& target, float dt)
{
    if (BudgetedFlight)
    {
        return MoveBudgeted(object, target, dt);
    }
    m_turning = false;
    Fvector delta;
    delta.sub(target, object.Position());
    const float distance = delta.magnitude();
    if (distance < .08f)
    {
        if (!MovementSegmentFree(object, object.Position(), target))
            return false;
        object.Position().set(target);
        return true;
    }
    delta.div(distance);
    float current_heading, current_pitch, bank;
    object.XFORM().getHPB(current_heading, current_pitch, bank);
    const float horizontal = sqrtf(delta.x * delta.x + delta.z * delta.z);
    const bool PreciseApproach = BudgetedFlight || DepartureLiftPending || distance <= m_landing_height;
    const float route_heading = horizontal > EPS_L ? delta.getH() : current_heading;
    const float turn = angle_difference_signed(route_heading, current_heading);
    // Brake yaw before crossing the desired heading, rather than retaining
    // the full turn rate through a nearby waypoint.
    const float rate_limit = std::min(m_turn_speed, sqrtf(2.f * m_turn_acceleration * std::abs(turn)));
    const float desired_rate = std::max(-rate_limit,
        std::min(rate_limit, turn * m_orientation_response));
    const float rate_change = m_turn_acceleration * dt;
    m_yaw_rate += std::max(-rate_change, std::min(rate_change, desired_rate - m_yaw_rate));
    const float turn_step = std::max(-std::abs(turn), std::min(std::abs(turn), m_yaw_rate * dt));
    const float heading = current_heading + turn_step;
    const float remaining_turn = angle_difference_signed(route_heading, heading);
    m_turning = horizontal > EPS_L && std::abs(remaining_turn) > m_max_direction_lag &&
        std::abs(turn_step) > EPS_S;
    if (m_taking_off && !DepartureLiftPending && object.Position().distance_to(m_departure_origin)>=m_landing_height) m_taking_off=false;
    const float phase_speed = m_status==EStatus::Landing ? m_landing_speed_factor :
        m_taking_off ? m_takeoff_speed_factor : 1.f;
    const float speed = std::min(m_speed*m_speed_scale*phase_speed,
        std::max(.15f, sqrtf(2.f * m_acceleration * distance) * .7f));
    Fvector desired, change, forward;
    const float steering_heading = heading + std::max(-m_max_direction_lag,
        std::min(m_max_direction_lag, remaining_turn));
    forward.setHP(steering_heading, 0.f);
    // Brake into a sharp turn instead of translating backwards while the
    // visible bird is still catching up. Its route remains the steering goal.
    // Slow down within the turning radius. Otherwise a flyer can repeatedly
    // pass a close waypoint while its heading is still following that point.
    // The turning-radius limit is needed only while changing heading. Applying
    // it to an aligned approach makes speed proportional to remaining distance,
    // leaving the flyer hovering above its perch before descent can begin.
    const float horizontal_speed_limit = std::abs(remaining_turn) > m_max_direction_lag ?
        distance * horizontal * m_turn_speed * .5f : speed;
    // A species may keep forward motion through wide cruise turns. Descent
    // and takeoff retain precise braking; all translation is still swept.
    const float MinimumForward = m_status == EStatus::Flying && !m_land && !m_taking_off ? TurnForwardFactor : 0.f;
    desired.mul(forward, std::min(horizontal_speed_limit,
        speed * horizontal * std::max(MinimumForward, cosf(remaining_turn))));
    desired.y = delta.y * speed;
    if (PreciseApproach)
    {
        // Complete the checked segment while the visual heading catches up.
        // A nearby waypoint must not require an ever smaller turning circle.
        desired.mul(delta, speed);
    }
    change.sub(desired, m_velocity);
    const float length = change.magnitude();
    if (length > m_acceleration * dt && length > EPS_L)
        change.mul(m_acceleration * dt / length);
    m_velocity.add(change);
    if (BudgetedFlight)
    {
        // Follow checked segments exactly. Visual heading and bank interpolate
        // independently without bending the path into nearby world geometry.
        const float AlongSpeed = std::min(speed, m_velocity.magnitude());
        m_velocity.mul(delta, AlongSpeed);
    }
    const float horizontal_speed = sqrtf(m_velocity.x * m_velocity.x + m_velocity.z * m_velocity.z);
    if (horizontal_speed > EPS_L && !PreciseApproach)
    {
        float velocity_heading = m_velocity.getH();
        const float sync_turn = angle_difference_signed(steering_heading, velocity_heading);
        velocity_heading += std::max(-m_direction_sync_speed * dt,
            std::min(m_direction_sync_speed * dt, sync_turn));
        const float lag = angle_difference_signed(velocity_heading, heading);
        velocity_heading = heading + std::max(-m_max_direction_lag, std::min(m_max_direction_lag, lag));
        forward.setHP(velocity_heading, 0.f);
        m_velocity.x = forward.x * horizontal_speed;
        m_velocity.z = forward.z * horizontal_speed;
    }
    float velocity_heading, pitch;
    m_velocity.getHP(velocity_heading, pitch);
    const float blend = 1.f - expf(-m_orientation_response * dt);
    const bool Upright = m_taking_off || DepartureLiftPending || m_status == EStatus::Landing;
    const float target_bank = Upright ? 0.f :
        std::max(-.5f, std::min(.5f, -m_yaw_rate * .25f));
    const Fvector position = object.Position();
    object.XFORM().setHPB(heading,
        Upright ? 0.f : current_pitch + (pitch * .5f - current_pitch) * blend,
        Upright ? 0.f : bank + (target_bank - bank) * blend);
    object.Position().set(position);
    const int steps = std::max(1, int(ceilf(m_velocity.magnitude() * dt / (m_radius * .5f))));
    for (int step = 0; step < steps; ++step)
    {
        Fvector next;
        const float travel = m_velocity.magnitude() * dt / steps;
        // Prevent overshooting an endpoint while braking.
        Fvector to_target;
        to_target.sub(target, object.Position());
        if (travel >= to_target.magnitude() && m_velocity.dotproduct(to_target) > 0.f &&
            (PreciseApproach || horizontal < EPS_L || std::abs(remaining_turn) <= m_max_direction_lag))
            next = target;
        else
            next.mad(object.Position(), m_velocity, dt / steps);
        if (!MovementSegmentFree(object, object.Position(), next))
        {
            // In a narrow passage momentum can cut the corner between otherwise
            // clear waypoints. Follow the checked segment rather than replan forever.
            Fvector along;
            along.sub(target, object.Position()).normalize_safe();
            next.mad(object.Position(), along, std::min(travel, object.Position().distance_to(target)));
            if (!MovementSegmentFree(object, object.Position(), next))
            {
                m_velocity.set(0,0,0);
                return false;
            }
            const float along_horizontal = along.x * along.x + along.z * along.z;
            if (!PreciseApproach && along_horizontal > EPS_L &&
                std::abs(angle_difference_signed(along.getH(), heading)) > m_max_direction_lag)
            {
                // Do not defeat heading synchronization when a narrow passage
                // asks us to follow the segment exactly. Turn before advancing.
                m_velocity.set(0,0,0);
                return false;
            }
            m_velocity.mul(along, speed);
        }
        object.Position().set(next);
    }
    if (object.Position().distance_to(target) < .08f && MovementSegmentFree(object, object.Position(), target))
    {
        object.Position().set(target);
        return true;
    }
    return false;
}

void CFlyingMovementController::RecoverFlight(CGameObject& Object, float Dt)
{
	if (grounded() || m_strategy == EStrategy::GroundOnly)
	{
		RecoveryActive = false;
		return;
	}
	if (m_status != EStatus::Failed && !(RecoveryActive && (m_status == EStatus::Arrived || m_status == EStatus::Idle)))
	{
		return;
	}
	RecoveryActive = true;
	RecoveryRetry = std::max(0.f, RecoveryRetry - Dt);
	if (RecoveryRetry > 0.f)
	{
		return;
	}
	if (g_RecoveryFrame != Device.dwFrame)
	{
		g_RecoveryFrame = Device.dwFrame;
		g_RecoveryBudget = 8u;
	}
	if (!g_RecoveryBudget)
	{
		return;
	}
	--g_RecoveryBudget;
	RecoveryRetry = .25f;
	const Fvector Origin = Object.Position();
	const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
	const float Lift = std::max(m_landing_height, m_radius * 2.f + .1f);
	const u32 ProbeIndex = RecoveryProbe++;
	const float Angle = float(Object.ID()) * 2.39996323f + float(ProbeIndex) * 2.39996323f;
	Fvector Offset;
	Offset.set(cosf(Angle), 0.f, sinf(Angle));
	Fvector Probe = Origin;
	Probe.mad(Offset, ProbeIndex % 5u ? std::max(.5f, m_landing_search_max_radius) * float(ProbeIndex % 5u) / 4.f : 0.f);
	Probe.y += Lift;
	Fvector Contact, Normal;
	if (Probe.x >= Bounds.min.x && Probe.x <= Bounds.max.x && Probe.z >= Bounds.min.z && Probe.z <= Bounds.max.z &&
		support(Object, Probe, std::min(m_max_distance, Lift + m_search_margin), Contact, Normal) &&
		landing_footprint(Object, Contact, Normal))
	{
		Fvector Feet = Contact;
		Feet.y += m_ground_offset;
		Fvector Approach = Feet;
		Approach.y += m_landing_height;
		// Recovery must leave the failed body region and use a directly checked
		// continuation, rather than starting another stationary global search.
		if (Feet.distance_to_sqr(m_destination) >= _sqr(std::max(.25f, m_landing_body_radius * 2.f)) &&
			clear_path(Object, Origin, Approach) && fly_to(Object, Feet, true, 0.f, true))
		{
			RecoveryActive = true;
			return;
		}
	}
	Fvector Air = Origin;
	Air.mad(Offset, std::max(1.f, m_radius * 2.f));
	Air.y += Lift;
	if (Air.x >= Bounds.min.x && Air.x <= Bounds.max.x && Air.z >= Bounds.min.z && Air.z <= Bounds.max.z &&
		clear_path(Object, Origin, Air) && fly_to(Object, Air, false, 0.f, true))
	{
		RecoveryActive = true;
	}
}

void CFlyingMovementController::update(CGameObject& object, float dt)
{
    clamp(dt, 0.f, .1f);
    if (AirValidationWaiting && m_status != EStatus::Flying && m_status != EStatus::Landing)
    {
        CFlyingWorkBudget::CancelPending(this, CFlyingWorkBudget::EKind::AirValidation);
        AirValidationWaiting = false;
    }
    if (LandingProbeWaiting && (m_status != EStatus::Landing ||
        object.Position().distance_to_sqr(m_destination) > _sqr(std::max(.16f, m_speed * m_speed_scale * m_landing_speed_factor * dt + .08f))))
    {
        CFlyingWorkBudget::CancelPending(this, CFlyingWorkBudget::EKind::LandingProbe);
        LandingProbeWaiting = false;
    }
    if (PendingCommand)
    {
        ProcessPendingCommand(object);
    }
    if (LandingRetargetPending)
    {
        if (!retarget_landing(object))
        {
            m_status = EStatus::Failed;
            release_landing();
        }
        return;
    }
    if (RouteStartPending)
    {
        begin_search(object);
        if (RouteStartPending && !ResumeCachedCruise(object))
        {
            return;
        }
    }
	if (m_status == EStatus::Arrived)
	{
		ResumeCachedCruise(object);
	}
    if (m_status == EStatus::Walking)
    {
        m_air_probe_timer -= dt;
        if (m_air_probe_timer <= 0.f && m_strategy == EStrategy::Auto)
        {
            m_air_probe_timer = .5f;
            float air_length;
            if (m_fallback_stage == EFallback::Walking)
            {
                if (direct_flight(object, m_resume_destination, m_resume_landing, air_length))
                {
                    { if (m_debug) Msg("[flying] auto resumes air: id=%u air corridor available", u32(object.ID())); }
                    resume_air_command(object);
                    return;
                }
            }
            else if (direct_flight(object, m_destination, true, air_length) &&
                ground_path_length(object.Position()) > 2.5f &&
                ground_path_length(object.Position()) > air_length * 1.1f + 1.f)
            {
                const Fvector target = m_destination;
                { if (m_debug) Msg("[flying] auto switches ground to air: id=%u shorter route ground=%.2f air=%.2f",
                    u32(object.ID()), ground_path_length(object.Position()), air_length); }
                fly_to(object, target, true);
                return;
            }
        }
        const Fvector previous = object.Position();
        const Fvector target = m_path[m_path_index];
        walk(object, target, dt, m_walk_speed);
        const float dx = target.x - object.Position().x;
        const float dz = target.z - object.Position().z;
        const float arrival_sqr = m_path_index + 1 == m_path.size() ? .0004f : .01f;
        if (dx * dx + dz * dz < arrival_sqr)
        {
            if (++m_path_index == m_path.size())
            {
                m_status = EStatus::Landed;
                m_velocity.set(0,0,0);
                if (m_fallback_stage == EFallback::Walking)
                    resume_air_command(object);
            }
        }
        else if (dt > 0.f)
        {
            // Compare progress to this frame's intended step, not a fixed millimetre.
            const float moved_x = object.Position().x - previous.x;
            const float moved_z = object.Position().z - previous.z;
            const float requested_step = std::min(sqrtf(dx * dx + dz * dz), m_walk_speed * dt);
            const float minimum_progress = requested_step * .1f;
            if (moved_x * moved_x + moved_z * moved_z >= minimum_progress * minimum_progress)
            {
                m_stuck_time = 0.f;
                return;
            }
            m_stuck_time += dt;
            if (m_stuck_time < .5f)
                return;
            { if (m_debug) Msg("! [flying] ground path blocked: id=%u next=(%.3f, %.3f, %.3f)",
                u32(object.ID()), target.x, target.y, target.z); }
            m_status = EStatus::GroundBlocked;
            m_velocity.set(0,0,0);
            if (m_fallback_stage == EFallback::Walking)
                resume_air_command(object);
            else
            {
                if (m_strategy == EStrategy::Auto)
                {
                    { if (m_debug) Msg("[flying] auto switches ground to air: id=%u ground step blocked", u32(object.ID())); }
                    const Fvector target = m_destination;
                    fly_to(object, target, true);
                }
            }
        }
        return;
    }
    if (m_status == EStatus::Failed)
        release_landing();
    if (!BudgetedFlight)
    {
        separate(object, dt);
    }
    if (m_status == EStatus::Planning)
    {
        // Queue wait is not search time: only a granted expansion consumes the timeout.
        const bool NearPerch = m_land && object.Position().distance_to_sqr(m_approach) <= _sqr(m_landing_height * 2.f);
        const float PlanningTimeout = NearPerch ? std::min(m_progress_timeout, 1.f) : m_progress_timeout;
        if (m_planning_time>PlanningTimeout)
        {
            if (m_debug) Msg("! [flying] planning timeout: id=%u time=%.2f",u32(object.ID()),m_planning_time);
            if (retarget_landing(object))
            {
                m_planning_time = 0.f;
                return;
            }
            m_search.reset(); m_velocity.set(0,0,0);
            if (!try_ground_fallback(object)) { m_status=EStatus::Failed; release_landing(); }
            return;
        }
        search_step(object);
        return;
    }
    if (m_status != EStatus::Flying && m_status != EStatus::Landing)
        return;
    if (!BudgetedFlight && m_land && m_corridor_check_timer <= dt && !landing_space_free(object, m_destination))
    {
        if (!retarget_landing(object))
        {
            m_velocity.set(0,0,0);
            m_status = EStatus::Failed;
            release_landing();
        }
        return;
    }
    if (m_status != EStatus::Landing && (m_path.empty() || m_path_index >= m_path.size()))
    {
        // A flying status without a waypoint must reach species recovery,
        // rather than staying active forever with zero translation.
        if (BudgetedFlight)
        {
            m_status = EStatus::Failed;
            m_velocity.set(0,0,0);
        }
        return;
    }
    if (!ValidateMovement(object, dt))
    {
        return;
    }
    const Fvector target = m_status == EStatus::Landing ? m_destination : m_path[m_path_index];
    const float waypoint_distance=object.Position().distance_to(target);
    if (!m_progress_valid || target.distance_to_sqr(m_progress_target)>.0001f)
    {
		if (BudgetedFlight && CruiseContinuity && !m_land && !m_taking_off && !DepartureLiftPending &&
			object.Position().distance_to_sqr(target) > .25f)
		{
			CruiseSegmentStart = object.Position();
			CruiseSegmentValid = true;
		}
        m_progress_valid=true; m_progress_target=target;
        m_best_waypoint_distance=waypoint_distance; m_progress_time=0.f;
    }
    else if (waypoint_distance<m_best_waypoint_distance-.01f)
    {
        m_best_waypoint_distance=waypoint_distance; m_progress_time=0.f;
    }
    else m_progress_time+=dt;
    const bool FinalApproach = m_land && (m_status == EStatus::Landing ||
        (!DepartureLiftPending && m_path_index + 1 == m_path.size() && waypoint_distance <= m_landing_height * 2.f));
    const float ProgressTimeout = FinalApproach ? std::min(m_progress_timeout, 1.f) : m_progress_timeout;
    if (!PendingCommand && m_progress_time>ProgressTimeout)
    {
        if (m_debug) Msg("! [flying] no route progress: id=%u distance=%.2f target=(%.2f, %.2f, %.2f)",
            u32(object.ID()),waypoint_distance,target.x,target.y,target.z);
        // Translation caused by separation or circling is not route progress.
        if (retarget_landing(object))
        {
            return;
        }
        m_velocity.set(0,0,0);
        if (!try_ground_fallback(object)) { m_status=EStatus::Failed; release_landing(); }
        return;
    }
    // Detect new obstacles before moving, and rebuild rather than abandoning the route.
    m_corridor_check_timer-=dt;
    const bool check_corridor=!BudgetedFlight && m_corridor_check_timer<=0.f;
    if (check_corridor) m_corridor_check_timer=m_corridor_check_interval;
    // Opt-in species validate through the shared work queue above. Other flyers
    // retain the original detailed movement sweeps and corridor checks.
    if (check_corridor && !clear_path(object, object.Position(), target))
    {
        if (m_status == EStatus::Landing || ++m_replans > 3)
        {
            if (retarget_landing(object))
                return;
            m_velocity.set(0,0,0);
            if (m_status == EStatus::Landing || !try_ground_fallback(object))
                m_status = EStatus::Failed;
        }
        else
            begin_search(object);
        return;
    }
    if (m_status == EStatus::Landing && check_corridor)
    {
        Fvector probe = m_destination, contact, normal;
        probe.y += .5f;
        if (!support(object, probe, m_ground_offset + .8f, contact, normal) ||
            std::abs(contact.y + m_ground_offset - m_destination.y) > .1f)
        {
            if (retarget_landing(object))
                return;
            m_velocity.set(0,0,0);
            m_status = EStatus::Failed;
            return;
        }
    }
    const bool NearContact = m_status == EStatus::Landing && waypoint_distance <= std::max(.16f, m_speed * m_speed_scale * m_landing_speed_factor * dt + .08f);
    if (BudgetedFlight && NearContact &&
        !CFlyingWorkBudget::TryAcquire(this, CFlyingWorkBudget::EKind::LandingProbe, LandingProbesPerFrame))
    {
        LandingProbeWaiting = true;
        m_progress_time = m_stuck_time = 0.f;
        return;
    }
    LandingProbeWaiting = false;
    const Fvector previous = object.Position();
    if (move(object, target, dt))
    {
        m_stuck_time = 0.f;
        if (BudgetedFlight && m_status != EStatus::Landing)
        {
            // A checked polyline vertex is a new anchor. Never sweep a diagonal
            // across two different legs when a coarse validation grant arrives.
            SafePosition = object.Position();
            SafePositionValid = true;
        }
        if (m_status == EStatus::Landing)
        {
            Fvector probe = m_destination, contact, normal;
            probe.y += .5f;
            if (!support(object, probe, m_ground_offset + .8f, contact, normal) ||
                std::abs(contact.y + m_ground_offset - m_destination.y) > .1f ||
                !landing_footprint(object, contact, normal))
            {
                if (retarget_landing(object))
                    return;
                m_velocity.set(0,0,0);
                m_status = EStatus::Failed;
                return;
            }
            Fvector Feet = contact;
            Feet.y += m_ground_offset;
            if (!landing_space_free(object, Feet) || !clear_path(object, SafePositionValid && BudgetedFlight ? SafePosition : previous, Feet, true))
            {
                if (retarget_landing(object)) return;
                m_velocity.set(0,0,0);
                m_status = EStatus::Failed;
                release_landing();
                return;
            }
            m_destination = Feet;
            object.Position().set(Feet);
            m_velocity.set(0,0,0);
            m_yaw_rate = 0.f;
            m_status = EStatus::Landed;
            CachedDepartureValid = BudgetedFlight;
            CachedDepartureRoot = Feet;
            CachedDepartureAir = m_approach;
            Fvector position = object.Position();
            float heading, pitch, bank;
            object.XFORM().getHPB(heading, pitch, bank);
            object.XFORM().setHPB(heading, 0.f, 0.f);
            object.Position().set(position);
            if (m_fallback_stage == EFallback::Descending)
            {
                // walk_to clears the old command; retain only the one pending air retry.
                const Fvector resume = m_resume_destination;
                const bool land = m_resume_landing;
                const bool walking = walk_to(object, m_ground_destination, m_walk_speed);
                m_resume_destination = resume;
                m_resume_landing = land;
                m_fallback_used = true;
                if (walking)
                    m_fallback_stage = EFallback::Walking;
                else
                    resume_air_command(object);
            }
        }
        else if (++m_path_index == m_path.size())
        {
            if (DepartureLiftPending)
            {
                // Keep the original command and plan its remaining route from
                // the collision-checked airborne position, not the foot point.
                DepartureLiftPending = false;
                m_taking_off = false;
                const Fvector DepartureVelocity = m_velocity;
                if (PendingCommand)
                {
                    m_status = EStatus::Arrived;
                    return;
                }
                begin_search(object);
                if (m_status == EStatus::Flying)
                {
                    m_velocity = DepartureVelocity;
                }
                return;
            }
            // Continue into the checked descent without stopping at the point
            // above the perch. A command ending in the air still stops normally.
			if (!m_land && ActivateNextCruise(object))
			{
				return;
			}
			if (!m_land && BudgetedFlight && CruiseContinuity && !m_taking_off && !CommandPending())
			{
				m_status = EStatus::Arrived;
				if (ResumeCachedCruise(object))
				{
					return;
				}
			}
            if (!m_land)
                m_velocity.set(0,0,0);
            m_status = m_land ? EStatus::Landing : EStatus::Arrived;
        }
        return;
    }
    m_stuck_time = !m_turning && previous.distance_to_sqr(object.Position()) < .000001f ?
        m_stuck_time + dt : 0.f;
    if (m_stuck_time > 1.f)
    {
        // A blocked final descent cannot be repaired by flying back to its
        // approach point. Abandon the perch instead of resetting progress forever.
        if (m_status == EStatus::Landing)
        {
            if (retarget_landing(object)) return;
            m_velocity.set(0,0,0);
            m_status = EStatus::Failed;
            release_landing();
            return;
        }
        if (++m_replans > 3)
        {
            if (retarget_landing(object))
                return;
            m_velocity.set(0,0,0);
            if (!try_ground_fallback(object))
                m_status = EStatus::Failed;
        }
        else
            begin_search(object);
    }
}

bool CFlyingMovementController::walk_to(CGameObject& object, const Fvector& target, float speed)
{
    if (!BudgetedFlight)
    {
        return StartGroundCommand(object, target, speed);
    }
    if (m_strategy == EStrategy::AirOnly || !grounded())
    {
        return m_strategy != EStrategy::GroundOnly && fly_to(object, target, true);
    }
    if (!_valid(target) || !_valid(speed) || speed <= 0.f || object.Position().distance_to(target) > m_max_distance)
    {
        return false;
    }
    ++CommandVersion;
    PreparationFailed = false;
    PendingTarget = PendingRoot = target;
    PendingRoot.y += m_ground_offset;
    PendingLanding = false;
    PendingGround = PendingCommand = true;
    PendingPreserve = true;
    PendingWalkSpeed = speed;
    LandingRequestValid = false;
    return true;
}

bool CFlyingMovementController::StartGroundCommand(CGameObject& object, const Fvector& target, float speed)
{
    if (m_strategy == EStrategy::AirOnly)
        return fly_to(object, target, true);
    if (!grounded())
    {
        { if (m_debug) Msg("! [flying] ground command rejected: id=%u no ground contact", u32(object.ID())); }
        return false;
    }
    if (!_valid(target) || !_valid(speed) || speed <= 0.f ||
        object.Position().distance_to(target) > m_max_distance)
        return false;
    const bool allow_air = m_strategy == EStrategy::Auto && m_fallback_stage != EFallback::Descending;
    auto& graph = ai().level_graph();
    Fvector contact = target;
    // The graph supplies connectivity; each actual step also tests world geometry.
    if (!graph.valid_vertex_position(contact))
        return allow_air && fly_to(object, target, true);
    const u32 target_node = graph.vertex_id(contact);
    if (!graph.valid_vertex_id(target_node) || !graph.is_accessible(target_node))
        return allow_air && fly_to(object, target, true);
    const float ground_y = graph.vertex_plane_y(target_node, contact.x, contact.z);
    if (std::abs(ground_y - target.y) > .5f)
        return allow_air && fly_to(object, target, true);
    contact.y = ground_y;
    float air_length = 0.f;
    const bool air_available = allow_air && direct_flight(object, contact, true, air_length);
    xr_vector<u32> vertices;
    bool ground_available = false;
    Fvector start_contact = object.Position();
    start_contact.y -= m_ground_offset;
    if (graph.valid_vertex_position(start_contact) && graph.valid_vertex_position(contact))
    {
        const u32 start = graph.vertex_id(start_contact), finish = graph.vertex_id(contact);
        if (graph.valid_vertex_id(start) && graph.valid_vertex_id(finish) &&
            graph.is_accessible(start) && graph.is_accessible(finish) &&
            graph.vertex_position(start).distance_to_sqr(start_contact) <= 4.f &&
            graph.vertex_position(finish).distance_to_sqr(contact) <= 4.f)
            ground_available = graph.Search(start, finish, vertices, m_max_distance * 2.f, 8192, 8192);
    }
    if (!ground_available)
    {
        if (!allow_air)
            { if (m_debug) Msg("! [flying] ground command rejected: id=%u no AI path to (%.3f, %.3f, %.3f)",
                u32(object.ID()), contact.x, contact.y, contact.z); }
        else
            { if (m_debug) Msg("[flying] auto selects air: id=%u no local ground path distance=%.2f",
                u32(object.ID()), object.Position().distance_to(contact)); }
        return allow_air && fly_to(object, contact, true);
    }
    float ground_length = 0.f;
    Fvector previous = object.Position();
    for (size_t i = 1; i < vertices.size(); ++i)
    {
        const Fvector step = graph.vertex_position(vertices[i]);
        ground_length += previous.distance_to(step);
        previous = step;
    }
    ground_length += previous.distance_to(contact);
    // Hysteresis avoids switching modes for a tiny path-length advantage.
    if (air_available && ground_length > 2.5f && ground_length > air_length * 1.1f + 1.f)
    {
        { if (m_debug) Msg("[flying] auto selects air: id=%u shorter route ground=%.2f air=%.2f",
            u32(object.ID()), ground_length, air_length); }
        return fly_to(object, contact, true);
    }
    if (m_strategy == EStrategy::Auto && allow_air)
        { if (m_debug) Msg("[flying] auto selects ground: id=%u length=%.2f direct_air=%s air_length=%.2f",
            u32(object.ID()), ground_length, air_available ? "yes" : "no", air_length); }
    stop();
    m_destination = contact;
    m_destination.y += m_ground_offset;
    m_walk_vertex = vertices.empty() ? graph.vertex_id(start_contact) : vertices.front();
    for (size_t i = 1; i < vertices.size(); ++i)
    {
        Fvector step = graph.vertex_position(vertices[i]);
        step.y += m_ground_offset;
        m_path.push_back(step);
    }
    m_path.push_back(m_destination);
    m_walk_speed = std::min(speed, 2.f);
    m_land = false;
    m_status = EStatus::Walking;
    return true;
}

bool CFlyingMovementController::walk(CGameObject& object, const Fvector& target,
    float dt, float speed)
{
    Fvector delta;
    delta.sub(target, object.Position());
    delta.y = 0.f;
    const float distance = delta.magnitude();
    if (distance < EPS_L)
        return true;
    delta.div(distance);
    float next_phase = m_hop_phase;
    if (m_ground_movement == EGroundMovement::Hop)
    {
        speed *= HopSpeedScale;
    }
    float stride = speed * dt;
    if (m_ground_movement == EGroundMovement::Hop && m_hop_height > 0.f)
    {
        const float phase = m_hop_phase + std::max(0.f, dt) * speed / m_hop_length;
        const float cycles = floorf(phase);
        next_phase = phase - cycles;
        // Zero horizontal speed on contact; maximum speed at the hop apex.
        // Integrating the pulse keeps the requested average movement speed.
        stride = m_hop_length * (cycles + .5f *
            (cosf(PI * m_hop_phase) - cosf(PI * next_phase)));
    }
    Fvector next;
    next.mad(object.Position(), delta, std::min(distance, stride));
    // Graph connectivity is two-dimensional. Terrain and ragdolls still block the body.
    auto& graph = ai().level_graph();
    Fvector ground = object.Position();
    ground.y -= m_ground_offset;
    if (!graph.valid_vertex_position(ground))
        return false;
    const u32 current = graph.valid_vertex_id(m_walk_vertex) ? m_walk_vertex : graph.vertex_id(ground);
    if (!graph.valid_vertex_id(current) || !graph.valid_vertex_position(next))
        return false;
    Fvector query = next;
    query.y -= m_ground_offset;
    const u32 node = graph.inside(current, query) ? current : graph.vertex(current, query);
    if (!graph.valid_vertex_id(node) || !graph.is_accessible(node) || !graph.inside(node, query))
        return false;
    next.y = graph.vertex_plane_y(node, next.x, next.z) + m_ground_offset;
    Fvector Probe = next, Contact, Normal;
    Probe.y += .3f;
    if (!support(object, Probe, m_ground_offset + .6f, Contact, Normal) ||
        std::abs(Contact.y + m_ground_offset - next.y) > .15f) return false;
    next.y = Contact.y + m_ground_offset;
    const Fmatrix PreviousTransform = object.XFORM();
    float heading, pitch, bank;
    object.XFORM().getHPB(heading, pitch, bank);
    const float turn = angle_difference_signed(delta.getH(), heading);
    heading += std::max(-m_turn_speed * dt, std::min(m_turn_speed * dt, turn));
    object.XFORM().setHPB(heading, 0.f, 0.f);
    object.Position().set(PreviousTransform.c);
    if (!clear_path(object, object.Position(), next, true))
    {
        object.XFORM() = PreviousTransform;
        return false;
    }
    m_walk_vertex = node;
    object.Position().set(next);
    m_hop_phase = next_phase;
    if (m_ground_movement == EGroundMovement::Hop && m_hop_height > 0.f)
    {
        const float dx = m_destination.x - next.x, dz = m_destination.z - next.z;
        const float arrival_blend = std::min(1.f, sqrtf(dx * dx + dz * dz) / .1f);
        const float Lift = sinf(PI * m_hop_phase) * arrival_blend;
        m_hop_lift = m_hop_height * Lift;
    }
    m_velocity.mul(delta, dt > EPS_L ? std::min(distance, stride) / dt : 0.f);
    return distance < .1f;
}

bool CFlyingMovementController::MoveBudgeted(CGameObject& Object, const Fvector& Target, float Dt)
{
	m_turning = false;
	Fvector Direction;
	Direction.sub(Target, Object.Position());
	const float Distance = Direction.magnitude();
	if (Distance < .08f)
	{
		Object.Position().set(Target);
		return true;
	}
	Direction.div(Distance);
	if (m_taking_off && !DepartureLiftPending && Object.Position().distance_to_sqr(m_departure_origin) >= _sqr(m_landing_height))
	{
		m_taking_off = false;
	}
	const float PhaseSpeed = m_status == EStatus::Landing ? m_landing_speed_factor : m_taking_off ? m_takeoff_speed_factor : 1.f;
	const float Limit = m_speed * m_speed_scale * PhaseSpeed;
	const float BrakeDistance = Limit * Limit / (.98f * m_acceleration);
	const bool ThroughPoint = CruiseContinuity && !m_land &&
		(BudgetedFlight || m_path_index + 1 < m_path.size() || HasNextCruise());
	const float DesiredSpeed = ThroughPoint || Distance >= BrakeDistance ? Limit : std::min(Limit, std::max(.15f, sqrtf(2.f * m_acceleration * Distance) * .7f));
	const float PreviousSpeed = m_velocity.magnitude();
	const float Speed = PreviousSpeed + std::max(-m_acceleration * Dt, std::min(m_acceleration * Dt, DesiredSpeed - PreviousSpeed));
	m_velocity.mul(Direction, Speed);
	const bool Arrived = Speed * Dt >= Distance - .08f;
	Fvector Position;
	if (Arrived)
	{
		Position = Target;
	}
	else
	{
		Position.mad(Object.Position(), Direction, Speed * Dt);
	}
	FlightPoseTick += Dt;
	if (!FlightPoseValid)
	{
		Object.XFORM().getHPB(FlightHeading, FlightPitch, FlightBank);
		FlightPoseValid = true;
		FlightPoseTick = std::max(.05f, FlightPoseTick);
	}
	if (FlightPoseTick >= .05f)
	{
		const float Step = FlightPoseTick;
		FlightPoseTick = 0.f;
		const float HorizontalSq = Direction.x * Direction.x + Direction.z * Direction.z;
		const float Turn = HorizontalSq > EPS_L ? angle_difference_signed(Direction.getH(), FlightHeading) : 0.f;
		const float RateLimit = std::min(m_turn_speed, sqrtf(2.f * m_turn_acceleration * std::abs(Turn)));
		const float DesiredRate = std::max(-RateLimit, std::min(RateLimit, Turn * m_orientation_response));
		m_yaw_rate += std::max(-m_turn_acceleration * Step, std::min(m_turn_acceleration * Step, DesiredRate - m_yaw_rate));
		FlightHeading += std::max(-std::abs(Turn), std::min(std::abs(Turn), m_yaw_rate * Step));
		const bool Upright = m_taking_off || DepartureLiftPending || m_status == EStatus::Landing;
		const float Blend = 1.f - expf(-m_orientation_response * Step);
		const float TargetPitch = Upright ? 0.f : asinf(std::max(-1.f, std::min(1.f, Direction.y))) * .5f;
		const float TargetBank = Upright ? 0.f : std::max(-.5f, std::min(.5f, -m_yaw_rate * .25f));
		FlightPitch += (TargetPitch - FlightPitch) * Blend;
		FlightBank += (TargetBank - FlightBank) * Blend;
		Object.XFORM().setHPB(FlightHeading, FlightPitch, FlightBank);
	}
	Object.Position().set(Position);
	return Arrived;
}

Fbox CFlyingMovementController::FlightBounds() const
{
    Fbox Bounds = Level().ObjectSpace.GetBoundingVolume();
    if (BudgetedFlight && CruiseContinuity)
    {
        // Keep a horizontal buffer; geometry bounds are not an air ceiling.
        const float MarginX = std::min(BoundaryMargin, (Bounds.max.x - Bounds.min.x) * .25f);
        const float MarginZ = std::min(BoundaryMargin, (Bounds.max.z - Bounds.min.z) * .25f);
        Bounds.min.x += MarginX;
        Bounds.max.x -= MarginX;
        Bounds.min.z += MarginZ;
        Bounds.max.z -= MarginZ;
    }
    return Bounds;
}

bool CFlyingMovementController::InsideFlightBounds(const Fvector& Point) const
{
    const Fbox Bounds = FlightBounds();
    return Point.x >= Bounds.min.x && Point.x <= Bounds.max.x &&
        Point.z >= Bounds.min.z && Point.z <= Bounds.max.z;
}

// Native crow entry/resumption cannot depend on the number of queued probes.
// This provisional air leg uses the existing coarse validation quota. It adds
// no spawn-time hull sweeps, spatial queries or path searches.
bool CFlyingMovementController::BeginAmbientCruise(CGameObject& Object, const Fvector& Heading, float Distance)
{
	if (!BudgetedFlight || !CruiseContinuity || active() || grounded() ||
		(m_status != EStatus::Idle && m_status != EStatus::Arrived) ||
		!_valid(Heading) || !_valid(Distance) || Distance <= 0.f)
	{
		return false;
	}
	Fvector Origin = Object.Position();
	const Fbox Bounds = FlightBounds();
	if (!_valid(Origin))
	{
		return false;
	}
	if (!InsideFlightBounds(Origin))
	{
		// Old saves/online placement may already be outside the buffer. Re-enter
		// above terrain instead of clamping a low root into a mountain slope.
		clamp(Origin.x, Bounds.min.x, Bounds.max.x);
		clamp(Origin.z, Bounds.min.z, Bounds.max.z);
		Origin.y = std::max(Origin.y, Bounds.max.y + m_landing_height);
		Object.Position().set(Origin);
		SafePosition = Origin;
		SafePositionValid = true;
	}
	Fvector Direction = Heading;
	Direction.y = 0.f;
	if (Direction.square_magnitude() < EPS_L)
	{
		Direction.setHP(float(Object.ID()) * 2.39996323f, 0.f);
	}
	Direction.normalize_safe();
	Distance = std::min(Distance, m_max_distance * .8f);
	// Reflect only the outward component before reaching the buffer edge.
	if (Origin.x + Direction.x * Distance < Bounds.min.x || Origin.x + Direction.x * Distance > Bounds.max.x)
	{
		Direction.x = -Direction.x;
	}
	if (Origin.z + Direction.z * Distance < Bounds.min.z || Origin.z + Direction.z * Distance > Bounds.max.z)
	{
		Direction.z = -Direction.z;
	}
	const auto GoalLambda = [&](const Fvector& Along)
	{
		Fvector Goal;
		Goal.mad(Origin, Along, Distance);
		clamp(Goal.x, Bounds.min.x, Bounds.max.x);
		clamp(Goal.z, Bounds.min.z, Bounds.max.z);
		return Goal;
	};
	Fvector Goal = GoalLambda(Direction);
	if (Goal.distance_to_sqr(Origin) < .25f)
	{
		// Turn inward at the level boundary instead of installing a zero leg.
		Direction.mul(-1.f);
		Goal = GoalLambda(Direction);
	}
	if (Goal.distance_to_sqr(Origin) < .25f)
	{
		return false;
	}
	const Fvector PreviousVelocity = m_velocity;
	const float PreviousYawRate = m_yaw_rate;
	const bool PreviousPoseValid = FlightPoseValid;
	const float PreviousPoseTick = FlightPoseTick;
	stop();
	m_strategy = EStrategy::AirOnly;
	m_land = false;
	AmbientLeg = true;
	m_destination = m_approach = Goal;
	m_path.push_back(Goal);
	Direction.sub(Goal, Origin).normalize_safe();
	if (PreviousVelocity.square_magnitude() > EPS_L)
	{
		m_velocity = PreviousVelocity;
	}
	else
	{
		m_velocity.mul(Direction, m_speed * m_speed_scale);
	}
	m_yaw_rate = PreviousYawRate;
	FlightPoseValid = PreviousPoseValid;
	FlightPoseTick = PreviousPoseTick;
	CruiseSegmentStart = Origin;
	CruiseSegmentValid = true;
	if (!SafePositionValid)
	{
		// Online placement is the initial safe root. Later resumptions retain
		// the last validation result instead of blessing an unchecked endpoint.
		SafePosition = Origin;
		SafePositionValid = true;
	}
	MovementCheckTimer = MovementCheckInterval * float(u32(Object.ID()) * 137u % 1024u) / 1024.f;
	m_status = EStatus::Flying;
	return true;
}

// Autonomous birds can resume the unfinished forward part of a cached route.
// Script commands, final descent and cached takeoff retain their stop semantics.
bool CFlyingMovementController::ResumeCachedCruise(CGameObject& Object)
{
	if (!BudgetedFlight || !CruiseContinuity || !CruiseSegmentValid || m_land ||
		m_taking_off || DepartureLiftPending)
	{
		return false;
	}
	if (!m_path.empty() && m_path_index < m_path.size())
	{
		m_status = EStatus::Flying;
		return true;
	}
	if (CommandPending())
	{
		return false;
	}
	// An exhausted leg must continue forwards, never return to its start.
	// No geometry work is needed to generate this coarse native continuation.
	Fvector Direction = m_velocity;
	if (Direction.square_magnitude() < EPS_L)
	{
		Direction = Object.XFORM().k;
		if (!m_path.empty())
		{
			Direction.sub(m_path.back(), CruiseSegmentStart);
		}
	}
	m_status = EStatus::Arrived;
	return BeginAmbientCruise(Object, Direction, CruiseLegDistance);
}

bool CFlyingMovementController::NeedsNextCruise(const Fvector& Position, float Progress) const
{
	if (!BudgetedFlight || !CruiseContinuity || !CruiseSegmentValid || HasNextCruise() ||
		CommandPending() || m_land || m_taking_off || DepartureLiftPending ||
		m_status != EStatus::Flying)
	{
		return false;
	}
	if (AmbientLeg)
	{
		// New online birds already have a cheap air path. Request their normal
		// successor immediately instead of waiting for 90% of this first leg.
		return true;
	}
	if (m_path_index + 1 != m_path.size())
	{
		return false;
	}
	const float Remaining = std::max(.01f, 1.f - Progress);
	const float ProgressDistanceSq = CruiseSegmentStart.distance_to_sqr(m_path.back()) * Remaining * Remaining;
	// A thousand birds cannot all prepare their successors inside the last 10%
	// of a short leg. Start earlier when the shared queue needs more lead time.
	const float QueueLeadTime = .2f + float(CFlyingWorkBudget::EstimatedWaitFrames(CFlyingWorkBudget::EKind::BehaviorProbe)) *
		std::min(.1f, std::max(.001f, Device.fTimeDelta));
	const float QueueDistanceSq = m_velocity.square_magnitude() * QueueLeadTime * QueueLeadTime;
	return Position.distance_to_sqr(m_path.back()) <= std::max(ProgressDistanceSq, QueueDistanceSq);
}

bool CFlyingMovementController::PrepareNextCruise(CGameObject& Object, const Fvector& Destination)
{
	if (!BudgetedFlight || !CruiseContinuity || HasNextCruise() || CommandPending() ||
		m_land || m_taking_off || DepartureLiftPending || m_path.empty() ||
		!_valid(Destination) || !InsideFlightBounds(Destination) || CruiseEndpoint().distance_to_sqr(Destination) < .25f ||
		CruiseEndpoint().distance_to_sqr(Destination) > _sqr(m_max_distance))
	{
		return false;
	}
	const Fvector Origin = CruiseEndpoint();
	// Called under the crow's shared geometry budget; no second A* planner.
	if (!clear_path(Object, Origin, Destination))
	{
		return false;
	}
	NextCruise.Origin = Origin;
	NextCruise.Destination = Destination;
	NextCruise.Path.push_back(Destination);
	return true;
}

bool CFlyingMovementController::ActivateNextCruise(CGameObject& Object)
{
	if (!CruiseContinuity || !HasNextCruise() || m_land || CommandPending() ||
		Object.Position().distance_to_sqr(NextCruise.Origin) > .01f)
	{
		return false;
	}
	m_path.swap(NextCruise.Path);
	m_path_index = 0;
	m_destination = m_approach = NextCruise.Destination;
	AmbientLeg = false;
	NextCruise.Clear();
	m_progress_valid = false;
	m_progress_time = m_stuck_time = 0.f;
	m_status = EStatus::Flying;
	SafePosition = Object.Position();
	SafePositionValid = true;
	// Keep velocity, yaw rate and interpolated pose across the route seam.
	return true;
}
