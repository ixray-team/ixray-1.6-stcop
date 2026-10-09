#include "StdAfx.h"
#include "ScavengerCrow.h"
#include "crow_behavior_timing.h"
#include "CrowSharedMemory.h"
#include "FlyingWorkBudget.h"
#include "../../../../xrServerEntities/xrServer_Objects_ALife_ScavengerCrow.h"
#include "../../../Level.h"
#include "../../../entity_alive.h"
#include "../../../Actor.h"
#include "../../../ai_space.h"
#include "../../../alife_simulator.h"
#include "../../../alife_object_registry.h"
#include "../../../alife_time_manager.h"
#include "../../../game_graph.h"
#include "../../../level_graph.h"
#include "../../../GamePersistent.h"
#include "../../../../xrEngine/Environment.h"
#include "../../../../xrSound/ai_sounds.h"
#include "../../../../xrCore/Save/SaveInterface.h"

namespace
{
using CrowBehaviorTiming::SCrowRange;
using CrowBehaviorTiming::SCrowReaction;
bool CrowFoodContact(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& Beak, Fvector& Point)
{
    return CCrowSharedMemory::FoodContact(Bird, Corpse, Beak, Point);
}

// Tree visuals often have no leaf collision triangles. Use their engine-owned
// transforms to guide procedural canopy shelter, never as a landing surface.
class SCrowTreeIndex
{
    xr_hash_map<u64, xr_vector<Fvector>> Cells;
    const void* LevelOwner = nullptr;
    size_t TreeCount = 0;
    u32 RefreshTime = 0;

    static u64 Key(int X, int Z)
    {
        return (u64(u32(X)) << 32) | u32(Z);
    }

public:
    template <typename TVisitor>
    void Visit(const Fvector& Point, float Radius, TVisitor Visitor)
    {
        const size_t Count = Device.m_trees_poses_pm.size() + Device.m_trees_poses_st.size();
        if (LevelOwner != g_pGameLevel || TreeCount != Count || Device.dwTimeGlobal - RefreshTime >= 5000u)
        {
            Cells.clear();
            auto AddTreesLambda = [&](const xr_vector<Fmatrix*>& Trees)
            {
                for (const auto* Transform : Trees)
                {
                    if (Transform && _valid(Transform->c))
                    {
                        const Fvector Position = Transform->c;
                        Cells[Key(int(floorf(Position.x / 32.f)), int(floorf(Position.z / 32.f)))].push_back(Position);
                    }
                }
            };
            AddTreesLambda(Device.m_trees_poses_pm);
            AddTreesLambda(Device.m_trees_poses_st);
            LevelOwner = g_pGameLevel;
            TreeCount = Count;
            RefreshTime = Device.dwTimeGlobal;
        }
        for (int X = int(floorf((Point.x - Radius) / 32.f)); X <= int(floorf((Point.x + Radius) / 32.f)); ++X)
        {
            for (int Z = int(floorf((Point.z - Radius) / 32.f)); Z <= int(floorf((Point.z + Radius) / 32.f)); ++Z)
            {
                const auto Cell = Cells.find(Key(X, Z));
                if (Cell == Cells.end())
                {
                    continue;
                }
                for (const Fvector& Tree : Cell->second)
                {
                    if (_sqr(Tree.x - Point.x) + _sqr(Tree.z - Point.z) <= _sqr(Radius))
                    {
                        Visitor(Tree);
                    }
                }
            }
        }
    }
};
SCrowTreeIndex g_CrowTrees;
}

struct CScavengerCrow::SBehavior
{
    enum class Phase : u8 { Sky, GroundApproach, RoofApproach, CorpseApproach, Ground, Roof, Corpse, Escape, ShelterApproach, Shelter, ShelterSearch, NightApproach, NightRest, NightGround, CorpseCircle };
    bool enabled = false, loaded = false, home_valid = false, debug = false, navigation_failure_handled = false;
    Phase phase = Phase::Sky;
    Phase PreviousTickPhase = Phase::Sky;
    CFlyingMovementController::EStatus PreviousTickStatus = CFlyingMovementController::EStatus::Idle;
    float BehaviorTick = 0.f;
    Fvector home = {0,0,0}, danger = {0,0,0};
    float satiety = .8f, remaining = 0.f, scan_timer = 0.f, retry_timer = 0.f;
    float safe_timer = 0.f, noise_timer = 0.f, peck_timer = 0.f, peck_phase = -1.f;
    float step_timer = 0.f, food_search_timer = 0.f;
    u32 corpse_id = u32(-1);
    SCrowRange sky, ground, roof, corpse, escape, safe, retry, peck_interval, initial_satiety, step, start_delay, corpse_peck, sated_peck, takeoff_height;
    float takeoff_spread = 8.f;
    SCrowRange speed_variation;
    SCrowRange escape_retry;
    float perception = .5f, peck_duration = .28f;
    float hunger_time = 900.f, hungry = .35f, full = .9f, seed_gain = .012f, meat_gain = .07f;
    float roam_radius = 40.f, sky_low = 12.f, sky_high = 25.f, sky_leg_distance = 40.f;
    float preferred_height = 20.f, height_variation = 8.f, vertical_step = 12.f;
    float landing_radius = 25.f, roof_min = 3.f, roof_max = 35.f, roof_slope = 35.f;
    float fear_radius = 10.f, food_safe_radius = 30.f, food_search_radius = 70.f;
    float noise_radius = 400.f, noise_power = .01f, noise_memory = 8.f;
    float escape_distance = 18.f, escape_height = 10.f, ground_chance = .6f;
    float seed_step_radius = .7f, ground_speed = .5f;
    u32 corpse_slots = 3, nav_attempts = 12;
    bool ground_hops = true, mapwide = true, travel_valid = false, departing = false;
    float perch_spacing = 3.f, landing_max_radius = 50.f;
    Fvector travel_target = {0,0,0};
    Fvector previous_position = {0,0,0};
    float corpse_side_distance = .22f, corpse_reach_distance = 1.f;
    bool FoodApproachNeeded = false;
    bool rain_shelter_enabled = true, raining = false, abandoned_shelter_valid = false;
    float rain_start = .1f, rain_stop = .03f, weather_interval = .5f, weather_timer = 0.f;
    float shelter_radius = 65.f, shelter_cover_height = 40.f, shelter_cover_radius = .15f;
    float shelter_clearance = .5f, shelter_relocate_distance = 15.f, goal_advance_distance = 3.f;
    float shelter_timer = 0.f, shelter_memory_timer = 0.f;
    Fvector abandoned_shelter = {0,0,0};
    SCrowRange shelter_retry, shelter_hold, shelter_relocate_delay, shelter_memory, shelter_dry_delay;
    u32 shelter_attempts = 4;
    bool night_enabled = true, night = false;
    bool WeatherRain = false, WeatherNight = false;
    SCrowReaction RainReaction, NightReaction;
    SCrowRange RainDelay, NightDelay, WakeDelay;
    float night_start = 20.f, night_end = 6.f, night_forage_chance = .35f;
    SCrowRange night_rest, night_forage, night_step;
    float fear = 0.f, fear_rise = 1.5f, fear_decay = .2f, fear_trigger = .35f, fear_calm = .1f;
    float fear_shot_gain = 1.f, fear_explosion_gain = 1.5f, shot_radius = 250.f, explosion_radius = 400.f;
    SCrowRange HeightChange, CircleTime, CircleRadius, CircleHeight;
    float HeightTarget = 20.f, HeightTimer = 0.f, HeightSpeed = 2.5f;
    float GoalPrefetchProgress = .9f;
    float GoalLookAhead = .8f, RouteWait = .8f, LandingStallLimit = 1.5f, LandingTotalLimit = 30.f;
    float LandingElapsed = 0.f, FailedPerchTimer = 0.f;
    float RejectedPerchTime = 10.f;
    Fvector FailedPerch = {0,0,0};
    float TreeCoverRadius = 3.f, TreeCoverHeight = 10.f;
    bool TreeShelter = true;
    float CircleDirection = 1.f, CircleDistance = 10.f, CircleAltitude = 6.f;
    float CircleProbability = .8f;
    bool ScanDue = false, HeightGoalDue = false;
    bool EscapeGoalPending = false;
    u32 GeometryBudget = 4, GeometryDepth = 0;
    bool GeometryDeferred = false, FoodSearchPending = false;
    u32 PerchAttempt[2] = {0,0}, ShelterAttempt = 0, RecoveryAttempt = 0;
    u32 SkyAttempt = 0, DepartureAttempt = 0, FoodAttempt = 0, FeedContactCursor = 0;
    bool HaveBlockedSkyGoal = false;
    Fvector BlockedSkyGoal = {0,0,0};

    class CGeometryJob
    {
        SBehavior& Behavior;
        bool Accepted;
    public:
        explicit CGeometryJob(SBehavior& Owner) : Behavior(Owner),
            Accepted(Owner.GeometryDepth != 0 || CFlyingWorkBudget::TryAcquire(&Owner,
                CFlyingWorkBudget::EKind::BehaviorProbe, Owner.GeometryBudget))
        {
            if (Accepted)
            {
                ++Behavior.GeometryDepth;
            }
            else
            {
                Behavior.GeometryDeferred = true;
            }
        }
        ~CGeometryJob()
        {
            if (Accepted)
            {
                --Behavior.GeometryDepth;
            }
        }
        explicit operator bool() const { return Accepted; }
        CGeometryJob(const CGeometryJob&) = delete;
        CGeometryJob& operator=(const CGeometryJob&) = delete;
    };
    ~SBehavior()
    {
        CFlyingWorkBudget::Cancel(this);
    }


    void load(const char* s)
    {
        enabled = READ_IF_EXISTS(pSettings, r_bool, s, "crow_ai_enabled", false);
        debug = READ_IF_EXISTS(pSettings,r_bool,s,"crow_ai_debug",false);
        sky.load(s,"crow_sky_time",20.f,50.f); ground.load(s,"crow_ground_time",6.f,14.f);
        roof.load(s,"crow_roof_time",10.f,30.f); corpse.load(s,"crow_corpse_time",60.f,180.f);
        escape.load(s,"crow_escape_time",12.f,25.f); safe.load(s,"crow_safe_time",20.f,40.f);
        retry.load(s,"crow_retry_time",.15f,.4f); peck_interval.load(s,"crow_peck_interval",.5f,1.1f);
        escape_retry.load(s,"crow_escape_retry_time",.1f,.25f);
        initial_satiety.load(s,"crow_initial_satiety",.25f,.9f);
        start_delay.load(s,"crow_ai_start_delay",.05f,.2f);
        shelter_retry.load(s,"crow_shelter_retry_time",.5f,1.f);
        shelter_hold.load(s,"crow_shelter_check_time",1.f,2.f);
        shelter_relocate_delay.load(s,"crow_shelter_relocate_delay",2.f,5.f);
        shelter_memory.load(s,"crow_shelter_avoid_time",30.f,60.f);
        shelter_dry_delay.load(s,"crow_shelter_dry_delay",5.f,120.f);
        RainDelay.load(s,"crow_rain_reaction_delay",5.f,90.f);
        NightDelay.load(s,"crow_night_reaction_delay",5.f,120.f);
        WakeDelay.load(s,"crow_day_reaction_delay",5.f,120.f);
        night_rest.load(s,"crow_night_rest_time",30.f,90.f);
        night_forage.load(s,"crow_night_forage_time",6.f,14.f);
        night_step.load(s,"crow_night_step_interval",4.f,8.f);
        takeoff_height.load(s,"crow_takeoff_height",8.f,12.f);
        speed_variation.load(s,"crow_flight_speed_scale",.85f,1.15f);
        HeightChange.load(s,"crow_sky_height_change_interval",3.f,6.f);
        CircleTime.load(s,"crow_corpse_circle_time",12.f,30.f);
        CircleRadius.load(s,"crow_corpse_circle_radius",8.f,14.f);
        CircleHeight.load(s,"crow_corpse_circle_height",4.f,10.f);
        corpse_peck.load(s,"crow_corpse_peck_interval",.8f,1.6f);
        sated_peck.load(s,"crow_sated_peck_interval",2.f,4.f);
        step.load(s,"crow_ground_step_interval",2.f,5.f);
#define CROW_FLOAT(field, key, value) field = READ_IF_EXISTS(pSettings, r_float, s, key, value); R_ASSERT3(_valid(field) && field > 0.f, "Invalid crow setting", key)
        CROW_FLOAT(perception,"crow_perception_interval",.25f);
        clamp(perception, .1f, .5f);
        CROW_FLOAT(weather_interval,"crow_weather_interval",.5f);
        CROW_FLOAT(shelter_radius,"crow_shelter_search_radius",65.f);
        CROW_FLOAT(shelter_cover_height,"crow_shelter_cover_height",40.f);
        CROW_FLOAT(shelter_cover_radius,"crow_shelter_cover_radius",.15f);
        CROW_FLOAT(shelter_clearance,"crow_shelter_clearance",.5f);
        CROW_FLOAT(shelter_relocate_distance,"crow_shelter_relocate_distance",15.f);
        CROW_FLOAT(goal_advance_distance,"crow_goal_advance_distance",3.f);
        CROW_FLOAT(GoalLookAhead,"crow_goal_lookahead_time",.8f);
        CROW_FLOAT(GoalPrefetchProgress,"crow_goal_prefetch_progress",.9f);
		clamp(GoalPrefetchProgress, .5f, .99f);
        CROW_FLOAT(RouteWait,"crow_route_wait_limit",.8f);
        CROW_FLOAT(LandingStallLimit,"crow_landing_stall_limit",1.5f);
        CROW_FLOAT(LandingTotalLimit,"crow_landing_total_limit",30.f);
        CROW_FLOAT(RejectedPerchTime,"crow_rejected_perch_time",10.f);
        CROW_FLOAT(HeightSpeed,"crow_sky_height_change_speed",2.5f);
        CROW_FLOAT(TreeCoverRadius,"crow_tree_cover_radius",3.f);
        CROW_FLOAT(TreeCoverHeight,"crow_tree_cover_height",10.f);
        CROW_FLOAT(fear_rise,"crow_fear_rise_speed",1.5f);
        CROW_FLOAT(fear_decay,"crow_fear_decay_speed",.2f);
        CROW_FLOAT(fear_trigger,"crow_fear_trigger_threshold",.35f);
        CROW_FLOAT(fear_calm,"crow_fear_calm_threshold",.1f);
        CROW_FLOAT(fear_shot_gain,"crow_fear_shot_gain",1.f);
        CROW_FLOAT(fear_explosion_gain,"crow_fear_explosion_gain",1.5f);
        CROW_FLOAT(shot_radius,"crow_shot_fear_radius",250.f);
        CROW_FLOAT(explosion_radius,"crow_explosion_fear_radius",400.f);
        CROW_FLOAT(peck_duration,"crow_peck_duration",.28f);
        CROW_FLOAT(hunger_time,"crow_hunger_time",900.f);
        CROW_FLOAT(hungry,"crow_hungry_threshold",.35f);
        CROW_FLOAT(full,"crow_full_threshold",.9f);
        CROW_FLOAT(seed_gain,"crow_seed_gain_per_peck",.002f);
        CROW_FLOAT(meat_gain,"crow_meat_gain_per_peck",.07f);
        CROW_FLOAT(roam_radius,"crow_roam_radius",40.f);
        CROW_FLOAT(takeoff_spread,"crow_takeoff_spread_radius",8.f);
        CROW_FLOAT(sky_leg_distance,"crow_sky_leg_distance",40.f);
        CROW_FLOAT(sky_low,"crow_sky_height_min",12.f);
        CROW_FLOAT(sky_high,"crow_sky_height_max",25.f);
        CROW_FLOAT(vertical_step,"crow_sky_vertical_step",12.f);
        CROW_FLOAT(landing_radius,"crow_perch_search_radius",25.f);
        CROW_FLOAT(landing_max_radius,"crow_perch_search_max_radius",50.f);
        CROW_FLOAT(perch_spacing,"crow_perch_spacing",3.f);
        CROW_FLOAT(roof_min,"crow_roof_height_min",3.f);
        CROW_FLOAT(roof_max,"crow_roof_height_max",35.f);
        CROW_FLOAT(roof_slope,"crow_roof_max_slope",35.f);
        CROW_FLOAT(fear_radius,"crow_fear_radius",10.f);
        CROW_FLOAT(food_safe_radius,"crow_corpse_safe_radius",30.f);
        CROW_FLOAT(food_search_radius,"crow_corpse_search_radius",70.f);
        CROW_FLOAT(noise_radius,"crow_noise_fear_radius",400.f);
        CROW_FLOAT(noise_power,"crow_noise_min_power",.01f);
        CROW_FLOAT(noise_memory,"crow_noise_memory_time",8.f);
        CROW_FLOAT(escape_distance,"crow_escape_distance",18.f);
        CROW_FLOAT(escape_height,"crow_escape_height",10.f);
        CROW_FLOAT(seed_step_radius,"crow_ground_step_radius",.7f);
        CROW_FLOAT(ground_speed,"crow_ground_speed",.5f);
        CROW_FLOAT(corpse_side_distance,"crow_corpse_side_distance",.22f);
        CROW_FLOAT(corpse_reach_distance,"crow_corpse_reach_distance",1.f);
#undef CROW_FLOAT
        height_variation=READ_IF_EXISTS(pSettings,r_float,s,"crow_sky_height_variation",8.f);
        R_ASSERT2(_valid(height_variation) && height_variation>=0.f,"Invalid crow height variation");
        ground_chance = READ_IF_EXISTS(pSettings,r_float,s,"crow_ground_choice_probability",.6f);
        CircleProbability = READ_IF_EXISTS(pSettings, r_float, s, "crow_corpse_circle_probability", .8f);
        R_ASSERT2(_valid(CircleProbability) && CircleProbability >= 0.f && CircleProbability <= 1.f, "Invalid crow circling probability");
        corpse_slots = READ_IF_EXISTS(pSettings,r_u32,s,"crow_corpse_slots",3);
        nav_attempts = READ_IF_EXISTS(pSettings,r_u32,s,"crow_navigation_attempts",12);
        GeometryBudget = READ_IF_EXISTS(pSettings, r_u32, s, "crow_behavior_probes_per_frame", 4u);
        clamp(GeometryBudget, 1u, 64u);
        CFlyingWorkBudget::Configure(CFlyingWorkBudget::EKind::BehaviorProbe, GeometryBudget);
        mapwide = READ_IF_EXISTS(pSettings,r_bool,s,"crow_roam_entire_level",true);
        ground_hops = READ_IF_EXISTS(pSettings,r_bool,s,"crow_ground_hops",true);
        rain_shelter_enabled=READ_IF_EXISTS(pSettings,r_bool,s,"crow_rain_shelter_enabled",true);
        TreeShelter=READ_IF_EXISTS(pSettings,r_bool,s,"crow_tree_shelter_enabled",true);
        night_enabled=READ_IF_EXISTS(pSettings,r_bool,s,"crow_night_enabled",true);
        night_start=READ_IF_EXISTS(pSettings,r_float,s,"crow_night_start_hour",20.f);
        night_end=READ_IF_EXISTS(pSettings,r_float,s,"crow_night_end_hour",6.f);
        night_forage_chance=READ_IF_EXISTS(pSettings,r_float,s,"crow_night_forage_probability",.35f);
        R_ASSERT2(_valid(night_start) && _valid(night_end) && night_start>=0.f && night_start<24.f &&
            night_end>=0.f && night_end<=24.f && night_start!=night_end &&
            _valid(night_forage_chance) && night_forage_chance>=0.f && night_forage_chance<=1.f &&
            fear_calm<fear_trigger && fear_trigger<=1.f,"Invalid crow night/fear configuration");
        rain_start=READ_IF_EXISTS(pSettings,r_float,s,"crow_rain_start_threshold",.1f);
        rain_stop=READ_IF_EXISTS(pSettings,r_float,s,"crow_rain_stop_threshold",.03f);
        shelter_attempts=READ_IF_EXISTS(pSettings,r_u32,s,"crow_shelter_search_attempts",4);
        R_ASSERT2(_valid(rain_start) && _valid(rain_stop) && rain_stop>=0.f && rain_start>rain_stop &&
            rain_start<=1.f && shelter_attempts>0 && shelter_attempts<=16 && shelter_hold.low>0.f &&
            shelter_retry.low>0.f,"Invalid crow shelter configuration");
        R_ASSERT2(hungry < full && full <= 1.f && initial_satiety.high <= 1.f && sky_high >= sky_low &&
            roof_max >= roof_min && roof_slope <= 65.f && ground_chance >= 0.f && ground_chance <= 1.f &&
            speed_variation.low>=.25f && speed_variation.high<=2.f &&
            landing_max_radius >= landing_radius && takeoff_height.low > 0.f && corpse_slots > 0 && nav_attempts > 0 && nav_attempts <= 64,
            "Invalid crow behavior configuration");
        R_ASSERT2(_valid(corpse_side_distance) && corpse_side_distance > 0.f &&
            _valid(corpse_reach_distance) && corpse_reach_distance >= corpse_side_distance, "Invalid crow feeding distance");
        satiety = initial_satiety.sample();
        preferred_height=Random.randF(sky_low,sky_high);
        HeightTarget = preferred_height;
        HeightTimer = HeightChange.sample();
        R_ASSERT2(HeightChange.low > 0.f && CircleTime.low > 0.f && CircleRadius.low >= 2.f &&
            CircleHeight.low > 0.f && LandingTotalLimit > LandingStallLimit,
            "Invalid crow altitude/circling/landing configuration");
    }

    void release_food(const CScavengerCrow& bird)
    {
        CCrowSharedMemory::Get().ReleaseFood(bird.ID());
        corpse_id = u32(-1);
    }
    u32 food_users(u32 id) const
    {
        return CCrowSharedMemory::Get().FoodUsers(id);
    }
    CEntityAlive* food() const
    {
        if (corpse_id == u32(-1)) return nullptr;
        auto* object = Level().Objects.net_Find(corpse_id);
        auto* entity = object ? smart_cast<CEntityAlive*>(object) : nullptr;
        return entity && !entity->getDestroy() && !entity->g_Alive() ? entity : nullptr;
    }
    bool unsafe(CScavengerCrow& bird, const Fvector& point, float radius, Fvector& threat, bool* acoustic = nullptr) const
    {
        if (acoustic) *acoustic=false;
        // Landing candidates must be checked even while the bird is airborne.
        // The direct actor test needs no spatial query.
        // First-person actor rendering is not a reliable perception filter.
        // Detect the actual living actor directly, even when its mesh is hidden.
        auto* actor=Actor();
        if (actor && !actor->getDestroy() && actor->g_Alive() &&
            point.distance_to_sqr(actor->Position())<_sqr(radius))
        { threat=actor->Position(); return true; }
        if (noise_timer > 0.f && point.distance_to_sqr(danger) < _sqr(noise_radius))
        { threat = danger; if (acoustic) *acoustic=true; return true; }
        // Ground predators cannot reach a cruising bird. Only landing/resting
        // tasks request the shared local observation.
        if (!bird.flight_controller().grounded() && !IsApproaching() &&
            phase != Phase::Ground && phase != Phase::Roof && phase != Phase::Corpse &&
            phase != Phase::Shelter && phase != Phase::NightRest && phase != Phase::NightGround)
            return false;
        if (bird.MonsterPeaceful) return false;
        const auto* Observation = CCrowSharedMemory::Get().Observe(point, radius);
        if (Observation)
        {
            for (u32 Id : Observation->Threats)
            {
                auto* Object = Level().Objects.net_Find(Id);
                auto* Entity = Object ? smart_cast<CEntityAlive*>(Object) : nullptr;
                if (Entity && !Entity->getDestroy() && Entity->g_Alive() &&
                    point.distance_to_sqr(Entity->Position()) < _sqr(radius))
                {
                    threat = Entity->Position();
                    return true;
                }
            }
        }
        return false;
    }
    float terrain_y(const Fvector& point) const
    {
        auto& graph = ai().level_graph();
        Fvector probe = point;
        probe.y = home.y;
        if (graph.valid_vertex_position(probe))
        {
            const u32 id = graph.vertex_id(probe);
            if (graph.valid_vertex_id(id))
            {
                const Fvector vertex=graph.vertex_position(id);
                // A distant hill may be far above the spawn altitude. Only
                // horizontal proximity matters for this terrain reference.
                if (_sqr(vertex.x-point.x)+_sqr(vertex.z-point.z)<_sqr(landing_radius))
                    return graph.vertex_plane_y(id,point.x,point.z);
            }
        }
        return home.y;
    }
    float sky_surface_y(CScavengerCrow& bird, const Fvector& point) const
    {
        const Fbox& bounds=Level().ObjectSpace.GetBoundingVolume();
        Fvector from=point;
        from.y=bounds.max.y+sky_high;
        collide::rq_result hit;
        if (Level().ObjectSpace.RayPick(from,Fvector().set(0,-1,0),
            bounds.max.y-bounds.min.y+sky_high,collide::rqtStatic,hit,&bird))
            return from.y-hit.range;
        return terrain_y(point);
    }
    void initialize_home(CScavengerCrow& bird)
    {
        if (home_valid) return;
        home = bird.Position();
        auto& graph = ai().level_graph();
        const u32 id = bird.ai_location().level_vertex_id();
        if (graph.valid_vertex_id(id)) home.y = graph.vertex_position(id).y;
        else
        {
            CGeometryJob GeometryJob(*this);
            if (!GeometryJob)
            {
                return;
            }
            Fvector from = home, contact, normal;
            from.y += .5f;
            if (CFlyingMovementController::support(bird,from,roof_max + sky_high,contact,normal))
                home.y = contact.y;
        }
        home_valid = true;
    }
    void begin_sky(CScavengerCrow& bird)
    {
        release_food(bird);
        phase = Phase::Sky; remaining = sky.sample(); retry_timer = 0.f; peck_phase = -1.f; travel_valid=false;
        FoodApproachNeeded = false;
        departing=false;
        LandingElapsed=0.f;
        EscapeGoalPending=false;
        SkyAttempt = DepartureAttempt = RecoveryAttempt = ShelterAttempt = 0;
        FoodSearchPending = false;
        FoodAttempt = 0;
        PerchAttempt[0] = PerchAttempt[1] = 0;
        HaveBlockedSkyGoal = false;
        previous_position=bird.Position();
        preferred_height=Random.randF(sky_low,sky_high);
        HeightTarget = preferred_height;
        HeightTimer = HeightChange.sample();
    }
    bool HasTreeCover(const Fvector& Root) const
    {
        if (!TreeShelter)
        {
            return false;
        }
        bool HasCover = false;
        g_CrowTrees.Visit(Root, TreeCoverRadius, [&](const Fvector& Tree)
        {
            if (Root.y >= Tree.y - .5f && Root.y + shelter_clearance < Tree.y + TreeCoverHeight)
            {
                HasCover = true;
            }
        });
        return HasCover;
    }
    void update_weather(float dt)
    {
        shelter_timer=std::max(0.f,shelter_timer-dt);
        shelter_memory_timer=std::max(0.f,shelter_memory_timer-dt);
        if (shelter_memory_timer<=0.f) abandoned_shelter_valid=false;
        if ((weather_timer-=dt)<=0.f)
        {
            weather_timer=weather_interval;
            const auto* environment=g_pGamePersistent ? g_pGamePersistent->Environment().CurrentEnv : nullptr;
            const float density=environment ? environment->rain_density : 0.f;
            const float hour=(g_pGamePersistent ? g_pGamePersistent->Environment().GetGameTime() : Level().GetGameDayTimeSec())/3600.f;
            WeatherNight=night_enabled && (night_start>night_end ? (hour>=night_start || hour<night_end) :
                (hour>=night_start && hour<night_end));
            WeatherRain=rain_shelter_enabled && (WeatherRain ? density>rain_stop : density>rain_start);
        }
        // These timers delay a decision, never locomotion. A reversed condition
        // cancels its pending reaction instead of executing an obsolete order.
        const bool RainChanged = RainReaction.Update(WeatherRain, dt, RainDelay, shelter_dry_delay);
        const bool NightChanged = NightReaction.Update(WeatherNight, dt, NightDelay, WakeDelay);
        raining = RainReaction.Active;
        night = NightReaction.Active;
        if (RainChanged || NightChanged)
        {
            retry_timer = 0.f;
            if (RainChanged)
            {
                shelter_timer = 0.f;
                if (!raining && phase == Phase::Shelter) remaining = 0.f;
            }
            if (debug) Msg("* [scavenger crow] environment reaction: rain=%s night=%s",
                raining ? "on" : "off", night ? "on" : "off");
        }
    }
    bool sheltered(CScavengerCrow& bird, const Fvector& root) const
    {
        if (HasTreeCover(root))
        {
            return true;
        }
        // A perch on top of a roof is exposed. Test actual cover above the
        // bird, including its immediate surroundings, rather than elevation.
        u32 covered=0;
        for (u32 i=0; i<5; ++i)
        {
            Fvector from=root;
            from.y+=shelter_clearance;
            if (i)
            {
                const float angle=float(i-1)*PI_DIV_2;
                from.x+=cosf(angle)*shelter_cover_radius;
                from.z+=sinf(angle)*shelter_cover_radius;
            }
            // RayPick culls back faces; that would miss the underside of
            // one-sided roofs. RayTest tests static cover from either side.
            const bool cover=Level().ObjectSpace.RayTest(from,Fvector().set(0,1,0),
                shelter_cover_height,collide::rqtStatic,nullptr,&bird);
            if (!i && !cover) return false;
            if (cover) ++covered;
        }
        return covered>=3;
    }
    bool shelter_allowed(CScavengerCrow& bird, const Fvector& root) const
    {
        Fvector threat;
        if (FailedPerchTimer > 0.f && root.distance_to_sqr(FailedPerch) < _sqr(perch_spacing))
        {
            return false;
        }
        if (abandoned_shelter_valid && _sqr(root.x-abandoned_shelter.x)+
            _sqr(root.z-abandoned_shelter.z)<_sqr(shelter_relocate_distance)) return false;
        return !unsafe(bird,root,fear_radius,threat) && sheltered(bird,root);
    }
    void enter_shelter(CScavengerCrow& bird)
    {
        release_food(bird);
        phase=Phase::Shelter; peck_phase=-1.f;
        remaining=raining ? shelter_hold.sample() : shelter_dry_delay.sample();

    }
    bool seek_shelter(CScavengerCrow& bird)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return false;
        }
        PROF_EVENT("CScavengerCrow::seek_shelter");
        auto& movement=bird.flight_controller();
        if (movement.status()==CFlyingMovementController::EStatus::Landed && shelter_allowed(bird,bird.Position()))
        { enter_shelter(bird); return true; }
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        movement.set_speed_scale(speed_variation.sample());
        const Fbox& bounds=Level().ObjectSpace.GetBoundingVolume();
        xr_vector<Fvector> Trees;
        if (TreeShelter)
        {
            // Bounded reservoir from nearby spatial cells, shared across birds.
            // No per-bird scan of all tree visuals on the level.
            u32 Seen = 0;
            g_CrowTrees.Visit(bird.Position(), shelter_radius, [&](const Fvector& Tree)
            {
                ++Seen;
                if (Trees.size() < 32u)
                {
                    Trees.push_back(Tree);
                }
                else
                {
                    const u32 Slot = u32(Random.randI(0, int(Seen)));
                    if (Slot < Trees.size())
                    {
                        Trees[Slot] = Tree;
                    }
                }
            });
        }
        for (u32 Slice = 0; Slice < 2u && ShelterAttempt < shelter_attempts; ++Slice)
        {
            const u32 i = ShelterAttempt++;
            const float angle=float(bird.ID())*2.39996323f+Random.randF(0.f,PI_MUL_2);
            const bool UseTree = !Trees.empty() && i < std::max(1u, shelter_attempts - 1u);
            const float radius=UseTree ? TreeCoverRadius * sqrtf(Random.randF(.05f,.7f)) :
                i ? shelter_radius*sqrtf(Random.randF(0.f,1.f)) : 0.f;
            Fvector probe=bird.Position(), contact, normal;
            if (UseTree)
            {
                probe = Trees[Random.randI(0, int(Trees.size()))];
            }
            probe.x+=cosf(angle)*radius; probe.z+=sinf(angle)*radius;
            if (probe.x<bounds.min.x || probe.x>bounds.max.x || probe.z<bounds.min.z || probe.z>bounds.max.z) continue;
            const float ground_y=UseTree ? probe.y : terrain_y(probe);
            // Start below possible roofs at several elevations, so downward
            // support rays can find covered floors and ledges, not only rooftops.
            for (u32 layer=0; layer<(UseTree ? 1u : 5u); ++layer)
            {
                probe.y=ground_y+.5f+float(layer)*roof_max*.25f;
                if (!CFlyingMovementController::support(bird,probe,roof_max+roof_min,contact,normal)) continue;
                Fvector root=contact; root.y+=movement.ground_offset();
                if (!shelter_allowed(bird,root)) continue;
                if (!movement.fly_to(bird,contact,true,perch_spacing,true)) continue;
                // Landing relocation must keep the selected rain cover.
                if (!movement.CommandPending() && !shelter_allowed(bird, movement.destination()))
                {
                    movement.stop();
                    continue;
                }
                release_food(bird); phase=Phase::ShelterApproach; peck_phase=-1.f;
                LandingElapsed = 0.f;
                ShelterAttempt = 0;
                shelter_timer = shelter_retry.sample();
                return true;
            }
        }
        if (ShelterAttempt < shelter_attempts)
        {
            GeometryDeferred = true;
            return false;
        }
        ShelterAttempt = 0;
        shelter_timer = shelter_retry.sample();
        return false;
    }
    bool recovery_goal(CScavengerCrow& bird)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return false;
        }
        auto& movement=bird.flight_controller();
        if (movement.grounded()) return false;
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        float heading,pitch,bank; bird.XFORM().getHPB(heading,pitch,bank);
        // A bounded fan of short, directly checked routes. Keep moving while
        // a new long route or rain shelter is considered; never recurse.
        for (u32 Slice = 0; Slice < 2u && RecoveryAttempt < 6u; ++Slice)
        {
            const u32 i = RecoveryAttempt++;
            const float turn=i ? ((i%2) ? 1.f : -1.f)*float((i+1)/2)*PI/4.f : 0.f;
            Fvector direction, point; direction.setHP(heading+turn,0.f);
            point.mad(bird.Position(),direction,std::min(8.f,movement.max_command_distance()*.5f));
            point.y+=1.f;
            const Fbox& bounds=Level().ObjectSpace.GetBoundingVolume();
            if (point.x<bounds.min.x || point.x>bounds.max.x || point.z<bounds.min.z || point.z>bounds.max.z ||
                !movement.clear_path(bird,bird.Position(),point)) continue;
            if (movement.fly_to(bird,point,false,0.f,true)) { departing=false; RecoveryAttempt=0; return true; }
        }
        if (RecoveryAttempt < 6u)
        {
            GeometryDeferred = true;
            return false;
        }
        RecoveryAttempt = 0;
        return false;
    }
    bool sky_goal(CScavengerCrow& bird, const Fvector* away = nullptr)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return false;
        }
        navigation_failure_handled=false;
        auto& movement = bird.flight_controller();
		if (movement.status() != CFlyingMovementController::EStatus::Flying)
		{
			movement.set_speed_scale(speed_variation.sample());
		}
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        if (away)
            movement.set_ground_movement(ground_hops ? CFlyingMovementController::EGroundMovement::Hop : CFlyingMovementController::EGroundMovement::Walk);
        if (movement.grounded())
        {
            // Distinct departure goals prevent a dense spawn from being pulled
            // back into one common point faster than separation can spread it.
            const Fvector departure=bird.Position();
            const Fbox& bounds=Level().ObjectSpace.GetBoundingVolume();
            float sector=fmodf(float(bird.ID())*2.39996323f,PI_MUL_2);
            if (away)
                sector=atan2f(departure.z-away->z,departure.x-away->x)+sinf(sector)*.8f;
            for (u32 Slice = 0; Slice < 2u && DepartureAttempt < nav_attempts; ++Slice)
            {
                const u32 attempt = DepartureAttempt++;
                const float angle=sector+float(attempt)*2.39996323f;
                const float radius=takeoff_spread*Random.randF(.5f,1.f);
                Fvector above=departure;
                above.x+=cosf(angle)*radius; above.z+=sinf(angle)*radius;
                above.y+=takeoff_height.sample();
                if (above.x<bounds.min.x || above.x>bounds.max.x || above.z<bounds.min.z || above.z>bounds.max.z)
                    continue;
                if (!movement.fly_to(bird,above,false,0.f,true)) continue;
                departing=true;
                HeightGoalDue = false;
                DepartureAttempt = SkyAttempt = 0;
                HaveBlockedSkyGoal = false;
                if (debug) Msg("* [scavenger crow] takeoff: id=%u to=(%.2f, %.2f, %.2f)",
                    u32(bird.ID()),above.x,above.y,above.z);
                return true;
            }
        }
        if (movement.grounded() && DepartureAttempt < nav_attempts)
        {
            GeometryDeferred = true;
            return false;
        }
        DepartureAttempt = 0;
		const bool Prefetch = movement.NeedsNextCruise(bird.Position(), GoalPrefetchProgress);
		const Fvector CruiseOrigin = Prefetch ? movement.CruiseEndpoint() : bird.Position();
        for (u32 Slice = 0; Slice < 2u && SkyAttempt < nav_attempts; ++Slice)
        {
            ++SkyAttempt;
            const float angle = Random.randF(0.f,PI_MUL_2);
            Fvector point = home;
            if (away)
            {
                Fvector direction;
                direction.sub(CruiseOrigin,*away); direction.y=0.f;
                if (direction.square_magnitude() < EPS_L) direction.set(cosf(angle),0,sinf(angle));
                direction.normalize_safe();
                point.mad(CruiseOrigin,direction,escape_distance);
                point.y = std::max(CruiseOrigin.y + escape_height, terrain_y(point) + sky_low);
                point.x += cosf(angle) * seed_step_radius;
                point.z += sinf(angle) * seed_step_radius;
            }
            else
            {
                const Fbox map_bounds = bird.flight_controller().FlightBounds();
                if (mapwide)
                {
                    if (!travel_valid || _sqr(CruiseOrigin.x-travel_target.x)+
                        _sqr(CruiseOrigin.z-travel_target.z)<_sqr(std::max(seed_step_radius,goal_advance_distance*1.5f)))
                    {
                        travel_target.set(Random.randF(map_bounds.min.x,map_bounds.max.x),0.f,
                            Random.randF(map_bounds.min.z,map_bounds.max.z));
                        preferred_height=Random.randF(sky_low,sky_high);
                        HeightTarget = preferred_height;
                        travel_target.y=sky_surface_y(bird,travel_target)+preferred_height;
                        travel_valid=true;
                    }
                    point=travel_target;
                    // Limit horizontal travel independently of the distant
                    // goal's altitude. Height is chosen locally below.
                    Fvector leg; leg.sub(point,CruiseOrigin); leg.y=0.f;
                    const float length=leg.magnitude();
                    const float leg_limit=std::min(sky_leg_distance,movement.max_command_distance()*.8f);
                    if (length>leg_limit) point.mad(CruiseOrigin,leg,leg_limit/length);
                }
                else
                {
                    point=CruiseOrigin;
                    const float radius = roam_radius * sqrtf(Random.randF(0.f,1.f));
                    point.x += cosf(angle)*radius; point.z += sinf(angle)*radius;
                }
                // Do not interpolate altitude from a remote map goal: on short
                // legs that collapsed almost every bird onto the minimum floor.
                // Each bird keeps its own cruising band, with gentle changes
                // between route segments. Movement still follows checked air paths.
                float height=preferred_height+Random.randF(-height_variation,height_variation);
                clamp(height,sky_low,sky_high);
                const float surface=sky_surface_y(bird,point);
                float desired_y=surface+height;
                clamp(desired_y,CruiseOrigin.y-vertical_step,CruiseOrigin.y+vertical_step);
                point.y=std::max(surface+sky_low,desired_y);
            }
            const Fbox bounds = bird.flight_controller().FlightBounds();
            if (point.x < bounds.min.x || point.x > bounds.max.x || point.z < bounds.min.z || point.z > bounds.max.z)
                continue;
            if (point.distance_to_sqr(CruiseOrigin)>_sqr(movement.max_command_distance())) continue;
            // Free roaming is not a fixed script destination: prefer another
            // reachable direction before stopping to run an incremental search.
            // Retain one blocked goal for full pathfinding if all corridors fail.
            if (!away && !Prefetch && !movement.clear_path(bird,CruiseOrigin,point))
            {
                if (!HaveBlockedSkyGoal) { BlockedSkyGoal=point; HaveBlockedSkyGoal=true; }
                travel_valid=false;
                continue;
            }
            if ((Prefetch ? movement.PrepareNextCruise(bird, point) : movement.fly_to(bird,point,false,0.f,true)))
            {
                SkyAttempt = 0;
                HaveBlockedSkyGoal = false;
                HeightGoalDue = false;
                return true;
            }
            if (!away) travel_valid=false;
        }
		if (Prefetch && SkyAttempt >= nav_attempts)
		{
			SkyAttempt = 0;
			HaveBlockedSkyGoal = false;
			return false;
		}
        if (SkyAttempt < nav_attempts)
        {
            GeometryDeferred = true;
            return false;
        }
        // Prefer a short safe continuation to a new stationary full search.
        if (!away && recovery_goal(bird))
        {
            SkyAttempt = 0;
            HaveBlockedSkyGoal = false;
            return true;
        }
        if (GeometryDeferred) return false;
        SkyAttempt = 0;
        if (movement.active())
        {
            // Keep moving while looking for a safe escape, but do not treat
            // the preceding cruise/circle route as an accepted escape order.
            if (away) retry_timer=escape_retry.sample();
            return !away;
        }
        if (HaveBlockedSkyGoal && movement.fly_to(bird,BlockedSkyGoal,false,0.f,true))
        {
            HaveBlockedSkyGoal = false;
            return true;
        }
        HaveBlockedSkyGoal = false;
        retry_timer = away ? escape_retry.sample() : retry.sample();
        navigation_failure_handled=true;
        if (debug) Msg("! [scavenger crow] sky goal rejected: id=%u retry=%.2fs from=(%.2f, %.2f, %.2f)",
            u32(bird.ID()),retry_timer,bird.Position().x,bird.Position().y,bird.Position().z);
        return false;
    }
    void flee(CScavengerCrow& bird, const Fvector& from)
    {
        if (raining && (phase==Phase::Shelter || phase==Phase::ShelterApproach))
        {
            abandoned_shelter=phase==Phase::ShelterApproach ? bird.flight().destination() : bird.Position();
            abandoned_shelter_valid=true; shelter_memory_timer=shelter_memory.sample();
            shelter_timer=shelter_relocate_delay.sample();
        }
        release_food(bird); danger=from; phase=Phase::Escape; remaining=escape.sample();
        SkyAttempt = DepartureAttempt = RecoveryAttempt = 0;
        HaveBlockedSkyGoal = false;
        safe_timer=safe.sample(); peck_phase=-1.f; retry_timer=0.f;
        // A rejected escape candidate must not keep an ordinary descent or
        // ground task alive through PreserveCurrentOnFailure.
        auto& Movement = bird.flight_controller();
        // Cancel queued food/landing commands even if the current path is airborne.
        // stop() preserves Landed and the cached departure corridor.
        Movement.stop();
        Movement.BeginCachedTakeoff(bird);
        travel_valid = false;
        FoodSearchPending = false;
        FoodApproachNeeded = false;
        fear = 1.f;
        Movement.SetCruiseContinuity(true, sky_leg_distance);
        LandingElapsed = 0.f;
        EscapeGoalPending = !sky_goal(bird,&danger);
    }
    bool seek_perch(CScavengerCrow& bird, bool on_roof)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return false;
        }
        auto& movement = bird.flight_controller();
        movement.set_speed_scale(speed_variation.sample());
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        // Try the local area, then a wider one when its perches are occupied.
        u32& Attempt = PerchAttempt[on_roof ? 1u : 0u];
        for (u32 Slice = 0; Slice < 2u && Attempt < nav_attempts * 2u; ++Slice)
        {
            const u32 i = Attempt++;
            const float angle=fmodf(float(bird.ID())*2.39996323f+float(i)*2.39996323f+
                Random.randF(-.5f,.5f),PI_MUL_2);
            const float search_radius=i<nav_attempts ? landing_radius : landing_max_radius;
            const float radius=search_radius * sqrtf(Random.randF(.1f,1.f));
            Fvector from=bird.Position(), contact, normal, threat;
            from.x += cosf(angle)*radius; from.z += sinf(angle)*radius;
            const float ground_y=terrain_y(from);
            from.y=ground_y + roof_max + sky_high;
            if (!CFlyingMovementController::support(bird,from,roof_max + sky_high + roof_min,contact,normal)) continue;
            const float elevation=contact.y-ground_y;
            if (on_roof ? (elevation < roof_min || elevation > roof_max || normal.y < cosf(roof_slope*PI/180.f)) :
                std::abs(elevation) > roof_min) continue;
            if (unsafe(bird,contact,fear_radius,threat)) continue;
            if (FailedPerchTimer > 0.f && contact.distance_to_sqr(FailedPerch) < _sqr(perch_spacing)) continue;
            // The controller reserves the final adjusted point immediately.
            // Retargeting preserves this spacing as well as the foot checks.
            if (!movement.fly_to(bird,contact,true,perch_spacing,true)) continue;
            phase=on_roof ? Phase::RoofApproach : Phase::GroundApproach;
            peck_phase=-1.f;
            LandingElapsed = 0.f;
            Attempt = 0;
            return true;
        }
        if (Attempt < nav_attempts * 2u)
        {
            GeometryDeferred = true;
            return false;
        }
        Attempt = 0;
        retry_timer=retry.sample();
        return false;
    }
    bool CircleGoal(CScavengerCrow& bird)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return false;
        }
        auto* Corpse = food();
        if (!Corpse)
        {
            return false;
        }
        auto& Movement = bird.flight_controller();
        Movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        Fvector Centre;
        Corpse->Center(Centre);
		const bool Prefetch = Movement.status() == CFlyingMovementController::EStatus::Flying &&
			!Movement.wants_landing() && !Movement.CommandPending() && !Movement.HasNextCruise();
		const Fvector Origin = Prefetch ? Movement.CruiseEndpoint() : bird.Position();
        const float CurrentAngle = atan2f(Origin.z - Centre.z, Origin.x - Centre.x);
        for (u32 Attempt = 0; Attempt < 2u; ++Attempt)
        {
            const float Angle = CurrentAngle + CircleDirection * (PI / 4.f + float(Attempt) * PI / 4.f);
            Fvector Point;
            Point.set(Centre.x + cosf(Angle) * CircleDistance,
                Centre.y + CircleAltitude + sinf(Angle * 2.f + float(bird.ID())) * .6f,
                Centre.z + sinf(Angle) * CircleDistance);
            const Fbox Bounds = bird.flight_controller().FlightBounds();
            if (Point.x < Bounds.min.x || Point.x > Bounds.max.x || Point.z < Bounds.min.z || Point.z > Bounds.max.z ||
                Point.distance_to(Origin) > Movement.max_command_distance() ||
                (!Prefetch && !Movement.clear_path(bird, Origin, Point)))
            {
                continue;
            }
            if (Prefetch ? Movement.PrepareNextCruise(bird, Point) : Movement.fly_to(bird, Point, false, 0.f, true))
            {
                return true;
            }
        }
        retry_timer = retry.sample();
        return false;
    }

    bool seek_food(CScavengerCrow& bird, bool CircleFirst = true, CEntityAlive* PreferredFood = nullptr)
    {
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            FoodSearchPending = true;
            return false;
        }
        FoodSearchPending = false;
        xr_vector<CEntityAlive*> corpses;
        if (PreferredFood)
        {
            FoodAttempt = 0;
            corpses.push_back(PreferredFood);
        }
        else if (const auto* Observation = CCrowSharedMemory::Get().Observe(bird.Position(), food_search_radius))
        {
            for (u32 Id : Observation->Food)
            {
                auto* Object = Level().Objects.net_Find(Id);
                auto* Entity = Object ? smart_cast<CEntityAlive*>(Object) : nullptr;
                if (!Entity || Entity->getDestroy() || Entity->g_Alive() ||
                    Entity->Position().distance_to_sqr(bird.Position()) > _sqr(food_search_radius)) continue;
                if (food_users(Id) < corpse_slots) corpses.push_back(Entity);
            }
        }
        std::sort(corpses.begin(),corpses.end(),[&](const CEntityAlive* a,const CEntityAlive* b)
            { return a->Position().distance_to_sqr(bird.Position()) < b->Position().distance_to_sqr(bird.Position()); });
        auto& movement=bird.flight_controller();
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        movement.set_speed_scale(speed_variation.sample());
        if (FoodAttempt >= corpses.size()) FoodAttempt = 0;
        for (u32 Slice = 0; Slice < 2u && FoodAttempt < corpses.size(); ++Slice)
        {
            auto* entity = corpses[FoodAttempt++];
            Fvector Threat;
            if (CircleFirst && (Random.randF(0.f, 1.f) < CircleProbability ||
                unsafe(bird, entity->Position(), food_safe_radius, Threat)))
            {
                release_food(bird);
                corpse_id = entity->ID();
                CCrowSharedMemory::Get().ClaimFood(bird.ID(), corpse_id);
                CircleDistance = CircleRadius.sample();
                CircleAltitude = CircleHeight.sample();
                CircleDirection = bird.ID() % 2 ? 1.f : -1.f;
                if (CircleGoal(bird))
                {
                    phase = Phase::CorpseCircle;
                    FoodAttempt = 0;
                    remaining = CircleTime.sample();
                    peck_phase = -1.f;
                    departing = false;
                    return true;
                }
                release_food(bird);
                continue;
            }
            Fvector threat;
            if (unsafe(bird,entity->Position(),food_safe_radius,threat)) continue;
            const auto* Perches = CCrowSharedMemory::Get().CorpsePerches(bird, entity->ID(), corpse_side_distance, movement.ground_offset());
            if (!Perches || Perches->empty()) continue;
            const u32 First = u32(bird.ID()) % u32(Perches->size());
            // Shared geometry is evaluated once; individual birds still reserve their slot.
            for (u32 i = 0; i < std::min(3u, u32(Perches->size())); ++i)
            {
                const auto& Perch = (*Perches)[(First + i) % Perches->size()];
                if (!Perch.Valid) continue;
                const Fvector contact = Perch.Contact;
                Fvector centre; entity->Center(centre);
                const Fbox& body = entity->BoundingBox();
                const float body_radius = .5f * std::max(body.max.x - body.min.x, body.max.z - body.min.z);
                if (FailedPerchTimer > 0.f && contact.distance_to_sqr(FailedPerch) < _sqr(perch_spacing)) continue;
                // Feed beside the ragdoll on static support. Do not snap several
                // metres away merely because the landing controller can relocate.
                if (!movement.fly_to(bird,contact,true,0.f,true))
                { CCrowSharedMemory::Get().RejectPerch(entity->ID(), contact); continue; }
                if (!movement.CommandPending() &&
                    (movement.destination().distance_to_sqr(centre) > _sqr(body_radius+corpse_reach_distance) ||
                    !CCrowSharedMemory::BodyOutsideCorpse(bird, *entity, movement.destination(), .16f)))
                {
                    movement.stop();
                    CCrowSharedMemory::Get().RejectPerch(entity->ID(), contact);
                    continue;
                }
                release_food(bird); corpse_id=entity->ID();
                CCrowSharedMemory::Get().ClaimFood(bird.ID(), corpse_id, &contact);
                phase=Phase::CorpseApproach; peck_phase=-1.f;
                FoodAttempt = 0;
                FoodApproachNeeded = false;
                LandingElapsed = 0.f;
                return true;
            }
        }
        if (FoodAttempt < corpses.size())
        {
            GeometryDeferred = true;
            FoodSearchPending = true;
            return false;
        }
        FoodAttempt = 0;
        return false;
    }
    void peck(CScavengerCrow& bird, float dt)
    {
        if (bird.flight().status()!=CFlyingMovementController::EStatus::Landed || bird.m_ground_pose_weight < .999f) return;
        if (peck_phase >= 0.f)
        {
            peck_phase += dt / peck_duration;
            if (peck_phase >= 1.f)
            {
                if (phase == Phase::Corpse) FoodApproachNeeded = !bird.m_peck_contact_reached;
                if (bird.m_peck_contact_reached || (!bird.m_peck_ik_enabled && phase != Phase::Corpse))
                    satiety=std::min(1.f,satiety+(phase==Phase::Corpse ? meat_gain : seed_gain));
                peck_phase=-1.f;
                peck_timer=phase==Phase::Corpse ? (satiety>=full ? sated_peck.sample() : corpse_peck.sample()) : peck_interval.sample();
            }
        }
        else if ((peck_timer -= dt) <= 0.f)
        {
            peck_phase=0.f;
            bird.m_peck_contact_reached=false;
            if (phase==Phase::Corpse)
            {
                auto* entity=food();
                if (entity)
                {
                    Fvector centre; entity->Center(centre);
                    Fvector direction; direction.sub(centre,bird.Position()); direction.y=0.f;
                    if (direction.square_magnitude()>EPS_L)
                    {
                        const Fvector position=bird.Position();
                        bird.XFORM().setHPB(direction.getH(),0.f,0.f); bird.Position()=position;
                    }
                }
            }
        }
    }
    void update_night(CScavengerCrow& bird, float dt)
    {
        auto& movement=bird.flight_controller();
        if (movement.grounded())
        {
            if (!movement.active() && movement.status()!=CFlyingMovementController::EStatus::Landed)
            {
                CGeometryJob GeometryJob(*this);
                if (!GeometryJob)
                {
                    return;
                }
                movement.restore_landed(bird);
            }
            if (movement.grounded())
            {
                if (phase!=Phase::NightRest && phase!=Phase::NightGround)
                {
                    CGeometryJob GeometryJob(*this);
                    if (!GeometryJob)
                    {
                        return;
                    }
                    // Cancel a pending automatic air retry from the preceding
                    // escape/ground detour before settling for the night.
                    movement.stop();
                    if (!movement.restore_landed(bird))
                    {
                        begin_sky(bird);
                        phase=Phase::NightApproach;
                        retry_timer=0.f;
                        update_night(bird,dt);
                        return;
                    }
                    movement.set_strategy(CFlyingMovementController::EStrategy::GroundOnly);
                    release_food(bird); peck_phase=-1.f;
                    phase=Phase::NightRest; remaining=night_rest.sample();
                }
                if (remaining<=0.f && !movement.active())
                {
                    const bool on_ground=std::abs(bird.Position().y-movement.ground_offset()-terrain_y(bird.Position()))<=roof_min;
                    phase=on_ground && Random.randF(0.f,1.f)<night_forage_chance ? Phase::NightGround : Phase::NightRest;
                    remaining=phase==Phase::NightGround ? night_forage.sample() : night_rest.sample();
                    peck_phase=-1.f; peck_timer=peck_interval.sample(); step_timer=night_step.sample();
                }

                if (phase==Phase::NightGround)
                {
                    peck(bird,dt);
                    if ((step_timer-=dt)<=0.f && movement.status()==CFlyingMovementController::EStatus::Landed && peck_phase<0.f)
                    {
                        CGeometryJob GeometryJob(*this);
                        if (!GeometryJob)
                        {
                            return;
                        }
                        step_timer=night_step.sample();
                        Fvector point=bird.Position(); point.y-=movement.ground_offset();
                        const float angle=Random.randF(0.f,PI_MUL_2);
                        point.x+=cosf(angle)*seed_step_radius; point.z+=sinf(angle)*seed_step_radius;
                        movement.set_strategy(CFlyingMovementController::EStrategy::GroundOnly);
                        movement.set_ground_movement(CFlyingMovementController::EGroundMovement::Hop);
                        movement.walk_to(bird,point,ground_speed);
                    }
                }
                return;
            }
        }
        // An airborne bird may descend once to finish its flight. Grounded
        // night activities above never invoke flight or an automatic air fallback.
        if (phase==Phase::NightApproach && movement.active() && movement.wants_landing()) return;
        if (phase!=Phase::NightApproach)
        {
            // Keep an existing cruise leg until the budget accepts its replacement.
            if (movement.wants_landing()) movement.stop();
            release_food(bird); peck_phase=-1.f;
            retry_timer=0.f; LandingElapsed=0.f;
        }
        phase=Phase::NightApproach;
        if (retry_timer>0.f) return;
        CGeometryJob GeometryJob(*this);
        if (!GeometryJob)
        {
            return;
        }
        movement.set_strategy(CFlyingMovementController::EStrategy::AirOnly);
        Fvector from=bird.Position(), contact, normal, threat;
        from.y+=.5f;
        if (CFlyingMovementController::support(bird,from,movement.max_command_distance(),contact,normal) &&
            (FailedPerchTimer <= 0.f || contact.distance_to_sqr(FailedPerch) >= _sqr(perch_spacing)) &&
            !unsafe(bird,contact,fear_radius,threat) && movement.fly_to(bird,contact,true,perch_spacing,true)) return;
        const bool landed_goal=seek_perch(bird,true) || (!GeometryDeferred && seek_perch(bird,false));
        phase=Phase::NightApproach;
        if (GeometryDeferred) return;
        if (!landed_goal) { retry_timer=retry.sample(); recovery_goal(bird); }
    }
    void UpdateInputs(CScavengerCrow& bird, float dt)
    {
        if (!enabled) return;
        initialize_home(bird);
        update_weather(dt);
        HeightTimer -= dt;
        if (HeightTimer <= 0.f)
        {
            HeightTimer = HeightChange.sample();
            HeightTarget = Random.randF(sky_low, sky_high);
            HeightGoalDue = true;
        }
        const float HeightDelta = HeightSpeed * dt;
        preferred_height += std::max(-HeightDelta, std::min(HeightDelta, HeightTarget - preferred_height));
        satiety=std::max(0.f,satiety-dt/hunger_time);
        const bool ImmediateFear = fear >= fear_trigger;
        fear=std::max(0.f,fear-fear_decay*dt);
        auto& movement=bird.flight_controller();
        if (departing && !movement.active()) departing=false;
        // Rest is due after actual cruising, not after spawn delay, takeoff,
        // route searches or hovering while a failed route is retried.
        const Fvector& velocity=movement.velocity();
        const float moved_x=bird.Position().x-previous_position.x;
        const float moved_z=bird.Position().z-previous_position.z;
        previous_position=bird.Position();
        if (phase!=Phase::Sky || (!departing && movement.status()==CFlyingMovementController::EStatus::Flying &&
            velocity.x*velocity.x+velocity.z*velocity.z>.01f &&
            moved_x*moved_x+moved_z*moved_z>.01f*dt*dt))
            remaining-=dt;
        safe_timer=std::max(0.f,safe_timer-dt);
        noise_timer=std::max(0.f,noise_timer-dt); retry_timer=std::max(0.f,retry_timer-dt);
        scan_timer-=dt; food_search_timer=std::max(0.f,food_search_timer-dt);
        const bool scan_due=scan_timer<=0.f;
        ScanDue = scan_due;
        const bool approaching=phase==Phase::GroundApproach || phase==Phase::RoofApproach || phase==Phase::CorpseApproach || phase==Phase::ShelterApproach || phase==Phase::NightApproach;
        const bool perched=phase==Phase::Ground || phase==Phase::Roof || phase==Phase::Corpse || phase==Phase::Shelter || phase==Phase::NightRest || phase==Phase::NightGround;
        if (scan_due)
        {
            scan_timer = std::max(.1f, std::min(.5f, perception * Random.randF(.8f, 1.2f)));
            Fvector threat;
            bool acoustic=false;
            const float radius=(phase==Phase::CorpseApproach || phase==Phase::Corpse) ? food_safe_radius : fear_radius;
            // No predator scans for cruising birds. Approach/rest phases are
            // authoritative while locomotion is still finishing its transition.
            bool threatened=(approaching || perched || movement.grounded()) &&
                unsafe(bird,bird.Position(),radius,threat,&acoustic);
            if (!threatened && approaching) threatened=unsafe(bird,movement.destination(),radius,threat,&acoustic);
            if (threatened)
            {
                danger=threat;
                // Sound events latch panic once; safety memory only blocks landing.
                // A living threat refreshes full panic until the bird leaves its range.
                if (!acoustic) fear=1.f;
            }
        }
        // Noise impulses can cross the threshold between perception scans.
        // Danger bypasses environmental reaction delays and task completion.
        if (phase!=Phase::Escape && (ImmediateFear || fear>=fear_trigger)) { flee(bird,danger); return; }
        if (!approaching && !perched && movement.status()==CFlyingMovementController::EStatus::Failed &&
            !navigation_failure_handled)
        {
            navigation_failure_handled=true;
            retry_timer=phase==Phase::Escape ? escape_retry.sample() : retry.sample();
            travel_valid=false;
            if (phase==Phase::Sky || phase==Phase::ShelterSearch) recovery_goal(bird);
        }
    }

    bool IsApproaching() const
    {
        return phase == Phase::GroundApproach || phase == Phase::RoofApproach ||
            phase == Phase::CorpseApproach || phase == Phase::ShelterApproach || phase == Phase::NightApproach;
    }
    bool NeedsFlightGoal(const CScavengerCrow& Bird) const
    {
        const auto& Movement = Bird.flight();
        if (Movement.HasNextCruise()) return false;
        return Movement.NeedsNextCruise(Bird.Position(), GoalPrefetchProgress);
    }
    void WatchNavigation(CScavengerCrow& Bird, float Dt)
    {
        auto& Movement = Bird.flight_controller();
        FailedPerchTimer = std::max(0.f, FailedPerchTimer - Dt);
        if (IsApproaching() && !Movement.CommandPending() && !Movement.active() &&
            !Movement.grounded() && !Movement.wants_landing() &&
            Movement.status() != CFlyingMovementController::EStatus::Failed)
        {
            // The landing was cancelled or its air leg ended without a descent.
            // An approach phase cannot wait indefinitely for a command that is gone.
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        if (Movement.CommandPending())
        {
            // Waiting for a global preparation slot is not a stalled movement.
            LandingElapsed = 0.f;
            return;
        }
        if (IsApproaching() && (!Movement.grounded() || Movement.status() == CFlyingMovementController::EStatus::Failed))
        {
            LandingElapsed += Dt;
            if (Movement.status() == CFlyingMovementController::EStatus::Failed ||
                Movement.stalled_time() > LandingStallLimit ||
                (Movement.status() == CFlyingMovementController::EStatus::Planning && Movement.planning_time() > RouteWait) ||
                LandingElapsed > LandingTotalLimit)
            {
                FailedPerch = Movement.destination();
                FailedPerchTimer = RejectedPerchTime;
                Movement.stop();
                begin_sky(Bird);
                LandingElapsed = 0.f;
                shelter_timer = 0.f;
                if (raining) phase = Phase::ShelterSearch;
                else if (night) phase = Phase::NightApproach;
                retry_timer = retry.sample();
                // A failed perch is abandoned, not retried in a descent loop.
                recovery_goal(Bird);
            }
        }
        else
        {
            LandingElapsed = 0.f;
            if (retry_timer <= 0.f && !Movement.wants_landing() &&
                ((Movement.status() == CFlyingMovementController::EStatus::Planning && Movement.planning_time() > RouteWait) ||
                (Movement.status() == CFlyingMovementController::EStatus::Flying && Movement.stalled_time() > LandingStallLimit)))
            {
                if (phase == Phase::Escape) EscapeGoalPending = !sky_goal(Bird, &danger);
                else recovery_goal(Bird);
                if (!GeometryDeferred) retry_timer = retry.sample();
            }
        }
    }
    bool UpdateEscape(CScavengerCrow& bird, float dt)
    {
        if (phase != Phase::Escape) return false;
        auto& movement = bird.flight_controller();

            if (remaining>0.f || fear>fear_calm)
            {
                if ((EscapeGoalPending || !movement.active() || movement.status() == CFlyingMovementController::EStatus::Failed || (movement.status()==CFlyingMovementController::EStatus::Flying &&
                    NeedsFlightGoal(bird))) && retry_timer<=0.f)
                    EscapeGoalPending = !sky_goal(bird,&danger);
                return true;
            }
            begin_sky(bird);
            if (!raining && !night) { sky_goal(bird); return true; }
            if (raining) phase=Phase::ShelterSearch;

        return false;
    }
    bool UpdateRain(CScavengerCrow& bird, float dt)
    {
        auto& movement = bird.flight_controller();
        const bool scan_due = ScanDue;
        if (raining && phase != Phase::Shelter)
        {
            if (phase==Phase::ShelterApproach)
            {
                if (!scan_due && movement.active()) return true;
                CGeometryJob GeometryJob(*this);
                if (!GeometryJob)
                {
                    return true;
                }
                if ((scan_due && !shelter_allowed(bird,movement.destination())) || !movement.active())
                {
                    if (movement.status()==CFlyingMovementController::EStatus::Landed &&
                        shelter_allowed(bird,bird.Position()))
                    { enter_shelter(bird); return true; }
                    movement.stop(); begin_sky(bird); phase=Phase::ShelterSearch;
                }
                else return true;
            }
            if (phase!=Phase::ShelterSearch)
            {
                // Cancel ordinary landing, feeding and walking immediately,
                // even while the next shelter probe is waiting for its timer.
                if (movement.wants_landing() || movement.grounded()) movement.stop();
                begin_sky(bird); phase=Phase::ShelterSearch;
            }
            if (shelter_timer<=0.f && (seek_shelter(bird) || GeometryDeferred)) return true;
            if (retry_timer<=0.f && (!movement.active() ||
                (movement.status()==CFlyingMovementController::EStatus::Flying && !movement.wants_landing() &&
                    NeedsFlightGoal(bird))))
            {
                if (movement.active() || !recovery_goal(bird)) sky_goal(bird);
            }
            return true;

        }
        if (!raining && phase == Phase::ShelterSearch) begin_sky(bird);
        if (!raining && night) return false;
        if (!raining && phase == Phase::ShelterApproach)
        { begin_sky(bird); sky_goal(bird); return true; }
        if (phase == Phase::Shelter)
        {
            if (!movement.grounded()) { begin_sky(bird); sky_goal(bird); return true; }
            if (remaining<=0.f)
            {
                CGeometryJob GeometryJob(*this);
                if (!GeometryJob)
                {
                    return true;
                }
                Fvector probe=bird.Position(), contact, normal;
                probe.y+=.1f;
                const bool supported=CFlyingMovementController::support(bird,probe,
                    movement.ground_offset()+.3f,contact,normal) &&
                    std::abs(contact.y+movement.ground_offset()-bird.Position().y)<.1f &&
                    movement.landing_footprint(bird,contact,normal);
                if (!raining || !supported || !shelter_allowed(bird,bird.Position()))
                { begin_sky(bird); sky_goal(bird); return true; }
                // Remain perched as long as the weather requires it, regardless
                // of hunger or the ordinary ground/roof rest interval.
                remaining=shelter_hold.sample();
            }

            return true;

        }
        return false;
    }
    void UpdateApproach(CScavengerCrow& bird, float dt)
    {
        auto& movement = bird.flight_controller();

            if (phase==Phase::CorpseApproach && !food()) { begin_sky(bird); sky_goal(bird); return; }
            if (movement.status()==CFlyingMovementController::EStatus::Failed)
            {
                begin_sky(bird); retry_timer=retry.sample();
                if (!recovery_goal(bird)) sky_goal(bird);
                return;
            }
            if (movement.status()==CFlyingMovementController::EStatus::Landed)
            {
                if (phase==Phase::ShelterApproach)
                {
                    CGeometryJob GeometryJob(*this);
                    if (!GeometryJob)
                    {
                        return;
                    }
                    if (shelter_allowed(bird,bird.Position())) enter_shelter(bird);
                    else { begin_sky(bird); sky_goal(bird); }
                    return;
                }
                if (phase==Phase::RoofApproach && bird.Position().y-movement.ground_offset()-terrain_y(bird.Position())<roof_min)
                { begin_sky(bird); sky_goal(bird); return; }
                if (phase==Phase::CorpseApproach)
                {
                    auto* entity=food();
                    Fvector centre; entity->Center(centre);
                    const Fbox& body=entity->BoundingBox();
                    const float body_radius=.5f*std::max(body.max.x-body.min.x,body.max.z-body.min.z);
                    if (bird.Position().distance_to_sqr(centre)>_sqr(body_radius+corpse_reach_distance))
                    { begin_sky(bird); sky_goal(bird); return; }
                }
                phase=phase==Phase::GroundApproach ? Phase::Ground : phase==Phase::RoofApproach ? Phase::Roof : Phase::Corpse;
                remaining=phase==Phase::Ground ? ground.sample() : phase==Phase::Roof ? roof.sample() : corpse.sample();
                peck_timer=phase==Phase::Corpse ? corpse_peck.sample() : peck_interval.sample();
                step_timer=phase==Phase::Corpse ? 0.f : step.sample();
            }
            return;

    }
    bool RestFinished(CScavengerCrow& Bird)
    {
        const auto Status = Bird.flight().status();
        if (Status == CFlyingMovementController::EStatus::Failed || Status == CFlyingMovementController::EStatus::GroundBlocked ||
            !Bird.flight().grounded() || remaining <= 0.f || (phase != Phase::Corpse && satiety <= hungry))
        {
            begin_sky(Bird);
            sky_goal(Bird);
            return true;
        }
        return false;
    }
    void UpdateGroundForaging(CScavengerCrow& Bird, float Dt)
    {
        if (RestFinished(Bird)) return;
        peck(Bird, Dt);
        auto& Movement = Bird.flight_controller();
        if ((step_timer -= Dt) <= 0.f && Movement.status() == CFlyingMovementController::EStatus::Landed && peck_phase < 0.f)
        {
            CGeometryJob GeometryJob(*this);
            if (!GeometryJob)
            {
                return;
            }
            step_timer = step.sample();
            Fvector Point = Bird.Position();
            Point.y -= Movement.ground_offset();
            const float Angle = Random.randF(0.f, PI_MUL_2);
            Point.x += cosf(Angle) * seed_step_radius;
            Point.z += sinf(Angle) * seed_step_radius;
            Movement.set_strategy(CFlyingMovementController::EStrategy::GroundOnly);
            Movement.set_ground_movement(ground_hops ? CFlyingMovementController::EGroundMovement::Hop : CFlyingMovementController::EGroundMovement::Walk);
            Movement.walk_to(Bird, Point, ground_speed);
        }
    }
    void UpdateRoofRest(CScavengerCrow& Bird, float Dt)
    {
        // Local head/wing motion belongs to the bird, not this rest scheme.
        RestFinished(Bird);
    }
    void UpdateCorpseFeeding(CScavengerCrow& Bird, float Dt)
    {
        auto* Corpse = food();
        if (!Corpse)
        {
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        Fvector Centre;
        Corpse->Center(Centre);
        const Fbox& Body = Corpse->BoundingBox();
        const float Radius = .5f * std::max(Body.max.x - Body.min.x, Body.max.z - Body.min.z);
        if (Bird.Position().distance_to_sqr(Centre) > _sqr(Radius + corpse_reach_distance))
        {
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        if (RestFinished(Bird)) { peck_phase = -1.f; return; }
        auto& Movement = Bird.flight_controller();
        if (Movement.status() != CFlyingMovementController::EStatus::Landed)
        {
            peck_phase = -1.f;
            return;
        }
        if (peck_phase < 0.f && (step_timer -= Dt) <= 0.f)
        {
            CGeometryJob GeometryJob(*this);
            if (!GeometryJob)
            {
                return;
            }
            step_timer = .25f;
            Fvector Facing; Facing.sub(Centre, Bird.Position()); Facing.y = 0.f;
            if (Facing.square_magnitude() > EPS_S)
            {
                const Fvector Position = Bird.Position();
                Bird.XFORM().setHPB(Facing.getH(), 0.f, 0.f);
                Bird.Position() = Position;
            }
            if (!CCrowSharedMemory::BodyOutsideCorpse(Bird, *Corpse, Bird.Position(), .16f))
            {
                peck_phase = -1.f;
                begin_sky(Bird); sky_goal(Bird);
                return;
            }
            Fvector Beak, Neck, Flesh;
            float Reach = 0.f;
            if (!Bird.GetRestingBeak(Beak, Neck, Reach) || !CrowFoodContact(Bird, *Corpse, Beak, Flesh))
            {
                begin_sky(Bird);
                sky_goal(Bird);
                return;
            }
            // The neck and beak must be able to reach the meat before a peck starts.
            if (FoodApproachNeeded || Neck.distance_to_sqr(Flesh) > _sqr(std::max(.01f, Reach - .01f)))
            {
                const auto* Perches = CCrowSharedMemory::Get().CorpsePerches(Bird, Corpse->ID(), corpse_side_distance, Movement.ground_offset());
                if (Perches && !Perches->empty())
                {
                    u32 ContactCandidates = 0;
                    const u32 First = FeedContactCursor % u32(Perches->size());
                    for (u32 Index = 0; Index < Perches->size(); ++Index)
                    {
                        const auto& Perch = (*Perches)[(First + Index) % Perches->size()];
                        if (!Perch.Valid) continue;
                        const float StepDistanceSq = _sqr(Perch.Contact.x - Bird.Position().x) + _sqr(Perch.Contact.z - Bird.Position().z);
                        if (StepDistanceSq < _sqr(.08f) || StepDistanceSq > _sqr(1.f)) continue;
                        if (++ContactCandidates > 2u) break;
                        FeedContactCursor = (First + Index + 1u) % u32(Perches->size());
                        Movement.set_strategy(CFlyingMovementController::EStrategy::GroundOnly);
                        Movement.set_ground_movement(CFlyingMovementController::EGroundMovement::Walk);
                        Fvector Root = Perch.Contact; Root.y += Movement.ground_offset();
                        if (CCrowSharedMemory::BodyOutsideCorpse(Bird, *Corpse, Root, .16f) &&
                            Movement.walk_to(Bird, Perch.Contact, ground_speed))
                        {
                            CCrowSharedMemory::Get().ClaimFood(Bird.ID(), Corpse->ID(), &Perch.Contact);
                            FoodApproachNeeded = false;
                            return;
                        }
                    }
                }
                // An unreachable or blocked mouthful requires a different approach.
                if (seek_food(Bird, false, Corpse) || GeometryDeferred) return;
                begin_sky(Bird);
                sky_goal(Bird);
                return;
            }
        }
        peck(Bird, Dt);
    }
    void UpdateCorpseCircle(CScavengerCrow& Bird, float Dt)
    {
        auto* Corpse = food();
        if (!Corpse)
        {
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        if (remaining <= 0.f)
        {
            Fvector Threat;
            if (satiety < full && !unsafe(Bird, Corpse->Position(), food_safe_radius, Threat) &&
                seek_food(Bird, false, Corpse)) return;
            if (GeometryDeferred) return;
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        const auto& Movement = Bird.flight();
        const float Advance = std::min(CircleDistance * .25f,
            std::max(1.f, Movement.velocity().magnitude() * GoalLookAhead));
        if (retry_timer <= 0.f && (!Movement.active() || Movement.status() == CFlyingMovementController::EStatus::Failed ||
            (Movement.status() == CFlyingMovementController::EStatus::Flying &&
                !Movement.HasNextCruise() && (Movement.NeedsNextCruise(Bird.Position(), GoalPrefetchProgress) ||
                Movement.destination().distance_to_sqr(Bird.Position()) < _sqr(Advance)))))
        {
            if (!CircleGoal(Bird) && !GeometryDeferred && !Movement.active()) recovery_goal(Bird);
        }
    }
    void UpdateCruise(CScavengerCrow& bird, float dt)
    {
        auto& movement = bird.flight_controller();
        // A finished cruise must acquire its next flight goal before optional
        // food/perch probes can consume the bird's one geometry job.
        if (retry_timer <= 0.f && !movement.active() && !movement.grounded())
        {
            sky_goal(bird);
            return;
        }
        if ((ScanDue || FoodSearchPending) && safe_timer <= 0.f && satiety <= hungry && retry_timer <= 0.f && food_search_timer <= 0.f)
        {
            if (seek_food(bird) || GeometryDeferred) return;
            food_search_timer = retry.sample();
        }

        if (!raining && remaining<=0.f && safe_timer<=0.f && satiety>hungry && retry_timer<=0.f)
        {
            const bool roof_first=Random.randF(0.f,1.f)>ground_chance;
            if (seek_perch(bird,roof_first) || GeometryDeferred || seek_perch(bird,!roof_first) || GeometryDeferred) return;
            // A failed search for a safe perch must never strand a bird in midair.
            if (!movement.active() && !recovery_goal(bird)) sky_goal(bird);
        }
        if (movement.status()==CFlyingMovementController::EStatus::Failed) travel_valid=false;
        if (retry_timer<=0.f && (!movement.active() || movement.status() == CFlyingMovementController::EStatus::Failed ||
            (HeightGoalDue && !departing && movement.status() == CFlyingMovementController::EStatus::Flying) ||
            (movement.status()==CFlyingMovementController::EStatus::Flying && !movement.wants_landing() &&
                NeedsFlightGoal(bird))))
        {
            departing=false;
            sky_goal(bird);
        }

    }
    void PlanBehavior(CScavengerCrow& Bird, float Dt)
    {
        PROF_EVENT("CScavengerCrow::PlanBehavior");
        // One arbitration point. Lower-priority behaviors cannot issue commands
        // while fear, rain or night owns the bird.
        if (UpdateEscape(Bird, Dt))
        {
            return;
        }
        if (UpdateRain(Bird, Dt))
        {
            return;
        }
        if (night)
        {
            update_night(Bird, Dt);
            return;
        }
        if (phase == Phase::NightApproach || phase == Phase::NightRest || phase == Phase::NightGround)
        {
            if (Bird.flight().wants_landing() || Bird.flight().grounded()) Bird.flight_controller().stop();
            begin_sky(Bird);
            sky_goal(Bird);
            return;
        }
        if (IsApproaching())
        {
            UpdateApproach(Bird, Dt);
            return;
        }
        switch (phase)
        {
        case Phase::Ground:
            UpdateGroundForaging(Bird, Dt);
            break;
        case Phase::Roof:
            UpdateRoofRest(Bird, Dt);
            break;
        case Phase::Corpse:
            UpdateCorpseFeeding(Bird, Dt);
            break;
        case Phase::CorpseCircle:
            UpdateCorpseCircle(Bird, Dt);
            break;
        default:
            UpdateCruise(Bird, Dt);
            break;
        }
    }
    void update(CScavengerCrow& Bird, float Dt)
    {
        if (!enabled)
        {
            return;
        }
        const bool HadDeferredGeometry = GeometryDeferred;
        GeometryDeferred = false;
        UpdateInputs(Bird, Dt);
        if (!home_valid) return;
        WatchNavigation(Bird, Dt);
        PlanBehavior(Bird, Dt);
        if (HadDeferredGeometry && !GeometryDeferred)
        {
            // A completed/changed task must not leave an unused ready grant at
            // the front of the flock's fairness queue until its expiry.
            CFlyingWorkBudget::CancelPending(this, CFlyingWorkBudget::EKind::BehaviorProbe);
        }
    }

};

CScavengerCrow::CScavengerCrow() : m_behavior(std::make_unique<SBehavior>()) {}
CScavengerCrow::~CScavengerCrow() { stop_crow_behavior(); CCrowSharedMemory::Get().UnregisterBird(ID()); }
void CScavengerCrow::Load(const char* section)
{
    CFlyingMonster::Load(section);
    m_behavior->load(section);
    if (m_behavior->enabled)
    {
        SetNativeWander(false);
        m_behavior->enabled=true;
    }
}
void CScavengerCrow::reinit()
{
    stop_crow_behavior();
    CFlyingMonster::reinit();
    m_behavior->enabled=READ_IF_EXISTS(pSettings,r_bool,cNameSect().c_str(),"crow_ai_enabled",false);
    m_behavior->home_valid=false; m_behavior->loaded=false;
}
void CScavengerCrow::start_crow_behavior()
{
    if (!g_Alive()) return;
    SpatialComponent->type |= ESPATIAL_TYPE::REACTTOSOUND;
    m_behavior->phase=SBehavior::Phase::Sky;
    m_behavior->remaining=m_behavior->sky.sample();
    m_behavior->departing=false; m_behavior->travel_valid=false;
    m_behavior->previous_position=Position();
    m_behavior->BehaviorTick = .1f * float(ID() % 97u) / 97.f;
    m_behavior->LandingElapsed = 0.f;
    m_behavior->FailedPerchTimer = 0.f;
    m_behavior->preferred_height = Random.randF(m_behavior->sky_low, m_behavior->sky_high);
    m_behavior->HeightTarget = m_behavior->preferred_height;
    m_behavior->HeightTimer = m_behavior->HeightChange.sample();
    m_behavior->weather_timer=0.f; m_behavior->shelter_timer=0.f;
    m_behavior->WeatherRain=false; m_behavior->WeatherNight=false;
    m_behavior->raining=false; m_behavior->night=false;
    m_behavior->RainReaction={}; m_behavior->NightReaction={};
    m_behavior->EscapeGoalPending=false;
    m_behavior->scan_timer=Random.randF(0.f,m_behavior->perception);
    m_behavior->retry_timer=m_behavior->start_delay.sample(); m_behavior->peck_phase=-1.f;
    m_behavior->loaded=false;
    if (m_behavior->debug) Msg("* [scavenger crow] autonomy start: id=%u enabled=%s delay=%.2fs",u32(ID()),
        m_behavior->enabled ? "true" : "false",m_behavior->retry_timer);
}
void CScavengerCrow::stop_crow_behavior()
{
    if (!m_behavior) return;
    CFlyingWorkBudget::Cancel(m_behavior.get());
    m_behavior->SkyAttempt = m_behavior->DepartureAttempt = m_behavior->RecoveryAttempt = m_behavior->ShelterAttempt = 0;
    m_behavior->PerchAttempt[0] = m_behavior->PerchAttempt[1] = 0;
    m_behavior->HaveBlockedSkyGoal = false;
    m_behavior->FoodSearchPending = false;
    m_behavior->FoodAttempt = 0;
    m_behavior->release_food(*this);
    m_behavior->peck_phase=-1.f;
}
void CScavengerCrow::EnsureAirMotion()
{
	auto& Movement = flight_controller();
	if (!m_behavior->enabled || !Local() || !g_Alive() || Movement.active() || Movement.grounded())
	{
		return;
	}
	const auto Status = Movement.status();
	if (Status != CFlyingMovementController::EStatus::Idle && Status != CFlyingMovementController::EStatus::Arrived)
	{
		return;
	}
	const bool Escaping = m_behavior->phase == SBehavior::Phase::Escape || m_behavior->fear >= m_behavior->fear_trigger;
	Fvector Direction = XFORM().k;
	if (Escaping)
	{
		Direction.sub(Position(), m_behavior->danger);
	}
	Movement.SetCruiseContinuity(true, m_behavior->sky_leg_distance);
	Movement.BeginAmbientCruise(*this, Direction, m_behavior->sky_leg_distance);
}
float CScavengerCrow::SpeciesBehaviorStep(float Dt)
{
	auto& Behavior = *m_behavior;
	Behavior.BehaviorTick += Dt;
	const auto Status = flight().status();
	const bool Urgent = (Behavior.fear >= Behavior.fear_trigger && Behavior.phase != SBehavior::Phase::Escape) ||
		Status != Behavior.PreviousTickStatus || Behavior.phase != Behavior.PreviousTickPhase;
	const bool Feeding = Behavior.peck_phase >= 0.f;
	const bool Cruising = Behavior.phase == SBehavior::Phase::Sky || Behavior.phase == SBehavior::Phase::Escape;
	const float Interval = Feeding ? 0.f : Cruising ? .1f : .05f;
	if (!Urgent && Behavior.BehaviorTick < Interval)
	{
		return 0.f;
	}
	const float Step = Behavior.BehaviorTick;
	Behavior.BehaviorTick = 0.f;
	Behavior.PreviousTickStatus = Status;
	Behavior.PreviousTickPhase = Behavior.phase;
	return Step;
}

void CScavengerCrow::update_species_behavior(float dt)
{
	// Reversing a completed cached cruise would send a frightened bird back
	// towards its threat. Escape continues only in an outward direction.
	flight_controller().SetCruiseContinuity(m_behavior->enabled, m_behavior->sky_leg_distance);
    m_behavior->update(*this,dt);
	// A newly online bird has no cached route. Decisions can wait for a shared
	// probe slot, but airborne locomotion must not wait with them.
	EnsureAirMotion();
	OfflineSyncTick += dt;
	if (OfflineSyncTick >= OfflineSyncInterval)
	{
		OfflineSyncTick = fmodf(OfflineSyncTick, OfflineSyncInterval);
		CCrowSharedMemory::Get().Update();
		SyncOfflineCrow();
	}
}

CSE_ALifeScavengerCrow* CScavengerCrow::GetOfflineServer() const
{
	if (!Local() || !ai().get_alife())
	{
		return nullptr;
	}

	// net_Spawn receives a temporary packet entity that cl_Process_Spawn destroys.
	// Resolve the ALife-owned object each time; never retain the packet entity.
	return smart_cast<CSE_ALifeScavengerCrow*>(ai().alife().objects().object(ID(), true));
}

void CScavengerCrow::SyncOfflineCrow()
{
    auto* OfflineServer = GetOfflineServer();
    if (!OfflineServer) return;
    auto& State = OfflineServer->State;
    State.Valid = true;
    State.UpdatedOffline = false;
    State.NeedsPlacement = false;
    State.Enabled = m_behavior->enabled;
    State.Satiety = m_behavior->satiety;
    State.Fear = m_behavior->fear;
    State.Danger = m_behavior->danger;
    State.Remaining = m_behavior->remaining;
    State.Altitude = m_behavior->preferred_height;
    State.Rain = m_behavior->RainReaction;
    State.Night = m_behavior->NightReaction;
    State.Corpse = m_behavior->corpse_id;
    State.Time = ai().alife().time_manager().game_time();
    using ERest = CSE_ALifeScavengerCrow::ERest;
    State.Rest = !flight().grounded() ? ERest::None :
        m_behavior->phase == SBehavior::Phase::Shelter ? ERest::Shelter :
        m_behavior->phase == SBehavior::Phase::NightRest || m_behavior->phase == SBehavior::Phase::NightGround ? ERest::Night :
        m_behavior->phase == SBehavior::Phase::Corpse ? ERest::Corpse :
        m_behavior->phase == SBehavior::Phase::Roof ? ERest::Roof : ERest::Ground;
}

bool CScavengerCrow::RestoreOfflineCrow()
{
    auto* OfflineServer = GetOfflineServer();
    if (!g_Alive() || !OfflineServer || (!OfflineServer->State.UpdatedOffline && !OfflineServer->State.NeedsPlacement) ||
        !OfflineServer->State.Enabled) return true;
    const auto& State = OfflineServer->State;
    auto& Movement = flight_controller();
    // Discard the pre-offline command. Its target can be occupied now.
    Movement.reset();
    const Fvector Predicted = OfflineServer->o_Position;
    float Heading, Pitch, Bank;
    XFORM().getHPB(Heading, Pitch, Bank);
    XFORM().setHPB(Heading, 0.f, 0.f);
    Position() = Predicted;
    const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
    const Fvector Anchor = ai().game_graph().valid_vertex_id(OfflineServer->m_tGraphID) ?
        ai().game_graph().vertex(OfflineServer->m_tGraphID)->level_point() : Predicted;
    bool Placed = false;
    for (u32 Attempt = 0; Attempt < 12u; ++Attempt)
    {
        Fvector Probe = Attempt < 6u ? Predicted : Anchor;
        const float Angle = float(ID()) * 2.39996323f + float(Attempt) * 2.39996323f;
        const float Radius = float(Attempt % 6u) * 2.f;
        Probe.x += cosf(Angle) * Radius;
        Probe.z += sinf(Angle) * Radius;
        if (Probe.x < Bounds.min.x || Probe.x > Bounds.max.x || Probe.z < Bounds.min.z || Probe.z > Bounds.max.z) continue;
        // Cast from above the entire level, so an underground prediction cannot
        // accidentally use the inside/bottom of a building as its support.
        Probe.y = Bounds.max.y + 1.f;
        Fvector Contact, Normal;
        if (!CFlyingMovementController::support(*this, Probe, Bounds.max.y - Bounds.min.y + 2.f, Contact, Normal)) continue;
        Fvector Feet = Contact;
        Feet.y += Movement.ground_offset();
        if (State.Rest != CSE_ALifeScavengerCrow::ERest::None)
        {
            Fvector Above = Feet;
            Above.y += .5f;
            if (!Movement.SpawnSpaceFree(*this, Feet) || !Movement.clear_path(*this, Feet, Above, true)) continue;
            Position() = Feet;
            if (!Movement.restore_landed(*this)) continue;
        }
        else
        {
            Fvector Air = Feet;
            Air.y += State.Altitude;
            if (!Movement.SpawnSpaceFree(*this, Air) || !Movement.clear_path(*this, Feet, Air)) continue;
            Position() = Air;
        }
        Placed = true;
		// Placement already resolved this support; do not queue another home
		// probe before the first native decision after entering online.
		m_behavior->home = Contact;
		m_behavior->home_valid = true;
        break;
    }
    if (!Placed)
    {
        // Water, very steep terrain or occupied perches can reject every local
        // landing. Re-enter in checked open air above the level's geometry,
        // then let the normal scheduler seek actual cover/support below.
        for (u32 Attempt = 0; Attempt < 12u; ++Attempt)
        {
            const float Angle = float(ID()) * 2.39996323f + float(Attempt) * 2.39996323f;
            Fvector Air = Anchor;
            Air.x += cosf(Angle) * float(Attempt);
            Air.z += sinf(Angle) * float(Attempt);
            Air.y = Bounds.max.y + std::max(State.Altitude, Radius() * 2.f + 1.f) + float(Attempt);
            if (Air.x < Bounds.min.x || Air.x > Bounds.max.x || Air.z < Bounds.min.z || Air.z > Bounds.max.z) continue;
            Fvector Above = Air;
            Above.y += 1.f;
            if (!Movement.SpawnSpaceFree(*this, Air) || !Movement.clear_path(*this, Air, Above)) continue;
            Movement.reset();
            Position() = Air;
            Placed = true;
            break;
        }
    }
    if (!Placed)
    {
        Position() = Predicted;
        Msg("! [scavenger crow] no safe online position: id=%u", u32(ID()));
        return false;
    }
    m_behavior->satiety = State.Satiety;
    m_behavior->fear = State.Fear;
    m_behavior->danger = State.Danger;
    m_behavior->remaining = State.Remaining;
    m_behavior->preferred_height = State.Altitude;
    m_behavior->HeightTarget = State.Altitude;
    m_behavior->RainReaction = State.Rain;
    m_behavior->NightReaction = State.Night;
    m_behavior->WeatherRain = State.Rain.Observed;
    m_behavior->WeatherNight = State.Night.Observed;
    m_behavior->raining = State.Rain.Active;
    m_behavior->night = State.Night.Active;
    m_behavior->phase = Movement.grounded() ? SBehavior::Phase::Ground : SBehavior::Phase::Sky;
    m_behavior->previous_position = Position();
    spatial_move();
    // Keep the server position consistent with the collision-checked client
    // position; never export the raw graph anchor as an airborne position.
    OfflineServer->o_Position = Position();
    SyncOfflineCrow();
    return true;
}
float CScavengerCrow::peck_weight() const
{
    if (!m_behavior->enabled || flight().status()!=CFlyingMovementController::EStatus::Landed || m_behavior->peck_phase<0.f)
        return 0.f;
    const float phase=m_behavior->peck_phase;
    const float blend=std::max(0.f,std::min(1.f,std::min(phase,1.f-phase)/.35f));
    return .5f-.5f*cosf(PI*blend);
}
bool CScavengerCrow::peck_surface(const Fvector& beak, Fvector& point)
{
    if (!m_behavior->enabled || m_behavior->peck_phase<0.f) return false;
    if (m_behavior->phase==SBehavior::Phase::Corpse)
    {
        auto* entity=m_behavior->food();
        if (!entity) return false;
        return CrowFoodContact(*this, *entity, beak, point);
    }
    if (m_behavior->phase!=SBehavior::Phase::Ground && m_behavior->phase!=SBehavior::Phase::NightGround) return false;
    Fvector from=beak, normal;
    from.y+=.05f;
    return CFlyingMovementController::support(*this,from,
        flight().ground_offset()+.5f,point,normal);
}
void CScavengerCrow::feel_sound_new(CObject* who,int type,CSound_UserDataPtr,const Fvector& position,float power)
{
    if (!g_Alive() || !Local() || !m_behavior->enabled || who==this || type==int(0xffffffff)) return;
    const bool explosion=(type&SOUND_TYPE_OBJECT_EXPLODING)!=0;
    const float radius=explosion ? m_behavior->explosion_radius : m_behavior->shot_radius;
    const float DistanceSq = Position().distance_to_sqr(position);
    if ((type & (SOUND_TYPE_SHOOTING | SOUND_TYPE_OBJECT_EXPLODING | SOUND_TYPE_BULLET_HIT))==0 ||
        power<m_behavior->noise_power || DistanceSq>=_sqr(radius)) return;
    const float gain=explosion ? m_behavior->fear_explosion_gain : m_behavior->fear_shot_gain;
    // A qualifying shot/explosion is an event, not a repeatedly sampled scan.
    // Keep the gain as a config switch, then latch full panic until it decays.
    if (gain <= 0.f) return;
    m_behavior->fear=1.f;
    m_behavior->danger=position; m_behavior->noise_timer=m_behavior->noise_memory;
    m_behavior->scan_timer=0.f;
}
void CScavengerCrow::save_species(NET_Packet& packet)
{
    SyncOfflineCrow();
    packet.w_u8(m_behavior->enabled); packet.w_float(m_behavior->satiety);
    packet.w_float(m_behavior->safe_timer); packet.w_u8(m_behavior->home_valid); packet.w_vec3(m_behavior->home);
}
void CScavengerCrow::load_species(IReader& reader,u8 version)
{
    if (version<2) return;
	// Keep the old packet field for save compatibility; native policy comes from config.
	reader.r_u8();
	m_behavior->enabled=READ_IF_EXISTS(pSettings,r_bool,cNameSect().c_str(),"crow_ai_enabled",true);
	m_behavior->satiety=reader.r_float();
    m_behavior->safe_timer=reader.r_float(); m_behavior->home_valid=reader.r_u8()!=0;
    reader.r_fvector3(m_behavior->home);
    clamp(m_behavior->satiety,0.f,1.f); m_behavior->loaded=true;
}
void CScavengerCrow::Serialize(ISaveObject& object)
{
    CFlyingMonster::Serialize(object);
    BEGIN_CHUNK(object,"ScavengerCrowBehavior")
    {
        object << m_behavior->enabled << m_behavior->satiety << m_behavior->safe_timer << m_behavior->home_valid;
        object << m_behavior->home.x << m_behavior->home.y << m_behavior->home.z;
        if (!object.IsSave())
		{
			m_behavior->enabled=READ_IF_EXISTS(pSettings,r_bool,cNameSect().c_str(),"crow_ai_enabled",true);
			clamp(m_behavior->satiety,0.f,1.f);
			m_behavior->loaded=true;
		}
    }
}
