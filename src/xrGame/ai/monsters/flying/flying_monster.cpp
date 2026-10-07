#include "StdAfx.h"
#include "flying_monster.h"
#include "../state_manager.h"
#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "../monster_velocity_space.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../PHMovementControl.h"
#include "../../../movement_manager.h"
#include "../../../moving_object.h"
#include "../../../ai_space.h"
#include "../../../level_graph.h"
#include "../../../sound_player.h"
#include "../../../Level.h"
#include "../../../../Include/xrRender/Kinematics.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"

namespace
{
// The flying subclass owns its AI; this adapter satisfies the inherited lifecycle.
class CFlyingStateAdapter final : public IStateManagerBase
{
public:
    void reinit() override {}
    void update() override {}
    void force_script_state(EMonsterState) override {}
    void execute_script_state() override {}
    void critical_finalize() override {}
    void remove_links(CObject*) override {}
    EMonsterState get_state_type() override { return eStateRest_Idle; }
    bool check_control_start_conditions(ControlCom::EControlType) override { return false; }
};
}

CFlyingMonster::CFlyingMonster()
{
    StateMan = new CFlyingStateAdapter();
}

CFlyingMonster::~CFlyingMonster()
{
    xr_delete(StateMan);
}

void CFlyingMonster::Load(const char* section)
{
    inherited::Load(section);
    m_flight.Load(section);
    m_wander = READ_IF_EXISTS(pSettings, r_bool, section, "flight_free_wander", false);
    m_wander_radius = READ_IF_EXISTS(pSettings, r_float, section, "flight_wander_radius", 40.f);
    m_wander_height = READ_IF_EXISTS(pSettings, r_float, section, "flight_wander_height", 15.f);
    clamp(m_wander_radius, 2.f, 200.f);
    clamp(m_wander_height, 1.f, 100.f);
    auto& idle_velocity = move().get_velocity(MonsterMovement::eVelocityParameterIdle);
    const char* idle = READ_IF_EXISTS(pSettings, r_string, section, "flight_idle_animation", "fly_idle");
    anim().AddAnim(eAnimStandIdle, idle, -1, &idle_velocity, PS_STAND);
    anim().AddAnim(eAnimStandTurnLeft, idle, -1, &idle_velocity, PS_STAND);
    anim().AddAnim(eAnimStandTurnRight, idle, -1, &idle_velocity, PS_STAND);
    for (u32 action = ACT_STAND_IDLE; action <= ACT_HOME_WALK_SMELLING; ++action)
        anim().LinkAction(EAction(action), eAnimStandIdle);
    PostLoad(section);
}

void CFlyingMonster::reinit()
{
    inherited::reinit();
    movement().enable_movement(false);
    m_flight.reset();
    m_last_frame = u32(-1);
    m_flight_blends.clear();
    m_animation_flying = false;
	GroundSpatialCellValid = false;
}

bool CFlyingMonster::net_Spawn(CSE_Abstract* data)
{
    m_loaded_flight_data = false;
    if (!inherited::net_Spawn(data))
        return false;
    movement().enable_movement(false);
    if (g_Alive())
    {
        m_flight.set_model_bounds(BoundingBox());
        if (!m_loaded_flight_data)
        {
            // Console spawns use the ray contact as the object's root. Place
            // the feet on that support before testing the first air route.
            const Fvector original=Position();
            if (m_flight.restore_landed(*this))
            {
                if (m_flight.debug_enabled()) Msg("* [flying] spawn support corrected: id=%u height=%.3f",u32(ID()),Position().y-original.y);
            }
            else
                m_flight.reset(); // Preserve a genuinely airborne spawn.
        }
        // No ground character/gravity may fight with airborne kinematic movement.
        character_physics_support()->movement()->DestroyCharacter();
        character_physics_support()->movement()->SetApplyGravity(false);
        if (!m_loaded_flight_data)
            m_wander_origin = Position();
        m_wander_timer = 0.f;
        auto* skeleton = Visual()->dcast_PKinematicsAnimated();
        m_fly_motion = skeleton->ID_Cycle_Safe(READ_IF_EXISTS(pSettings, r_string,
            cNameSect().c_str(), "flight_animation", ""));
        m_idle_motion = skeleton->ID_Cycle_Safe(READ_IF_EXISTS(pSettings, r_string,
            cNameSect().c_str(), "flight_idle_animation", ""));
        R_ASSERT2(m_fly_motion.valid() && m_idle_motion.valid(), "Flying monster animations are missing");
        play_flight_animation(false);
        restore_command();
    }
    return true;
}

void CFlyingMonster::net_Destroy()
{
    m_flight.stop();
    m_flight_blends.clear();
    inherited::net_Destroy();
}

void CFlyingMonster::RefreshGroundSpatialCell(const Fvector& RegisteredPosition)
{
	GroundSpatialCellValid = false;
	if (!ai().get_level_graph())
	{
		return;
	}
	const auto& Header = ai().level_graph().header();
	const Fbox& Box = Header.box();
	const float MinCellSize = Header.cell_size() * .5f;
	float Distance = std::max(Box.max.x - Box.min.x, Box.max.z - Box.min.z) * .5f;
	if (MinCellSize <= 0.f || Distance <= MinCellSize)
	{
		return;
	}
	// CQuadTree stores point positions in X/Z leaves. Mirror its subdivision
	// boundaries once, so moving inside the same leaf needs no remove/insert.
	const int Depth = std::abs(iFloor(log(2.f * Distance / MinCellSize) / log(2.f) + .5f));
	Fvector Centre;
	Centre.add(Box.min, Box.max).mul(.5f);
	GroundSpatialCell.min.set(-flt_max, -flt_max, -flt_max);
	GroundSpatialCell.max.set(flt_max, flt_max, flt_max);
	for (int Index = 0; Index < Depth; ++Index)
	{
		Distance *= .5f;
		if (RegisteredPosition.x <= Centre.x)
		{
			GroundSpatialCell.max.x = Centre.x;
			Centre.x -= Distance;
		}
		else
		{
			GroundSpatialCell.min.x = Centre.x;
			Centre.x += Distance;
		}
		if (RegisteredPosition.z <= Centre.z)
		{
			GroundSpatialCell.max.z = Centre.z;
			Centre.z -= Distance;
		}
		else
		{
			GroundSpatialCell.min.z = Centre.z;
			Centre.z += Distance;
		}
	}
	GroundSpatialCellValid = true;
}

void CFlyingMonster::spatial_move()
{
	if (getDestroy())
	{
		return;
	}
	if (!g_Alive() || H_Parent())
	{
		GroundSpatialCellValid = false;
		inherited::spatial_move();
		return;
	}
	Fvector Centre;
	Center(Centre);
	const float BoundRadius = Radius();
	auto* MovingObject = get_moving_object();
	const bool RootChanged = MovingObject &&
		(MovingObject->position().x != Position().x || MovingObject->position().y != Position().y || MovingObject->position().z != Position().z);
	const bool SphereChanged = SpatialComponent->sphere.R != BoundRadius ||
		SpatialComponent->sphere.P.x != Centre.x || SpatialComponent->sphere.P.y != Centre.y || SpatialComponent->sphere.P.z != Centre.z;
	if (!RootChanged && !SphereChanged)
	{
		return;
	}
	if (SphereChanged)
	{
		// Exact bounds remain current for rendering and hit queries, including
		// rotation and animation changes. Only redundant updates are skipped.
		SpatialComponent->sphere.P = Centre;
		SpatialComponent->sphere.R = BoundRadius;
	}
	ISpatialOwner::spatial_move();
	if (RootChanged)
	{
		if (!GroundSpatialCellValid)
		{
			RefreshGroundSpatialCell(MovingObject->position());
		}
		// The lower boundary belongs to the previous leaf; the upper one to
		// this leaf, matching CQuadTree::neighbour_index's <= comparisons.
		if (GroundSpatialCellValid &&
			Position().x > GroundSpatialCell.min.x && Position().x <= GroundSpatialCell.max.x &&
			Position().z > GroundSpatialCell.min.z && Position().z <= GroundSpatialCell.max.z)
		{
			MovingObject->update_position();
		}
		else
		{
			MovingObject->on_object_move();
			RefreshGroundSpatialCell(Position());
		}
	}
}

void CFlyingMonster::UpdateCL()
{
	if (!g_Alive())
	{
		inherited::UpdateCL();
		return;
	}
	// Keep ordinary object/spatial/visual updates, bypass the ground movement/control loop.
	CEntityAlive::UpdateCL();
	if (m_last_frame == Device.dwFrame)
		return;
	m_last_frame = Device.dwFrame;
	const float dt = std::min(Device.fTimeDelta, .1f);
	if (!sound().playing_sounds().empty())
	{
		sound().update(dt);
	}
	if (Local())
	{
		{
			m_flight.update(*this, dt);
		}
		const float BehaviorDt = SpeciesBehaviorStep(dt);
		if (BehaviorDt > 0.f)
		{
			update_species_behavior(BehaviorDt);
		}
		if (m_wander)
		{
			m_wander_timer -= dt;
			if (!m_flight.active() && m_wander_timer <= 0.f)
			{
				choose_wander_destination();
			}
		}
	}
	else if (!NET.empty())
	{
		Position().set(NET.back().p_pos);
		const Fvector position = Position();
		XFORM().setHPB(NET.back().o_model, NET.back().o_torso.pitch, NET.back().o_torso.roll);
		Position().set(position);
	}
	float heading, pitch, bank;
	XFORM().getHPB(heading, pitch, bank);
	movement().m_body.current.yaw = heading;
	movement().m_body.current.pitch = pitch;
	movement().m_body.target = movement().m_body.current;
	spatial_move();
	{
		update_flight_animation(dt);
	}
	if (Local())
	{
		if (!m_wander && g_Alive() && !getDestroy())
		{
			{
				m_flight.RecoverFlight(*this, dt);
			}
		}
	}
}

void CFlyingMonster::shedule_Update(u32 dt)
{
    if (!g_Alive())
    {
        inherited::shedule_Update(dt);
        return;
    }
    CEntityAlive::shedule_Update(dt);
    restore_command();
    if (Local())
    {
        net_update next = {};
        next.dwTimeStamp = Level().timeServer();
        next.p_pos = Position();
        XFORM().getHPB(next.o_model, next.o_torso.pitch, next.o_torso.roll);
        next.o_torso.yaw = next.o_model;
        next.fHealth = GetfHealth();
        NET.push_back(next);
        while (NET.size() > 3)
            NET.pop_front();
    }
}

void CFlyingMonster::Die(CObject* who)
{
    m_wander = false;
    m_flight.stop();
    // The inherited physics support owns death/ragdoll from this point onward.
    character_physics_support()->movement()->SetApplyGravity(true);
    inherited::Die(who);
}

void CFlyingMonster::net_Export(NET_Packet& packet)
{
    if (Local() && !NET.empty())
    {
        NET.back().p_pos = Position();
        XFORM().getHPB(NET.back().o_model, NET.back().o_torso.pitch, NET.back().o_torso.roll);
    }
    inherited::net_Export(packet);
}

void CFlyingMonster::play_flight_animation(bool flying)
{
    auto* skeleton = Visual()->dcast_PKinematicsAnimated();
    const MotionID motion = flying ? m_fly_motion : m_idle_motion;
    const u16 partition = skeleton->LL_GetMotionDef(motion)->bone_or_part;
    m_flight_blends.clear();
    m_animation_flying = flying;
    // PlayCycle(motion) returns nullptr for an all-partitions motion, even when
    // playback succeeds. Keep the actual blend of each partition instead.
    for (u16 part = 0; part < MAX_PARTS; ++part)
    {
        if ((partition == BI_NONE || partition == part) && skeleton->partitions().part(part).Name)
        {
            CBlend* blend = skeleton->PlayCycle(part, motion);
            R_ASSERT2(blend, "Could not start flying monster animation partition");
            m_flight_blends.push_back(blend);
        }
    }
    R_ASSERT2(!m_flight_blends.empty(), "Flying monster animation has no skeleton partitions");
}

void CFlyingMonster::update_flight_animation(float)
{
    auto* skeleton = Visual()->dcast_PKinematicsAnimated();
    skeleton->UpdateTracks();
    const bool flying = m_flight.active() && !m_flight.grounded();
    const MotionID motion = flying ? m_fly_motion : m_idle_motion;
    bool restart = m_flight_blends.empty() || flying != m_animation_flying;
    for (CBlend* blend : m_flight_blends)
        restart |= blend->blend_state() != CBlend::eAccrue || blend->motionID != motion;
    if (restart)
        play_flight_animation(flying);
    // A dedicated ground/perch clip can be supplied by the species. Until then freeze
    // the idle pose after contact rather than flapping continuously on the ground.
    const float speed = m_flight.grounded() ?
        0.f : skeleton->LL_GetMotionDef(motion)->Speed();
    for (CBlend* blend : m_flight_blends)
        blend->speed = speed;
}

void CFlyingMonster::choose_wander_destination()
{
    // Optional native random flight for species without a goal planner.
    Fvector target = m_wander_origin;
    target.x += Random.randF(-m_wander_radius, m_wander_radius);
    target.z += Random.randF(-m_wander_radius, m_wander_radius);
    target.y += m_wander_height + Random.randF(-m_wander_height * .25f, m_wander_height * .25f);
    m_flight.fly_to(*this, target, false);
    m_wander_timer = Random.randF(1.f, 3.f);
}

void CFlyingMonster::capture_command()
{
    m_restore_command = m_flight.active() && (!m_flight.grounded() || m_flight.fallback_pending());
    m_restore_landed = m_flight.grounded() && !m_flight.fallback_pending();
    m_restore_landing = m_flight.wants_landing();
    m_restore_destination = m_flight.destination();
}

void CFlyingMonster::restore_command()
{
    if (g_Alive() && Local() && (m_restore_command || m_restore_landed))
    {
        if (m_restore_landed)
            m_flight.restore_landed(*this);
        else
            m_flight.fly_to(*this, m_restore_destination, m_restore_landing);
        m_restore_command = false;
        m_restore_landed = false;
    }
}

void CFlyingMonster::save(NET_Packet& packet)
{
    inherited::save(packet);
    capture_command();
    packet.w_u8(2);
    packet.w_u8(m_restore_command);
    packet.w_u8(m_restore_landing);
    packet.w_u8(m_restore_landed);
    packet.w_vec3(m_restore_destination);
    packet.w_u8(m_wander);
    packet.w_vec3(m_wander_origin);
    save_species(packet);
}

void CFlyingMonster::load(IReader& reader)
{
    inherited::load(reader);
    m_loaded_flight_data = true;
    const u8 version = reader.r_u8();
    R_ASSERT2(version == 1 || version == 2, "Unsupported flying monster save version");
    m_restore_command = reader.r_u8() != 0;
    m_restore_landing = reader.r_u8() != 0;
    m_restore_landed = reader.r_u8() != 0;
    reader.r_fvector3(m_restore_destination);
    m_wander = reader.r_u8() != 0;
    reader.r_fvector3(m_wander_origin);
    load_species(reader, version);
}

void CFlyingMonster::Serialize(ISaveObject& object)
{
    inherited::Serialize(object);
    if (!object.IsSave())
        m_loaded_flight_data = true;
    if (object.IsSave())
        capture_command();
    BEGIN_CHUNK(object, "CFlyingMonster")
    {
        object << m_restore_command << m_restore_landing << m_restore_landed;
        object << m_restore_destination.x << m_restore_destination.y << m_restore_destination.z;
        object << m_wander << m_wander_origin.x << m_wander_origin.y << m_wander_origin.z;
    }
}
