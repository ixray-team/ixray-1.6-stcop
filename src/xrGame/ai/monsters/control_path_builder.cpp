#include "StdAfx.h"
#include "control_path_builder.h"
#include "control_manager.h"
#include "basemonster/base_monster.h"
#include "../../game_location_selector.h"
#include "../../level_location_selector.h"
#include "../../detail_path_manager.h"
#include "../../ai_object_location.h"
#include "../../ai_space.h"
#include "../../movement_manager_space.h"
#include "../../level_path_manager.h"
#include "../../Actor.h"
#include "../../actor_memory.h"
#include "../../visual_memory_manager.h"
#include "../../path_manager_params.h"

#ifdef DEBUG
extern bool show_restrictions(CRestrictedObject *object);
#endif

CControlPathBuilder::CControlPathBuilder(CCreature *monster) : CMovementManager(monster)
{
}

CControlPathBuilder::~CControlPathBuilder()
{
}

void CControlPathBuilder::load(const char* section)
{
}

void CControlPathBuilder::reinit()
{
	// todo: remove call twice [CustomMonster reinit && control_manager reinit]
	inherited::reinit			();
	inherited_com::reinit		();

	m_data.use_dest_orientation		= false;
	m_data.dest_orientation.set		(0.f,0.f,0.f);

	m_data.try_min_time				= true;

	m_data.target_position.set		(0.f,0.f,0.f);
	m_data.target_node				= u32(-1);

	m_data.enable					= false;
	m_data.extrapolate				= false;

	m_data.velocity_mask			= u32(-1);
	m_data.desirable_mask			= u32(-1);

	m_data.path_type				= MovementManager::ePathTypeLevelPath;
	m_data.game_graph_target_vertex	= u32(-1);
	YieldSampling = false;
	YieldActive = false;
	YieldRetryTime = 0;
}

void CControlPathBuilder::update_schedule()
{
	PROF_EVENT("Path Builder/Schedule Update");

	// the one and only reason is because of the restriction-change, so wait until
	// position and node will be in valid state
	if (m_data.path_type != MovementManager::ePathTypePatrolPath)
	{
		if (!accessible(m_data.target_position) || ((m_data.target_node != u32(-1)) && (!accessible(m_data.target_node))))
		{
			return;
		}
	}

	Fvector NavigationTarget = m_data.target_position;
	u32 NavigationVertex = m_data.target_node;
	const bool Yielding = UpdateYieldTarget(NavigationTarget, NavigationVertex);

	if (m_data.reset_actuality && !Yielding)
		make_inactual();

	// set enabled
	enable_movement(m_data.enable);

	// set params only if enable params
	if (m_data.enable)
	{
		detail().set_path_type(eDetailPathTypeSmooth);

		// установить direction
		detail().set_use_dest_orientation(m_data.use_dest_orientation && !Yielding);
		if (m_data.use_dest_orientation)	detail().set_dest_direction(m_data.dest_orientation);

		detail().set_try_min_time(m_data.try_min_time);

		extrapolate_path(m_data.extrapolate);

		detail().set_velocity_mask(m_data.velocity_mask);
		detail().set_desirable_mask(m_data.desirable_mask);

		set_path_type(m_data.path_type);
		if (m_data.path_type == MovementManager::ePathTypeGamePath)
		{
			// check if we have alife task
			if (m_data.game_graph_target_vertex != u32(-1))
			{
				set_game_dest_vertex(GameGraph::_GRAPH_ID(m_data.game_graph_target_vertex));
				game_selector().set_selection_type(eSelectionTypeMask);
			}
			else
			{
				// else just wandering through the game graph
				game_selector().set_selection_type(eSelectionTypeRandomBranching);
			}
		}
		else if (m_data.path_type != MovementManager::ePathTypePatrolPath)
		{
			// set target
			// TODO: make it VERIFY
			if (m_data.target_node == u32(-1)) return;

			detail().set_dest_position(NavigationTarget);
			set_level_dest_vertex(NavigationVertex);
		}
	}

	update_path();

	m_man->notify(ControlCom::eventPathUpdated, 0);
}

bool CControlPathBuilder::UpdateYieldTarget(Fvector& Target, u32& Vertex)
{
	CBaseMonster& Monster = *inherited_com::m_object;
	const u32 Now = Device.dwTimeGlobal;
	const Fvector Position = Monster.Position();
	if (!base_level_params()->UsePathLimits || !m_data.enable || !Monster.g_Alive() ||
		m_data.path_type != MovementManager::ePathTypeLevelPath || Monster.GetScriptControl() ||
		Monster.animation_movement_controlled())
	{
		YieldSampling = false;
		YieldActive = false;
		return false;
	}

	if (YieldActive)
	{
		if (Now - YieldStartTime < 5000 && Position.distance_to_sqr(YieldPosition) > .25f)
		{
			Target = YieldPosition;
			Vertex = YieldVertex;
			return true;
		}
		YieldActive = false;
		YieldSampling = false;
		YieldRetryTime = Now;
		// The control data still contains the original goal; resume it below.
		return false;
	}

	// Do not mistake an idle animation, a path build or an arrival for a blockage.
	if (wait_for_distributed_computation() || !is_moving_on_path() || old_desirable_speed() < .1f ||
		Position.distance_to_sqr(Target) < 1.f)
	{
		YieldSampling = false;
		return false;
	}
	if (!YieldSampling || Position.distance_to_sqr(YieldSamplePosition) > .16f)
	{
		YieldSamplePosition = Position;
		YieldSampleTime = Now;
		YieldSampling = true;
		return false;
	}
	if (Now - YieldSampleTime < 2000 || (YieldRetryTime && Now - YieldRetryTime < 2000))
	{
		return false;
	}
	YieldRetryTime = Now;

	xr_vector<ISpatialShared> Neighbours;
	g_SpatialSpace->q_sphere(Neighbours, 0, ESPATIAL_TYPE::COLLIDEABLE, Position, 6.f);
	CBaseMonster* Blocker = nullptr;
	float Closest = flt_max;
	const float OwnRadius = std::clamp(Monster.Radius(), .35f, 2.f);
	Fvector Forward = Fvector().sub(Target, Position);
	Forward.y = 0.f;
	Forward.normalize_safe();
	for (const ISpatialShared& Spatial : Neighbours)
	{
		CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
		CBaseMonster* Other = Object && !Object->getDestroy() ? Object->cast_base_monster() : nullptr;
		if (!Other || Other == &Monster || !Other->g_Alive() || Other == Monster.EnemyMan.get_enemy() ||
			Other->EnemyMan.get_enemy() == &Monster)
		{
			continue;
		}
		Fvector Offset = Fvector().sub(Other->Position(), Position);
		if (std::abs(Offset.y) > 1.5f)
		{
			continue;
		}
		Offset.y = 0.f;
		const float Distance = Offset.magnitude();
		const float Clearance = OwnRadius + std::clamp(Other->Radius(), .35f, 2.f) + .5f;
		if (Distance > Clearance || Offset.dotproduct(Forward) < -.25f || Distance >= Closest)
		{
			continue;
		}
		// One member of a moving pair yields. An idle creature cannot yield for us.
		if (Monster.ID() < Other->ID() && Other->movement().enabled() && Other->movement().old_desirable_speed() > .1f)
		{
			continue;
		}
		Blocker = Other;
		Closest = Distance;
	}
	if (!Blocker)
	{
		return false;
	}

	Fvector Away = Fvector().sub(Position, Blocker->Position());
	Away.y = 0.f;
	if (Away.square_magnitude() < EPS_L)
	{
		Away.set(-Forward.x, 0.f, -Forward.z);
	}
	Away.normalize_safe();
	const Fvector Side = Fvector().set(-Away.z, 0.f, Away.x);
	const float Step = std::min(OwnRadius + std::clamp(Blocker->Radius(), .35f, 2.f) + 1.f,
		base_level_params()->MaxPathLength);
	const u32 StartVertex = Monster.ai_location().level_vertex_id();
	for (u32 Attempt = 0; Attempt < 4; ++Attempt)
	{
		Fvector Direction = Fvector().mul(Side, Attempt % 2 ? -1.f : 1.f);
		Direction.mad(Away, Attempt < 2 ? .75f : 1.5f);
		Direction.normalize_safe();
		Fvector Candidate = Fvector().mad(Position, Direction, Step);
		const u32 Node = ai().level_graph().check_position_in_direction(StartVertex, Position, Candidate);
		if (!valid_destination(Candidate, Node) || !accessible(Node) || !accessible(Candidate))
		{
			continue;
		}
		Candidate.y = ai().level_graph().vertex_plane_y(Node, Candidate.x, Candidate.z);
		bool Occupied = false;
		for (const ISpatialShared& Spatial : Neighbours)
		{
			CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
			CCreature* Other = Object && !Object->getDestroy() ? Object->cast_creature() : nullptr;
			if (!Other || Other == &Monster || !Other->g_Alive() || std::abs(Other->Position().y - Candidate.y) > 1.5f)
			{
				continue;
			}
			Fvector Offset = Fvector().sub(Candidate, Other->Position());
			Offset.y = 0.f;
			const float Clearance = OwnRadius + std::clamp(Other->Radius(), .35f, 2.f) + .25f;
			if (Offset.square_magnitude() < Clearance * Clearance)
			{
				Occupied = true;
				break;
			}
		}
		if (Occupied)
		{
			continue;
		}
		YieldPosition = Target = Candidate;
		YieldVertex = Vertex = Node;
		YieldStartTime = Now;
		YieldActive = true;
		return true;
	}
	return false;
}

void CControlPathBuilder::on_travel_point_change(const u32 &previous_travel_point_index)
{
	inherited::on_travel_point_change	(previous_travel_point_index);
	m_man->notify	(ControlCom::eventTravelPointChange, 0);
}

void CControlPathBuilder::on_build_path()
{
	m_man->notify	(ControlCom::eventPathBuilt, 0);
}


//////////////////////////////////////////////////////////////////////////
// Special Build Path
//////////////////////////////////////////////////////////////////////////
bool CControlPathBuilder::build_special(const Fvector &target, u32 node, u32 vel_mask)
{
	if (!accessible(target)) return false;
	
	if (node == u32(-1)) {
		// нода в прямой видимости?
		restrictions().add_border(object().Position(), target);
		node = ai().level_graph().check_position_in_direction(object().ai_location().level_vertex_id(),object().Position(),target);
		restrictions().remove_border();

		if (!ai().level_graph().valid_vertex_id(node) || !accessible(node)) 
			return false;
	}

	enable_movement						(true);

	detail().set_velocity_mask			(vel_mask);	
	detail().set_desirable_mask			(vel_mask);

	detail().set_try_min_time			(false); 
	detail().set_use_dest_orientation	(false);

	detail().set_path_type				(eDetailPathTypeSmooth);
	set_path_type						(MovementManager::ePathTypeLevelPath);

	detail().set_dest_position			(target);
	set_level_dest_vertex				(node);

	set_build_path_at_once				();
	update_path							();	

	// check if path built successfully
	if (!path_completed() && (detail().time_path_built() >= Device.dwTimeGlobal))	
		return true;

	return false;
}

//////////////////////////////////////////////////////////////////////////
// Services
//////////////////////////////////////////////////////////////////////////

bool CControlPathBuilder::is_path_end(float dist_to_end)
{
	if (!is_path_built())			return false;
	//if (path_completed())			return true;
	if ( !is_moving_on_path() )
		return true;
	
	u32 cur_point_idx	= detail().curr_travel_point_index();
	u32	path_size		= (u32)detail().path().size();
	if (path_size < 2)
		return true;
	if (cur_point_idx + 1 >= path_size)	
		return true;
	
	// count distance from current object position to the path end
	float cur_dist_to_end = object().Position().distance_to(detail().path()[detail().curr_travel_point_index()+1].position);
	for (u32 i=detail().curr_travel_point_index()+1; i<detail().path().size()-1; i++) {		
		cur_dist_to_end += detail().path()[i].position.distance_to(detail().path()[i+1].position);
		if (cur_dist_to_end > dist_to_end) 
			break;
	}

	if ((cur_dist_to_end < dist_to_end)) 
		return true;
	return false;
}

bool CControlPathBuilder::valid_destination(const Fvector &pos, u32 node)
{
	return (
		ai().level_graph().valid_vertex_id(node) &&
		ai().level_graph().valid_vertex_position(pos) && 
		ai().level_graph().inside(node, pos)
	);
}

bool CControlPathBuilder::valid_and_accessible(Fvector &pos, u32 node)
{
	if (!valid_destination(pos, node) || !accessible(node))	return false;

	fix_position(Fvector().set(pos),node,pos);
	return true;
}



void CControlPathBuilder::fix_position(const Fvector &pos, u32 node, Fvector &res_pos)
{
	VERIFY(accessible(node));
	VERIFY(ai().level_graph().inside(node, pos));

	res_pos.set	(pos);
	res_pos.y	= ai().level_graph().vertex_plane_y(node,res_pos.x,res_pos.z);

	if (!accessible(res_pos)) {
		u32	level_vertex_id = restrictions().accessible_nearest(Fvector().set(res_pos),res_pos);
		
#ifdef DEBUG		
		if (level_vertex_id != node) {
			Msg		("! src_node[%d] res_node[%d] src_pos[%f,%f,%f] res_pos[%f,%f,%f]",node,level_vertex_id,VPUSH(pos),VPUSH(res_pos));
		}
		VERIFY3((level_vertex_id == node) || show_restrictions(m_restricted_object),"Invalid restrictions (see log for details) for object ",*(CControl_Com::m_object->cName()));
#endif
	}
}

bool CControlPathBuilder::is_moving_on_path()
{
	return (!detail().completed(inherited_com::m_object->Position()) && enabled());
}

bool CControlPathBuilder::get_node_in_radius(u32 src_node, float min_radius, float max_radius, u32 attempts, u32 &dest_node)
{
	Fvector vertex_position = ai().level_graph().vertex_position(src_node);

	for (u32 i=0; i<attempts; i++) {
		Fvector			dir;
		dir.random_dir	();
		dir.normalize	();

		Fvector			new_pos;
		new_pos.mad		(vertex_position, dir, Random.randF(min_radius, max_radius));

		restrictions().add_border		(vertex_position,new_pos);
		dest_node		= ai().level_graph().check_position_in_direction(src_node, vertex_position, new_pos);
		restrictions().remove_border	();
		if (dest_node != u32(-1) && accessible(dest_node)) return true;
	}
	return false;
}

void CControlPathBuilder::make_inactual()
{
	// switch once
	enable_movement(!enabled());
	// switch twice
	enable_movement(!enabled());
}

bool CControlPathBuilder::can_use_distributed_computations () const
{
	CActor* pActor = Actor();

	if (pActor == nullptr)
		return true;

	VERIFY(inherited_com::m_object);
	if (pActor->memory().visual().visible_right_now(inherited_com::m_object)) return false;
	return inherited::can_use_distributed_computations();
}

u32	 CControlPathBuilder::find_nearest_vertex				(const u32 &level_vertex_id, const Fvector &target_position, const float &range)
{
	return ai().level_graph().SearchNearestVertex(level_vertex_id,target_position,range);
}

bool CControlPathBuilder::is_path_built()
{
	return actual_all();
}

