////////////////////////////////////////////////////////////////////////////
//	Module 		: level_path_manager.h
//	Created 	: 02.10.2001
//  Modified 	: 12.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Level path manager inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _VertexEvaluator,\
	typename _vertex_id_type,\
	typename _index_type\
>

#define CLevelManagerTemplate CBasePathManager<ILevelGraph,_VertexEvaluator,_vertex_id_type,_index_type>

TEMPLATE_SPECIALIZATION
IC	CLevelManagerTemplate::CBasePathManager	(CRestrictedObject *object) :
	inherited(object)
{
}

TEMPLATE_SPECIALIZATION
IC	void CLevelManagerTemplate::reinit(const ILevelGraph *graph)
{
	inherited::reinit			(graph);
	SearchDestVertexId = u32(-1);
	PlannedRoute.clear();
	ExpensiveGoals.clear();
	ExpensiveRoute = false;
	HasExpensiveAttempt = false;
	LastCacheCleanupTime = 0;
}

TEMPLATE_SPECIALIZATION
IC	bool CLevelManagerTemplate::actual() const
{
	if (GraphRevision != ai().level_graph().EditorRevision)
	{
		return false;
	}
	return						(inherited::actual(this->m_object->object().ai_location().level_vertex_id(), this->dest_vertex_id()));
}

TEMPLATE_SPECIALIZATION
IC	void CLevelManagerTemplate::build_path	(const _vertex_id_type start_vertex_id, const _vertex_id_type dest_vertex_id)
{
	if (GraphRevision != ai().level_graph().EditorRevision)
	{
		GraphRevision = ai().level_graph().EditorRevision;
		PlannedRoute.clear();
		ExpensiveGoals.clear();
		HasExpensiveAttempt = false;
		ExpensiveRoute = false;
	}
	PROF_EVENT("Build Path/Level Path");
	
	THROW						(ai().level_graph().valid_vertex_id(start_vertex_id) && ai().level_graph().valid_vertex_id(dest_vertex_id));
	
	MaintainPathCache(dest_vertex_id);
	SearchDestVertexId = dest_vertex_id;
	const auto* Parameters = this->evaluator();
	if (Parameters && Parameters->UsePathLimits)
	{
		before_search(start_vertex_id, dest_vertex_id);
		const auto& Graph = ai().level_graph();
		const bool Reused = !PlannedRoute.empty() && PlannedRoute.back() == dest_vertex_id &&
			Graph.SelectPathSegment(PlannedRoute, start_vertex_id, this->m_path, Parameters->MaxPathLength);
		if (Reused)
		{
			this->m_failed = false;
		}
		else
		{
			ExpensiveRoute = false;
			this->m_failed = !Graph.SearchLimited(start_vertex_id, dest_vertex_id, this->m_path,
				Parameters->max_range, Parameters->max_iteration_count, Parameters->max_visited_node_count,
				Parameters->MaxPathLength, &PlannedRoute);
			if (this->m_failed && Graph.HasFourTransitions(dest_vertex_id) &&
				(!HasExpensiveAttempt || Device.dwTimeGlobal - LastExpensiveSearchTime >= 10000) &&
				std::none_of(ExpensiveGoals.begin(), ExpensiveGoals.end(), [&](const auto& Attempt) { return Attempt.VertexId == dest_vertex_id; }))
			{
				// One expanded-budget attempt per destination while its record is retained.
				// Keep a work ceiling even in recovery; meter limits still select the active segment.
				ExpensiveGoals.push_back({dest_vertex_id, Device.dwTimeGlobal});
				LastExpensiveSearchTime = Device.dwTimeGlobal;
				HasExpensiveAttempt = true;
				const bool Found = Graph.SearchLimited(start_vertex_id, dest_vertex_id, this->m_path,
					Parameters->max_range, std::max(65536u, u32(Parameters->max_iteration_count)),
					std::max(65536u, Parameters->max_visited_node_count), Parameters->MaxPathLength, &PlannedRoute);
				this->m_failed = !Found || PlannedRoute.empty() || PlannedRoute.back() != dest_vertex_id;
				ExpensiveRoute = !this->m_failed;
				if (this->m_failed)
				{
					this->m_path.clear();
					PlannedRoute.clear();
				}
			}
		}
		after_search();
		this->m_current_index = u32(-1);
		this->m_intermediate_index = u32(-1);
		this->m_actuality = !this->m_failed;
	}
	else
	{
		ExpensiveRoute = false;
		PlannedRoute.clear();
		inherited::build_path(start_vertex_id, dest_vertex_id);
	}

#ifdef DEBUG
	if (this->failed()) {
		Msg						("! NPC %s couldn't build path from \n~ [%d][%f][%f][%f]\n~ to\n~ [%d][%f][%f][%f]",*this->m_object->object().cName(),start_vertex_id,VPUSH(ai().level_graph().vertex_position(start_vertex_id)),dest_vertex_id,VPUSH(ai().level_graph().vertex_position(dest_vertex_id)));
	}
#endif
}

TEMPLATE_SPECIALIZATION
IC	void CLevelManagerTemplate::before_search			(const _vertex_id_type start_vertex_id, const _vertex_id_type dest_vertex_id)
{
	if (this->m_object) {
		this->m_object->add_border	(start_vertex_id,dest_vertex_id);
		VERIFY					(!this->m_object->applied() || ai().level_graph().is_accessible(start_vertex_id));
		VERIFY					(!this->m_object->applied() || ai().level_graph().is_accessible(dest_vertex_id));
	}
}

TEMPLATE_SPECIALIZATION
IC	void CLevelManagerTemplate::after_search			()
{
	if (this->m_object)
		this->m_object->remove_border();
}

TEMPLATE_SPECIALIZATION
IC	bool CLevelManagerTemplate::check_vertex			(const _vertex_id_type vertex_id) const
{
	return						(inherited::check_vertex(vertex_id) && (!this->m_object || this->object().accessible(vertex_id)));
}

TEMPLATE_SPECIALIZATION
IC	void CLevelManagerTemplate::on_restrictions_change	()
{
	ExpensiveRoute = false;
	PlannedRoute.clear();
	this->m_failed_start_vertex_id	= _vertex_id_type(-1);
	this->m_failed_dest_vertex_id		= _vertex_id_type(-1);
}

TEMPLATE_SPECIALIZATION
IC void CLevelManagerTemplate::MaintainPathCache(u32 ActiveGoal)
{
	const u32 Now = Device.dwTimeGlobal;
	for (auto& Attempt : ExpensiveGoals)
	{
		if (Attempt.VertexId == ActiveGoal)
		{
			Attempt.LastUsed = Now;
		}
	}
	if (Now - LastCacheCleanupTime < 30000)
	{
		return;
	}
	LastCacheCleanupTime = Now;
	const auto OldSize = ExpensiveGoals.size();
	ExpensiveGoals.erase(std::remove_if(ExpensiveGoals.begin(), ExpensiveGoals.end(), [&](const auto& Attempt)
	{
		return Attempt.VertexId != ActiveGoal && Now - Attempt.LastUsed >= 60000;
	}), ExpensiveGoals.end());
	if (OldSize != ExpensiveGoals.size() || (ExpensiveGoals.empty() && ExpensiveGoals.capacity()))
	{
		ExpensiveGoals.shrink_to_fit();
	}
	if ((PlannedRoute.empty() && PlannedRoute.capacity()) || (!PlannedRoute.empty() && PlannedRoute.back() != ActiveGoal))
	{
		xr_vector<u32>().swap(PlannedRoute);
		ExpensiveRoute = false;
	}
}

TEMPLATE_SPECIALIZATION
IC bool CLevelManagerTemplate::IsPartialPath() const
{
	return this->evaluator() && this->evaluator()->UsePathLimits && !this->path().empty() && this->path().back() != SearchDestVertexId;
}

TEMPLATE_SPECIALIZATION
IC bool CLevelManagerTemplate::completed() const
{
	return !IsPartialPath() && inherited::completed();
}

#undef TEMPLATE_SPECIALIZATION
#undef CLevelManagerTemplate
