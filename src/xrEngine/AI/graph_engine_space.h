////////////////////////////////////////////////////////////////////////////
//	Module 		: graph_engine_space.h
//	Created 	: 21.03.2002
//  Modified 	: 26.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Graph engine
////////////////////////////////////////////////////////////////////////////

#pragma once

template <typename _condition_type, typename _value_type>
class COperatorConditionAbstract;

template <typename _world_property>
class CConditionState;

template <typename _world_property, typename _edge_value_type>
class COperatorAbstract;

template <typename _dist_type, typename _index_type, typename _iteration_type>
struct SBaseParameters;
struct SGameVertex;

namespace GraphEngineSpace
{
	struct CSolverConditionValue
	{
		u32	m_condition;
		bool m_value;

		IC CSolverConditionValue(const u32& condition, bool value)
		{
			m_condition = condition;
			m_value = value;
		}

		IC bool operator==(const u32& condition) const
		{
			return (condition == m_condition);
		}
	};

	using CSolverConditionStorage = xr_vector<CSolverConditionValue>;
	using CWorldProperty = COperatorConditionAbstract<u32, bool>;
	using CWorldState = CConditionState<CWorldProperty>;
	using CWorldOperator = COperatorAbstract<CWorldProperty, u16>;
	using CSolverBaseParameters = SBaseParameters<u16, CWorldState, u32>;
	using CBaseParameters = SBaseParameters<float, u32, u32>;
};
