////////////////////////////////////////////////////////////////////////////
//	Module 		: space_restrictor_wrapper.h
//	Created 	: 28.11.2005
//  Modified 	: 28.11.2005
//	Author		: Dmitriy Iassenev
//	Description : space restrictor wrapper
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "shapedata.h"

class ISE_ALifeSpaceRestrictor;
class ILevelGraph;
class CGraphEngineEditor;

class CSpaceRestrictorWrapper {
private:
	friend struct border_merge_predicate;

public:
	typedef ISE_ALifeSpaceRestrictor			object_type;
	typedef xr_vector<u32>						BORDER;

private:
	object_type				*m_object;
	ILevelGraph				*m_level_graph;
	CGraphEngineEditor			*m_graph_engine;
	BORDER					m_border;
	BORDER					m_internal;
	Fmatrix					m_xform;

private:
			void			clear					();
			void			fill_shape				(const CShapeData::shape_def &data);
			bool			build_border			();
			void			verify_connectivity		();
			bool			inside					(const Fvector &position, float radius = EPS_L) const;
			bool			inside					(u32 level_vertex_id, bool partially_inside, float radius = EPS_L) const;
	IC		ILevelGraph		&level_graph			() const;
	IC		CGraphEngineEditor	&graph_engine			() const;

public:
							CSpaceRestrictorWrapper	(ISE_ALifeSpaceRestrictor *object);
	IC		object_type		&object					() const;
			bool			verify					(ILevelGraph &level_graph, CGraphEngineEditor &graph_engine, bool no_separator_check);
};

#include "space_restrictor_wrapper_inline.h"