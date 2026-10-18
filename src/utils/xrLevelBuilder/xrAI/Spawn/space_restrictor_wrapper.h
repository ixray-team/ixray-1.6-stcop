////////////////////////////////////////////////////////////////////////////
//	Module 		: space_restrictor_wrapper.h
//	Created 	: 28.11.2005
//  Modified 	: 28.11.2005
//	Author		: Dmitriy Iassenev
//	Description : space restrictor wrapper
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "shapedata.h"

class CSE_ALifeSpaceRestrictor;
class CLevelGraph;

class CSpaceRestrictorWrapper {
private:
	friend struct border_merge_predicate;

public:
	typedef CSE_ALifeSpaceRestrictor			object_type;
	typedef xr_vector<u32>						BORDER;

private:
	object_type				*m_object;
	CLevelGraph				*m_level_graph;
	BORDER					m_border;
	BORDER					m_internal;
	Fmatrix					m_xform;

private:
			void			clear					();
			void			fill_shape				(const CShapeData::shape_def &data);
			void			build_border			();
			void			verify_connectivity		();
			bool			inside					(const Fvector &position, float radius = EPS_L) const;
			bool			inside					(u32 level_vertex_id, bool partially_inside, float radius = EPS_L) const;
	IC		CLevelGraph		&level_graph			() const;

public:
							CSpaceRestrictorWrapper	(CSE_ALifeSpaceRestrictor *object);
	IC		object_type		&object					() const;
			void			verify					(CLevelGraph &level_graph, bool no_separator_check);
};

#include "space_restrictor_wrapper_inline.h"