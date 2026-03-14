////////////////////////////////////////////////////////////////////////////
//	Module 		: property_storage.h
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Property storage class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "graph_engine_space.h"
#include "../xrScripts/script_export_space.h"

class CPropertyStorage
{
public:
	typedef GraphEngineSpace::CSolverConditionValue		CConditionValue;
	typedef GraphEngineSpace::CSolverConditionStorage	CConditionStorage;

public:
	CConditionStorage			m_storage;

public:
	IC		void				clear			();
	IC		void				set_property	(const u32 &condition_id, const bool &value);
	IC		const bool	&property		(const u32 &condition_id) const;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
#include "property_storage_inline.h"