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

XREFLECT(LuaExport, Name="property_storage", Constructible)
class CPropertyStorage
{
public:
	typedef GraphEngineSpace::CSolverConditionValue		CConditionValue;
	typedef GraphEngineSpace::CSolverConditionStorage	CConditionStorage;

public:
	CConditionStorage			m_storage;

public:
	IC		void				clear			();
	XPROPERTY(LuaCallable)
	IC		void				set_property	(const u32 &condition_id, const bool &value);
	XPROPERTY(LuaCallable)
	IC		const bool	&property		(const u32 &condition_id) const;
};
#include "property_storage_inline.h"