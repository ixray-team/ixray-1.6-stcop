////////////////////////////////////////////////////////////////////////////
//	Module 		: property_evaluator.h
//	Created 	: 12.03.2004
//  Modified 	: 12.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Property evaluator
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "property_storage.h"
#include "../xrScripts/script_export_space.h"
#include "action_management_config.h"

class CScriptGameObject;

template <typename _object_type>
class CPropertyEvaluator 
{
public:
	_object_type		*m_object;
	CPropertyStorage	*m_storage;
#ifdef LOG_ACTION
	LPCSTR				m_evaluator_name;
#endif

public:
	IC							CPropertyEvaluator	(_object_type *object = 0, LPCSTR evaluator_name = "");
	virtual 					~CPropertyEvaluator	();
	IC		void				init				(_object_type *object, LPCSTR evaluator_name);
	virtual void				setup				(_object_type *object, CPropertyStorage *storage);
	virtual void				Load				(LPCSTR section);
	virtual	bool			evaluate			();
	IC		const bool &property			(const u32 &condition_id) const;

	virtual	void				save				(NET_Packet &packet) {}
	virtual	void				load				(IReader &packet) {}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
typedef CPropertyEvaluator<CScriptGameObject> CScriptPropertyEvaluator;
#include "property_evaluator_inline.h"