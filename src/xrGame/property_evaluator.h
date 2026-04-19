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

class CScriptGameObject;

template <typename _object_type>
class CPropertyEvaluator
{
public:
	_object_type* m_object;
	CPropertyStorage* m_storage;
#ifdef LOG_ACTION
	const char* m_evaluator_name;
#endif

public:
	CPropertyEvaluator(_object_type* object = 0, const char* evaluator_name = "")
	{
		init(object, evaluator_name);
	}
	virtual ~CPropertyEvaluator() = default;

	IC void init(_object_type* object, const char* evaluator_name);
	IC const bool& property(const u32& condition_id) const;

	virtual	void save(NET_Packet& packet) {}
	virtual	void load(IReader& packet) {}
	virtual void setup(_object_type* object, CPropertyStorage* storage);
	virtual void Load(const char* section);
	virtual	bool evaluate();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
typedef CPropertyEvaluator<CScriptGameObject> CScriptPropertyEvaluator;
#include "property_evaluator_inline.h"