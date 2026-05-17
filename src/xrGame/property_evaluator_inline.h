////////////////////////////////////////////////////////////////////////////
//	Module 		: property_evaluator_inline.h
//	Created 	: 12.03.2004
//  Modified 	: 12.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Property evaluator inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

#define TEMPLATE_SPECIALIZATION template <typename _object_type>
#define CEvaluator				CPropertyEvaluator<_object_type>

TEMPLATE_SPECIALIZATION
IC	void CEvaluator::init			(_object_type *object, const char* evaluator_name)
{
	m_object			= object;
#ifdef LOG_ACTION
	m_evaluator_name	= evaluator_name;
#endif
	m_storage			= 0;
}

TEMPLATE_SPECIALIZATION
void CEvaluator::setup				(_object_type *object, CPropertyStorage *storage)
{
	m_object			= object;
	m_storage			= storage;
}

TEMPLATE_SPECIALIZATION
void CEvaluator::Load				(const char* section)
{
}

TEMPLATE_SPECIALIZATION
bool CEvaluator::evaluate	()
{
	return				(0);
}

TEMPLATE_SPECIALIZATION
IC const bool& CEvaluator::property(const u32& condition_id) const
{
	VERIFY(m_storage);
	return (m_storage->property(condition_id));
}

#undef TEMPLATE_SPECIALIZATION
#undef CEvaluator