////////////////////////////////////////////////////////////////////////////
//	Module 		: property_evaluator_const.h
//	Created 	: 12.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Property evaluator const
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "property_evaluator.h"

template <typename _object_type>
class CPropertyEvaluatorConst : public CPropertyEvaluator<_object_type> {
protected:
	typedef CPropertyEvaluator<_object_type>		inherited;

protected:
	inherited::_value_type			m_value;

public:
	IC					CPropertyEvaluatorConst	(inherited::_value_type value, LPCSTR evaluator_name = "");
	virtual inherited::_value_type	evaluate				()
	{
		return			(this->m_value);
	}
};


#include "property_evaluator_const_inline.h"