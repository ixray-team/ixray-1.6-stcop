////////////////////////////////////////////////////////////////////////////
//	Module 		: associative_vector_compare_predicate.h
//	Created 	: 14.10.2005
//  Modified 	: 14.10.2005
//	Author		: Dmitriy Iassenev
//	Description : associative vector compare predicate template class
////////////////////////////////////////////////////////////////////////////

#pragma once

template <
	typename in_key_type,
	typename in_data_type,
	typename in_compare_predicate_type
>
class associative_vector_compare_predicate :
	public in_compare_predicate_type
{
private:
	using inherited = in_compare_predicate_type;

public:
	using _key_type					= in_key_type;
	using _data_type				= in_data_type;
	using _compare_predicate_type	= in_compare_predicate_type;

public:
    typedef std::pair<_key_type, _data_type>						value_type;

public:
	IC						associative_vector_compare_predicate	();
    IC						associative_vector_compare_predicate	(const _compare_predicate_type &compare_predicate);
	IC		bool			operator()								(const _key_type &lhs, const _key_type &rhs) const;
    IC		bool			operator()								(const value_type &lhs, const value_type &rhs) const;
    IC		bool			operator()								(const value_type &lhs, const _key_type &rhs) const;
    IC		bool			operator()								(const _key_type &lhs, const value_type &rhs) const;
};

#include "associative_vector_compare_predicate_inline.h"