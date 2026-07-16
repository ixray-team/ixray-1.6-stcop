////////////////////////////////////////////////////////////////////////////
//	Module 		: associative_vector.h
//	Created 	: 14.10.2005
//  Modified 	: 14.10.2005
//	Author		: Dmitriy Iassenev
//	Description : associative vector container
////////////////////////////////////////////////////////////////////////////

#pragma once

template <typename in_key_type, typename in_data_type, typename in_compare_predicate_type>
class associative_vector_compare_predicate :
	public in_compare_predicate_type
{
private:
	using inherited = in_compare_predicate_type;

public:
	using _key_type = in_key_type;
	using _data_type = in_data_type;
	using _compare_predicate_type = in_compare_predicate_type;

public:
	using value_type = std::pair<_key_type, _data_type>;

public:
	associative_vector_compare_predicate() = default;
	associative_vector_compare_predicate(const _compare_predicate_type& compare_predicate)
		: inherited(compare_predicate) {};

	bool operator()(const _key_type& lhs, const _key_type& rhs) const
	{
		return (inherited::operator()(lhs, rhs));
	}

	bool operator()(const value_type& lhs, const value_type& rhs) const
	{
		return (operator()(lhs.first, rhs.first));
	}

	bool operator()(const value_type& lhs, const _key_type& rhs) const
	{
		return (operator()(lhs.first, rhs));
	}

	bool operator()(const _key_type& lhs, const value_type& rhs) const
	{
		return (operator()(lhs, rhs.first));
	}
};

template <typename _key_type, typename _data_type, typename _compare_predicate_type = std::less<_key_type>>
class associative_vector : 
	protected xr_vector<std::pair<_key_type, _data_type>>,
	protected associative_vector_compare_predicate<_key_type, _data_type, _compare_predicate_type>
{
private:
	using self_type = associative_vector<_key_type, _data_type, _compare_predicate_type>;
	using inherited = xr_vector<std::pair<_key_type,_data_type>>;

public:
    using value_compare = associative_vector_compare_predicate<_key_type, _data_type, _compare_predicate_type>;

    using allocator_type = typename inherited::allocator_type;
    using const_pointer = typename inherited::const_pointer;
    using const_reference = typename inherited::const_reference;
    using const_iterator = typename inherited::const_iterator;
    using const_reverse_iterator = typename inherited::const_reverse_iterator;
    using pointer = typename inherited::pointer;
    using reference = typename inherited::reference;
    using iterator = typename inherited::iterator;
    using reverse_iterator = typename inherited::reverse_iterator;
    using difference_type = typename allocator_type::difference_type;
    using key_compare = _compare_predicate_type;
    using key_type = _key_type;
    using mapped_type = _data_type;
    using size_type = typename inherited::size_type;
    using value_type = typename inherited::value_type;
    using insert_result = std::pair<iterator, bool>;
    using equal_range_result = std::pair<iterator, iterator>;
    using const_equal_range_result = std::pair<const_iterator, const_iterator>;

private:
	IC		void						actualize			() const;

public:
	template <typename _iterator_type>
	IC									associative_vector	(_iterator_type first, _iterator_type last, const key_compare &predicate = key_compare(), const allocator_type &allocator = allocator_type());
	IC									associative_vector	(const key_compare &predicate = key_compare(), const allocator_type &allocator = allocator_type());
	IC						explicit	associative_vector	(const key_compare &predicate);
	IC		iterator					begin				();
	IC		iterator					end					();
	IC		reverse_iterator			rbegin				();
	IC		iterator					rend				();
	IC		insert_result				insert				(const value_type &value);
	IC		iterator					insert				(iterator where, const value_type &value);
	template <class _iterator_type>
	IC		void						insert				(_iterator_type first, _iterator_type last);
	IC		void						erase				(iterator element);
	IC		void						erase				(iterator first, iterator last);
	IC		size_type					erase				(const key_type &key);
	IC		void						clear				();
	IC		iterator					find				(const key_type &key);
	IC		iterator					lower_bound			(const key_type &key);
	IC		iterator					upper_bound			(const key_type &key);
	IC		equal_range_result			equal_range			(const key_type &key);
	IC		void						swap				(self_type &object);

public:
	IC		const_iterator				begin				() const;
	IC		const_iterator				end					() const;
	IC		const_reverse_iterator		rbegin				() const;
	IC		const_reverse_iterator		rend				() const;
	IC		const_iterator				find				(const key_type &key) const;
	IC		const_iterator				lower_bound			(const key_type &key) const;
	IC		const_iterator				upper_bound			(const key_type &key) const;
	IC		const_equal_range_result	equal_range			(const key_type &key) const;
	IC		size_type					count				(const key_type &key) const;
	IC		size_type					max_size			() const;
	IC		u32							size				() const;
	IC		bool						empty				() const;
	IC		key_compare					key_comp			() const;
	IC		value_compare				value_comp			() const;
	IC		allocator_type				get_allocator		() const;

	IC const key_type& get_key_at(const size_t index) const
	{
		const inherited* obj_inherited = static_cast<const inherited*>(this);
		return (*obj_inherited)[index].first;
	}

	IC const mapped_type& get_value_at(const size_t index) const
	{
		const inherited* obj_inherited = static_cast<const inherited*>(this);
		return (*obj_inherited)[index].second;
	}

	IC mapped_type& get_value_at(const size_t index)
	{
		inherited* obj_inherited = static_cast<inherited*>(this);
		return (*obj_inherited)[index].second;
	}
public:
	IC		mapped_type					&operator[]			(const key_type &key);
	IC		self_type					&operator=			(const self_type &right);
	IC		bool						operator<			(const self_type &right) const;
	IC		bool						operator<=			(const self_type &right) const;
	IC		bool						operator>			(const self_type &right) const;
	IC		bool						operator>=			(const self_type &right) const;
	IC		bool						operator==			(const self_type &right) const;
	IC		bool						operator!=			(const self_type &right) const;
};

template <typename _key_type, typename _data_type, typename _compare_predicate_type>
IC void swap(associative_vector<_key_type, _data_type, _compare_predicate_type>	&left, associative_vector<_key_type, _data_type, _compare_predicate_type> &right)
{
	left.swap(right);
}

#include "associative_vector_inline.h"