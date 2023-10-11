// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
#pragma once
# include <typeinfo>
# include <luabind/detail/type_traits.hpp>

namespace luabind 
{

	class type_id
	{
	public:
		constexpr type_id()
			: id(&typeid(null_type))
		{}

		constexpr type_id(std::type_info const& id)
			: id(&id)
		{}

		inline bool operator!=(type_id const& other) const
		{
			return *id != *other.id;
		}

		inline bool operator==(type_id const& other) const
		{
			return *id == *other.id;
		}

		inline bool operator<(type_id const& other) const
		{
			return id->before(*other.id);
		}

		inline size_t hash_code() const
		{
			return id->hash_code();
		}

		inline char const* name() const
		{
			return id->name();
		}

	private:
		std::type_info const* id;
	};

} // namespace luabind