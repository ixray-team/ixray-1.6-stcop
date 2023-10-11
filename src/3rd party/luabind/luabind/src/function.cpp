// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)



#include <luabind/function.hpp>
#include <luabind/detail/object.hpp>
#include <luabind/make_function.hpp>
#include <luabind/detail/conversion_policies/conversion_policies.hpp>
#include <luabind/detail/object.hpp>
#include <luabind/lua_extensions.hpp>

namespace luabind {

    bool g_allow_nil_conversion = false;

    LUABIND_API bool is_nil_conversion_allowed()
    {
	    return g_allow_nil_conversion;
    }

    LUABIND_API void allow_nil_conversion(bool allowed)
    {
	    g_allow_nil_conversion = allowed;
    }

	namespace detail {

		namespace {

			int function_destroy(lua_State* L)
			{
				function_object* fn = *(function_object**)lua_touserdata(L, 1);
				luabind_delete(fn);
				return 0;
			}

			void push_function_metatable(lua_State* L)
			{
				lua_pushstring(L, "luabind.function");
				lua_rawget(L, LUA_REGISTRYINDEX);

				if(lua_istable(L, -1))
					return;

				lua_pop(L, 1);

				lua_newtable(L);

				lua_pushstring(L, "__gc");
				lua_pushcclosure(L, &function_destroy, 0);
				lua_rawset(L, -3);

				lua_pushstring(L, "luabind.function");
				lua_pushvalue(L, -2);
				lua_rawset(L, LUA_REGISTRYINDEX);
			}

			// A pointer to this is used as a tag value to identify functions exported
			// by luabind.
			int function_tag = 0;

			// same, but for non-default functions (not from m_default_members)
			int function_tag_ndef = 0;

		} // namespace unnamed

		LUABIND_API bool is_luabind_function(lua_State* L, int index, bool allow_default /*= true*/)
		{
			if(!lua_getupvalue(L, index, 2))
				return false;
			void* tag = lua_touserdata(L, -1);
			bool result = tag == &function_tag && allow_default || tag == &function_tag_ndef;
			lua_pop(L, 1);
			return result;
		}

		namespace
		{

			inline bool is_luabind_function(object const& obj)
			{
				obj.push(obj.interpreter());
				bool result = detail::is_luabind_function(obj.interpreter(), -1);
				lua_pop(obj.interpreter(), 1);
				return result;
			}

		} // namespace unnamed

		LUABIND_API void add_overload(
			object const& context, char const* name, object const& fn)
		{
			function_object* f = *touserdata<function_object*>(std::get<1>(getupvalue(fn, 1)));
			f->name = name;

			if(object overloads = context[name])
			{
				if(is_luabind_function(overloads) && is_luabind_function(fn))
				{
					f->next = *touserdata<function_object*>(std::get<1>(getupvalue(overloads, 1)));
					f->keepalive = overloads;
				}
			}

			context[name] = fn;
		}

		LUABIND_API object make_function_aux(lua_State* L, function_object* impl, bool default_scope /*= false*/)
		{
			void* storage = lua_newuserdata(L, sizeof(function_object*));
			push_function_metatable(L);
			*(function_object**)storage = impl;
			lua_setmetatable(L, -2);

			void* tag = default_scope ? &function_tag : &function_tag_ndef;
			lua_pushlightuserdata(L, tag);
			lua_pushcclosure(L, impl->entry, 2);
			stack_pop pop(L, 1);

			return object(from_stack(L, -1));
		}

		void invoke_context::format_error(
			lua_State* L, function_object const* overloads) const
		{
			char const* function_name =
				overloads->name.empty() ? "<unknown>" : overloads->name.c_str();

			int stacksize = lua_gettop(L);

			if(candidate_index == 0)
			{
				// Overloads
				lua_pushstring(L, "No matching overload found, candidates:\n");
				int count = 0;
				for(function_object const* f = overloads; f != 0; f = f->next)
				{
					if(count != 0)
						lua_pushstring(L, "\n");
					f->format_signature(L, function_name);
					++count;
				}
			}
			else
			{
				// Ambiguous
				lua_pushstring(L, "Ambiguous, candidates:\n");
				for(int i = 0; i < candidate_index; ++i)
				{
					if(i != 0)
						lua_pushstring(L, "\n");
					candidates[i]->format_signature(L, function_name);
				}
			}

			// Print all passed arguments and their types
			lua_pushfstring(L, "\nPassed arguments [%d]: ", stacksize); // Args total cnt
			if (stacksize == 0)
				lua_pushstring(L, "<zero arguments>\n");
			else
			{
				for (int _index = 1; _index <= stacksize; _index++)
				{
					if (_index > 1)
						lua_pushstring(L, ", ");

					// Arg Type
					lua_pushstring(L, lua_typename(L, lua_type(L, _index)));
					// Arg Value
					lua_pushstring(L, " (");
					const char* text = lua52L_tolstring(L, _index, NULL); // automatically pushed to stack
					lua_pushstring(L, ")");
				}
			lua_pushstring(L, "\n");
			}

			lua_concat(L, lua_gettop(L) - stacksize);
		}

	} // namespace detail
} // namespace luabind

