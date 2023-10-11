// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef LUABIND_MAKE_FUNCTION_081014_HPP
#define LUABIND_MAKE_FUNCTION_081014_HPP

#include <luabind/config.hpp>
#include <luabind/detail/object.hpp>
#include <luabind/detail/call.hpp>
#include <luabind/detail/deduce_signature.hpp>
#include <luabind/detail/format_signature.hpp>

namespace luabind {

	namespace detail
	{
# ifndef LUABIND_NO_EXCEPTIONS
		LUABIND_API void handle_exception_aux(lua_State* L);
# endif

		// MSVC complains about member being sensitive to alignment (C4121)
		// when F is a pointer to member of a class with virtual bases.
# ifdef _MSC_VER
#  pragma pack(push)
#  pragma pack(16)
# endif

		template <class F, class Signature, class InjectorList>
		struct function_object_impl : function_object
		{
			function_object_impl(F f)
				: function_object(&entry_point), f(f)
			{}

			int call(lua_State* L, invoke_context& ctx, const int args) const
			{
				return call_best_match<InjectorList, Signature>(L, *this, ctx, f, args);
			}

			int format_signature(lua_State* L, char const* function, bool concat = true) const
			{
				return detail::format_signature(L, function, Signature(), concat);
			}

			static bool invoke_defer(lua_State* L, function_object_impl* impl, invoke_context& ctx, int& results)
			{
				bool exception_caught = false;

				try {
					results = invoke<InjectorList, Signature>(L, *impl, ctx, impl->f);
				}
				catch(...) {
					exception_caught = true;
#ifndef LUABIND_NO_EXCEPTIONS
					handle_exception_aux(L);
#endif
				}

				return exception_caught;
			}

			static int entry_point(lua_State* L)
			{
				function_object_impl const* impl_const = *(function_object_impl const**)lua_touserdata(L, lua_upvalueindex(1));

				// TODO: Can this be done differently?
				function_object_impl* impl = const_cast<function_object_impl*>(impl_const);
				invoke_context ctx;
				int results = 0;

# ifndef LUABIND_NO_EXCEPTIONS
				bool exception_caught = invoke_defer(L, impl, ctx, results);
				if(exception_caught) lua_error(L);
# else
				results = invoke<InjectorList, Signature>(L, *impl, ctx, impl->f);
# endif
#ifdef XRAY_SCRIPTS_NO_BACKWARDS_COMPATIBILITY
				if(!ctx) {
					ctx.format_error(L, impl);
					lua_error(L);
				}
#endif

				return results;
			}

			F f;
		};

# ifdef _MSC_VER
#  pragma pack(pop)
# endif

		LUABIND_API object make_function_aux(lua_State* L, function_object* impl, bool default_scope = false);
		LUABIND_API void add_overload(object const&, char const*, object const&);

	} // namespace detail

	template <class F, typename... SignatureElements, typename... PolicyInjectors >
	object make_function(lua_State* L, F f, bool default_scope, meta::type_list< SignatureElements... >, meta::type_list< PolicyInjectors... >)
	{
		return detail::make_function_aux(L, luabind_new<detail::function_object_impl<F, meta::type_list< SignatureElements... >, meta::type_list< PolicyInjectors...>>>(f), default_scope);
	}

	template <class F, typename... PolicyInjectors >
    object make_function(lua_State* L, F f, bool default_scope, meta::type_list< PolicyInjectors... >)
	{
		return make_function(L, f, default_scope, deduce_signature_t<F>(), meta::type_list< PolicyInjectors... >());
	}

	template <class F>
	object make_function(lua_State* L, F f, bool default_scope)
	{
		return make_function(L, f, default_scope, deduce_signature_t<F>(), no_policies());
	}

} // namespace luabind

#endif // LUABIND_MAKE_FUNCTION_081014_HPP

