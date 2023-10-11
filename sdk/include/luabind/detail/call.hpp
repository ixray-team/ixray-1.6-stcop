// Copyright Daniel Wallin 208.Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
#pragma once

#include <luabind/config.hpp>
#include <typeinfo>
#include <luabind/detail/meta.hpp>
#include <luabind/detail/policy.hpp>
#include <luabind/yield_policy.hpp>
#include <luabind/detail/decorate_type.hpp>
#include <luabind/detail/object.hpp>
#include <tuple>

#pragma warning(push)
#pragma warning(disable: 4251)

namespace luabind::detail
{
	struct invoke_context;

	struct LUABIND_API function_object
	{
		function_object(lua_CFunction entry)
			: entry(entry)
			, next(0)
		{}

		virtual ~function_object()
		{}

		virtual int call(lua_State* L, invoke_context& ctx, const int args) const = 0;
		virtual int format_signature(lua_State* L, char const* function, bool concat = true) const = 0;

		lua_CFunction entry;
		luabind::string name;
		function_object* next;
		object keepalive;
	};

	struct LUABIND_API invoke_context
	{
		invoke_context()
			: best_score((std::numeric_limits<int>::max)())
			//This need to avoid static analyzer's treats
			, candidates{ nullptr,nullptr,nullptr,nullptr,nullptr,
						 nullptr,nullptr,nullptr,nullptr,nullptr }
			, candidate_index(0)
		{}

		operator bool() const
		{
			return candidate_index == 1;
		}

		void format_error(lua_State* L, function_object const* overloads) const;

		int best_score;
		function_object const* candidates[10];	// This looks like it could crash if you provide too many overloads?
		int candidate_index;
	};

	namespace call_detail_new {

		/*
			Compute Stack Indices
			Given the list of argument converter arities, computes the stack indices that each converter addresses.
		*/

		template< typename ConsumedList, unsigned int CurrentSum, unsigned int... StackIndices >
		struct compute_stack_indices;

		template< unsigned int Consumed0, unsigned int... Consumeds, unsigned int CurrentSum, unsigned int... StackIndices >
		struct compute_stack_indices< meta::index_list< Consumed0, Consumeds... >, CurrentSum, StackIndices... >
		{
			using type = typename compute_stack_indices< meta::index_list< Consumeds... >, CurrentSum + Consumed0, StackIndices..., CurrentSum >::type;
		};

		template< unsigned int CurrentSum, unsigned int... StackIndices >
		struct compute_stack_indices< meta::index_list< >, CurrentSum, StackIndices... >
		{
			using type = meta::index_list< StackIndices... >;
		};

		template< typename Foo >
		struct FooFoo {	// Foo!
			enum { consumed_args = Foo::consumed_args };
		};


		template< typename PolicyList, typename StackIndexList >
		struct policy_list_postcall;

		template< typename Policy0, typename... Policies, typename StackIndexList >
		struct policy_list_postcall< meta::type_list< call_policy_injector<Policy0>, Policies... >, StackIndexList > {
			static void postcall(lua_State* L, int results) {
				Policy0::postcall(L, results, StackIndexList());
				policy_list_postcall< meta::type_list< Policies... >, StackIndexList >::postcall(L, results);
			}
		};

		template< typename ConverterPolicy, typename StackIndexList, bool has_postcall >
		struct converter_policy_postcall {
			static void postcall(lua_State* L, int results) {
				ConverterPolicy::postcall(L, results, StackIndexList());
			}
		};

		template< typename ConverterPolicy, typename StackIndexList >
		struct converter_policy_postcall< ConverterPolicy, StackIndexList, false > {
			static void postcall(lua_State* /*L*/, int /*results*/) {
			}
		};

		template< unsigned int Index, typename Policy, typename... Policies, typename StackIndexList >
		struct policy_list_postcall< meta::type_list< converter_policy_injector< Index, Policy >, Policies... >, StackIndexList > {
			static void postcall(lua_State* L, int results) {
				converter_policy_postcall < Policy, StackIndexList, converter_policy_injector< Index, Policy >::has_postcall >::postcall(L, results);
				policy_list_postcall< meta::type_list< Policies... >, StackIndexList >::postcall(L, results);
			}
		};

		template< typename StackIndexList >
		struct policy_list_postcall< meta::type_list< >, StackIndexList > {
			static void postcall(lua_State* /*L*/, int /*results*/) {}
		};

	}

	// VC2013RC doesn't support expanding a template and its member template in one expression, that's why we have to to incrementally build
	// the converter list instead of a single combined expansion.
	template< typename ArgumentList, typename PolicyList, typename CurrentList = meta::type_list<>, unsigned int Counter = 1 >
	struct compute_argument_converter_list;

	template< typename Argument0, typename... Arguments, typename PolicyList, typename... CurrentConverters, unsigned int Counter >
	struct compute_argument_converter_list< meta::type_list<Argument0, Arguments... >, PolicyList, meta::type_list<CurrentConverters...>, Counter >
	{
		using converter_type = typename policy_detail::get_converter_policy<Counter, PolicyList>::type;
		using this_specialized = typename converter_type::template specialize<Argument0, lua_to_cpp >::type;
		using type = typename compute_argument_converter_list<meta::type_list<Arguments...>, PolicyList, meta::type_list<CurrentConverters..., this_specialized>, Counter + 1>::type;
	};

	template<typename PolicyList, typename... CurrentConverters, unsigned int Counter >
	struct compute_argument_converter_list< meta::type_list<>, PolicyList, meta::type_list<CurrentConverters...>, Counter >
	{
		using type = meta::type_list<CurrentConverters...>;
	};

	template< typename ConverterList >
	struct build_consumed_list;

	template< typename... Converters >
	struct build_consumed_list< meta::type_list< Converters... > > 
	{
		using consumed_list = meta::index_list< call_detail_new::FooFoo<Converters>::consumed_args... >;
	};

	template< typename SignatureList, typename PolicyList >
	struct invoke_traits;

	// Specialization for free functions
	template< typename ResultType, typename... Arguments, typename PolicyList >
	struct invoke_traits< meta::type_list<ResultType, Arguments... >, PolicyList >
	{
		using signature_list = meta::type_list<ResultType, Arguments...>;
		using policy_list = PolicyList;
		using result_type = ResultType;
		using result_converter = specialized_converter_policy_n<0, PolicyList, result_type, cpp_to_lua >;
		using argument_list = meta::type_list<Arguments...>;

		using decorated_argument_list = meta::type_list< decorate_type_t<Arguments>... >;
		// note that this is 0-based, so whenever you want to fetch from the converter list, you need to add 1
		using argument_index_list = typename meta::make_index_range< 0, sizeof...(Arguments) >::type;
		using argument_converter_list = typename compute_argument_converter_list<argument_list, PolicyList>::type;
		using argument_converter_tuple_type = typename meta::make_tuple<argument_converter_list>::type;
		using consumed_list = typename build_consumed_list<argument_converter_list>::consumed_list;
		using stack_index_list = typename call_detail_new::compute_stack_indices< consumed_list, 1 >::type;

		enum 
		{ 
			arity = meta::sum<consumed_list>::value 
		};
	};

	template< typename StackIndexList, typename SignatureList, unsigned int End = meta::size<SignatureList>::value, unsigned int Index = 1 >
	struct match_struct 
	{
		template< typename TupleType >
		static int match(lua_State* L, TupleType& tuple)
		{
			const int this_match = std::get<Index - 1>(tuple).match(L, decorate_type_t<typename SignatureList::template at<Index>>(), meta::get<StackIndexList, Index - 1>::value);
			return this_match >= 0 ?	// could also sum them up unconditionally
				this_match + match_struct<StackIndexList, SignatureList, End, Index + 1>::match(L, tuple)
				: no_match;
		}
	};

	template< typename StackIndexList, typename SignatureList, unsigned int Index >
	struct match_struct< StackIndexList, SignatureList, Index, Index >
	{
		template< typename TupleType >
		static int match(lua_State* /*L*/, TupleType&)
		{
			return 0;
		}
	};

	template< typename PolicyList, typename Signature, typename F >
	struct invoke_struct
	{
		using traits = invoke_traits< Signature, PolicyList >;

		template< bool IsMember, bool IsVoid, typename IndexList >
		struct call_struct;

		template< unsigned int... ArgumentIndices >
		struct call_struct< false /*member*/, false /*void*/, meta::index_list<ArgumentIndices...> >
		{
			static void call(lua_State* L, F& f, typename traits::argument_converter_tuple_type& argument_tuple)
			{
				using decorated_list = typename traits::decorated_argument_list;
				using stack_indices = typename traits::stack_index_list;
				using result_converter = typename traits::result_converter;

				result_converter().to_lua(L,
					f((std::get<ArgumentIndices>(argument_tuple).to_cpp(L,
						typename meta::get<decorated_list, ArgumentIndices>::type(),
						meta::get<stack_indices, ArgumentIndices>::value))...
					)
				);

				meta::init_order{
					(std::get<ArgumentIndices>(argument_tuple).converter_postcall(L,
					typename meta::get<typename traits::decorated_argument_list, ArgumentIndices>::type(),
					meta::get<typename traits::stack_index_list, ArgumentIndices>::value), 0)...
				};
			}
		};

		template< unsigned int... ArgumentIndices >
		struct call_struct< false /*member*/, true /*void*/, meta::index_list<ArgumentIndices...> >
		{
			static void call(lua_State* L, F& f, typename traits::argument_converter_tuple_type& argument_tuple)
			{
				using decorated_list = typename traits::decorated_argument_list;
				using stack_indices = typename traits::stack_index_list;

				// This prevents unused warnings with empty parameter lists
				(void)L;

				f(std::get<ArgumentIndices>(argument_tuple).to_cpp(L,
					typename meta::get<decorated_list, ArgumentIndices>::type(),
					meta::get<stack_indices, ArgumentIndices>::value)...

				);

				meta::init_order{
					(std::get<ArgumentIndices>(argument_tuple).converter_postcall(L,
					typename meta::get<typename traits::decorated_argument_list, ArgumentIndices>::type(),
					meta::get<typename traits::stack_index_list, ArgumentIndices>::value), 0)...
				};
			}
		};

		template< unsigned int ClassIndex, unsigned int... ArgumentIndices >
		struct call_struct< true /*member*/, false /*void*/, meta::index_list<ClassIndex, ArgumentIndices...> >
		{
			static void call(lua_State* L, F& f, typename traits::argument_converter_tuple_type& argument_tuple)
			{
				using decorated_list = typename traits::decorated_argument_list;
				using stack_indices = typename traits::stack_index_list;
				using result_converter = typename traits::result_converter;

				auto& object = std::get<0>(argument_tuple).to_cpp(L,
					typename meta::get<typename traits::decorated_argument_list, 0>::type(), 1);

				result_converter().to_lua(L,
					(object.*f)(std::get<ArgumentIndices>(argument_tuple).to_cpp(L,
						typename meta::get<decorated_list, ArgumentIndices>::type(),
						meta::get<stack_indices, ArgumentIndices>::value)...
						)
				);

				meta::init_order{
					(std::get<ArgumentIndices>(argument_tuple).converter_postcall(L,
					typename meta::get<typename traits::decorated_argument_list, ArgumentIndices>::type(),
					meta::get<typename traits::stack_index_list, ArgumentIndices>::value), 0)...
				};
			}
		};

		template< unsigned int ClassIndex, unsigned int... ArgumentIndices >
		struct call_struct< true /*member*/, true /*void*/, meta::index_list<ClassIndex, ArgumentIndices...> >
		{
			static void call(lua_State* L, F& f, typename traits::argument_converter_tuple_type& argument_tuple)
			{
				using decorated_list = typename traits::decorated_argument_list;
				using stack_indices = typename traits::stack_index_list;

				auto& object = std::get<0>(argument_tuple).to_cpp(L, typename meta::get<typename traits::decorated_argument_list, 0>::type(), 1);

				(object.*f)(std::get<ArgumentIndices>(argument_tuple).to_cpp(L,
					typename meta::get<decorated_list, ArgumentIndices>::type(),
					meta::get<stack_indices, ArgumentIndices>::value)...
					);

				meta::init_order{
					(std::get<ArgumentIndices>(argument_tuple).converter_postcall(L,
					typename meta::get<typename traits::decorated_argument_list, ArgumentIndices>::type(),
					meta::get<typename traits::stack_index_list, ArgumentIndices>::value), 0)...
				};
			}
		};

		template< typename TupleType >
		static int call_fun(lua_State* L, invoke_context& ctx, F& f, const int args, TupleType& tuple)
		{
			int results = 0;

			call_struct<
				std::is_member_function_pointer<F>::value,
				std::is_void<typename traits::result_type>::value,
				typename traits::argument_index_list
			>::call(L, f, tuple);

			results = lua_gettop(L) - args;
			if (has_call_policy<PolicyList, yield_policy>::value) {
				results = lua_yield(L, results);
			}

			call_detail_new::policy_list_postcall < PolicyList, typename meta::push_front< typename traits::stack_index_list, meta::index<traits::arity> >::type >::postcall(L, results);

			return results;
		}

		static int call_best_match(lua_State* L, function_object const& self, invoke_context& ctx, F& f, const int args)
		{
			// Even match needs the tuple, since pointer_converters buffer the cast result
			typename traits::argument_converter_tuple_type converter_tuple;

			int score = no_match;
			if (traits::arity == args)
			{
				using struct_type = match_struct< typename traits::stack_index_list, typename traits::signature_list >;
				score = struct_type::match(L, converter_tuple);
			}

			if (score >= 0 && score < ctx.best_score) {
				ctx.best_score = score;
				ctx.candidates[0] = &self;
				ctx.candidate_index = 1;
			}
			else if (score == ctx.best_score) {
				ctx.candidates[ctx.candidate_index++] = &self;
			}

			int results = 0;
			if (self.next)
				results = self.next->call(L, ctx, args);

			if (ctx.best_score == score && ctx.candidate_index == 1)
				results = call_fun(L, ctx, f, args, converter_tuple);

			return results;
		}

		static int invoke(lua_State* L, function_object const& self, invoke_context& ctx, F& f)
		{
			int const arguments = lua_gettop(L);

#ifndef XRAY_SCRIPTS_NO_BACKWARDS_COMPATIBILITY
			if (!self.next)
			{
				// Even match needs the tuple, since pointer_converters buffer the cast result
				typename traits::argument_converter_tuple_type converter_tuple;

				using struct_type = match_struct< typename traits::stack_index_list, typename traits::signature_list >;
				ctx.best_score = struct_type::match(L, converter_tuple);
				ctx.candidates[0] = &self;
				ctx.candidate_index = 1;

				return call_fun(L, ctx, f, arguments, converter_tuple);
			}
#endif

			return call_best_match(L, self, ctx, f, arguments);
		}

	};

	template< typename PolicyList, typename Signature, typename F>
	inline int call_best_match(lua_State* L, function_object const& self, invoke_context& ctx, F& f, const int args)
	{
		return invoke_struct<PolicyList, Signature, F>::call_best_match(L, self, ctx, f, args);
	}

	template< typename PolicyList, typename Signature, typename F>
	inline int invoke(lua_State* L, function_object const& self, invoke_context& ctx, F& f)
	{
		return invoke_struct<PolicyList, Signature, F>::invoke(L, self, ctx, f);
	}
}