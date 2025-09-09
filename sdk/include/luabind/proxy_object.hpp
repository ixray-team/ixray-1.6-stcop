// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#pragma once

namespace luabind
{
	class object;
	LUABIND_API lua_State* GetFuckingLuaStateByObject(object** pObj);

	namespace detail
	{
		class proxy_object;
		class proxy_raw_object;
		class proxy_array_object;

		template<class T>
		void convert_to_lua(lua_State* L, const T& v);

		template<int Index, class T, typename... Policies>
		void convert_to_lua_p(lua_State* L, const T& v, const policy_cons<Policies...>);

		template<int Index>
		struct push_args_from_tuple
		{
		private:

            template<typename... Ts>
            static void applyImpl(lua_State* L, const std::tuple<Ts...>&, std::true_type /* Counter at end */)
            {
            }

            template<typename... Ts>
            static void applyImpl(lua_State* L, const std::tuple<Ts...>& x, std::false_type /* Counter not at end */)
            {
                convert_to_lua(L, *std::get<Index - 1>(x));
                push_args_from_tuple<Index + 1>::apply(L, x);
            }

            template<typename... Ts, typename... Policies>
            static void applyImpl(lua_State* L, const std::tuple<Ts...>&, const policy_cons<Policies...>, std::true_type /* Counter at end */)
            {
            }

            template<typename... Ts, typename... Policies>
            static void applyImpl(lua_State* L, const std::tuple<Ts...>& x, const policy_cons<Policies...> p, std::false_type /* Counter not at end */)
            {
                convert_to_lua_p<Index>(L, *std::get<Index - 1>(x), p);
                push_args_from_tuple<Index + 1>::apply(L, x, p);
            }

		public:

			template<typename... Ts, typename... Policies>
			static void apply(lua_State* L, const std::tuple<Ts...>& x, const policy_cons<Policies...> p)
			{
                applyImpl(L, x, p, std::bool_constant<(Index - 1) == sizeof...(Ts)>());
			}

			template<typename... Ts>
			static void apply(lua_State* L, const std::tuple<Ts...>& x) 
			{
                applyImpl(L, x, std::bool_constant<(Index - 1) == sizeof...(Ts)>());
			}
		};

		template<typename... Ts>
		class proxy_caller
		{
            using tuple_t = std::tuple<Ts...>;
		    friend class luabind::object;
		public:

			proxy_caller(luabind::object* o, const tuple_t& args) : m_obj(o), m_args(args), m_called(false){}
			proxy_caller(luabind::object* o, tuple_t&& args) : m_obj(o), m_args(std::move(args)), m_called(false){}

			proxy_caller(const proxy_caller& rhs) : m_obj(rhs.m_obj), m_args(rhs.m_args), m_called(rhs.m_called)
			{
				rhs.m_called = true;
			}

			proxy_caller(proxy_caller&& rhs) : m_obj(rhs.m_obj), m_args(std::move(rhs.m_args)), m_called(rhs.m_called)
			{
				rhs.m_called = true;
			}

			~proxy_caller() LUABIND_DTOR_NOEXCEPT;
			operator object();

			template<typename... Policies>
            luabind::object operator[](const policy_cons<Policies...>);

		private:

			luabind::object* m_obj;
			tuple_t m_args;
			mutable bool m_called;
		};

		class LUABIND_API proxy_object
		{
		    friend class luabind::object;
		    friend class luabind::detail::proxy_array_object;
		    friend class luabind::detail::proxy_raw_object;
		public:

			template<typename T>
			proxy_object& operator=(const T& val)
			{
				//std::cout << "proxy assigment\n";
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				m_key.get(L);
				detail::convert_to_lua(L, val);
				lua_settable(L, -3);
				// pop table
				lua_pop(L, 1);
				return *this;
			}

			template<class T, class Policies>
			void assign(const T& val, const Policies& p)
			{
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				detail::getref(L, m_key.get_ref());
				detail::convert_to_lua_p(L, val, p);
				lua_settable(L, -3);
				// pop table
				lua_pop(L, 1);
			}

			//template<class T>
			//detail::proxy_object operator[](const T& key) const;

			proxy_object& operator=(const object& p);
			proxy_object& operator=(const proxy_object& p);
			proxy_object& operator=(const proxy_raw_object& p);
			proxy_object& operator=(const proxy_array_object& p);

			void swap(const proxy_object& rhs);
			proxy_object* operator->()
			{ return this; }

			operator luabind::object();

			int type() const
			{
				pushvalue();
				detail::stack_pop p(lua_state(), 1);
				return lua_type(lua_state(), -1);
			}

#define LUABIND_PROXY_RAW_AT_BODY											\
			{																										\
				lua_State* L = lua_state();\
				pushvalue();\
				detail::convert_to_lua(L, key);\
				lua_rawget(L, -2);\
				detail::lua_reference ref;\
				ref.set(L);\
				lua_pop(L, 1);\
				return object(L, ref, true);\
			}

			template<class T>
			inline object raw_at(const T& key);

#define LUABIND_PROXY_AT_BODY \
			{\
				lua_State* L = lua_state();\
				pushvalue();\
				detail::convert_to_lua(L, key);\
				lua_gettable(L, -2);\
				detail::lua_reference ref;\
				ref.set(L);\
				lua_pop(L, 1);\
				return object(L, ref, true);\
			}

			template<class T>
			inline object at(const T& key);

			bool is_valid() const { return true; }
			lua_State* lua_state() const;
			void pushvalue() const;
			void set() const;

			// this is a safe substitute for an implicit converter to bool
			typedef void (proxy_object::*member_ptr)() const;
			operator member_ptr() const
			{
				if (is_valid()) return &proxy_object::dummy;
				return nullptr;
			}

		private:

			void dummy() const {}

			proxy_object(luabind::object* o, const lua_reference& key)
				: m_obj(o)
				, m_key(key)
			{
			}

			luabind::object* m_obj;
#pragma warning(push)
#pragma warning(disable:4251)
			detail::lua_reference m_key;
#pragma warning(pop)
		};


		class LUABIND_API proxy_raw_object
		{
		    friend class luabind::object;
		    friend class luabind::detail::proxy_array_object;
		    friend class luabind::detail::proxy_object;
		public:

			template<typename T>
			proxy_raw_object& operator=(const T& val)
			{
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				detail::getref(L, m_key.get_ref());
				detail::convert_to_lua(L, val);
				lua_rawset(L, -3);
				// pop table
				lua_pop(L, 1);
				return *this;
			}

			template<typename T, typename... Policies>
			void assign(const T& val, const policy_cons<Policies...> p)
			{
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				detail::getref(L, m_key.get_ref());
				detail::convert_to_lua_p(L, val, p);
				lua_settable(L, -3);
				// pop table
				lua_pop(L, 1);
			}

			proxy_raw_object& operator=(const object& p);
			proxy_raw_object& operator=(const proxy_object& p);
			proxy_raw_object& operator=(const proxy_raw_object& p);
			proxy_raw_object& operator=(const proxy_array_object& p);
			void swap(const proxy_raw_object& rhs);
			proxy_raw_object* operator->()
			{ return this; }

			operator luabind::object();

			int type() const
			{
				pushvalue();
				detail::stack_pop p(lua_state(), 1);
				return lua_type(lua_state(), -1);
			}

			template<class T>
			inline object raw_at(const T& key);

			template<class T>
			inline object at(const T& key);

			bool is_valid() const { return true; }
			lua_State* lua_state() const;
			void pushvalue() const;
			void set() const;

			// this is a safe substitute for an implicit converter to bool
			typedef void (proxy_raw_object::*member_ptr)() const;
			operator member_ptr() const
			{
				if (is_valid()) return &proxy_raw_object::dummy;
				return nullptr;
			}

		private:

			void dummy() const {}

			proxy_raw_object(luabind::object* o, const lua_reference& key)
				: m_obj(o)
				, m_key(key)
			{
			}

			luabind::object* m_obj;
#pragma warning(push)
#pragma warning(disable:4251)
			detail::lua_reference m_key;
#pragma warning(pop)
		};


		class LUABIND_API proxy_array_object
		{
		    friend class luabind::object;
		    friend class luabind::detail::proxy_object;
		    friend class luabind::detail::proxy_raw_object;
		public:

			template<typename T>
			proxy_array_object& operator=(const T& val)
			{
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				detail::convert_to_lua(L, val);
				lua_rawseti(L, -2, m_key);

				// pops the table
				lua_pop(L, 1);
				return *this;
			}

			template<typename T, typename... Policies>
			void assign(const T& val, const policy_cons<Policies...> p)
			{
				lua_State* L = GetFuckingLuaStateByObject(&m_obj);
				detail::convert_to_lua_p(L, val, p);
				lua_rawseti(L, -2, m_key);
				// pop table
				lua_pop(L, 1);
			}

			proxy_array_object& operator=(const object& p);
			proxy_array_object& operator=(const proxy_object& p);
			proxy_array_object& operator=(const proxy_raw_object& p);
			proxy_array_object& operator=(const proxy_array_object& p);
			void swap(const proxy_array_object& rhs);

			proxy_array_object* operator->()
			{ return this; }
			operator luabind::object();

			int type() const
			{
				pushvalue();
				detail::stack_pop p(lua_state(), 1);
				return lua_type(lua_state(), -1);
			}

#define LUABIND_PROXY_ARRAY_RAW_AT_BODY\
			{\
				pushvalue();\
				detail::convert_to_lua(m_obj->m_state, key);\
				lua_rawget(m_obj->m_state, -2);\
				lua_reference ref;\
				ref.set(m_obj->m_state);\
				lua_pop(m_obj->m_state, 1);\
				return object(m_obj->m_state, ref, true);\
			}

#define LUABIND_PROXY_ARRAY_AT_BODY\
			{\
				pushvalue();\
				detail::convert_to_lua(m_obj->m_state, key);\
				lua_gettable(m_obj->m_state, -2);\
				lua_reference ref;\
				ref.set(m_obj->m_state);\
				lua_pop(m_obj->m_state, 1);\
				return object(m_obj->m_state, ref, true);\
			}

			template<class T>
			inline object at(const T& key);

			template<class T>
			inline object raw_at(const T& key);

			bool is_valid() const { return true; }
			lua_State* lua_state() const;
			void pushvalue() const;
			void set() const;

			// this is a safe substitute for an implicit converter to bool
			typedef void (proxy_array_object::*member_ptr)() const;
			operator member_ptr() const
			{
				if (is_valid()) return &proxy_array_object::dummy;
				return nullptr;
			}

		private:

			void dummy() const {}

			proxy_array_object(luabind::object* o, int key)
				: m_obj(o)
				, m_key(key)
			{}
			luabind::object* m_obj;
			int m_key;
		};

		template<Direction>
		struct primitive_converter;

		struct tuple_object_ref;

	} // detail
}