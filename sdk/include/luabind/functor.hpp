#pragma once
#include <type_traits>
#include "object.hpp"
#include "detail/format_signature.hpp"

namespace luabind
{
    template<typename TResult>
    class functor : public adl::object
    {
    public:
        functor() {}
        functor(const adl::object& obj) : adl::object(obj) {}

        template<typename... Args>
        TResult operator()(Args &&...args) const
        {
            return call_function<TResult>(*static_cast<const adl::object*>(this), std::forward<Args>(args)...);
        }
    };

    template<>
    template<typename... Args>
    void functor<void>::operator()(Args &&...args) const
    {
        call_function<void>(*static_cast<const adl::object*>(this), std::forward<Args>(args)...);
    }

    namespace detail
    {
        template<typename T>
        struct type_to_string<functor<T>>
        {
            static void get(lua_State* L)
            {
                lua_pushstring(L, "function<");
                type_to_string<T>::get(L);
                lua_pushstring(L, ">");
                lua_concat(L, 3);
            }
        };
    }

    template<typename T>
    struct default_converter<functor<T>> : native_converter_base<functor<T>>
    {
        static int compute_score(lua_State* luaState, int index)
        {
            return lua_isfunction(luaState, index) || lua_isnil(luaState, index) ? 0 : no_match;
        }

        static functor<T> to_cpp_deferred(lua_State* luaState, int index)
        {
            if (lua_isnil(luaState, index))
                return functor<T>();
            return object(from_stack(luaState, index));
        }

        static void to_lua_deferred(lua_State* luaState, functor<T> const& value)
        {
            value.push(luaState);
        }
    };

    template<typename T>
    struct default_converter<const functor<T>> : default_converter<functor<T>>
    {};

    template<typename T>
    struct default_converter<const functor<T>&> : default_converter<functor<T>>
    {};
}