////////////////////////////////////////////////////////////////////////////
//	Module 		: script_callback_ex.h
//	Created 	: 06.02.2004
//  Modified 	: 11.01.2005
//	Author		: Sergey Zhemeitsev and Dmitriy Iassenev
//	Description : Script callbacks with return value
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "ai_space.h"
#include "script_engine.h"

IC bool compare_safe(const luabind::object& o1, const luabind::object& o2)
{
    if ((o1.type() == LUA_TNIL) && (o2.type() == LUA_TNIL))
        return (true);

    return (o1 == o2);
}

template <typename _return_type>
class CScriptCallbackEx
{
public:
    typedef _return_type return_type;

private:
    typedef luabind::functor<_return_type> functor_type;
    typedef luabind::object object_type;
    typedef bool (CScriptCallbackEx::* unspecified_bool_type)() const;

protected:
    functor_type m_functor;
    object_type m_object;

private:
    bool empty() const { return (!!m_functor.lua_state()); }

public:
    CScriptCallbackEx() = default;
    CScriptCallbackEx(const CScriptCallbackEx& callback)
    {

        clear();
        *this = callback;
    }

    virtual ~CScriptCallbackEx() = default;

    CScriptCallbackEx& operator=(const CScriptCallbackEx& callback)
    {
        clear();

        if (callback.m_functor.is_valid() && callback.m_functor.lua_state())
            m_functor = callback.m_functor;

        if (callback.m_object.is_valid() && callback.m_object.lua_state())
            m_object = callback.m_object;

        return (*this);
    }

    bool operator==(const CScriptCallbackEx& callback) const
    {
        return compare_safe(m_object, (callback.m_object)) && m_functor == (callback.m_functor);
    }

    bool operator==(const object_type& object) const { return compare_safe(m_object, object); }

    void set(const functor_type& functor)
    {
        clear();
        m_functor = functor;
    }

    void set(const functor_type& functor, const object_type& object)
    {
        clear();

        m_functor = functor;
        m_object = object;
    }

    void clear()
    {
        m_functor.~functor_type();
        new (&m_functor) functor_type();

        m_object.~object_type();
        new (&m_object) object_type();
    }

    operator unspecified_bool_type() const { return (!m_functor.is_valid() ? 0 : &CScriptCallbackEx::empty); }

    template <class... Args>
    _return_type operator()(Args&&... args) const
    {
        try {
            if (m_functor) {
                VERIFY(m_functor.is_valid());
                if (m_object.is_valid()) {
                    VERIFY(m_object.is_valid());
                    return (_return_type)m_functor.operator()(m_object, std::forward<Args>(args)...);
                }
                else
                    return (_return_type)m_functor.operator()(std::forward<Args>(args)...);
            }
        }
#if XRAY_EXCEPTIONS
        catch (luabind::error& e) {
            if (e.state())
                ai().script_engine().print_output(e.state(), "", LUA_ERRRUN);
            else
                ai().script_engine().print_output(ai().script_engine().lua(), "", LUA_ERRRUN);
        }
#endif
        catch (std::exception&) {
            ai().script_engine().print_output(ai().script_engine().lua(), "", LUA_ERRRUN);
        }
        catch (...) {
            const_cast<CScriptCallbackEx<return_type>*>(this)->clear();
        }
        return _return_type();
    }
};