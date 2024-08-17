////////////////////////////////////////////////////////////////////////////
//	Module 		: script_utility.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of condlist and other stuff based on xr_logic.script
////////////////////////////////////////////////////////////////////////////

#pragma once

class CConfigInfoportion
{
public:
	CConfigInfoportion();
	~CConfigInfoportion();

private:
	float m_fProbability;
};

template <typename ReturnType>
struct CAnyCallable
{
    CAnyCallable(void) : m_any{}, m_argument_count{} {}
    ~CAnyCallable(void) {}

    CAnyCallable(const CAnyCallable<ReturnType>& inst) : m_any{ inst.m_any }, m_argument_count{ inst.m_argument_count } {}

    template <typename F>
    CAnyCallable(F&& func) : m_any(func)
    {
    }

    template <typename... Args>
    ReturnType operator()(Args... arguments) const
    {
        std::function<ReturnType(Args...)> myfunction;

        auto function = (ReturnType(*)(Args...))m_any;
        myfunction = function;

        return std::invoke(myfunction, std::forward<Args>(arguments)...);
    }

    inline u16 getArgumentsCount(void) noexcept { return this->m_argument_count; }

private:
    void* m_any;
    u16 m_argument_count;
};