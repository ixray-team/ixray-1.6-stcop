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
    CAnyCallable(void) : m_pFunction{} {}
    ~CAnyCallable(void) {}

    CAnyCallable(const CAnyCallable<ReturnType>& inst) : m_pFunction{ inst.m_pFunction } {}

    template <typename F>
    CAnyCallable(F&& func) : m_pFunction{func}
    {
    }

    template <typename... Args>
    ReturnType operator()(Args... arguments) const
    {
        std::function<ReturnType(Args...)> myfunction;

        auto function = (ReturnType(*)(Args...))m_pFunction;
        myfunction = function;

        return std::invoke(myfunction, std::forward<Args>(arguments)...);
    }

private:
    void* m_pFunction;
};