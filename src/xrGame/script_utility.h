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
    CAnyCallable(void) = default;
    ~CAnyCallable(void) = default;

    template <typename F>
    CAnyCallable(F&& func) : CAnyCallable(std::function(func))
    {
    }

    template <typename... Args>
    CAnyCallable(std::function<ReturnType(Args...)> func) : m_any(func), m_argument_count(sizeof...(Args))
    {
    }

    template <typename... Args>
    ReturnType operator()(Args... arguments)
    {
        std::function<ReturnType(Args...)> myfunction;

        try
        {
            myfunction = std::any_cast<std::function<ReturnType(Args...)>>(this->m_any);
        }
        catch (std::bad_any_cast error)
        {
            Msg("[IXRAY][Scripting]: failed to cast the function!");

            return ReturnType();
        }

        return std::invoke(myfunction, std::forward<Args>(arguments)...);
    }

    inline std::uint16_t getArgumentsCount(void) noexcept { return this->m_argument_count; }

private:
    std::any m_any;
    std::uint16_t m_argument_count;
};