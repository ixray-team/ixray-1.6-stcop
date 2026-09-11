#pragma once

namespace xray
{
    class noncopyable 
    {
    public:
        noncopyable() = default;
        virtual ~noncopyable() = default;

    private:
        noncopyable(const noncopyable&) = delete;
        noncopyable& operator=(const noncopyable&) = delete;
    };
}