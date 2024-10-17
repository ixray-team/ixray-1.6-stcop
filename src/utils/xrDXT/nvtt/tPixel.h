#pragma once

#include <math.h>
#include <memory.h>

#pragma warning(disable:4201)

namespace nv
{
    // modulo value x between [lo,hi]
    // allows value 'hi'
    template<class _Type>
    inline _Type Clamp(const _Type& x, const _Type& lo, const _Type& hi)
    {
        if (x < lo)
            return lo;
        else if (x > hi)
            return hi;
        else
            return x;
    }
}

class rgba_t
{
public:
    union
    {
        unsigned long u;
        unsigned char p[4];
        struct
        {
            unsigned char r;
            unsigned char g;
            unsigned char b;
            unsigned char a;
        };
    };

    rgba_t() {}

    rgba_t(unsigned char _r, unsigned char _g, unsigned char _b, unsigned char _a)
    {
        a = _a;
        r = _r;
        g = _g;
        b = _b;
    }

    unsigned long bgra()
    {
        return (unsigned long)a << 24 | (unsigned long)r << 16 | (unsigned long)g << 8 | (unsigned long)b;
    }

    rgba_t& operator+=(const rgba_t& v) // incrementation by a rgba_t
    {
        r = (unsigned char)nv::Clamp((int)((int)r + (int)v.r), 0, 255);
        g = (unsigned char)nv::Clamp((int)g + (int)v.g, 0, 255);
        b = (unsigned char)nv::Clamp((int)b + (int)v.b, 0, 255);
        a = (unsigned char)nv::Clamp((int)a + (int)v.a, 0, 255);
        return *this;
    }

    rgba_t& operator-=(const rgba_t& v); // decrementation by a rgba_t
    rgba_t& operator*=(const float d); // multiplication by a constant
    rgba_t& operator/=(const float d); // division by a constant

    rgba_t& operator=(const rgba_t& v)
    {
        r = v.r;
        g = v.g;
        b = v.b;
        a = v.a;
        return *this;
    }

    friend rgba_t operator+(const rgba_t& v1, const rgba_t& v2)
    {
        int r = nv::Clamp((int)v1.r + (int)v2.r, 0, 255);
        int g = nv::Clamp((int)v1.g + (int)v2.g, 0, 255);
        int b = nv::Clamp((int)v1.b + (int)v2.b, 0, 255);
        int a = nv::Clamp((int)v1.a + (int)v2.a, 0, 255);
        return rgba_t((unsigned char)r, (unsigned char)g, (unsigned char)b, (unsigned char)a);
    }

    friend rgba_t operator/(const rgba_t& v, float s)
    {
        return rgba_t(
            (unsigned char)(v.r / s),
            (unsigned char)(v.g / s),
            (unsigned char)(v.b / s),
            (unsigned char)(v.a / s));
    }

    friend rgba_t operator/(const rgba_t& v, int s)
    {
        return rgba_t(
            (unsigned char)(v.r / s),
            (unsigned char)(v.g / s),
            (unsigned char)(v.b / s),
            (unsigned char)(v.a / s));
    }

    void set(unsigned char _r, unsigned char _g, unsigned char _b, unsigned char _a)
    {
        r = _r;
        g = _g;
        b = _b;
        a = _a;
    }

    void SetToZero()
    {
        r = g = b = a = 0;
    }
};
