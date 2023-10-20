#pragma once

// maps unsigned 8 bits/channel to D3DCOLOR
constexpr u32 color_argb(u32 a, u32 r, u32 g, u32 b) {
    return ((a & 0xff) << 24) | ((r & 0xff) << 16) | ((g & 0xff) << 8) | (b & 0xff);
}

constexpr u32 color_rgba(u32 r, u32 g, u32 b, u32 a) {
    return color_argb(a, r, g, b);
}

constexpr u32 color_argb_f(float a, float r, float g, float b) {
    s32	 _r = clampr(iFloor(r * 255.f), 0, 255);
    s32	 _g = clampr(iFloor(g * 255.f), 0, 255);
    s32	 _b = clampr(iFloor(b * 255.f), 0, 255);
    s32	 _a = clampr(iFloor(a * 255.f), 0, 255);
    return color_argb(_a, _r, _g, _b);
}

constexpr u32 color_rgba_f(float r, float g, float b, float a) {
    return color_argb_f(a, r, g, b);
}

constexpr u32 color_xrgb(u32 r, u32 g, u32 b) {
    return color_argb(0xff, r, g, b);
}

constexpr u32 color_get_R(u32 rgba) {
    return (((rgba) >> 16) & 0xff);
}

constexpr u32 color_get_G(u32 rgba) {
    return (((rgba) >> 8) & 0xff);
}

constexpr u32 color_get_B(u32 rgba) {
    return ((rgba) & 0xff);
}

constexpr u32 color_get_A(u32 rgba) {
    return ((rgba) >> 24);
}

constexpr u32 subst_alpha(u32 rgba, u32 a) {
    return rgba & ~color_rgba(0, 0, 0, 0xff) | color_rgba(0, 0, 0, a);
}

constexpr u32 bgr2rgb(u32 bgr) {
    return color_rgba(color_get_B(bgr), color_get_G(bgr), color_get_R(bgr), 0);
}

constexpr u32 rgb2bgr(u32 rgb) {
    return bgr2rgb(rgb);
}

struct _color {
public:
    float r = 0;
    float g = 0;
    float b = 0;
    float a = 255.0f;

    constexpr _color() = default;

    constexpr _color(float red, float green, float blue, float alpha = 255.0f)
        : r(red), g(green), b(blue), a(alpha) {
    }

    constexpr explicit _color(u32 rgba_val) {
        constexpr float f = 1.0f / 255.0f;
        a = f * float((rgba_val >> 24) & 0xff);
        r = f * float((rgba_val >> 16) & 0xff);
        g = f * float((rgba_val >> 8) & 0xff);
        b = f * float((rgba_val >> 0) & 0xff);
    }

    _color& operator=(u32 dw) noexcept { return set(dw); }

    _color& operator=(const _color& other) {
        a = other.a;
        r = other.r;
        g = other.g;
        b = other.b;
        return *this;
    }

    constexpr _color& set(u32 dw) {
        constexpr float f = 1.0f / 255.0f;
        a = f * float((dw >> 24) & 0xff);
        r = f * float((dw >> 16) & 0xff);
        g = f * float((dw >> 8) & 0xff);
        b = f * float((dw >> 0) & 0xff);
        return *this;
    };

    constexpr _color& set(float _r, float _g, float _b, float _a) {
        r = _r;
        g = _g;
        b = _b;
        a = _a;
        return *this;
    };

    constexpr _color& set(const _color& dw) {
        r = dw.r;
        g = dw.g;
        b = dw.b;
        a = dw.a;
        return *this;
    };

    inline u32 get() const {
        return color_rgba_f(r, g, b, a);
    }

    IC u32 get_windows() const {
        BYTE _a, _r, _g, _b;
        _a = (BYTE)(a * 255.0f);
        _r = (BYTE)(r * 255.0f);
        _g = (BYTE)(g * 255.0f);
        _b = (BYTE)(b * 255.0f);
        return ((u32)(_a << 24) | (_b << 16) | (_g << 8) | (_r));
    };

    constexpr _color& set_windows(u32 dw) {
        constexpr float f = 1.0f / 255.0f;
        a = f * (float)(BYTE)(dw >> 24);
        b = f * (float)(BYTE)(dw >> 16);
        g = f * (float)(BYTE)(dw >> 8);
        r = f * (float)(BYTE)(dw >> 0);
        return *this;
    };

    constexpr _color& adjust_contrast(float f) { // >1 - contrast will be increased
        r = 0.5f + f * (r - 0.5f);
        g = 0.5f + f * (g - 0.5f);
        b = 0.5f + f * (b - 0.5f);
        return *this;
    };

    constexpr _color& adjust_contrast(const _color& in, float f) { // >1 - contrast will be increased
        r = 0.5f + f * (in.r - 0.5f);
        g = 0.5f + f * (in.g - 0.5f);
        b = 0.5f + f * (in.b - 0.5f);
        return *this;
    };

    constexpr _color& adjust_saturation(float s) {
        // Approximate values for each component's contribution to luminance.
        // Based upon the NTSC standard described in ITU-R Recommendation BT.709.
        float grey = r * 0.2125f + g * 0.7154f + b * 0.0721f;
        r = grey + s * (r - grey);
        g = grey + s * (g - grey);
        b = grey + s * (b - grey);
        return *this;
    };

    constexpr _color& adjust_saturation(const _color& in, float s) {
        // Approximate values for each component's contribution to luminance.
        // Based upon the NTSC standard described in ITU-R Recommendation BT.709.
        float grey = in.r * 0.2125f + in.g * 0.7154f + in.b * 0.0721f;
        r = grey + s * (in.r - grey);
        g = grey + s * (in.g - grey);
        b = grey + s * (in.b - grey);
        return *this;
    };

    constexpr _color& modulate(_color& in) {
        r *= in.r;
        g *= in.g;
        b *= in.b;
        a *= in.a;
        return *this;
    };

    constexpr _color& modulate(const _color& in1, const _color& in2) {
        r = in1.r * in2.r;
        g = in1.g * in2.g;
        b = in1.b * in2.b;
        a = in1.a * in2.a;
        return *this;
    };

    constexpr _color& negative(const _color& in) {
        r = 1.0f - in.r;
        g = 1.0f - in.g;
        b = 1.0f - in.b;
        a = 1.0f - in.a;
        return *this;
    };

    constexpr _color& negative() {
        r = 1.0f - r;
        g = 1.0f - g;
        b = 1.0f - b;
        a = 1.0f - a;
        return *this;
    };

    constexpr _color& sub_rgb(float s) {
        r -= s;
        g -= s;
        b -= s;
        //a=1.0f-a;
        return *this;
    };

    constexpr _color& add_rgb(float s) {
        r += s;
        g += s;
        b += s;
        return *this;
    };

    constexpr _color& add_rgba(float s) {
        r += s;
        g += s;
        b += s;
        a += s;
        return *this;
    };

    constexpr _color& mul_rgba(float s) {
        r *= s;
        g *= s;
        b *= s;
        a *= s;
        return *this;
    };

    constexpr _color& mul_rgb(float s) {
        r *= s;
        g *= s;
        b *= s;
        return *this;
    };

    constexpr _color& mul_rgba(const _color& c, float s) {
        r = c.r * s;
        g = c.g * s;
        b = c.b * s;
        a = c.a * s;
        return *this;
    };

    constexpr _color& mul_rgb(const _color& c, float s) {
        r = c.r * s;
        g = c.g * s;
        b = c.b * s;
        return *this;
    };

    // SQ magnitude
    constexpr float magnitude_sqr_rgb() const {
        return r * r + g * g + b * b;
    }

    // magnitude
    inline float magnitude_rgb() const {
        return _sqrt(magnitude_sqr_rgb());
    }

    constexpr float intensity() const {
        return (r + g + b) / 3.0f;
    }

    // Normalize
    inline _color& normalize_rgb() {
        VERIFY(magnitude_sqr_rgb() > EPS_S);
        return mul_rgb(1.0f / magnitude_rgb());
    }

    inline _color& normalize_rgb(const _color& c) {
        VERIFY(c.magnitude_sqr_rgb() > EPS_S);
        return mul_rgb(c, 1.0f / c.magnitude_rgb());
    }

    constexpr _color& lerp(const _color& c1, const _color& c2, float t) {
        float invt = 1.0f - t;
        r = c1.r * invt + c2.r * t;
        g = c1.g * invt + c2.g * t;
        b = c1.b * invt + c2.b * t;
        a = c1.a * invt + c2.a * t;
        return *this;
    }

    constexpr _color& lerp(const _color& c1, const _color& c2, const _color& c3, float t) {
        if (t > 0.5f) {
            return lerp(c2, c3, t * 2.0f - 1.0f);
        }
        else {
            return lerp(c1, c2, t * 2.0f);
        }
    }

    inline bool similar_rgba(const _color& v, float E = EPS_L) const {
        return _abs(r - v.r) < E && _abs(g - v.g) < E && _abs(b - v.b) < E && _abs(a - v.a) < E;
    };

    inline bool similar_rgb(const _color& v, float E = EPS_L) const {
        return _abs(r - v.r) < E && _abs(g - v.g) < E && _abs(b - v.b) < E;
    };
};

using Fcolor = _color;

inline BOOL _valid(const Fcolor& c) {
    return _valid(c.r) && _valid(c.g) && _valid(c.b) && _valid(c.a);
}
