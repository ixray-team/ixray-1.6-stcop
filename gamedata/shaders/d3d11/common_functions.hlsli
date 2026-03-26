#ifndef common_functions_h_included
#define common_functions_h_included

//	contrast function
float Contrast(float Input, float ContrastPower)
{
    // piecewise contrast function
    bool IsAbovefloat = Input > 0.5f;
    float ToRaise = saturate(2.0f * (IsAbovefloat ? 1.0f - Input : Input));
    float Output = 0.5f * pow(ToRaise, ContrastPower);
    Output = IsAbovefloat ? 1.0f - Output : Output;
    return Output;
}

#ifndef SRGB_GAMMA
#define SRGB_GAMMA 2.2
#endif

#ifndef USE_LEGACY_LIGHT
	#define PushGamma(x) pow(abs(x), SRGB_GAMMA)
	#define PopGamma(x) pow(abs(x), rcp(SRGB_GAMMA))
#else
	#define PushGamma(x) abs(x)
	#define PopGamma(x) abs(x)
#endif

#ifndef USE_CGIM_WHITE_TWEAK
	#define RCP_WHITE_SQR 0.34602f
	#define INV_TONEMAP_COEF_ONE 0.61592f
	#define INV_TONEMAP_COEF_TWO 1.44565f
#else
	#define RCP_WHITE_SQR 0.416233f
	#define INV_TONEMAP_COEF_ONE 0.335068f
	#define INV_TONEMAP_COEF_TWO 1.20125f
#endif

/*------------------------------------------------------------------------------
    Sum of vector components
------------------------------------------------------------------------------*/
float Sum3(float3 v)
{
    return v.x + v.y + v.z;
}

float3 tonemap(float3 rgb, float scale)
{	
	rgb = rgb * scale;
	rgb = rgb * (1.0f + rgb * RCP_WHITE_SQR) * rcp(rgb + 1.0f);
	
	return PopGamma(rgb);
}

float3 detonemap(float3 rgb)
{
	rgb = PushGamma(rgb);
	
	float3 scale = rgb * rgb - INV_TONEMAP_COEF_ONE * rgb + 1.0f;
	rgb = rgb + sqrt(scale) - 1.0f;
	
	return rgb * INV_TONEMAP_COEF_TWO;  
}

void RemapVector(inout float3 View)
{
    float3 ViewPos = abs(View);
    float ViewPosMax = max(ViewPos.x, max(ViewPos.y, ViewPos.z));

    View *= rcp(ViewPosMax);
    View.y = View.y * 2.0 - 1.0;
}

float Curve(float A, float B, float C, float D, float E, float F, float x)
{
    return ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
}

float3 Curve(float A, float B, float C, float D, float E, float F, float3 x)
{
    return ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
}

float3 Uncharted2Tonemap(float3 Color, float A, float B, float C, float D, float E, float F, float WhitePoint)
{
    float P = Curve(A, B, C, D, E, F, WhitePoint);
    float3 U = Curve(A, B, C, D, E, F, Color);
    return U / P;
}

float3 Uncharted2Tonemap(float3 Color)
{
    float A = 0.15f;
    float B = 0.5f;
    float C = 0.1f;
    float D = 0.4f;
    float E = 0.02f;
    float F = 0.3f;
    float WhitePoint = 1.7f;

    return Uncharted2Tonemap(Color, A, B, C, D, E, F, WhitePoint);
}

float3 Tonemap_Uchimura(float3 x, float P, float a, float m, float l, float c, float b)
{
    // Derived constants
    float l0 = (P - m) * l / a;
    float L0 = m - m / a;
    float L1 = m + (1.0f - m) / a;

    float3 L = m + a * (x - m);

    float3 T = m * pow(x / m, c) + b;

    float S0 = m + l0;
    float S1 = m + a * l0;
    float C2 = a * P / (P - S1);

    float3 S = P - (P - S1) * exp(-C2 * (x - S0) / P);

    float3 w0 = 1.0f - smoothstep(0.0f, m, x);
    float3 w2 = step(m + l0, x);
    float3 w1 = 1.0f - w0 - w2;

    return T * w0 + L * w1 + S * w2;
}

float3 Tonemap_Uchimura(float3 color)
{
    const float P = 1.0f;
    const float a = 1.0f;
    const float m = 0.22f;
    const float l = 0.4f;
    const float c = 1.33f;
    const float b = 0.0f;

    return Tonemap_Uchimura(color, P, a, m, l, c, b);
}

// ============================================================================
// GT7 Tone Mapping (HLSL)
// Based on Polyphony Digital sample implementation / SIGGRAPH 2025 slides.
// Input / output space: linear Rec.2020
//
// Notes:
// - 1.0 in framebuffer space == 100 nits physical luminance
// - SDR path assumes GT paper white = 250 nits, then rescales to sRGB 100 nits
// - Uses ICtCp as UCS, same as sample default
// ============================================================================

#ifndef GT7_REFERENCE_LUMINANCE
    #define GT7_REFERENCE_LUMINANCE 100.0f
#endif

#ifndef GT7_SDR_PAPER_WHITE
    #define GT7_SDR_PAPER_WHITE 250.0f
#endif

// ----------------------------------------------------------------------------
// Luminance scale helpers
// ----------------------------------------------------------------------------

float GT7_FrameBufferValueToPhysicalValue(float fbValue)
{
    return fbValue * GT7_REFERENCE_LUMINANCE;
}

float GT7_PhysicalValueToFrameBufferValue(float physical)
{
    return physical / GT7_REFERENCE_LUMINANCE;
}

// ----------------------------------------------------------------------------
// Utility
// ----------------------------------------------------------------------------

float GT7_SmoothStep(float x, float edge0, float edge1)
{
    if (x <= edge0)
        return 0.0f;
    if (x >= edge1)
        return 1.0f;

    float t = (x - edge0) * rcp(edge1 - edge0);
    return t * t * (3.0f - 2.0f * t);
}

float GT7_ChromaCurve(float x, float a, float b)
{
    return 1.0f - GT7_SmoothStep(x, a, b);
}

float3 LinearSRGBToRec2020(float3 color)
{
    // linear sRGB / Rec.709 -> linear Rec.2020
    const float3x3 M =
    {
        0.6274040f, 0.3292820f, 0.0433136f,
        0.0690970f, 0.9195400f, 0.0113612f,
        0.0163916f, 0.0880132f, 0.8955950f
    };

    return mul(M, color);
}

float3 Rec2020ToLinearSRGB(float3 color)
{
    // linear Rec.2020 -> linear sRGB / Rec.709
    const float3x3 M =
    {
        1.6604960f, -0.5876560f, -0.0728403f,
       -0.1245470f,  1.1328950f, -0.0083480f,
       -0.0181540f, -0.1005970f,  1.1187510f
    };

    return mul(M, color);
}

// ----------------------------------------------------------------------------
// ST2084 / PQ
// ----------------------------------------------------------------------------

float GT7_EotfSt2084(float n, float exponentScaleFactor)
{
    n = saturate(n);

    const float m1  = 0.1593017578125f;
    const float m2  = 78.84375f * exponentScaleFactor;
    const float c1  = 0.8359375f;
    const float c2  = 18.8515625f;
    const float c3  = 18.6875f;
    const float pqC = 10000.0f;

    float np = pow(n, 1.0f * rcp(m2));
    float l  = max(np - c1, 0.0f);
    l = l / (c2 - c3 * np);
    l = pow(l, 1.0f * rcp(m1));

    return GT7_PhysicalValueToFrameBufferValue(l * pqC);
}

float GT7_EotfSt2084(float n)
{
    return GT7_EotfSt2084(n, 1.0f);
}

float GT7_InverseEotfSt2084(float v, float exponentScaleFactor)
{
    const float m1  = 0.1593017578125f;
    const float m2  = 78.84375f * exponentScaleFactor;
    const float c1  = 0.8359375f;
    const float c2  = 18.8515625f;
    const float c3  = 18.6875f;
    const float pqC = 10000.0f;

    float physical = GT7_FrameBufferValueToPhysicalValue(v);
    float y = max(physical * rcp(pqC), 0.0f);

    float ym = pow(y, m1);
    return exp2(m2 * (log2(c1 + c2 * ym) - log2(1.0f + c3 * ym)));
}

float GT7_InverseEotfSt2084(float v)
{
    return GT7_InverseEotfSt2084(v, 1.0f);
}

// ----------------------------------------------------------------------------
// ICtCp conversion (linear Rec.2020 <-> ICtCp)
// ----------------------------------------------------------------------------

float3 GT7_RgbToICtCp(float3 rgb)
{
    float l = dot(rgb, float3(1688.0f, 2146.0f,  262.0f)) * 0.000244140625f; //4096
    float m = dot(rgb, float3( 683.0f, 2951.0f,  462.0f)) * 0.000244140625f;
    float s = dot(rgb, float3(  99.0f,  309.0f, 3688.0f)) * 0.000244140625f;

    float lPQ = GT7_InverseEotfSt2084(l);
    float mPQ = GT7_InverseEotfSt2084(m);
    float sPQ = GT7_InverseEotfSt2084(s);

    float I  = (2048.0f * lPQ + 2048.0f * mPQ) * 0.000244140625f;
    float Ct = (6610.0f * lPQ - 13613.0f * mPQ + 7003.0f * sPQ) * 0.000244140625f;
    float Cp = (17933.0f * lPQ - 17390.0f * mPQ - 543.0f * sPQ) * 0.000244140625f;

    return float3(I, Ct, Cp);
}

float3 GT7_ICtCpToRgb(float3 ictcp)
{
    float l = ictcp.x + 0.00860904f * ictcp.y + 0.11103f  * ictcp.z;
    float m = ictcp.x - 0.00860904f * ictcp.y - 0.11103f  * ictcp.z;
    float s = ictcp.x + 0.56003100f * ictcp.y - 0.320627f * ictcp.z;

    float lLin = GT7_EotfSt2084(l);
    float mLin = GT7_EotfSt2084(m);
    float sLin = GT7_EotfSt2084(s);

    float3 rgb;
    rgb.r = max( 3.43661f   * lLin - 2.50645f   * mLin + 0.0698454f * sLin, 0.0f);
    rgb.g = max(-0.79133f   * lLin + 1.98360f   * mLin - 0.1922710f * sLin, 0.0f);
    rgb.b = max(-0.0259499f * lLin - 0.0989137f * mLin + 1.1248600f * sLin, 0.0f);
    return rgb;
}

// ----------------------------------------------------------------------------
// GT Tone Mapping Curve V2
// ----------------------------------------------------------------------------

float GT7_ToneCurveV2(
    float x,
    float peakIntensity,
    float alpha,
    float midPoint,
    float linearSection,
    float toeStrength)
{
    if (x < 0.0f)
        return 0.0f;

    float k  = (linearSection - 1.0f) * rcp(alpha - 1.0f);
    float kA = peakIntensity * linearSection + peakIntensity * k;
    float kB = -peakIntensity * k * exp(linearSection * rcp(k));
    float kC = -1.0f * rcp(k * peakIntensity);

    float weightLinear = GT7_SmoothStep(x, 0.0f, midPoint);
    float weightToe    = 1.0f - weightLinear;

    float shoulder = kA + kB * exp(x * kC);

    if (x < linearSection * peakIntensity)
    {
        float toeMapped = midPoint * pow(x * rcp(midPoint), toeStrength);
        return weightToe * toeMapped + weightLinear * x;
    }
    else
    {
        return shoulder;
    }
}

float3 GT7_ToneCurveV2(
    float3 x,
    float peakIntensity,
    float alpha,
    float midPoint,
    float linearSection,
    float toeStrength)
{
    return float3(
        GT7_ToneCurveV2(x.r, peakIntensity, alpha, midPoint, linearSection, toeStrength),
        GT7_ToneCurveV2(x.g, peakIntensity, alpha, midPoint, linearSection, toeStrength),
        GT7_ToneCurveV2(x.b, peakIntensity, alpha, midPoint, linearSection, toeStrength)
    );
}

// ----------------------------------------------------------------------------
// GT7 Tonemap core
// ----------------------------------------------------------------------------

float3 GT7Tonemap(
    float3 color,
    float peakNits,
    float sdrCorrectionFactor,
    float blendRatio,
    float fadeStart,
    float fadeEnd)
{
    // GT7 sample params
    const float alpha         = 0.25f;
    const float grayPoint     = 0.538f;
    const float linearSection = 0.444f;
    const float toeStrength   = 1.280f;

    float framebufferLuminanceTarget = GT7_PhysicalValueToFrameBufferValue(peakNits);

    float3 targetUcs = GT7_RgbToICtCp(framebufferLuminanceTarget.xxx);
    float framebufferLuminanceTargetUcs = targetUcs.x;

    // Original color in UCS
    float3 ucs = GT7_RgbToICtCp(color);

    // Step 1: per-channel twisted color
    float3 skewedRgb = GT7_ToneCurveV2(
        color,
        framebufferLuminanceTarget,
        alpha,
        grayPoint,
        linearSection,
        toeStrength);

    // Luminance from twisted color
    float3 skewedUcs = GT7_RgbToICtCp(skewedRgb);

    // Step 2/3: preserve original chroma, but fade it in highlights
    float chromaScale = GT7_ChromaCurve(
        ucs.x * rcp(framebufferLuminanceTargetUcs),
        fadeStart,
        fadeEnd);

    float3 scaledUcs = float3(
        skewedUcs.x,
        ucs.y * chromaScale,
        ucs.z * chromaScale);

    float3 scaledRgb = GT7_ICtCpToRgb(scaledUcs);

    // Step 4: blend twisted and untwisted results
    float3 blended = lerp(skewedRgb, scaledRgb, blendRatio);

    // Output clamp + SDR correction
    return sdrCorrectionFactor * min(blended, framebufferLuminanceTarget.xxx);
}

// ----------------------------------------------------------------------------
// Convenient overloads
// ----------------------------------------------------------------------------

float3 GT7Tonemap(float3 color, float peakNits)
{
    // HDR path
    const float sdrCorrectionFactor = 1.0f;
    const float blendRatio = 0.6f;
    const float fadeStart  = 0.98f;
    const float fadeEnd    = 1.16f;

    return GT7Tonemap(
        color,
        peakNits,
        sdrCorrectionFactor,
        blendRatio,
        fadeStart,
        fadeEnd);
}

float3 GT7Tonemap(float3 color)
{
    // SDR path:
    // GT paper white = 250 nits, then scale back to sRGB white (100 nits)
    const float peakNits = GT7_SDR_PAPER_WHITE;
    const float sdrCorrectionFactor = 1.0f * rcp(GT7_PhysicalValueToFrameBufferValue(GT7_SDR_PAPER_WHITE));
    const float blendRatio = 0.6f;
    const float fadeStart  = 0.98f;
    const float fadeEnd    = 1.16f;

    return GT7Tonemap(
        color,
        peakNits,
        sdrCorrectionFactor,
        blendRatio,
        fadeStart,
        fadeEnd);
}

float3 Crossfeed(float3 rgb, float factor)
{
    float a = 1.f - factor;
    float b = factor * 0.5f;
    return float3(
        rgb.r * a + (rgb.g + rgb.b) * b,
        rgb.g * a + (rgb.b + rgb.r) * b,
        rgb.b * a + (rgb.r + rgb.g) * b);
}

float3 Vibrance(float3 rgb, float vibrance)
{
    float lum = dot(rgb, LUMINANCE_VECTOR);
    float3 mask = (rgb - lum.xxx);
    mask = saturate(mask);
    float lumMask = dot(LUMINANCE_VECTOR, mask);
    lumMask = 1.0 - lumMask;
    return lerp(lum.xxx, rgb, 1.0 + vibrance * lumMask);
}

float3 b_remap(float3 color, float2 threshold)
{
    float thres1 = min(threshold.x, threshold.y);
    float thres2 = max(threshold.x, threshold.y);
    float brightness = (color.r + color.g + color.b) / 3.0;
    float factor = smoothstep(thres1, thres2, brightness);
    return color * factor;
}

// Функции генерации случайных чисел [0, 1]
// START
/*------------------------------------------------------------------------------
    Interleaved Gradient Noise style hash
    Procedural cheap scalar noise in [0..1]
------------------------------------------------------------------------------*/
float IGN(float2 p)
{
    /*----------------------------------------------------------------------
        Procedural scalar hash.
        Can be replaced later with texture-based noise if needed.
    ----------------------------------------------------------------------*/
    float h = dot(p, float2(0.06711056f, 0.00583715f));
    return frac(52.9829189f * frac(h));
}

float Hash(float n)
{
    return frac(sin(n) * 43758.5453123f);
}

float Hash(float2 n)
{
    return Hash(Hash(n.x) + n.y);
}

float Hash(uint2 n)
{
    return Hash(float2(n.x, n.y));
}

float Hash(float3 n)
{
    return Hash(Hash(dot(n.xy, float2(12.989, 78.233))) + n.z);
}

float2 Hash2(float s)
{
    return sin(float2(3.0f, 7.0f) * s);
}

float2 Hash22(float2 value)
{
    return float2(
        Hash(dot(value, float2(12.989, 78.233))),
        Hash(dot(value, float2(39.346, 11.135))));
}

float3 Hash23(float2 value)
{
    return float3(
        Hash(dot(value, float2(12.989, 78.233))),
        Hash(dot(value, float2(39.346, 11.135))),
        Hash(dot(value, float2(73.156, 52.235))));
}

float2 Hash32(float3 value)
{
    return float2(
        Hash(dot(value, float3(12.989, 78.233, 123.134f))),
        Hash(dot(value, float3(39.346, 11.135, 543.142f))));
}

float3 Hash33(float3 value)
{
    return float3(
        Hash(dot(value, float3(12.989, 78.233, 123.134f))),
        Hash(dot(value, float3(39.346, 11.135, 543.142f))),
        Hash(dot(value, float3(73.156, 52.235, 143.425f))));
}

float iqnoise( in float2 p )
{
    uint2 i = uint2(floor( p ));
    float2 f = frac( p );
	
    #if INTERPOLANT==1
    // quintic interpolant
    float2 u = f*f*f*(f*(f*6.0-15.0)+10.0);
    #else
    // cubic interpolant
    float2 u = f*f*(3.0-2.0*f);
    #endif    

    return lerp( lerp(  Hash( i + uint2(0,0) ), 
                        Hash( i + uint2(1,0) ), u.x),
                lerp(   Hash( i + uint2(0,1) ), 
                        Hash( i + uint2(1,1) ), u.x), u.y);
}

// END

float GetBorderAtten(float2 tc, float2 att)
{
    att.x *= pos_decompression_params2.y * pos_decompression_params2.z;
    float2 factors = saturate(min(1.0f - tc, tc) * rcp(att));
    return factors.x * factors.y;
}

bool GetBorderAtten(float2 tc)
{
    float2 factors = min(1.0f - tc, tc);
    return min(factors.x, factors.y) > 0.0f;
}

float GetMaxDirLength(float3 Point, float3 RDir)
{
	float3 FirstPoint = RDir - Point * RDir;
	float3 LastPoint = -Point * RDir;
	
	float3 MaxPoint = max(FirstPoint, LastPoint);
	return min(MaxPoint.x, min(MaxPoint.y, MaxPoint.z));
}

// Hashed Alpha Testing
// The implementation was taken from https://cwyman.org/papers/i3d17_hashedAlpha.pdf document by Chris Wyman and Morgan McGuire
float hashed_alpha_test(float3 position)
{
    if (m_taa_jitter.z < 0.0f)
    {
        return def_aref;
    }
    // Find the discretized derivatives of our coordinates
    float maxDeriv = max(length(ddx(position.xyz)), length(ddy(position.xyz)));
    float pixScale = rcp(def_aref * maxDeriv); // Let's use def_aref as temporary pixel scale
    float pixScaleLog2 = log2(pixScale);

    // Find two nearest log-discretized noise scales
    float2 pixScales = float2(exp2(floor(pixScaleLog2)), exp2(ceil(pixScaleLog2)));

    // Compute alpha thresholds at our two noise scales
    float2 alpha = float2(Hash(floor(pixScales.x * position.xyz)), Hash(floor(pixScales.y * position.xyz)));

    // Factor to interpolate lerp with
    float lerpFactor = frac(log2(pixScale));

    // Interpolate alpha threshold from noise at two scales
    float x = lerp(alpha.x, alpha.y, lerpFactor);

    // Pass into CDF to compute uniformly distrib threshold
    float a = min(lerpFactor, 1.0 - lerpFactor);
    float3 cases;
    cases.x = x * x * rcp(2.0 * a * (1.0 - a));
    cases.y = (x - 0.5 * a) * rcp(1.0 - a);
    cases.z = 1.0 - ((1.0 - x) * (1.0 - x) * rcp(2.0 * a * (1.0 - a)));

    // Find our final, uniformly distributed alpha threshold
    float thresh = (x < (1.0 - a)) ? ((x < a) ? cases.x : cases.y) : cases.z;

    // R1 sequence to animate our noise for TAA/FSR/DLSS
    // Todo: Check if player has enabled TAA/upscaling to enable anim
    thresh = frac(thresh + m_taa_jitter.z);

    // Clamp alpha
    return clamp(thresh, 0.063f, 1.0f);
}

// https://media.steampowered.com/apps/valve/2015/Alex_Vlachos_Advanced_VR_Rendering_GDC2015.pdf
// page 49

#ifndef IMAGE_BITRATE
	#define IMAGE_BITRATE 255
#endif

float3 deband_color(float3 image, float2 uv, float bitrate)
{
    float3 dither = dot(float2(171.0, 231.0), uv.xy + m_taa_jitter.w).xxx;
    dither = 2.0f * frac(dither / float3(103.0, 71.0, 97.0)) - 1.0f;

    return image + dither * rcp(bitrate);
}
float3 deband_color(float3 image, float2 uv)
{
    return deband_color(image, uv, IMAGE_BITRATE);
}

//Builds a cotangent frame. Source: http://www.thetenthplanet.de/archives/1180
void build_contangent_frame(float3 position, float3 normal, float2 uv, out float3 tangent, out float3 binormal)
{
    float4 duv = float4(ddx(uv), ddy(uv));
    float3 dp1perp = cross(normal, ddx(position));
    float3 dp2perp = cross(ddy(position), normal);
	
    tangent = dp2perp * duv.x + dp1perp * duv.z;
    binormal = dp2perp * duv.y + dp1perp * duv.w;
	
    float invmax = rsqrt(max(dot(tangent, tangent), dot(binormal, binormal)));
	
	tangent *= invmax;
	binormal *= invmax;
}

float4 combine_bloom(float3 low, float4 high)
{
    return float4(low.xyz + high.xyz * high.w, 1.f);
}

//#define NEW_FOGGIN
#ifdef NEW_FOGGIN
    #define F_base 1.f
    #define F_dens 0.002f
#endif

float calc_fogging(float3 pos)
{   
    float fog;
    #ifndef NEW_FOGGIN
        fog = saturate(length(pos - eye_position) * fog_params.w + fog_params.x);
        return fog * fog;
    #else // NEW_FOGGIN
        //float a = 1.0f;
        //float b = 0.002f;
        float denom = F_base - exp(-F_dens * (fog_params.z - fog_params.y));
        float dist = length(pos - eye_position);
        return saturate((F_base - exp(-F_dens * (dist - fog_params.y))) / denom);
    #endif
}

float2 unpack_tc_base(float2 tc, float du, float dv)
{
    return (tc.xy + float2(du, dv)) * (32.f / 32768.f); //! Increase from 32bit to 64bit floating point
}

float3 unpack_normal(float3 v)
{
    return 2 * v - 1;
}

float3 unpack_bx2(float3 v)
{
    return 2 * v - 1;
}

float3 unpack_bx4(float3 v)
{
    return 4 * v - 2;
} //! reduce the amount of stretching from 4*v-2 and increase precision

float2 unpack_tc_lmap(float2 tc)
{
    return tc * (1.f / 32768.f);
} // [-1  .. +1 ]

float4 unpack_color(float4 c)
{
    return c.bgra;
}

float4 unpack_D3DCOLOR(float4 c)
{
    return c.bgra;
}

float3 unpack_D3DCOLOR(float3 c)
{
    return c.bgr;
}

float3 p_hemi(float2 tc)
{
    float4 t_lmh = s_hemi.Sample(smp_rtlinear, tc);

#ifdef USE_SOC_LIGHTING
	float r_lmh = (1.0/3.0);
	return dot(t_lmh.xyz, float3(r_lmh, r_lmh, r_lmh));
#else // USE_SOC_LIGHTING
	return t_lmh.w;
#endif // USE_SOC_LIGHTING
}

float get_hemi(float4 lmh)
{
#ifdef USE_SOC_LIGHTING
	float r_lmh = (1.0/3.0);
	return dot(lmh.xyz, float3(r_lmh, r_lmh, r_lmh));
#else // USE_SOC_LIGHTING
	return lmh.w;
#endif // USE_SOC_LIGHTING
}

float get_sun(float4 lmh)
{
#ifdef USE_SOC_LIGHTING
	return lmh.w;
#else // USE_SOC_LIGHTING
	return lmh.y;
#endif // USE_SOC_LIGHTING
}

float3 v_sun(float3 N)
{
    return L_sun_color.xyz * dot(N, -L_sun_dir_w.xyz);
}

float3 calc_reflection(float3 pos_w, float3 norm_w)
{
    return reflect(normalize(pos_w - eye_position), norm_w);
}

#endif //	common_functions_h_included
