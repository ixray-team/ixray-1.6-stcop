#ifndef TONEMAP_GT7_H
#define TONEMAP_GT7_H

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
        float toeMapped = midPoint * pow(max(x, 0.0f) * rcp(midPoint), toeStrength);
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


#endif
