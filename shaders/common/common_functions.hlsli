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
	#define GammaToLinear(x) pow(abs(x), SRGB_GAMMA)
	#define LinearToGamma(x) pow(abs(x), rcp(SRGB_GAMMA))
#else
	#define GammaToLinear(x) abs(x)
	#define LinearToGamma(x) abs(x)
#endif

// What a pass writes to the scene of a gamma colour, and back: the scene holds linear light in the frame that lights
// by itself and in the forward level frame above low shading quality, whose light model leaves the two above undone
#if !defined(USE_LEGACY_LIGHT) || defined(USE_LINEAR_SCENE)
	#define SceneFromGamma(x) pow(abs(x), SRGB_GAMMA)
	#define SceneToGamma(x) pow(abs(x), rcp(SRGB_GAMMA))
#else
	#define SceneFromGamma(x) abs(x)
	#define SceneToGamma(x) abs(x)
#endif

// What the tone curve's output still needs for the display, and back: the encoding tonemap() leaves undone in the
// forward level frame above low shading quality, whose light model drops the first pair
#if defined(USE_LEGACY_LIGHT) && defined(USE_LINEAR_SCENE)
	#define ToneToDisplay(x) SceneToGamma(x)
	#define DisplayToTone(x) SceneFromGamma(x)
#else
	#define ToneToDisplay(x) (x)
	#define DisplayToTone(x) (x)
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

float3 tonemap(float3 rgb, float scale)
{	
	rgb = rgb * scale;
	rgb = rgb * (1.0f + rgb * RCP_WHITE_SQR) * rcp(rgb + 1.0f);
	
	return LinearToGamma(rgb);
}

float3 detonemap(float3 rgb)
{
	rgb = GammaToLinear(rgb);
	
	float3 scale = rgb * rgb - INV_TONEMAP_COEF_ONE * rgb + 1.0f;
	rgb = rgb + sqrt(scale) - 1.0f;
	
	return rgb * INV_TONEMAP_COEF_TWO;
}

// The final pass's exposure, a multiplier for display values: ExposureResponse stops for a stop of the scene's average
// log2 luminance, plus the bias in stops, within ExposureMin and ExposureMax stops
static const float ExposureResponse = 0.15f;
static const float ExposureMin = -0.5f;
static const float ExposureMax = 2.0f;

#ifdef USE_LEGACY_LIGHT
// The old light model's exposure, by the scene's average display luminance (bloom_lum_copy keeps that one then): the
// classic adaptation with its middle gray of 0.8, low luminance of 0.01 and amount of 0.7
float DisplayExposure(float Luminance, float Bias)
{
	return clamp(0.86f / (Luminance * 0.7f + 0.307f), 1.0f / 128.0f, 20.0f) * exp2(Bias);
}

// Its tone curve over exposed display values, with its white point
float3 LegacyToneMapping(float3 Color)
{
	return Color * (1.0f + Color * RCP_WHITE_SQR) / (Color + 1.0f);
}
#else
float DisplayExposure(float LogLuminance, float Bias)
{
	return exp2(clamp(Bias - LogLuminance * ExposureResponse, ExposureMin, ExposureMax));
}
#endif

// The same for the scene's own values, with the exposure grade in stops the final pass applies to them first
float SceneExposure(float LogLuminance, float Bias, float Grade)
{
	return SceneFromGamma(DisplayExposure(LogLuminance, Bias) * exp2(Grade / SRGB_GAMMA));
}

void RemapVector(inout float3 View)
{
    float3 ViewPos = abs(View);
    float ViewPosMax = max(ViewPos.x, max(ViewPos.y, ViewPos.z));

    View *= rcp(ViewPosMax);
    View.y = View.y * 2.0 - 1.0;
}

float3 CommerceToneMapping(float3 color, float startCompression, float desaturation, float channelMix, float white)
{
    // Lisence Creative Commons Attribution 4.0 International CC BY 4.0
    // taken from https://modelviewer.dev/examples/tone-mapping
    // article and original code by Emmett Lalish https://github.com/elalish 
    
    //float startCompression = 0.8 - 0.04; //0.8-0.04;
    //float desaturation = 0.15f; // 0.15

    // The operator's toe is left out: it is counted for linear light, and on display values it crushed the shadows

    // The shoulder is shared between the colour as a whole, which keeps its hue, and each channel by itself, which
    // keeps the shading of a bright surface whose strongest channel is already near white: channelMix of the second
    // The shoulder reaches white at the white point: u is how far past its start a value is, in shoulders, and
    // u (1 + u / w^2) / (1 + u) the share of the shoulder it takes; with the white point far away it is u / (1 + u)
    float d = 1.f - startCompression;
    float w = max(white - startCompression, d) / d;
    float rcp_w2 = 1.f / (w * w);
    float peak = max(color.r, max(color.g, color.b));
    float up = max(peak - startCompression, 0.f) / d;
    float3 us = max(color - startCompression, 0.f) / d;
    float3 whole = color * (peak > startCompression ? min(startCompression + d * up * (1.f + up * rcp_w2) / (1.f + up), 1.f) / peak : 1.f);
    float3 single = color > startCompression ? min(startCompression + d * us * (1.f + us * rcp_w2) / (1.f + us), 1.f) : color;
    color = lerp(whole, single, channelMix);
    peak = max(color.r, max(color.g, color.b));

    // The upper half of the range loses colour as it brightens, by desaturation at white: the operator's own
    // desaturation acts only on what it compresses, which is next to nothing in a frame exposed for display
    float bright = saturate(peak * 2.f - 1.f);
    return lerp(color, dot(color, LUMINANCE_VECTOR).xxx, desaturation * bright * bright);
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
    float WhitePoint = 1.35f; //1.7f; CGIM

    return Uncharted2Tonemap(Color, A, B, C, D, E, F, WhitePoint);
}

float3 b_remap(float3 color, float2 threshold)
{
    float thres1 = min(threshold.x, threshold.y);
    float thres2 = max(threshold.x, threshold.y);
    float brightness = (color.r + color.g + color.b) / 3.0;
    float factor = smoothstep(thres1, thres2, brightness);
    return color * factor;
}

//[numthreads(64,1,1)]
uint2 thread_remap_8x8(uint thread)
{
    return uint2((thread >> 1u) & 7u, (thread & 1u) | ((thread >> 3u) & 6u));
}

// Функции генерации случайных чисел [0, 1]
// START

// Hash without Sine, https://www.shadertoy.com/view/4djSRW
/* MIT License. Copyright (c)2014 David Hoskins.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. */
float Hash(float p)
{
    p = frac(p * 0.1031f);
    p *= p + 33.33f;
    p *= p + p;
    return frac(p);
}

float Hash(float2 p)
{
    float3 p3 = frac(float3(p.xyx) * 0.1031f);
    p3 += dot(p3, p3.yzx + 33.33f);
    return frac((p3.x + p3.y) * p3.z);
}

float Hash(float3 p3)
{
    p3 = frac(p3 * 0.1031f);
    p3 += dot(p3, p3.yzx + 33.33f);
    return frac((p3.x + p3.y) * p3.z);
}

float2 Hash22(float2 value)
{
    return float2
	(
        Hash(dot(value, float2(12.989, 78.233))),
        Hash(dot(value, float2(39.346, 11.135)))
	);
}

float3 Hash23(float2 value)
{
    return float3
	(
        Hash(dot(value, float2(12.989, 78.233))),
        Hash(dot(value, float2(39.346, 11.135))),
        Hash(dot(value, float2(73.156, 52.235)))
	);
}

float2 Hash32(float3 value)
{
    return float2
	(
        Hash(dot(value, float3(12.989, 78.233, 123.134f))),
        Hash(dot(value, float3(39.346, 11.135, 543.142f)))
	);
}

float3 Hash33(float3 value)
{
    return float3
	(
        Hash(dot(value, float3(12.989, 78.233, 123.134f))),
        Hash(dot(value, float3(39.346, 11.135, 543.142f))),
        Hash(dot(value, float3(73.156, 52.235, 143.425f)))
	);
}

// END

// A share of the view, 0 to 1 a side, as the UV of a screen target, which may be larger than the view and hold it
// in its corner
float2 ViewToTarget(float2 UV)
{
	return min(UV * view_uv.xy, view_uv.zw);
}

// The same in what the frame before left, whose view may have had another size
float2 ViewToTargetBefore(float2 UV)
{
	return min(UV * view_uv_prev.xy, view_uv_prev.zw);
}

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

float calc_fogging(float3 pos)
{
	return saturate(length(pos - eye_position) * fog_params.w + fog_params.x);
}

float2 unpack_tc_base(float2 tc, float du, float dv)
{
    return (tc.xy + float2(du, dv)) * (32.f / 32768.f); //! Increase from 32bit to 64bit floating point
}

float3 unpack_normal(float3 v)
{
    return 2 * v.zyx - 1;
}

float3 unpack_bx2(float3 v)
{
    return 2 * v - 1;
}

float3 unpack_bx4(float3 v)
{
    return 4 * v - 2;
}

float2 unpack_tc_lmap(float2 tc)
{
    return tc * (1.f / 32768.f);
} // [-1  .. +1 ]

float4 unpack_D3DCOLOR(float4 c)
{
    return c.bgra;
}

float3 unpack_D3DCOLOR(float3 c)
{
    return c.bgr;
}

float get_hemi(float4 lmh)
{
#ifdef USE_SOC_LIGHTING
	return lmh.y;
#else
	return lmh.w;
#endif
}

float get_sun(float4 lmh)
{
#ifdef USE_SOC_LIGHTING
	return lmh.w;
#else
	return lmh.y;
#endif
}

float3 v_sun(float3 N)
{
    return L_sun_color.xyz * dot(N, -L_sun_dir_w.xyz);
}

float get_noise(float2 co)
{
    return (frac(sin(dot(co.xy ,float2(12.9898f,78.233f))) * 43758.5453f))*0.5f;
}

#endif //	common_functions_h_included


float calc_cyclic(float x)
{
    float f = 1.4142f * sin(x * 3.14159f);
    return f * f - 1.0f;
}

float2 calc_xz_wave(float2 dir2D, float frac)
{
    return dir2D * frac;
}
