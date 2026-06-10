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

void RemapVector(inout float3 View)
{
    float3 ViewPos = abs(View);
    float ViewPosMax = max(ViewPos.x, max(ViewPos.y, ViewPos.z));

    View *= rcp(ViewPosMax);
    View.y = View.y * 2.0 - 1.0;
}

float3 CommerceToneMapping(float3 color, float startCompression, float desaturation)
{
    // Lisence Creative Commons Attribution 4.0 International CC BY 4.0
    // taken from https://modelviewer.dev/examples/tone-mapping
    // article and original code by Emmett Lalish https://github.com/elalish 
    
    //float startCompression = 0.8 - 0.04; //0.8-0.04;
    //float desaturation = 0.15f; // 0.15

    float x = min(color.r, min(color.g, color.b));
    float offset = (x < 0.08f) ? (x - 6.25f * x * x) : 0.04f;
    color -= offset;

    float peak = max(color.r, max(color.g, color.b));
    if (peak < startCompression)
        return color;

    float d = 1.f - startCompression;
    float newPeak = 1.f - d * d / (peak + d - startCompression);
    color *= newPeak / peak;

    float g = 1.f - 1.f / (desaturation * (peak - newPeak) + 1.f);
    return lerp(color, newPeak.xxx, g);
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

//[numthreads(64,1,1)]
uint2 thread_remap_8x8(uint thread)
{
    return uint2((thread >> 1u) & 7u, (thread & 1u) | ((thread >> 3u) & 6u));
}

// Функции генерации случайных чисел [0, 1]
// START

float Hash(float n)
{
    return frac(sin(n) * 43758.5453123f);
}

float Hash(float2 n)
{
    return Hash(Hash(n.x) + n.y);
}

float Hash(float3 n)
{
    return Hash(Hash(dot(n.xy, float2(12.989, 78.233))) + n.z);
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

float3 deband_color(float3 image, float2 uv, float bitrate = IMAGE_BITRATE)
{
    float3 dither = dot(float2(171.0, 231.0), uv.xy + m_taa_jitter.w).xxx;
    dither = 2.0f * frac(dither / float3(103.0, 71.0, 97.0)) - 1.0f;

    return image + dither * rcp(bitrate);
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
    #ifndef NEW_FOGGIN
        return saturate(length(pos - eye_position) * fog_params.w + fog_params.x);
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

#endif //	common_functions_h_included

