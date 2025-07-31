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

float3 tonemap(float3 rgb, float scale)
{	
	rgb = rgb * scale;
	return PopGamma(rgb * (1.0f + rgb * 0.34602f) * rcp(rgb + 1.0f));
}

float3 detonemap(float3 rgb)
{
	rgb = PushGamma(rgb);
	float3 r = rgb * rgb - 0.61592f * rgb + 1.0f;  
	return (rgb + sqrt(r) - 1.0f) * 1.445f;  
}

void RemapVector(inout float3 View)
{
    float3 ViewPos = abs(View);
    float ViewPosMax = max(ViewPos.x, max(ViewPos.y, ViewPos.z));

    View *= rcp(ViewPosMax);
    View.y = View.y * 2.0 - 1.0;
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

#define IMAGE_BITRATE float3(0xff, 0xff, 0xff)

// Deband color function (by Hozar 2002) - may be huita
float3 deband_color(float3 image, float2 uv)
{
    float3 dither = Hash23(cos(uv.xy * timers.x) * 1245.0f);

    float3 color = saturate(image) * IMAGE_BITRATE;
    float3 pq = frac(color);

    color -= pq;
    pq = step(dither, pq);

    color += pq;
    color *= rcp(IMAGE_BITRATE);

    return color;
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
    return t_lmh.w;
}

float get_hemi(float4 lmh)
{
    return lmh.w;
}

float get_sun(float4 lmh)
{
    return lmh.y;
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
