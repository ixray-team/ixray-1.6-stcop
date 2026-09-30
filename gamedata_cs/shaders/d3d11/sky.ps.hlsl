#include "common.hlsli"
#include "rainbow_draw.h"

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

#ifndef DISABLE_MOTION_VECTORS
    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;
#endif

    float4 hpos : SV_POSITION;
};

TextureCube s_sky0 : register(t0);
TextureCube s_sky1 : register(t1);

static const float4 constant_view_angle = float4(0.5, 1.0, -1.0, 0.999);
static const float2 constant_position = float2(0.9, -0.6);

struct sky
{
    float4 Color : SV_Target0;
#ifndef DISABLE_MOTION_VECTORS
    float2 Velocity : SV_Target1;
#endif
};

void main(in v2p I, out sky O)
{
	float3 TexCoord = I.p;
	
#ifndef USE_FULL_SKY_SPHERE
    RemapVector(TexCoord);
#endif

	float3 s0 = s_sky0.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 s1 = s_sky1.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 sky = lerp(s0, s1, I.factor.w);
    
#ifdef USE_BGRA_SKYCOLOR
    sky *= L_sky_color.zyx;
#else
    sky *= L_sky_color.xyz;
#endif

	float diff_green_red = L_sun_color.g - L_sun_color.r;
	float diff_green_blue = L_sun_color.g - L_sun_color.b;
	float amount = (diff_green_red + 0.05f) + (diff_green_blue - 0.05f);
	if (TexCoord.z >= constant_view_angle.x && TexCoord.z <= constant_view_angle.y && TexCoord.y >= constant_view_angle.z && TexCoord.y <= constant_view_angle.w && amount > 0 && rain_params.x > 0)
	{
		bool white = false;
		float4 rb = draw_rainbow(TexCoord.xy, constant_position, 1, true, white);
		sky += rb.rgb * amount * 8.f;
	}

#ifdef USE_LEGACY_SKY_TONEMAP
	O.Color = float4(detonemap(sky * 0.66f), 0.0f);
#else
	O.Color = float4(GammaToLinear(sky), 0.0f);
#endif

#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
#endif
}

