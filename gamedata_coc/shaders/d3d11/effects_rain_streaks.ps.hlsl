#include "common.hlsli"

#include "rain_common.hlsli"

float4 rain_refraction_params;

float4 rain_lights_position[8];
float4 rain_lights_color[8];
float4 rain_lights_direction[8];

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
    float3 World : TEXCOORD2;

#ifndef DISABLE_MOTION_VECTORS
    float4 HPosCurr : TEXCOORD3;
    float4 HPosOld : TEXCOORD4;
#endif

    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

struct RainOutput
{
    float4 Color : SV_Target0;

#ifndef DISABLE_MOTION_VECTORS
    float4 Velocity : SV_Target1;
    float4 Reactive : SV_Target2;
#else
    float4 Reactive : SV_Target1;
#endif
};

static const float RainOpacity = 0.8f;
static const float RainRefractionOffset = 0.0125f;
static const float RainLift = 0.15f;
static const float RainFresnel = 0.1f;
static const float RainReactive = 0.8f;
static const float RainLightScale = 0.35f;
static const float RainSoftDistance = 0.4f;
static const float RainNearFade = 0.3f;

float3 RainLampLight(float3 World, float3 ViewDirWorld)
{
    float3 Light = 0.0f;
    int Count = (int)rain_refraction_params.y;

    [loop]
    for (int It = 0; It < Count; ++It)
    {
        float3 ToLight = rain_lights_position[It].xyz - World;
        float Distance2 = dot(ToLight, ToLight);
        float Range = rain_lights_position[It].w;

        float Attenuation = saturate(1.0f - Distance2 / (Range * Range));
        Attenuation *= Attenuation;

        float3 LightDir = ToLight * rsqrt(max(Distance2, 1e-6f));

        float CosCone = rain_lights_direction[It].w;
        if (CosCone > -1.5f)
        {
            Attenuation *= smoothstep(CosCone, CosCone + 0.05f, dot(-LightDir, rain_lights_direction[It].xyz));
        }

        float Forward = saturate(dot(ViewDirWorld, LightDir));
        Light += rain_lights_color[It].rgb * Attenuation * (0.3f + 2.5f * pow(Forward, 8.0f));
    }

    return Light * RainLightScale;
}

void main(in v2p I, out RainOutput O)
{
    float2 DropTC = (0.25f + I.Tex0 * 7.0f) / 32.0f;
    float4 Drop = s_base.SampleLevel(smp_rtlinear, DropTC, 0);

    float2 ScreenTC = I.HPos.xy * scaled_screen_res.zw;

    float SceneZ = GbufferGetPointRealJitter(ScreenTC, s_position.SampleLevel(smp_nofilter, ScreenTC, 0).x).z;
    float Fade = saturate((SceneZ - I.Point.z) / RainSoftDistance) * saturate((I.Point.z - RainNearFade) / RainNearFade);

    float Mask = Drop.w * I.Color.w * Fade;

    float2 UDir = float2(ddx(I.Tex0.x), ddy(I.Tex0.x));
    float2 VDir = float2(ddx(I.Tex0.y), ddy(I.Tex0.y));
    UDir *= rsqrt(max(dot(UDir, UDir), 1e-12f));
    VDir *= rsqrt(max(dot(VDir, VDir), 1e-12f));

    float2 TexNormal = Drop.xy * 2.0f - 1.0f;
    float3 Normal = float3(TexNormal.x * VDir + TexNormal.y * UDir, 1.0f);

    clip(Mask - 1.0f / 255.0f);

    float HemiFactor = saturate(dot(L_hemi_color.rgb, float3(1.0f, 1.0f, 1.0f)));

    float2 RefractPx = Normal.xy * RainRefractionOffset * scaled_screen_res.y * rain_refraction_params.x;
    float2 RefractTC = saturate(ScreenTC + RefractPx * scaled_screen_res.zw);
    float SceneDepth = s_position.SampleLevel(smp_nofilter, RefractTC, 0).x;
    RefractTC = SceneDepth < I.HPos.z ? ScreenTC : RefractTC;

    float3 Refracted = s_rain_refraction.SampleLevel(smp_rtlinear, RefractTC, 0).rgb;
    float BackgroundLum = dot(Refracted, LUMINANCE_VECTOR);

    float2 ReflectDir = normalize(float3(Normal.xy * (0.2f + HemiFactor * 0.5f), 1.0f)).xy;
    float3 Reflected = s_rain_refraction.SampleLevel(smp_rtlinear, saturate(ScreenTC + ReflectDir), 0).rgb;

    Reflected *= RainFresnel;
    Refracted *= 1.0f - RainFresnel;

    float3 Scattered = RainSkyTint() * BackgroundLum * RainLift;

    float3 ViewDirWorld = normalize(I.World - eye_position);
    float3 DropLight = Reflected + Scattered + RainLampLight(I.World, ViewDirWorld) + RainSunScatter(ViewDirWorld);

    O.Color.xyz = Refracted + DropLight;
    O.Color.w = Mask * RainOpacity;

    float DropShare = saturate(dot(DropLight, LUMINANCE_VECTOR) / max(dot(O.Color.xyz, LUMINANCE_VECTOR), 1e-4f));

#ifndef DISABLE_MOTION_VECTORS
    float2 DropVelocity = I.HPosCurr.xy / I.HPosCurr.w - I.HPosOld.xy / I.HPosOld.w;
    O.Velocity = float4(DropVelocity, 0.0f, O.Color.w * DropShare);
#endif

    O.Reactive = float4(1.0f, 0.0f, 0.0f, O.Color.w * RainReactive);
}
