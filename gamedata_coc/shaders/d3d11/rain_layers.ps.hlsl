#include "common.hlsli"
#include "rain_common.hlsli"


float4 rain_layers_params;
float4 rain_layers_direction;

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

static const float RainLayerDistance[4] = { 9.0f, 18.0f, 36.0f, 72.0f };
static const float RainLayerSpacing[4] = { 0.45f, 0.8f, 1.4f, 2.5f };
static const float RainLayerLength[4] = { 1.6f, 3.0f, 5.5f, 10.0f };
static const float RainLayerOpacity = 0.35f;
static const float RainLayerHalfWidth = 0.03f;
static const float RainLayerRefraction = 6.0f;

float RainHash(float X)
{
    return frac(sin(X * 12.9898f) * 43758.5453f);
}

float RainLayer(float U, float Columns, float Height, float Footprint, int Layer, float Present, out float Across)
{
    float Seed = Layer * 17.31f;

    float Column = fmod(floor(U) + Columns * 4.0f, Columns);
    float H1 = RainHash(Column + Seed);
    float H2 = RainHash(Column + Seed + 5.17f);
    float H3 = RainHash(Column + Seed + 9.71f);

    float Offset = frac(U) - (0.5f + (H1 - 0.5f) * 0.6f);
    float Width = clamp(Footprint, RainLayerHalfWidth, 0.5f);
    float Profile = saturate(1.0f - abs(Offset) / Width) * (RainLayerHalfWidth / Width);
    Across = Offset / Width;

    float Period = RainLayerLength[Layer] * 2.5f;
    float V = frac(Height / Period + H2);
    float Segment = smoothstep(0.0f, 0.1f, V) * smoothstep(0.45f, 0.3f, V);

    float Streak = Profile * Segment * step(H3, Present);

    float Average = RainLayerHalfWidth * 0.3f * Present;
    return lerp(Streak, Average, saturate(Footprint * 2.0f - 1.0f));
}

void main(PSInputFullscreen I, out RainOutput O)
{
    float Density = rain_layers_params.x;
    float Present = saturate(0.3f + 0.35f * Density);
    float Strength = RainLayerOpacity * saturate(0.4f + 0.6f * Density);

    float3 PointView = GbufferGetPointRealJitter(I.texcoord, 0.9f);
    float3 Ray = mul((float3x3)m_invV, PointView / PointView.z);
    float3 RayDir = normalize(Ray);

    float SceneZ = GbufferGetPointRealJitter(I.texcoord, s_position.SampleLevel(smp_nofilter, I.texcoord, 0).x).z;

    float HorizontalLength = length(Ray.xz);

    float Angle = atan2(Ray.z, Ray.x);
    float2 RayDdx = ddx(Ray.xz);
    float2 RayDdy = ddy(Ray.xz);
    float InvLength2 = rcp(max(dot(Ray.xz, Ray.xz), 1e-6f));
    float AngleFootprint = max(abs(Ray.x * RayDdx.y - Ray.z * RayDdx.x), abs(Ray.x * RayDdy.y - Ray.z * RayDdy.x)) * InvLength2;

    float3 Fall = rain_layers_direction.xyz;
    float2 Tangent = float2(-sin(Angle), cos(Angle));
    float TiltPerHeight = dot(Fall.xz, Tangent) / min(Fall.y, -0.05f);

    float3 SunScatter = RainSunScatter(RayDir);
    float3 SkyTint = RainSkyTint();

    float3 Color = 0.0f;
    float Alpha = 0.0f;

    [branch]
    if (HorizontalLength > 0.02f)
    {
        [unroll]
        for (int Layer = 0; Layer < 4; ++Layer)
        {
            float Distance = RainLayerDistance[Layer];
            float Depth = Distance / HorizontalLength;
            float3 World = eye_position + Ray * Depth;

            float Visibility = saturate((SceneZ - Depth) / (0.15f * Depth + 0.5f));
            Visibility *= lerp(saturate(rain_layers_params.y * 2.0f), 1.0f, saturate((Distance - 10.0f) / 30.0f));
            Visibility *= 1.0f - calc_fogging(World);

            float Columns = max(round(2.0f * PI * Distance / RainLayerSpacing[Layer]), 1.0f);
            float ColumnsPerMeter = Columns / (2.0f * PI * Distance);
            float U = Angle / (2.0f * PI) * Columns + (dot(eye_position.xz, Tangent) - World.y * TiltPerHeight) * ColumnsPerMeter;
            float Height = World.y + rain_layers_params.z * timers.x;

            float Across;
            float Streak = RainLayer(U, Columns, Height, AngleFootprint / (2.0f * PI) * Columns, Layer, Present, Across);
            float LayerAlpha = Streak * Strength * Visibility;

            [branch]
            if (LayerAlpha > 1.0f / 255.0f)
            {
                float2 Offset = float2(-Across * RainLayerRefraction * rain_layers_params.w * (1.0f - Layer * 0.2f), 0.0f);
                float3 Refracted = s_rain_refraction.SampleLevel(smp_rtlinear, saturate(I.texcoord + Offset * scaled_screen_res.zw), 0).rgb;
                float3 LayerColor = Refracted * 0.9f + SkyTint * dot(Refracted, LUMINANCE_VECTOR) * 0.2f + SunScatter;

                Color += (1.0f - Alpha) * LayerAlpha * LayerColor;
                Alpha += (1.0f - Alpha) * LayerAlpha;
            }
        }
    }

    clip(Alpha - 1.0f / 255.0f);

    O.Color = float4(Color / max(Alpha, 1e-4f), Alpha);

#ifndef DISABLE_MOTION_VECTORS
    O.Velocity = 0.0f;
#endif

    O.Reactive = float4(1.0f, 0.0f, 0.0f, Alpha * 0.5f);
}
