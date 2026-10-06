#ifndef HUD_RAINDROPS_HLSLI
#define HUD_RAINDROPS_HLSLI

// r: lifetime, g: normal Y, b: normal X, a: phase
Texture2D s_hud_rain;

static const float kHudRainDensity = 0.225f;
static const float kHudRainUvScale = 1.25f;
static const float kHudRainRefraction = 0.05f;

struct HudRainDrops
{
    float2 tangent;
    float3 tilt;
    float coverage;
};

float3 HudRain_Layer(float4 texel, float speed, float weight)
{
    float2 normal = texel.yz * 2.0f - 1.0f;
    float phase = 1.0f - frac(texel.w + hud_rain.x * speed);
    float mask = saturate(phase * texel.x - kHudRainDensity) * weight;
    return float3(normal * mask, mask);
}

HudRainDrops HudRain_Evaluate(float3 position, float upFacing)
{
    HudRainDrops result = (HudRainDrops)0;

    float amount = hud_rain.y;
    float facing = saturate(upFacing);
    if (amount <= 0.0f || facing <= 0.0f)
        return result;

    float3 dp1 = ddx(position);
    float3 dp2 = ddy(position);
    float3 surface = cross(dp1, dp2);
    float3 axisWeight = abs(normalize(surface));

    int3 axis = (axisWeight.x > axisWeight.y && axisWeight.x > axisWeight.z) ? int3(0, 1, 2) :
                (axisWeight.y > axisWeight.z) ? int3(1, 2, 0) :
                                                int3(2, 0, 1);

    float2 uv = float2(position[axis.y], position[axis.z]) * kHudRainUvScale;
    float4 grad = float4(dp1[axis.y], dp1[axis.z], dp2[axis.y], dp2[axis.z]);
    float weight = pow(axisWeight[axis.x], 5.0f);

    float4 coarse = s_hud_rain.SampleGrad(smp_base, uv * 15.0f, grad.xy, grad.zw);
    float4 fine = s_hud_rain.SampleGrad(smp_base, (uv + float2(0.23f, 0.46f)) * 8.0f, grad.xy, grad.zw);

    float3 drops = HudRain_Layer(coarse, 0.2f, weight) + HudRain_Layer(fine, 0.1f, weight);
    drops.xy = clamp(drops.xy, -1.0f, 1.0f);
    drops *= facing * amount;

    // Projection axes follow the surface. Flip the first axis with the face
    // so the drop stays convex on both sides of the mesh.
    float side = surface[axis.x] >= 0.0f ? 1.0f : -1.0f;
    float3 axisU, axisV;
    if (axis.x == 0)
    {
        axisU = float3(0.0f, side, 0.0f);
        axisV = float3(0.0f, 0.0f, 1.0f);
    }
    else if (axis.x == 1)
    {
        axisU = float3(0.0f, 0.0f, side);
        axisV = float3(1.0f, 0.0f, 0.0f);
    }
    else
    {
        axisU = float3(side, 0.0f, 0.0f);
        axisV = float3(0.0f, 1.0f, 0.0f);
    }

    result.tangent = drops.xy;
    result.tilt = mul((float3x3)m_WV, axisU * drops.x + axisV * drops.y);
    result.coverage = drops.z;
    return result;
}

void HudRain_OffsetColorUv(inout float2 uv, HudRainDrops drops)
{
#ifdef USE_BUMP
    uv += drops.tangent * kHudRainRefraction;
#else
    uv += drops.tangent * (kHudRainRefraction * 0.1f);
#endif
}

void HudRain_Perturb(inout float3 eyeNormal, HudRainDrops drops, float scale)
{
    eyeNormal = normalize(eyeNormal + drops.tilt * scale);
}

float HudRain_NormalScale(bool surfaceHasBump)
{
    float luma = dot(max(L_hemi_color.xyz, 0.0f), float3(0.30f, 0.59f, 0.11f));
    float scale = max(luma * 30.0f, 3.0f);
    if (!surfaceHasBump)
        scale = max(scale * 0.1f, 3.0f);
    return scale;
}

void HudRain_Wet(inout IXRayMaterial surface, HudRainDrops drops)
{
#ifndef USE_LEGACY_LIGHT
    surface.Roughness = saturate(surface.Roughness - drops.coverage * 0.45f);
#else
    surface.Gloss = saturate(surface.Gloss + drops.coverage * 0.5f);
#endif
}

#endif
