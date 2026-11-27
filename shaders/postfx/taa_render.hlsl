/*
        Simple TAA

        References:
        - https://gdcvault.com/play/1022970/Temporal-Reprojection-Anti-Aliasing-in
        - https://research.nvidia.com/labs/rtr/publication/yang2020survey/
        - https://github.com/iryoku/smaa
        - https://michaldrobot.com/2014/08/13/hraa-siggraph-2014-slides-available/
        - https://gpuopen.com/learn/optimized-reversible-tonemapper-for-resolve/
        - https://research.activision.com/publications/2020-03/dynamic-temporal-antialiasing-and-upsampling-in-call-of-duty
        - https://dl.acm.org/doi/10.1145/3681758.3697996

        Author:
        - LVutner

        ---IX-Ray Engine---
*/

#include "common\common.hlsli"

Texture2D s_image_prev : register(t25);
Texture2D s_reactive : register(t26);

#define TAA_BLEND_WEIGHT 0.9f
#define TAA_REACTIVE_BLEND_WEIGHT 0.4f
#define TAA_HUD_DEPTH 0.02f

// The targets may be larger than the view, which is in their corner: a texel of them in their own UV, and their size in texels
#define TAA_TEXEL scaled_screen_res.zw
#define TAA_TEXELS (1.0f / scaled_screen_res.zw)

static const int2 offset_3x3[9] = { int2(-1, -1), int2(0, -1), int2(1, -1), int2(-1, 0), int2(0, 0), int2(1, 0), int2(-1, 1), int2(0, 1), int2(1, 1) };

// Baked for the "asphalt" scene
static const float3 kdop_axes[8] = {
    float3(0.997167f, -0.054820f, 0.051500f),
    float3(0.043682f, -0.727663f, 0.684542f),
    float3(-0.451553f, 0.808335f, -0.377751f),
    float3(-0.003001f, 0.615645f, -0.788018f),
    float3(0.130192f, -0.063137f, -0.989476f),
    float3(0.775079f, -0.584236f, -0.240668f),
    float3(0.589837f, 0.171644f, -0.789070f),
    float3(0.898010f, -0.436338f, -0.056459f)
};

float3 taa_tonemap(float3 c)
{
    return saturate(c * rcp(max(c.x, max(c.y, c.z)) + 1.0f));
}

float3 taa_tonemap_inverse(float3 c)
{
    return c * rcp(1.0f - max(c.x, max(c.y, c.z)));
}

// Catmull-Rom in five bilinear taps without the corners (Jimenez, Filmic SMAA, SIGGRAPH 2016): one bilinear tap blurs the history more every frame the image moves by a fraction of a pixel
float3 history_catmull_rom(float2 texcoord)
{
    float2 position = texcoord * TAA_TEXELS;
    float2 center = floor(position - 0.5f) + 0.5f;
    float2 f = position - center;
    float2 w0 = f * (-0.5f + f * (1.0f - 0.5f * f));
    float2 w1 = 1.0f + f * f * (-2.5f + 1.5f * f);
    float2 w2 = f * (0.5f + f * (2.0f - 1.5f * f));
    float2 w3 = f * f * (-0.5f + 0.5f * f);
    float2 w12 = w1 + w2;

    // No tap leaves the view the frame before had for what lies beside it in the target
    float2 tc0 = min((center - 1.0f) * TAA_TEXEL, view_uv_prev.zw);
    float2 tc12 = min((center + w2 / w12) * TAA_TEXEL, view_uv_prev.zw);
    float2 tc3 = min((center + 2.0f) * TAA_TEXEL, view_uv_prev.zw);

    float4 color = float4(taa_tonemap(s_image_prev.SampleLevel(smp_rtlinear, float2(tc12.x, tc0.y), 0).xyz), 1.0f) * (w12.x * w0.y);
    color += float4(taa_tonemap(s_image_prev.SampleLevel(smp_rtlinear, float2(tc0.x, tc12.y), 0).xyz), 1.0f) * (w0.x * w12.y);
    color += float4(taa_tonemap(s_image_prev.SampleLevel(smp_rtlinear, tc12, 0).xyz), 1.0f) * (w12.x * w12.y);
    color += float4(taa_tonemap(s_image_prev.SampleLevel(smp_rtlinear, float2(tc3.x, tc12.y), 0).xyz), 1.0f) * (w3.x * w12.y);
    color += float4(taa_tonemap(s_image_prev.SampleLevel(smp_rtlinear, float2(tc12.x, tc3.y), 0).xyz), 1.0f) * (w12.x * w3.y);
    return max(color.xyz / color.w, 0.0f);
}

void get_3x3_depth(float2 texcoord, float2 gather_texcoord, inout float d_3x3[9])
{
    float4 d_gather0 = s_position.Gather(smp_nofilter, gather_texcoord);
    float4 d_gather1 = s_position.Gather(smp_nofilter, gather_texcoord, int2(-1, -1));
    d_3x3[0] = d_gather1.w;
    d_3x3[1] = d_gather1.z;
    d_3x3[2] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(1, -1)).x;
    d_3x3[3] = d_gather1.x;
    d_3x3[4] = d_gather0.w;
    d_3x3[5] = d_gather0.z;
    d_3x3[6] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 1)).x;
    d_3x3[7] = d_gather0.x;
    d_3x3[8] = d_gather0.y;
}

void get_3x3_color(float2 texcoord, float2 gather_texcoord, inout float3 c_3x3[9])
{
    float4 c_gather0_r = s_image.GatherRed(smp_nofilter, gather_texcoord);
    float4 c_gather0_g = s_image.GatherGreen(smp_nofilter, gather_texcoord);
    float4 c_gather0_b = s_image.GatherBlue(smp_nofilter, gather_texcoord);
    c_3x3[0] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, -1)).xyz;
    c_3x3[1] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(0, -1)).xyz;
    c_3x3[2] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(1, -1)).xyz;
    c_3x3[3] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 0)).xyz;
    c_3x3[4] = float3(c_gather0_r.w, c_gather0_g.w, c_gather0_b.w);
    c_3x3[5] = float3(c_gather0_r.z, c_gather0_g.z, c_gather0_b.z);
    c_3x3[6] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 1)).xyz;
    c_3x3[7] = float3(c_gather0_r.x, c_gather0_g.x, c_gather0_b.x);
    c_3x3[8] = float3(c_gather0_r.y, c_gather0_g.y, c_gather0_b.y);
}

float3 kdop_clipping(float3 mean, float3 prev_color, float3 colors[9], float gamma)
{
    float3 dir = prev_color - mean;
    float2 near_far = float2(-10000.0f, 10000.0f);

    [unroll] for (int axis_idx = 0; axis_idx < 8; axis_idx++) {
        float3 axis = kdop_axes[axis_idx];
        float2 moments = 0.0f;

        [unroll] for (int color_idx = 0; color_idx < 9; color_idx++) {
            float t = dot(colors[color_idx], axis);
            moments += float2(t, t * t);
        }

        moments *= 1.0f / 9.0f;
        float sigma = max(sqrt(moments.y - moments.x * moments.x), 1e-5f);
        float proj_pos = dot(mean, axis);
        float2 extent = float2(min(moments.x - gamma * sigma, proj_pos), max(moments.x + gamma * sigma, proj_pos));
        float2 t_01 = (extent - proj_pos) * (1.0f / dot(dir, axis));
        near_far.x = max(near_far.x, min(t_01.x, t_01.y));
        near_far.y = min(near_far.y, max(t_01.x, t_01.y));
    }

    if (near_far.x <= near_far.y && (near_far.x > 0.0f || near_far.y > 0.0f)) {
        return mean + saturate(near_far.x > 0.0f ? near_far.x : near_far.y) * dir;
    }

    return mean;
}

float4 ps_main(PSInputFullscreen I) : SV_Target
{
    // https://wojtsterna.blogspot.com/2018/02/directx-11-hlsl-gatherred.html
    float2 gather_texcoord = I.texcoord.xy + TAA_TEXEL * 0.5f;
    float d_3x3[9];
    float3 c_3x3[9];
    get_3x3_depth(I.texcoord.xy, gather_texcoord, d_3x3);
    get_3x3_color(I.texcoord.xy, gather_texcoord, c_3x3);

    // The nearest neighbour's motion, of the centre's own layer: the HUD is always nearer, and the world beside it took its motion and trailed it
    bool is_hud = d_3x3[4] < TAA_HUD_DEPTH;
    float3 mean = 0.0f;
    int2 depth_offset = 0;
    float depth_closest = 1.0f;

    [unroll] for (int sample_idx = 0; sample_idx < 9; sample_idx++) {
        c_3x3[sample_idx] = taa_tonemap(c_3x3[sample_idx]);
        mean += c_3x3[sample_idx] * (1.0f / 9.0f);
        float depth = d_3x3[sample_idx];

        if (depth < depth_closest && (depth < TAA_HUD_DEPTH) == is_hud) {
            depth_closest = depth;
            depth_offset = offset_3x3[sample_idx];
        }
    }

    // The motion is a share of the view; the lookup is in the target's UV, in the view the frame before had
    float2 motion_vector = s_velocity[clamp(I.hpos.xy + depth_offset, 0, scaled_screen_res.xy - 1)].xy * float2(0.5f, -0.5f);
    float2 reprojected_view = I.texcoord.xy / view_uv.xy - motion_vector;

    if (any(reprojected_view != saturate(reprojected_view))) {
        return float4(taa_tonemap_inverse(c_3x3[4]), 0.0f);
    }

    float3 history = history_catmull_rom(reprojected_view * view_uv_prev.xy);
    float reactive = s_reactive[I.hpos.xy].x;

    // A wider window for what stands still
    float gamma = 1.5f - min(1.0f, length(motion_vector * scaled_screen_res.xy * 0.2f));
    history = kdop_clipping(mean, history, c_3x3, lerp(gamma, 0.5f, reactive));
    float3 color = lerp(c_3x3[4], history, lerp(TAA_BLEND_WEIGHT, TAA_REACTIVE_BLEND_WEIGHT, reactive));
    return float4(DitherR11G11B10(taa_tonemap_inverse(color), I.hpos.xy), 1.0f);
}
