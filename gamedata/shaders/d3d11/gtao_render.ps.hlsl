/*
        Ground-Truth Based Ambient Occlusion

        References:
        - Practical Real-Time Strategies for Accurate Indirect Occlusion [Jimenez et. al];
        - Screen space indirect lighting with visibility bitmask [Olivier Therrien, Yannick Levesque, Guillaume Gilet]

        Credits:
        - MartyMcFly (huge help with understanding bitmask AO concept)
        - Michal Drobot (approximate arccos function)

        Author:
        - LVutner

        Do not judge me. I had to butcher the AO algorithm to save some performance.

        ---IX-Ray Engine---
*/

#include "common.hlsli"

struct PSInput
{
    float4 hpos : SV_Position;
    float4 texcoord : TEXCOORD0;
};

float gtao_parameters; // Factor used to transform world space radius to screen space
uniform Texture2D s_half_depth;

// Arccosine approximation by Michal Drobot
// TODO: Make a 'fastmath' like library
float fastacos_approx(float x)
{
    float res = -0.156583 * abs(x) + 1.5707964;
    res *= sqrt(1.0 - abs(x));
    return x >= 0.0 ? res : 3.1415927 - res;
}

float example_how_to_not_implement_gtao(float3 view_position, float3 view_normal, float zbuffer, float2 texcoord, float jitter)
{
    // Exclude HUD, and far plane geometry
    if (zbuffer < 0.02f || view_position.z > 60.0)
    {
        return 1.0;
    }

    // Few constants
    const float GTAO_PI = 3.1415927;
    const float GTAO_HALF_PI = 1.5707964;

    int GTAO_DIRECTIONS = 4; // Amount of hemisphere slices
    float GTAO_THICKNESS = 0.75; // Thickness factor
    float GTAO_RADIUS = 1.0; // World space radius
    float GTAO_SECTORS = 24.0; // Amount of sectors distributed around the hemisphere slice

    // Bias the depth to avoid self-intersections
    view_position *= 0.996;

    // View direction
    float3 view_direction = -normalize(view_position);

    // Screen-space radius
    float screen_radius = (GTAO_RADIUS * gtao_parameters) / view_position.z;

    // Slice scale
    float2 slice_scale = float2(pos_decompression_params2.z, -pos_decompression_params2.w) * screen_radius;

    // Accumulated occlusion
    float occlusion = 0.0;

    [loop]
    for (int i = 0; i < GTAO_DIRECTIONS; i++)
    {
        // Generate uniform[no] disk. This is generally incorrect, but we don't care as this should run fast
        float increment = (jitter + float(i)) / float(GTAO_DIRECTIONS);
        float phi = increment * 2457.56; // If you know, you know :)

        float3 slice_direction = (float3)0.0;
        sincos(phi, slice_direction.y, slice_direction.x);
        slice_direction.xy *= increment; // No sqrt(x), we want more detail in crevices

        float2 omega[2] = {-slice_direction.xy * slice_scale, slice_direction.xy * slice_scale};

        // Math from GTAO paper
        float3 ortho_direction = slice_direction - (dot(slice_direction.xy, view_direction.xy) * view_direction);
        float3 axis = cross(slice_direction, view_direction);
        float3 proj_normal = view_normal - axis * dot(view_normal, axis);
        float proj_normal_length = length(proj_normal);

        float cos_n = saturate(dot(proj_normal, view_direction) / proj_normal_length);
        float sign_n = dot(ortho_direction, proj_normal) >= 0.0 ? 1.0 : -1.0;
        float n = sign_n * fastacos_approx(cos_n);

        // Init bitfield
        uint bitfield = 0u;

        // Process each side
        [unroll]
        for (int side = 0; side < 2; side++)
        {
            float2 s_texcoord = texcoord.xy + omega[side];

            // Guard band
            if (dot(s_texcoord.xy - saturate(s_texcoord.xy), 1.0) != 0.0)
            {
                break;
            }

            // Sample the view space position
            float s_view_z = depth_unpack.x * rcp(s_position.SampleLevel(smp_nofilter, s_texcoord, 0.0).x - depth_unpack.y);
            // float s_view_z = s_half_depth.SampleLevel(smp_nofilter, s_texcoord, 0.0).x;
            float3 s_view_position = float3((s_texcoord * 2.0 - 1.0) * pos_decompression_params.xy, 1.0) * s_view_z;

            // Get front and back vectors
            float3 s_front_vector = s_view_position - view_position;
            float3 s_back_vector = s_front_vector - view_direction * GTAO_THICKNESS;

            float2 s_horizon_v = float2(dot(s_front_vector, view_direction), dot(s_back_vector, view_direction));
            s_horizon_v *= float2(rsqrt(dot(s_front_vector, s_front_vector)), rsqrt(dot(s_back_vector, s_back_vector)));
            s_horizon_v = float2(fastacos_approx(s_horizon_v.x), fastacos_approx(s_horizon_v.y));
            s_horizon_v = side == 0 ? s_horizon_v : -s_horizon_v.yx; // Tricky part - sorting. Thanks to MartyMcFly for help!
            s_horizon_v = saturate((s_horizon_v + n + GTAO_HALF_PI) / GTAO_PI);

            // Floor=full sector | round=half sector
            uint a = uint(s_horizon_v.x * GTAO_SECTORS);
            uint b = uint(floor(saturate(s_horizon_v.y - s_horizon_v.x) * GTAO_SECTORS));

            // Update the bitfield
            bitfield |= ((1u << b) - 1u) << a;
        }
        occlusion += saturate(1.0 - countbits(bitfield) / GTAO_SECTORS); // Attempt to account for bias...
    }
    // Normalize
    occlusion /= GTAO_DIRECTIONS;

    // Because we are *always* working in sRGB, it makes sense to square AO contribution
    return occlusion * occlusion;
}

uint main(PSInput I) : SV_Target
{
    uint2 pixel_index = uint2(floor(I.hpos.xy)) & 3;

    // This noise pattern seems to work best
    float noise_pattern[16] =
	{
		0.40625, 0.71875, 0.03125, 0.84375,
		0.90625, 0.78125, 0.34375, 0.21875,
		0.53125, 0.15625, 0.65625, 0.28125,
		0.96875, 0.59375, 0.09375, 0.46875
	};

    float jitter = noise_pattern[pixel_index.x + pixel_index.y * 4u];

    // Unpack G-Buffer data...
    float3 Normal, Point; float Depth;
    {
        Depth = s_position.SampleLevel(smp_nofilter, I.texcoord, 0.0).x;

        Normal = s_normal.SampleLevel(smp_nofilter, I.texcoord, 0.0).xyz;
        Normal = normalize(Normal.xyz - 0.5f);
        Normal.z = -Normal.z;

        float s_view_z = depth_unpack.x / (Depth - depth_unpack.y);
        Point = float3((I.texcoord * 2.0 - 1.0) * pos_decompression_params.xy, 1.0) * s_view_z;
    }

    // Init
    float occlusion = example_how_to_not_implement_gtao(Point, Normal, Depth, I.texcoord, jitter);

    // Pack the data into R32_UINT
    uint2 packed = uint2(asuint(f32tof16(Point.z)), (asuint(f32tof16(occlusion)) << 16));
    return packed.x | packed.y;
}

