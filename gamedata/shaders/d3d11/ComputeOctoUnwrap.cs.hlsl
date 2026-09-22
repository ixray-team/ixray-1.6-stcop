#include "common_sky.hlsli"

Texture2D<float4> s_sky_view_lut : register(t0);
RWTexture2D<float4> u_sky_octo : register(u0);


// 520x520 physical texture:
// 512x512 logical octahedral map + 4 texels on every side.
static const float SKY_OCTAHEDRAL_PADDING = 4.0f;

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint output_width;
    uint output_height;

    u_sky_octo.GetDimensions(output_width, output_height);

    if (dispatch_id.x >= output_width || dispatch_id.y >= output_height)
    {
        return;
    }

    const float2 output_resolution = float2(output_width, output_height);
    const float2 logical_resolution = output_resolution - 2.0f * SKY_OCTAHEDRAL_PADDING;

    // Pixel-center coordinates are important here.
    //
    // For a 520x520 output:
    //   physical [4 .. 515] -> logical 512x512 core
    //   physical [0 .. 3]   -> left/top padding
    //   physical [516..519] -> right/bottom padding
    //
    // Padding coordinates intentionally leave the [0, 1] interval.
    const float2 logical_uv = (float2(dispatch_id.xy) + 0.5f - SKY_OCTAHEDRAL_PADDING) / logical_resolution;
    const float2 encoded_direction = logical_uv * 2.0f - 1.0f;
    // sky_gt7_octahedral_decode must not saturate encoded_direction.
    // Coordinates outside the logical square are required to generate
    // seam-aware padding.
    const float3 ray_direction = sky_gt7_octahedral_decode(encoded_direction);
    // X-Ray stores the direction in which sunlight propagates.
    // Sky functions expect the direction from the point to the Sun.
    const float3 sun_direction = safe_normalize(-L_sun_dir_w);
    const float camera_elevation = sky_get_camera_elevation();
    const float4 sky_radiance = sky_sample_view_lut(s_sky_view_lut, smp_rtlinear, ray_direction, sun_direction, camera_elevation);
    // Keep the octahedral map in linear HDR.
    // Do not apply exposure, gamma correction or tone mapping here.
    u_sky_octo[dispatch_id.xy] = float4(max(sky_radiance.rgb, 0.0f), 1.0f);
}
