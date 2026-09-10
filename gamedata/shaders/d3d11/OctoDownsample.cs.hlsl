#include "common_sky.hlsli"

Texture2D<float4> s_sky_octo_input : register(t0);
RWTexture2D<float4> u_sky_octo_output : register(u0);

// x = input padding, texels
// y = output padding, texels
// z = blur radius in output logical texels
// w = filter mode:
//     0 = box downsample
//     1 = 5x5 binomial blur/downsample
//     2 = normalized Lambertian diffuse irradiance
uniform float4 sky_octo_filter_params;

//-----------------------------------------------------------------------------
// Sampling
//-----------------------------------------------------------------------------

float4 sky_sample_octo_encoded(
    float2 encoded_direction,
    float2 input_resolution,
    float input_padding)
{
    // Любая координата, включая находящуюся за краем octomap,
    // сначала переводится в направление.
    const float3 direction =
        sky_gt7_octahedral_decode(
            encoded_direction);

    // Затем направление канонически кодируется обратно.
    // Благодаря этому фильтр корректно пересекает octahedral seams.
    const float2 uv =
        sky_gt7_octahedral_texture_uv(
            direction,
            input_resolution,
            input_padding);

    return s_sky_octo_input.SampleLevel(
        smp_rtlinear,
        uv,
        0.0f);
}

//-----------------------------------------------------------------------------
// Box filter used for 512 -> 128
//-----------------------------------------------------------------------------

float4 sky_octo_box_downsample(
    float2 encoded_center,
    float2 output_core_resolution,
    float2 input_core_resolution,
    float2 input_resolution,
    float input_padding)
{
    const float factor_float =
        max(
            round(
                input_core_resolution.x /
                output_core_resolution.x),
            1.0f);

    const uint factor =
        (uint) factor_float;

    // Размер одного output texel в encoded-пространстве [-1, 1].
    const float2 output_texel_size =
        2.0f / output_core_resolution;

    const float2 sub_texel_size =
        output_texel_size / factor_float;

    float4 sum = 0.0f;

    [loop]
    for (uint y = 0; y < factor; ++y)
    {
        [loop]
        for (uint x = 0; x < factor; ++x)
        {
            const float2 sub_texel =
                float2(x, y) + 0.5f;

            const float2 offset =
                -0.5f * output_texel_size +
                sub_texel * sub_texel_size;

            sum +=
                sky_sample_octo_encoded(
                    encoded_center + offset,
                    input_resolution,
                    input_padding);
        }
    }

    return sum /
        max(factor_float * factor_float, 1.0f);
}

//-----------------------------------------------------------------------------
// Seam-aware 5x5 blur used for 128 -> 32
//-----------------------------------------------------------------------------

float sky_octo_binomial_weight(int offset)
{
    const int absolute_offset = abs(offset);

    if (absolute_offset == 0)
        return 6.0f;

    if (absolute_offset == 1)
        return 4.0f;

    return 1.0f;
}

float4 sky_octo_blur_downsample(
    float2 encoded_center,
    float2 output_core_resolution,
    float2 input_resolution,
    float input_padding,
    float blur_radius)
{
    const float2 output_texel_size =
        2.0f / output_core_resolution;

    // kernel offset [-2..2] покрывает диапазон
    // [-blur_radius..+blur_radius] output texels.
    const float2 blur_step =
        output_texel_size *
        (max(blur_radius, 0.0f) * 0.5f);

    float4 sum = 0.0f;
    float total_weight = 0.0f;

    [unroll]
    for (int y = -2; y <= 2; ++y)
    {
        const float weight_y =
            sky_octo_binomial_weight(y);

        [unroll]
        for (int x = -2; x <= 2; ++x)
        {
            const float weight_x =
                sky_octo_binomial_weight(x);

            const float weight =
                weight_x * weight_y;

            const float2 offset =
                float2(x, y) * blur_step;

            sum +=
                sky_sample_octo_encoded(
                    encoded_center + offset,
                    input_resolution,
                    input_padding) *
                weight;

            total_weight += weight;
        }
    }

    return sum / max(total_weight, 1e-6f);
}

//-----------------------------------------------------------------------------
// 8 sample Cosine-weighted diffuse irradiance 
//-----------------------------------------------------------------------------
void sky_build_orthonormal_basis(
    float3 normal,
    out float3 tangent,
    out float3 bitangent)
{
    const float3 reference_axis =
        abs(normal.y) < 0.999f
            ? float3(0.0f, 1.0f, 0.0f)
            : float3(1.0f, 0.0f, 0.0f);

    tangent = safe_normalize(cross(reference_axis, normal));
    bitangent = cross(normal, tangent);
}

float4 sky_sample_octo_direction(
    float3 direction,
    float2 input_resolution,
    float input_padding)
{
    const float2 uv =
        sky_gt7_octahedral_texture_uv(
            direction,
            input_resolution,
            input_padding);

    return s_sky_octo_input.SampleLevel(
        smp_rtlinear,
        uv,
        0.0f);
}

float3 sky_octo_diffuse_irradiance(
    float2 encoded_normal,
    float2 input_resolution,
    float input_padding)
{
    const float3 normal =
        sky_gt7_octahedral_decode(encoded_normal);

    float3 tangent;
    float3 bitangent;

    sky_build_orthonormal_basis(
        normal,
        tangent,
        bitangent);

// 32 precomputed cosine-weighted Vogel directions.
//
// local_direction.xy lies on the unit disk.
// local_direction.z = sqrt(1 - dot(xy, xy)).
//
// The resulting hemisphere distribution has PDF = NdotL / PI,
// therefore E / PI is simply the arithmetic mean of the samples.

    static const float3 sample_directions[32] =
    {
        float3(0.125000000f, 0.000000000f, 0.992156742f),
        float3(-0.159645045f, 0.146247939f, 0.976281209f),
        float3(0.024436233f, -0.278438271f, 0.960143218f),
        float3(0.201222239f, 0.262458779f, 0.943729304f),

        float3(-0.369267557f, -0.065318231f, 0.927024811f),
        float3(0.349802466f, -0.222515696f, 0.910013736f),
        float3(-0.117002079f, 0.435241902f, 0.892678554f),
        float3(-0.223135654f, -0.429634123f, 0.875000000f),

        float3(0.484115115f, 0.176798064f, 0.856956825f),
        float3(-0.503641109f, 0.207895728f, 0.838525492f),
        float3(0.242788294f, -0.518824483f, 0.819679816f),
        float3(0.179414374f, 0.572001296f, 0.800390530f),

        float3(-0.540757006f, -0.313379738f, 0.780624750f),
        float3(0.634369523f, -0.139464360f, 0.760345316f),
        float3(-0.387145845f, 0.550675126f, 0.739509973f),
        float3(-0.089439655f, -0.690199644f, 0.718070331f),

        float3(0.549071757f, 0.462758258f, 0.695970545f),
        float3(-0.738878471f, 0.030554945f, 0.673145601f),
        float3(0.538955125f, -0.536332334f, 0.649519053f),
        float3(-0.036058186f, 0.779791515f, 0.625000000f),

        float3(-0.512817532f, -0.614526793f, 0.599478940f),
        float3(0.812359593f, 0.109301834f, 0.572821962f),
        float3(-0.688310641f, 0.478908616f, 0.544862368f),
        float3(0.188086057f, -0.836061382f, 0.515388203f),

        float3(0.435033266f, 0.759191055f, 0.484122918f),
        float3(-0.850448410f, -0.271316239f, 0.450693909f),
        float3(0.826102401f, -0.381680263f, 0.414578099f),
        float3(-0.357888202f, 0.855155562f, 0.375000000f),

        float3(-0.319407334f, -0.888033758f, 0.330718914f),
        float3(0.849908635f, 0.446688159f, 0.279508497f),
        float3(-0.944034646f, 0.248844503f, 0.216506351f),
        float3(0.536595808f, -0.834529771f, 0.125000000f)
    };

    float3 irradiance = 0.0f;

    [unroll]
    for (uint sample_index = 0; sample_index < 32; ++sample_index)
    {
        const float3 local_direction =
            sample_directions[sample_index];

        const float3 world_direction =
            tangent * local_direction.x +
            bitangent * local_direction.y +
            normal * local_direction.z;

        irradiance +=
            sky_sample_octo_direction(
                world_direction,
                input_resolution,
                input_padding).rgb;
    }

    // Stores E / PI, matching AmbientLightingImpl, which multiplies
    // the result directly by diffuse albedo without another 1 / PI.
    return irradiance * (1.0f / 32.0f);
}


//-----------------------------------------------------------------------------
// Entry point
//-----------------------------------------------------------------------------

[numthreads(8, 4, 1)]
void main(uint2 dispatch_id : SV_DispatchThreadID)
{
    uint output_width;
    uint output_height;

    u_sky_octo_output.GetDimensions(
        output_width,
        output_height);

    if (dispatch_id.x >= output_width ||
        dispatch_id.y >= output_height)
    {
        return;
    }

    uint input_width;
    uint input_height;

    s_sky_octo_input.GetDimensions(
        input_width,
        input_height);

    const float input_padding =
        sky_octo_filter_params.x;

    const float output_padding =
        sky_octo_filter_params.y;

    const float blur_radius =
        sky_octo_filter_params.z;

    const float2 input_resolution =
        float2(input_width, input_height);

    const float2 output_resolution =
        float2(output_width, output_height);

    const float2 input_core_resolution =
        input_resolution -
        2.0f * input_padding;

    const float2 output_core_resolution =
        output_resolution -
        2.0f * output_padding;

    // Padding texels получают logical coordinate за пределами [0,1].
    const float2 logical_uv =
        (
            float2(dispatch_id) +
            0.5f -
            output_padding
        ) /
        output_core_resolution;

    const float2 encoded_center =
        logical_uv * 2.0f - 1.0f;

    float4 filtered;

    const uint filter_mode =
    (uint) round(sky_octo_filter_params.w);

    if (filter_mode == 2u)
    {
        filtered = float4(
        sky_octo_diffuse_irradiance(
            encoded_center,
            input_resolution,
            input_padding),
        1.0f);
    }
    else if (filter_mode == 1u)
    {
        filtered =
        sky_octo_blur_downsample(
            encoded_center,
            output_core_resolution,
            input_resolution,
            input_padding,
            blur_radius);
    }
    else
    {
        filtered =
        sky_octo_box_downsample(
            encoded_center,
            output_core_resolution,
            input_core_resolution,
            input_resolution,
            input_padding);
    }

    // Octomap хранится в linear HDR.
    u_sky_octo_output[dispatch_id] =
        float4(
            max(filtered.rgb, 0.0f),
            1.0f);
}
