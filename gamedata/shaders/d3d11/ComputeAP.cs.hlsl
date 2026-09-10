#include "common_sky.hlsli"

Texture2D<float4> s_transmittance_lut : register(t0);
Texture2D<float4> s_multi_scattering_lut : register(t1);

RWTexture3D<float4> u_aerial_perspective : register(u0);

// Максимальная дистанция, которую покрывает volume.
// Единицы измерения — километры.
uniform float sky_aerial_max_distance; // hardcoded temp

// На каждый depth slice выполняются две новые выборки атмосферы.
// Это соответствует исходной реализации clouds_v3.
#ifndef SKY_AERIAL_SLICE_STEPS
    #define SKY_AERIAL_SLICE_STEPS 2
#endif

//-----------------------------------------------------------------------------
// Aerial-perspective integration
//-----------------------------------------------------------------------------

void sky_integrate_aerial_segment(
    float3 ray_origin,
    float3 ray_direction,
    float3 sun_direction,
    float segment_start,
    float segment_end,
    float molecular_phase,
    float aerosol_phase,
    inout float4 accumulated_inscattering,
    inout float4 accumulated_transmittance)
{
    float segment_length = segment_end - segment_start;

    if (segment_length <= 0.0f)
    {
        return;
    }

    float step_length =
        segment_length /
        float(SKY_AERIAL_SLICE_STEPS);

    [unroll]
    for (uint step_index = 0;
         step_index < SKY_AERIAL_SLICE_STEPS;
         ++step_index)
    {
        // Выборка в центре текущего интеграционного интервала.
        float sample_distance =
            segment_start +
            (float(step_index) + 0.5f) *
            step_length;

        float3 sample_position =
            ray_origin +
            ray_direction * sample_distance;

        float distance_to_earth_center =
            length(sample_position);

        float3 zenith_direction =
            sample_position /
            max(distance_to_earth_center, 1e-6f);

        float altitude =
            max(
                distance_to_earth_center -
                SKY_EARTH_RADIUS,
                0.0f);

        float normalized_altitude =
            altitude /
            SKY_ATMOSPHERE_THICKNESS;

        float sun_zenith_cosine =
            dot(
                zenith_direction,
                sun_direction);

        float4 aerosol_absorption;
        float4 aerosol_scattering;
        float4 molecular_absorption;
        float4 molecular_scattering;
        float4 extinction;

        sky_get_collision_coefficients(
            altitude,
            aerosol_absorption,
            aerosol_scattering,
            molecular_absorption,
            molecular_scattering,
            extinction);

        float4 sun_transmittance =
            sky_transmittance_to_sun(
                s_transmittance_lut,
                smp_rtlinear,
                sample_position,
                sun_direction);

#if SKY_ENABLE_MULTIPLE_SCATTERING

        float4 multiple_scattering =
            sky_sample_multiscattering_lut(
                s_multi_scattering_lut,
                smp_rtlinear,
                sun_zenith_cosine,
                normalized_altitude);

#else

        float4 multiple_scattering =
            float4(
                0.0f,
                0.0f,
                0.0f,
                0.0f);

#endif

        float4 scattering =
            molecular_scattering +
            aerosol_scattering;

        float4 single_scattering =
            molecular_scattering * molecular_phase +
            aerosol_scattering * aerosol_phase;

        float4 source =
            SKY_SUN_SPECTRAL_IRRADIANCE *
            (
                sun_transmittance *
                single_scattering +

                scattering *
                multiple_scattering
            );

        float4 step_transmittance =
            exp(-step_length * extinction);

        // Аналитическое интегрирование источника внутри сегмента.
        float4 integrated_source =
            source *
            (1.0f - step_transmittance) /
            max(extinction, 1e-7f);

        accumulated_inscattering +=
            accumulated_transmittance *
            integrated_source;

        accumulated_transmittance *=
            step_transmittance;
    }
}

//-----------------------------------------------------------------------------
// Compute entry point
//-----------------------------------------------------------------------------

// 8 * 4 = 32 потока — один wavefront.
//
// Каждый поток отвечает за одну XY-координату и последовательно
// рассчитывает все depth slices. Это позволяет не интегрировать
// атмосферу заново от камеры для каждого voxel.
[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint output_width;
    uint output_height;
    uint output_depth;

    u_aerial_perspective.GetDimensions(
        output_width,
        output_height,
        output_depth);

    const uint2 output_pixel =
        dispatch_id.xy;

    if (output_pixel.x >= output_width ||
        output_pixel.y >= output_height)
    {
        return;
    }

    float2 screen_uv =
        (float2(output_pixel) + 0.5f) /
        float2(output_width, output_height);

    // Используем ту же формулу высоты камеры, что и ComputeSkyView.
    // Высота атмосферы задаётся в километрах.
    float camera_elevation =
        max(
            0.002f * eye_position.y + 0.2f,
            0.0f);

    // Атмосферная модель локальна относительно камеры по XZ.
    // Реальная world position нужна только для получения ориентации луча.
    float3 ray_origin =
        float3(
            0.0f,
            SKY_EARTH_RADIUS + camera_elevation,
            0.0f);

    float3 ray_direction = sky_world_ray_direction_from_screen_uv(screen_uv);

    // X-Ray хранит направление распространения солнечного света.
    // Атмосферные функции ожидают направление от точки к Солнцу.
    float3 sun_direction =
        safe_normalize(-L_sun_dir_w);

    float scattering_angle_cosine =
        dot(
            -ray_direction,
            sun_direction);

    float molecular_phase =
        sky_molecular_phase(
            scattering_angle_cosine);

    float aerosol_phase =
        sky_aerosol_phase(
            scattering_angle_cosine);

    float atmosphere_start;
    float atmosphere_end;

    const bool intersects_atmosphere =
        sky_atmosphere_ray_interval(
            ray_origin,
            ray_direction,
            atmosphere_start,
            atmosphere_end);

    float maximum_distance =
        max(
            sky_aerial_max_distance,
            0.0f);

    float integration_limit =
        intersects_atmosphere
            ? min(atmosphere_end, maximum_distance)
            : 0.0f;

    float4 accumulated_inscattering =
        float4(
            0.0f,
            0.0f,
            0.0f,
            0.0f);

    float4 accumulated_transmittance =
        float4(
            1.0f,
            1.0f,
            1.0f,
            1.0f);

    float previous_distance = 0.0f;

    [loop]
    for (uint slice_index = 0;
         slice_index < output_depth;
         ++slice_index)
    {
        float unit_depth =
            output_depth > 1
                ? float(slice_index) /
                  float(output_depth - 1)
                : 0.0f;

        // Квадратичное распределение даёт больше точности
        // вблизи камеры.
        float requested_distance =
            maximum_distance *
            unit_depth *
            unit_depth;

        float current_distance =
            min(
                requested_distance,
                integration_limit);

        float segment_start =
            max(
                previous_distance,
                atmosphere_start);

        sky_integrate_aerial_segment(
            ray_origin,
            ray_direction,
            sun_direction,
            segment_start,
            current_distance,
            molecular_phase,
            aerosol_phase,
            accumulated_inscattering,
            accumulated_transmittance);

#if SKY_ENABLE_SPECTRAL

        // Match the existing SkyView radiance gain; never scale AP opacity.
        float3 scattering_rgb =
            4.0f * sky_linear_srgb_from_spectral_samples(
                accumulated_inscattering);

#else

        float3 scattering_rgb =
            max(
                accumulated_inscattering.rgb,
                0.0f);

#endif

        // В RGB хранится linear HDR inscattering.
        //
        // Alpha — скалярная атмосферная opacity:
        //     alpha = 1 - transmittance.
        //
        // При композиции:
        //     result = scene * (1 - alpha) + scattering.rgb
        float scalar_transmittance =
            dot(
                saturate(accumulated_transmittance),
                float4(
                    0.25f,
                    0.25f,
                    0.25f,
                    0.25f));

        u_aerial_perspective[
            uint3(output_pixel, slice_index)
        ] = float4(
            max(scattering_rgb, 0.0f),
            1.0f - scalar_transmittance);

        previous_distance =
            current_distance;
    }
}
