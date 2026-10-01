#include "common_clouds.hlsli"
#include "common_aerial.hlsli"

// Kept bound for the next sampling implementation. The current deterministic
// raymarch only queries its dimensions and does not sample its values.
Texture3D<float4> s_cloud_fastnoise : register(t1);
// Bound through pass.T after the shadow-blur UAV has been removed.
Texture2D<float4> s_cloud_shadow_map : register(t2);
uniform float4x4 cloud_shadow_view_projection; // camera-relative km -> light clip
uniform float4 cloud_shadow_params; // w: 0 = legacy, 1 = coarse map + one local probe
Texture2D<float4> s_cloud_transmittance_lut : register(t3);
Texture3D<float4> s_cloud_aerial_perspective : register(t4); // ambient AP
Texture3D<float4> s_cloud_aerial_direct : register(t5);
Texture3D<float4> s_cloud_aerial_transmittance : register(t6);
Texture2D<float4> s_cloud_sky_octo_small : register(t7);
RWTexture2D<float4> u_procedural_clouds : register(u0);

static const uint CLOUD_MAX_VIEW_STEPS = 1000u;
static const uint CLOUD_SHADOW_STEPS = 6u;
static const float CLOUD_SHADOW_RANGE_KM = 3.5f;
// Replace the first legacy shadow segment: ~83 m, sampled at its midpoint (~42 m).
static const float CLOUD_SHADOW_LOCAL_RANGE_KM = CLOUD_SHADOW_RANGE_KM / float(CLOUD_SHADOW_STEPS);
// AP and cloud visibility share the distance in atmosphere_config.h.
static const float CLOUD_AP_SCATTERING_SCALE = 1.0f;
static const float CLOUD_FADE_START_KM = 96.0f;
static const float CLOUD_FADE_END_KM = SKY_AP_MAX_DISTANCE_KM;
static const float CLOUD_MAX_DISTANCE_KM = CLOUD_FADE_END_KM;
static const float CLOUD_TRANSMITTANCE_CUTOFF = 0.01f;
static const float CLOUD_PHASE_G = 0.8f;
static const float CLOUD_PHASE_BACKWARD_G = -0.5f;
static const float CLOUD_PHASE_FORWARD_WEIGHT = 0.5f;
static const float CLOUD_MS_PHASE_G = 0.0f; // isotropic secondary scattering
static const float CLOUD_MS_DEPTH_POWER = 0.25f;
static const float CLOUD_MS_HEIGHT_POWER = 0.25f;
static const float CLOUD_TYPE = 1.0f;
static const float CLOUD_LIGHTING_SCALE = 1.0 * SKY_RADIANCE_SCALE;
static const float CLOUD_AMBIENT_STRENGTH = 2.0f;
// The complete random interval is one texel wide: [-0.5, +0.5).
static const float CLOUD_NOISE_OFFSET_RADIUS_TEXELS = 0.5f;

static const float2 VOGEL_DISK_6[6] =
{
    float2(0.0475f, 0.2849f), // i = 0
    float2(-0.4357f, -0.2515f), // i = 1
    float2(0.4496f, -0.4746f), // i = 2
    float2(-0.1650f, 0.7454f), // i = 3
    float2(-0.2745f, -0.7111f), // i = 4
    float2(0.6171f, 0.3667f) // i = 5
};

uint cloud_hash(uint value)
{
    value ^= value >> 16u;
    value *= 0x7feb352du;
    value ^= value >> 15u;
    value *= 0x846ca68bu;
    return value ^ (value >> 16u);
}

float cloud_random_01(uint seed)
{
    // Keep 24 significant bits: this maps exactly to the mantissa of a float.
    return float(cloud_hash(seed) & 0x00ffffffu) * (1.0f / 16777216.0f);
}

float3 cloud_noise_texel_offset(uint2 pixel, uint step_index, uint sequence_index)
{
    uint seed = pixel.x * 1973u + pixel.y * 9277u;
    seed ^= step_index * 26699u;
    seed ^= sequence_index * 31847u;

    float3 random_01 = float3(
        cloud_random_01(seed ^ 0x68bc21ebu),
        cloud_random_01(seed ^ 0x02e5be93u),
        cloud_random_01(seed ^ 0x967a889bu));
    return (random_01 * 2.0f - 1.0f) * CLOUD_NOISE_OFFSET_RADIUS_TEXELS;
}

float3 cone_offsets(float2 disksample, float spread, float3 conedir)
{
    float2 offset = disksample * spread;
    float3 offsetAxis = abs(conedir.z) < 0.999f ? float3(0.0f, 0.0f, 1.0f) : float3(1.0f, 0.0f, 0.0f);
    float3 raydir = conedir + offsetAxis * offset.x + float3(offset.y, -offset.y, 0.0f) * offset.y;
    return normalize(raydir);
}

// Legacy reference: one full-density probe, followed by cheap cone probes.
// Extinction has one owner in HLSL and is shared with the view integral.
float cloud_shadow(float3 world_position, float3 position, float3 sun, float2 layer, float profile, float3 noise_texel_offset)
{
    const float ds = CLOUD_SHADOW_RANGE_KM / float(CLOUD_SHADOW_STEPS);
    float tau = 0.0f;
    float3 offset = sun * 0.5f * ds;
    float h = (length(position + offset) - layer.x) * layer.y;
    float unused_coverage;
    tau += cloud_density(world_position + offset, h, profile, noise_texel_offset, unused_coverage) * (CLOUD_EXTINCTION_KM_INV * ds);
    [loop]
    for (uint i = 1u; i < CLOUD_SHADOW_STEPS; ++i)
    {
        offset = cone_offsets(VOGEL_DISK_6[i], 0.2f, sun) * ((float(i) + 0.5f) * ds); //sun * ((float(i) + 0.5f) * ds);
        h = (length(position + offset) - layer.x) * layer.y;
        tau += cloud_density_cheap(world_position + offset, h, profile, noise_texel_offset, unused_coverage) * (CLOUD_EXTINCTION_KM_INV * ds);
    }
    return exp(-tau);
}

float cloud_shadow_from_map(float3 camera_relative_position)
{
    float4 clip = mul(cloud_shadow_view_projection, float4(camera_relative_position, 1.0f));
    float3 projected = clip.xyz / clip.w;
    float2 uv = projected.xy * float2(0.5f, -0.5f) + 0.5f;
    if (any(uv < 0.0f) || any(uv > 1.0f) || projected.z < 0.0f || projected.z > 1.0f)
        return 1.0f;
    float4 shadow = s_cloud_shadow_map.SampleLevel(smp_rtlinear, uv, 0.0f);
    if (shadow.b <= shadow.g)
        return 1.0f;
    // Coarse assumption: total extinction is uniform along the stored interval.
    // Not a reconstruction of the actual density distribution or legacy 0.5-km cone.
    float fraction = saturate((projected.z - shadow.g) / (shadow.b - shadow.g));
    return exp(-max(shadow.r, 0.0f) * fraction);
}

float4 cloud_render(uint2 pixel, uint2 size)
{
    // Keep the reserved noise SRV reflected and bound without using it to perturb
    // either ray positions or 3D density coordinates.
    uint fast_width, fast_height, fast_depth;
    s_cloud_fastnoise.GetDimensions(fast_width, fast_height, fast_depth);
    if (fast_width == 0u || fast_height == 0u || fast_depth == 0u)
        return float4(0.0f, 0.0f, 0.0f, 1.0f);

    float camera_height = max(eye_position.y * cloud_layer_params.z, 0.0f);
    if (cloud_layer_params.x <= camera_height || cloud_layer_params.y <= cloud_layer_params.x)
        return float4(0.0f, 0.0f, 0.0f, 1.0f);

    // History uses an unjittered grid; sky.ps corrects the scene jitter at composition.
    float2 uv = (float2(pixel) + 0.5f) / float2(size);
    float3 direction = sky_world_ray_direction_from_screen_uv(uv);
    float3 origin = cloud_planet_camera();
    float3 world_origin = eye_position * cloud_layer_params.z;
    float2 layer = float2(SKY_EARTH_RADIUS + cloud_layer_params.x, rcp(cloud_layer_params.y - cloud_layer_params.x));

    float ray_start = sky_ray_sphere_intersection(origin, direction, layer.x);
    float ray_stop = min(CLOUD_MAX_DISTANCE_KM, sky_ray_sphere_intersection(origin, direction, SKY_EARTH_RADIUS + cloud_layer_params.y));
    float ground_hit = sky_ray_sphere_intersection(origin, direction, SKY_EARTH_RADIUS);
    if (ray_start < 0.0f || ray_stop <= ray_start || (ground_hit >= 0.0f && ground_hit < ray_start))
        return float4(0.0f, 0.0f, 0.0f, 1.0f);

    // Fixed budget and uniform integration segments.
    float min_steps = clamp(32.f, 1.f, float(CLOUD_MAX_VIEW_STEPS));
    float max_steps = clamp(140.f, min_steps, float(CLOUD_MAX_VIEW_STEPS));
    float zenith = saturate(direction.y);
    uint steps = uint(ceil(lerp(min_steps, max_steps, 1.0f - zenith * zenith)));
    float ds = (ray_stop - ray_start) / float(steps);

    float3 sun = safe_normalize(-L_sun_dir_w);
    float cos_angle = clamp(dot(direction, sun), -1.0f, 1.0f);
    float primary_phase = lerp(cloud_henyey_greenstein(cos_angle, CLOUD_PHASE_BACKWARD_G), cloud_henyey_greenstein(cos_angle, CLOUD_PHASE_G), CLOUD_PHASE_FORWARD_WEIGHT);//    +pow(max(cos_angle, 0.0f), 64.0f) * 0.5f;
    float secondary_phase = cloud_henyey_greenstein(cos_angle, CLOUD_MS_PHASE_G);

    // One atmospheric sunlight colour for the whole layer; no global planet mask.
    uint lut_width, lut_height;
    s_cloud_transmittance_lut.GetDimensions(lut_width, lut_height);
    float sun_radius = SKY_EARTH_RADIUS + 0.5f * (cloud_layer_params.x + cloud_layer_params.y);
    float horizon_mu = -sqrt(max(sun_radius * sun_radius - SKY_EARTH_RADIUS * SKY_EARTH_RADIUS, 0.0f)) / sun_radius;
    float2 sun_uv = sky_transmittance_uv_from_r_mu(sun_radius, max(sun.y, horizon_mu), float2(lut_width, lut_height));
    float4 sun_T = s_cloud_transmittance_lut.SampleLevel(smp_rtlinear, sun_uv, 0.0f);
    float3 direct_source = sky_source_rgb(SKY_SUN_SPECTRAL_IRRADIANCE * sun_T);
    bool has_sun = any(direct_source > 0.0f);

    // Layer-uniform zenith ambient: one lookup per surviving ray, before marching.
    // Small octomap is already linear HDR with SkyView's x4 gain, padding = 1.
    // This is a blurred sky-colour proxy, not a hemispherical irradiance integral.
    float3 atmosphere_ambient_rgb = max(sky_sample_gt7_octahedral_map(s_cloud_sky_octo_small, smp_rtlinear, float3(0.0f, 1.0f, 0.0f), 1.0f).rgb, 0.0f);
    float amb_luma = dot(atmosphere_ambient_rgb, LUMINANCE_VECTOR);
    atmosphere_ambient_rgb = lerp(amb_luma.xxx, atmosphere_ambient_rgb.rgb, 0.6f);

    float transmittance = 1.0f;
    float opacity = 0.0f;
    float distance_sum = 0.0f;
    float3 radiance = 0.0f;
    [loop]
    for (uint i = 0u; i < steps; ++i)
    {
        if (transmittance <= CLOUD_TRANSMITTANCE_CUTOFF)
            break;

        // The integration point stays at the segment midpoint. Only the discrete
        // 3D-noise lookup moves, preserving the ray integration distances.
        float distance = ray_start + (float(i) + 0.5f) * ds;
        float3 position = origin + direction * distance;
        float3 world_position = world_origin + direction * distance;
        float h = (length(position) - layer.x) * layer.y;
        float cloud_coverage;
        // Stable with the current replace-only history resolve. sequence_index can
        // advance once the resolve blends repeated measurements of the same pixel.
        float3 noise_texel_offset = cloud_noise_texel_offset(pixel, i, 0u);
        // Shared profile; preserve the author's current shaping parameters.
        float profile = cloud_vertical_profile(h);
        float density = cloud_density(
            world_position, h, profile, noise_texel_offset, cloud_coverage);
        if (density <= 0.0f)
            continue;

        float step_T = exp(-density * CLOUD_EXTINCTION_KM_INV * ds);
        // Reduce contrast gradually inside the AP range as well as beyond it.
        // The LUT boundary must not mark the start of a separate rapid fade.
        float visibility = 1.0f - smoothstep(CLOUD_FADE_START_KM, CLOUD_FADE_END_KM, distance);
        float weight = transmittance * (1.0f - step_T) * visibility;
        // Retain physical cloud occlusion/early exit independently of visible opacity.
        transmittance *= step_T;
        if (weight <= 0.0f)
            continue;

        // Planet occlusion stays local, including partially sunlit cloud layers.
        float sun_dot_position = dot(position, sun);
        bool planet_shadow = sun_dot_position < 0.0f &&
            dot(position, position) - sun_dot_position * sun_dot_position <= SKY_EARTH_RADIUS * SKY_EARTH_RADIUS;
        float3 direct = 0.0f;
        [branch]
        if (has_sun && !planet_shadow)
        {
            float attenuated_light;
            [branch]
            if (cloud_shadow_params.w < 0.5f)
                attenuated_light = cloud_shadow(world_position, position, sun, layer, profile, noise_texel_offset);
            else
            {
                float3 local_offset = sun * (0.5f * CLOUD_SHADOW_LOCAL_RANGE_KM);
                float local_h = (length(position + local_offset) - layer.x) * layer.y;
                float unused_coverage;
                float local_density = cloud_density(world_position + local_offset, local_h,
                    cloud_vertical_profile(local_h), noise_texel_offset, unused_coverage);
                float local_tau = local_density * (CLOUD_EXTINCTION_KM_INV * CLOUD_SHADOW_LOCAL_RANGE_KM);
                // The map integrates only up to the sunward end of the local segment.
                // Multiplying a map lookup at the receiver would count that segment twice.
                float distant_light = cloud_shadow_from_map(direction * distance + sun * CLOUD_SHADOW_LOCAL_RANGE_KM);
                attenuated_light = distant_light * exp(-local_tau);
            }
            // Dimensional profile = final eroded density; step size ds is in km.
            // This artistic MS term intentionally depends on the view step size.
            float powder = 1.0f - attenuated_light * attenuated_light;
            float ms_volume = saturate(Remap(profile * ds, 0.1f, 1.0f, 0.1f, 1.0f));// * pow(saturate(profile * CLOUD_TYPE), 0.5f);
            ms_volume *= pow(saturate(attenuated_light), CLOUD_MS_DEPTH_POWER);
            ms_volume *= pow(saturate(h), CLOUD_MS_HEIGHT_POWER);
            float direct_scattering = 1.0 * powder * attenuated_light * primary_phase + 2.0 * ms_volume * secondary_phase;
            direct = direct_source * direct_scattering;
        }
        float ambient_scattering = pow(saturate(1.0 - 0.9 * (profile)), 0.5f);// * lerp(0.5, 1.1, h * h);
        //atmosphere_ambient_rgb *= lerp(0.8, 1.0, h);
        float3 ambient = atmosphere_ambient_rgb * (CLOUD_AMBIENT_STRENGTH * ambient_scattering);

        // Ambient is already in SkyView's radiance units; scale direct exactly once.
        radiance += weight * (direct * CLOUD_LIGHTING_SCALE + 1.0f * ambient);
        opacity += weight;
        distance_sum += weight * distance;
    }

    if (opacity <= 1e-6f)
        return float4(0.0f, 0.0f, 0.0f, 1.0f);

    // Mean-depth AP uses the same faded weights as premultiplied radiance/opacity.
    float depth_km = distance_sum / opacity;
    uint ap_width, ap_height, ap_depth;
    s_cloud_aerial_perspective.GetDimensions(ap_width, ap_height, ap_depth);
    float3 ap_uv = sky_aerial_uv(uv, depth_km, ap_depth);
    float3 ap_ambient = s_cloud_aerial_perspective.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
    float3 ap_direct = s_cloud_aerial_direct.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
    float3 ap_T = s_cloud_aerial_transmittance.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
    radiance = radiance * saturate(ap_T) + CLOUD_AP_SCATTERING_SCALE * max(ap_ambient + ap_direct, 0.0f) * saturate(opacity);

    // Fade the silhouette too: raw=(0,0,0,1) restores sky_view exactly.
    return float4(radiance, 1.0f - saturate(opacity));
}

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint width, height;
    u_procedural_clouds.GetDimensions(width, height);
    if (dispatch_id.x >= width || dispatch_id.y >= height)
        return;

    u_procedural_clouds[dispatch_id.xy] = cloud_render(
        cloud_trace_pixel(dispatch_id.xy), uint2(cloud_screen_params.xy));
}
