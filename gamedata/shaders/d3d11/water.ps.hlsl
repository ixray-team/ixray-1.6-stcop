// water pseudo pbr

#include "common.hlsli"
#include "reflections.hlsli"
#include "shadow.hlsli"
#include "metalic_roughness_light.hlsli"

struct vf
{
    float2 tbase    : TEXCOORD0;
    float2 tnorm0   : TEXCOORD1;
    float2 tnorm1   : TEXCOORD2;
    float3 M1       : TEXCOORD3;
    float3 M2       : TEXCOORD4;
    float3 M3       : TEXCOORD5;
    float3 v2point  : TEXCOORD6;
    float4 tctexgen : TEXCOORD7;
    float3 pos      : TEXCOORD8;
    float4 c0       : COLOR0;
    float4 hpos     : SV_POSITION;
};

uniform float3 water_intensity;

Texture2D   s_nmap;
TextureCube s_env0;
TextureCube s_env1;

Texture2D   s_leaves;
Texture2D   s_caustic;

struct WaterLayerMaterial
{
    float3 BaseColor;
    float3 F0;
    float  Metalness;
    float  Roughness;
    float  Transmission;
    float  Alpha;
};

float3 SafeNormalize(float3 v)
{
    return normalize(v + 1e-6f);
}

float2 WaterEnvBRDFApprox(float NdotV, float Roughness)
{
    NdotV = min(NdotV, 0.998f);

    float N2 = NdotV * NdotV;
    float R2 = Roughness * Roughness;

    float4 Fac =
        float4(0.0187f, 1.0133f, 1.0000f, 1.0000f) +
        float4(1.9496f, -2.4717f, -0.0333f, 2.0508f) * NdotV +
        float4(1.2265f, -1.2172f, -1.3097f, 0.2342f) * Roughness +
        float4(-7.6907f, 3.4300f, 0.5972f, -26.9406f) * NdotV * Roughness +
        float4(18.3314f, 1.4794f, 19.3537f, 11.1429f) * N2 +
        float4(-0.2894f, 0.5564f, 1.5052f, 7.0828f) * R2 +
        float4(-19.3056f, -2.2456f, -28.2302f, 18.5470f) * N2 * Roughness +
        float4(7.0144f, -1.8934f, 1.3307f, 50.6469f) * NdotV * R2 +
        float4(1.5728f, 1.3618f, 15.2939f, -63.3557f) * N2 * R2;

    return saturate(Fac.xy / Fac.zw);
}

float3 WaterComputeDiffuseIrradiance(float3 N, float Hemi)
{
    float3 LightDirection = mul((float3x3)m_invV, N).xyz;

    #ifdef IBL_REMAP_IRRADANCE
        RemapVector(LightDirection);
    #endif

    #ifdef USE_NORMAL_HEMI_DISTRIBUTION
        Hemi = min(Hemi, LightDirection.y * 0.375f + 0.375f);
    #endif

        float3 SampleLast = env_s0.SampleLevel(smp_rtlinear, LightDirection, 0.0f).xyz;
        float3 SampleNext = env_s1.SampleLevel(smp_rtlinear, LightDirection, 0.0f).xyz;

    #ifdef USE_CGIM_SKY_TWEAK
        float TopToDown = saturate(LightDirection.y);
        TopToDown *= TopToDown;

        float Factor = SMALLSKY_TOP_VECTOR_POWER; // tuning
        Factor = saturate(Factor + (1.0f - Factor) * TopToDown) + (1.0f - Factor) * 0.5f; // tuning

        Hemi *= Factor * Factor;
        float3 Irradiance = lerp(SampleLast, SampleNext, L_hemi_color.w);
    #else
        float3 Irradiance = lerp(SampleLast, SampleNext, L_hemi_color.w);
    #endif

    #ifdef USE_DIFFUSE_SKY_COLOR
        #ifdef USE_BGRA_SKYCOLOR
            Irradiance *= L_sky_color.zyx;
        #else
            Irradiance *= L_sky_color.xyz;
        #endif
    #else
        Irradiance *= L_hemi_color.xyz;
    #endif

    #ifdef USE_LEGACY_LIGHT
        Irradiance *= Irradiance;
    #endif

    return Irradiance * Hemi;
}

float3 WaterComputeSpecularIrradiance(float3 R, float Hemi, float Roughness)
{
    float3 ReflectDir = R;
    float2 Rotation = 0.0f;
    sincos(L_sky_color.w, Rotation.x, Rotation.y);

    ReflectDir.xz = float2(
        ReflectDir.x * Rotation.y - ReflectDir.z * Rotation.x,
        ReflectDir.x * Rotation.x + ReflectDir.z * Rotation.y
    );

    #ifndef USE_FULL_SKY_SPHERE
        RemapVector(ReflectDir);
    #endif

    #ifndef IBL_MAX_LOD
        float4 MipLevels = 0.0f;
        s_env0.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
        float2 Lod = MipLevels.w * Roughness;

        #ifdef USE_HQ_SKY2_LOD
            s_env1.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
            Lod.y = MipLevels.w * Roughness;
        #endif
    #else
        float2 Lod = IBL_MAX_LOD * Roughness;
    #endif

    float3 SampleLast = s_env0.SampleLevel(smp_rtlinear, ReflectDir, Lod.x).xyz;
    float3 SampleNext = s_env1.SampleLevel(smp_rtlinear, ReflectDir, Lod.y).xyz;

    float3 Irradiance = lerp(SampleLast, SampleNext, L_ambient.w);

    #ifdef USE_BGRA_SKYCOLOR
        Irradiance *= L_sky_color.zyx;
    #else
        Irradiance *= L_sky_color.xyz;
    #endif

    Irradiance *= Hemi;

    return Irradiance;
}

float3 WaterEvaluateAmbientLighting(
    float3 DiffuseIrradiance,
    float3 SpecularIrradiance,
    float  NdotV,
    float3 BaseColor,
    float  Metalness,
    float  Roughness,
    float3 F0)
{
    DiffuseIrradiance *= (1.0f - Metalness) * BaseColor;

    float2 BRDF = WaterEnvBRDFApprox(NdotV, Roughness);
    float3 F = lerp(F0, BaseColor, Metalness) * BRDF.x + BRDF.y;

    return DiffuseIrradiance * (1.0f - saturate(F)) + SpecularIrradiance * F;
}

float3 EvaluateWaterAmbientIBL(
    float3 View,
    float3 Normal,
    float3 F0,
    float3 BaseColor,
    float  Metalness,
    float  Roughness,
    float  Hemi,
    float3 OverrideReflections,
    float  OverrideFactor)
{
    float3 Reflect = reflect(View, Normal);
    float NdotV = max(0.0f, dot(Normal, -View));

    float3 DiffuseIrradiance  = WaterComputeDiffuseIrradiance(Normal, Hemi) + L_ambient.xyz;
    float3 SpecularIrradiance = WaterComputeSpecularIrradiance(Reflect, Hemi, Roughness);

    SpecularIrradiance = lerp(SpecularIrradiance, OverrideReflections, OverrideFactor);

    return WaterEvaluateAmbientLighting(
        DiffuseIrradiance,
        SpecularIrradiance,
        NdotV,
        BaseColor,
        Metalness,
        Roughness,
        F0
    );
}

WaterLayerMaterial BuildWaterMaterial(float4 baseSample, float3 tintedBaseColor, float3 FresnelBase)
{
    WaterLayerMaterial M;
    M.BaseColor = saturate(tintedBaseColor);
    M.Metalness = 0.0f;
    M.F0        = float3(0.020f, 0.020f, 0.020f);
    M.Roughness = 0.02f; //saturate(0.00f + (1.0f - baseSample.w) * 0.09f); // tuning
    M.Transmission = saturate( (1.0f - Luminance(FresnelBase))); // tuning
    M.Alpha = saturate(0.12f + Luminance(FresnelBase) * 0.88f); // tuning
    return M;
}

WaterLayerMaterial BuildFoamMaterial(float4 foamSample, float3 foamColor, WaterLayerMaterial WaterM)
{
    WaterLayerMaterial M;
    float3 FoamTint = saturate(lerp(WaterM.BaseColor, foamColor, 0.65f)); // tuning
    FoamTint = lerp(FoamTint, max(FoamTint, WaterM.BaseColor), 0.35f);
    M.BaseColor = FoamTint;
    M.Metalness = 0.0f;
    M.F0        = float3(0.040f, 0.040f, 0.040f); // tuning
    M.Roughness = 0.78f; // tuning
    M.Transmission = saturate(0.12f + 0.18f * (1.0f - foamSample.w)); // tuning
    M.Alpha = saturate(0.1f + 0.9f * foamSample.w); // tuning
    return M;
}

WaterLayerMaterial BlendMaterials(
    WaterLayerMaterial WaterM,
    WaterLayerMaterial FoamM,
    float FoamFactor)
{
    WaterLayerMaterial M;

    float BaseColorFactor    = FoamFactor * 1.00f; // tuning
    float F0Factor           = FoamFactor * 1.00f; // tuning
    float RoughnessFactor    = FoamFactor * 1.00f; // tuning
    float TransmissionFactor = FoamFactor * 1.00f; // tuning
    float AlphaFactor        = FoamFactor * 1.00f; // tuning

    M.BaseColor    = lerp(WaterM.BaseColor,    FoamM.BaseColor,    BaseColorFactor);
    M.F0           = lerp(WaterM.F0,           FoamM.F0,           F0Factor);
    M.Metalness    = WaterM.Metalness;
    M.Roughness    = lerp(WaterM.Roughness,    FoamM.Roughness,    RoughnessFactor);
    M.Transmission = lerp(WaterM.Transmission, FoamM.Transmission, TransmissionFactor);
    M.Alpha        = lerp(WaterM.Alpha,        FoamM.Alpha,        AlphaFactor);

    return M;
}

float Pow5(float x)
{
    float x2 = x * x;
    return x2 * x2 * x;
}

float D_GGX(float linearRoughness, float NoH)
{
    float oneMinusNoHSquared = 1.0f - NoH * NoH;
    float a = NoH * linearRoughness;
    float k = linearRoughness / max(oneMinusNoHSquared + a * a, 1e-6f);
    float d = (k * k) * (1.0f / PI);
    return d;
}

float V_SmithGGXCorrelated(float linearRoughness, float NoV, float NoL)
{
    float a2 = linearRoughness * linearRoughness;

    float GGXV = NoL * sqrt(max((NoV - a2 * NoV) * NoV + a2, 1e-6f));
    float GGXL = NoV * sqrt(max((NoL - a2 * NoL) * NoL + a2, 1e-6f));

    return 0.5f / max(GGXV + GGXL, 1e-6f);
}

float3 F_Schlick(float3 f0, float VoH)
{
    return f0 + (1.0f - f0) * Pow5(1.0f - VoH);
}

float3 EvaluateSunSpecularAlt(
    float3 N,
    float3 V,
    float3 L,
    float3 f0,
    float  linearRoughness,
    float3 sunColor,
    float  shadow)
{
    float3 h = SafeNormalize(V + L);

    float NoH = saturate(dot(N, h));
    float NoV = saturate(dot(N, V));
    float NoL = saturate(dot(N, L));
    float LoH = saturate(dot(L, h));

    if (NoL <= 0.0f || NoV <= 0.0f)
        return 0.0f;

    float D = D_GGX(linearRoughness, NoH);
    float Vis = V_SmithGGXCorrelated(linearRoughness, NoV, NoL);
    float3 F = F_Schlick(f0, LoH);

    float3 Fr = (D * Vis) * F;
    return Fr * sunColor * (NoL * shadow);
}
float3 FakeWaterSSS(
    float3 N,
    float3 N_spec,
    float3 V,
    float3 L,
    float3 waterSSSTint,
    float3 ambientLight,
    float3 reflectionLight,
    float3 sunLight,
    float  waterDepth,
    float  shadow)
{
    float3 H = SafeNormalize(V + L);

    float NdotV = saturate(dot(N, V));
    float NspecDotH = saturate(dot(N_spec, H));
    float VdotNegL = saturate(dot(V, -L));

    float glintLobe = pow(NspecDotH, 5.0f); // tuning
    float forwardLobe = pow(VdotNegL, 2.0f); // tuning

    float slopeMask = 1.0f - saturate(N.y);
    float grazing = pow(1.0f - NdotV, 1.5f); // tuning
    float depthFactor = 1.0f - exp(-waterDepth * 0.25f); // tuning
    float shape = (0.25f + 0.75f * grazing) * (0.25f + 0.75f * slopeMask); // tuning

    float3 bodyLight = lerp(ambientLight, Luminance(ambientLight).xxx, .5f);
    bodyLight += lerp(reflectionLight, Luminance(reflectionLight), .5f) * 0.20f; // tuning
    float3 bodySSS = bodyLight * shape * depthFactor * 2.f;

    float3 directionalSSS = sunLight * (0.32 + 0.32 * shadow) * max(glintLobe, forwardLobe * 0.5f); // tuning
    float3 directSSS = 0.5f * directionalSSS * shape * depthFactor;

    return waterSSSTint * (bodySSS + directSSS);
    //return float3(shape.xxx * depthFactor.xxx);
}


static const float WATER_BASE_BOMBING   = 0.5f; // tuning
static const float WATER_NORMAL_BOMBING = 0.20f; // tuning
static const float WATER_FOAM_BOMBING   = 0.30f; // tuning

float sum4(float4 value)
{
    return value.x + value.y + value.z + value.w;
}

float sum3(float3 value)
{
    return value.x + value.y + value.z;
}


float4 texture_no_tile(float2 uv, float variation_strength, Texture2D tex)
{
    float random_value = iqnoise(1.0 * uv);//blue_noise.SampleLevel(smp_rtlinear, float3(uv, 0.f), 0).x;

    float2 uv_ddx = ddx(uv);
    float2 uv_ddy = ddy(uv);

    float tile_selector = random_value * 8.0;
    float blend_factor  = frac(tile_selector);

#if 1
    float tile_index_a = floor(tile_selector);
    float tile_index_b = tile_index_a + 1.0;
#else
    float tile_index_a = floor(tile_selector + 0.5);
    float tile_index_b = floor(tile_selector);
    blend_factor = min(blend_factor, 1.0 - blend_factor) * 2.0;
#endif

    float2 offset_a = sin(float2(3.0, 7.0) * tile_index_a);
    float2 offset_b = sin(float2(3.0, 7.0) * tile_index_b);

    float4 color_a = tex.SampleGrad(smp_base, uv + variation_strength * offset_a, uv_ddx, uv_ddy);

    float4 color_b = tex.SampleGrad(smp_base, uv + variation_strength * offset_b, uv_ddx, uv_ddy);

    float color_bias = 0.1 * sum3((color_a - color_b).rgb);
    float final_blend = smoothstep(0.2, 0.8, blend_factor - color_bias);

    return lerp(color_a, color_b, final_blend);
}

float4 main(vf I, float4 pos2d : SV_POSITION) : SV_Target
{
    WaterLayerMaterial WaterM, FoamM;
    float3 ViewFromEyeToPoint = SafeNormalize(I.v2point);
    float3 V = -ViewFromEyeToPoint;

    float4 base = s_base.Sample(smp_base, I.tbase); //texture_no_tile(I.tbase, 1.0, s_base); // 
    base.rgb = PushGamma(base.rgb);

    float3 n0 = s_nmap.Sample(smp_base, I.tnorm0).xyz;
    float3 n1 = s_nmap.Sample(smp_base, I.tnorm1).xyz;
    float3 Navg = n0 + n1 - 1.0f;
    Navg.z = abs(Navg.z);

    float3 Nw = SafeNormalize(mul(float3x3(I.M1, I.M2, I.M3), Navg.xyz));
    /*--------------------------------------------------------------------------
                    Ugly tricks - do not try this at home
    --------------------------------------------------------------------------*/
    
    float3 dNx = ddx(Nw);
    float3 dNy = ddy(Nw);
    float gradient = pow( 0.5 * (dot(dNx, dNx) + dot(dNy, dNy)), 0.33);
    float variance = max(dot(dNx, dNx), dot(dNy, dNy));
    float3 L = -SafeNormalize(L_sun_dir_w.xyz);
    float3 H = SafeNormalize(V + L);

    float slopeMask = 1.0f - saturate(Nw.z);
    float glintMask = smoothstep(0.05f, 0.95f, gradient * 13.0f); // tuning

    float NdotH = saturate(dot(Nw, H));
    float specNeed = 1.0f - pow(NdotH, 5.0); // tuning

    float glintStrength = 0.95f; // tuning
    float bend = min(glintMask * glintStrength, 0.95f); // tuning

    float3 N_spec = SafeNormalize(lerp(Nw, H, 0.1 + 0.9 * bend));
    
    float3 envd0 = env_s0.SampleLevel(smp_rtlinear, Nw, 0).xyz;
    float3 envd1 = env_s1.SampleLevel(smp_rtlinear, Nw, 0).xyz;
    float3 envd  = lerp(envd0, envd1, L_ambient.w) * L_hemi_color.xyz;

    float3 color = I.c0.xyz + envd * envd * I.c0.w;
    float3 tintedBaseColor = base.xyz * color;


    float3 WaterPoint = I.tctexgen.z *
        float3(pos2d.xy * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);

    float NdotV = saturate(dot(Nw, V));
    float3 FresnelBase = FresnelSchlick(float3(0.020f, 0.020f, 0.020f), NdotV);

    WaterM = BuildWaterMaterial(base, tintedBaseColor, FresnelBase);
    WaterLayerMaterial FinalM = WaterM;

    float4 Point = GbufferGetPoint(pos2d.xy);

    float3 waterPos = Point.xyz * rcp(Point.z) * I.tctexgen.z;
    float waterDepth = length(waterPos - Point.xyz) * 1.0f; // tuning
    
    float4 foam =  s_leaves.Sample(smp_base, I.tbase);// texture_no_tile(I.tbase, 1.0, s_leaves); // s_leaves.Sample(smp_base, I.tbase);
    float3 foamColor = PushGamma(foam.xyz) + 0.18f;
    foam.w *= 1.0f - base.w;

    float calc_cos = -dot(float3(I.M1.z, I.M2.z, I.M3.z), ViewFromEyeToPoint);
    float calc_depth = saturate(waterDepth * calc_cos);

    float fFoamFactor = smoothstep(0.025f, 0.05f, calc_depth); // tuning
    fFoamFactor *= smoothstep(0.1f, 0.075f, calc_depth);       // tuning

    float FoamLayerFactor = saturate(foam.w * fFoamFactor);

    FoamM = BuildFoamMaterial(foam, foamColor, WaterM);

    FinalM = BlendMaterials(WaterM, FoamM, FoamLayerFactor); // tuning
    
    float roughnessAA = saturate(FinalM.Roughness + variance * 1.f);
    FinalM.Roughness = roughnessAA;

    /*==========================================================================
        original SSR / VSLR acquisition
    ==========================================================================*/
    float3 ReflectionOverride = 0.0f;
    float  ReflectionOverrideFactor = 0.0f;

    #ifdef USE_SSLR_ON_WATER
        float3 ReflectDir = reflect(ViewFromEyeToPoint, Nw);
        float3 ReflectVS = mul((float3x3)m_V, ReflectDir);
        float3 ReflectPoint = WaterPoint * 0.99f + ReflectVS * 0.025f; // tuning

        float4 sslr = ScreenSpaceLocalReflections(ReflectPoint, ReflectVS);

        #ifdef USE_OFFSCREEN_REFLECTIONS
            ReflectPoint = mul(m_env_view, float4(ReflectPoint, 1.0f)).xyz;
            ReflectVS    = mul((float3x3)m_env_view, ReflectVS);

            float4 vslr = FastViewReflections(ReflectPoint, ReflectVS);

            float Fog = saturate(length(vslr.xyz) * fog_params.w + fog_params.x);
            vslr.w *= 1.0f - Fog * Fog;

            vslr.xyz = s_env.SampleLevel(smp_rtlinear, vslr.xyz, 0.0f);
            vslr.xyz *= rcp(1.00001f - vslr.xyz);

            ReflectionOverride = lerp(ReflectionOverride, vslr.xyz, vslr.w);
            ReflectionOverrideFactor = max(ReflectionOverrideFactor, vslr.w);
        #endif // USE_OFFSCREEN_REFLECTIONS

        ReflectionOverride = lerp(ReflectionOverride, sslr.xyz, sslr.w);
        ReflectionOverrideFactor = max(ReflectionOverrideFactor, sslr.w);

    #else // USE_SSLR_ON_WATER
        #ifdef USE_OFFSCREEN_REFLECTIONS
            float3 ReflectDir = reflect(ViewFromEyeToPoint, Nw);
            float3 ReflectVS = mul((float3x3)m_V, ReflectDir);
            ReflectVS = mul((float3x3)m_env_view, ReflectVS);

            float4 vslr = s_env.SampleLevel(smp_rtlinear, ReflectVS.xyz, 0.0f);
            vslr.xyz *= rcp(1.00001f - vslr.xyz);

            float Fog = saturate(vslr.w * fog_params.w + fog_params.x);
            vslr.w = 1.0f - Fog * Fog;

            ReflectionOverride = vslr.xyz;
            ReflectionOverrideFactor = vslr.w;
        #endif // USE_OFFSCREEN_REFLECTIONS
    #endif // USE_SSLR_ON_WATER

    float3 ambientIBL = EvaluateWaterAmbientIBL(
        ViewFromEyeToPoint,
        Nw,
        FinalM.F0,
        FinalM.BaseColor,
        FinalM.Metalness,
        FinalM.Roughness,
        I.c0.w,
        ReflectionOverride,
        ReflectionOverrideFactor
    );

    float3 transmittedColor = FinalM.BaseColor;
    float3 final = ambientIBL; //lerp(ambientIBL, transmittedColor, 0.5 * FinalM.Transmission);
    float alpha = FinalM.Alpha;

    float gr = Luminance(base.rgb);
    float3 Fc = 0.05f * water_intensity.xxx * lerp(gr.xxx, base.rgb, 0.5); // tuning
    final = lerp(Fc, final, alpha);

    alpha = min(alpha, saturate(waterDepth));
    alpha = max(1.0f - exp(-4.0f * waterDepth), alpha); // tuning

    float Shadow = 1.0f;

    #ifndef USE_R2_STATIC_SUN
        int cascade_index;
        float3 smap_texcoord;

        bool is_in_bounds = calc_cascades(mul(m_invV, float4(WaterPoint, 1.0f)).xyz, m_shadow_sun, cascade_index, smap_texcoord );

        if (is_in_bounds)
        {
            Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0f, cascade_index );
        }

        if (cascade_index >= 2)
        {
            float3 Factor = smoothstep(0.499f, 0.498f, abs(smap_texcoord - 0.5f));
            float Fade = Factor.x * Factor.y * Factor.z;
            Shadow = lerp(1.0f, Shadow, Fade);
        }
    #endif // USE_R2_STATIC_SUN

    //lvunter
    float3 Light = s_accumulator.Load(int3(pos2d.xy, 0), 0).xyz;
    Light *= 1.0f - base.w;

    float2 CausticTexcoord = mul(m_invV, float4(Point.xyz, 1.0f)).xz * 0.45f; // tuning
    float3 Caustic = s_caustic.Sample(smp_base, CausticTexcoord).yyy;
    Caustic += ddx(Caustic) * float3(1.25f, 0.0f, -1.25f); // tuning
    Caustic += ddy(Caustic) * float3(1.25f, 0.0f, -1.25f); // tuning

    float3 sunColor = max(0.0f, L_sun_color.xyz); // tuning
    //float3 L = -SafeNormalize(L_sun_dir_w.xyz);

    float3 SunSpecular = EvaluateSunSpecularAlt(N_spec, V, L, FinalM.F0, 0.002f, sunColor, Shadow ); // temnp roughness tuning
    SunSpecular = min(SunSpecular, 2.7f); // tuning / clamp
    final += SunSpecular;


    float3 waterSSSTint = lerp(transmittedColor, float3(0.10f, 0.22f, 0.16f), 0.5) * water_intensity.xxx; // tuning float3(0.10f, 0.22f, 0.16f)

    float3 SSS = FakeWaterSSS( Nw, N_spec, V, L, waterSSSTint, ambientIBL, ReflectionOverride, sunColor, waterDepth, Shadow );
    final += 2.0 * SSS;

    final += Caustic * Light * (exp(-2.0f * waterDepth));; // tuning

    return lerp(float4(final, alpha), fog_color, calc_fogging(I.pos.xyz));
    //return float4(frac(I.tbase),0.0f, 1.0f);
    //return float4(gradient.xxx * 50.0f, 1.0f);
    //return float4(iqnoise(I.tbase.xy).xxx, 1.0f);
    //return float4((1-exp(-waterDepth * 0.01)).xxx, 1.0f);
    //return float4(SSS, 1.f);


}
