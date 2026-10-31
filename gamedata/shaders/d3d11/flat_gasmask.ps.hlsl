// Flat visor glass cracks and reflections.
#include "common.hlsli"
#include "shared\VisorDrops.hlsli"

#include "shared\extra_cb.hlsli"
Texture2D s_droplets : register(t25);
Texture2D s_mask_nm_1 : register(t26);
Texture2D s_mask_nm_2 : register(t49);
Texture2D s_mask_nm_3 : register(t50);
Texture2D s_mask_nm_4 : register(t51);
Texture2D s_mask_nm_5 : register(t52);
Texture2D s_mask_nm_6 : register(t53);
Texture2D s_mask_nm_7 : register(t54);
Texture2D s_mask_nm_8 : register(t55);
Texture2D s_mask_nm_9 : register(t56);
Texture2D s_mask_nm_10 : register(t28);

//-' Gasmask settings
#define GM_DIST_INT 				0.025f //Refraction intensity
#define GM_DIFF_INT 				0.25f //Diffuse cracks intensity
#define GM_VIG_INT 					0.5f //Vignette intensity

//-' Glass reflections settings
#define GM_VIS_NUM 					16 //Reflection quality
#define GM_VIS_RADIUS 				0.0f //Reflection radius
#define GM_VIS_INTENSITY 			0.35f //Reflection intensity

float visor_ratio(float s)
{
	float ratio = screen_res.y / screen_res.x; // 0.75 for 4:3 (normalscreen) and 0.625 for 16:10 (widescreen) resolution
	return (s - 0.5f) * ratio + 0.5f;
}
	
float3 visor_reflection(float3 image, float2 tc)
{
	float4 final = float4(0.f, 0.f, 0.f, 0.f);
	float2 center = float2(0.5f, 0.5f);

	float x = length(float2(tc.x, visor_ratio(tc.y)) - center);

	if (x < GM_VIS_RADIUS)
		return image;
	else
	{
		// With a zero reflection radius, the limiting attenuation is one.
		float p = 1.f;

		for (int i = 0; i < GM_VIS_NUM; i++)
		{
			float N = 1.f - p / 0.6f + 0.4f * (i / (GM_VIS_NUM - 1.f)) * p;
			float2 m = (center - tc) * -N + center;
			final += s_image.SampleLevel(smp_rtlinear, m, 0);
		}
		final /= GM_VIS_NUM;
			
		float3 reflected = (image.xyz + GM_VIS_INTENSITY.xxx * final.xxx) / (1.f + GM_VIS_INTENSITY.xxx);	
		return reflected;		
	}
}
	
float4 main(PSInputFullscreen I) : SV_Target
{
	// Sample gasmask texture
	float4 gasmask_tex = float4(1.f, 1.f, 1.f, 1.f);
	
	if (mask_control.x == 1)
	{	
		gasmask_tex = s_mask_nm_1.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 2)
	{
		gasmask_tex = s_mask_nm_2.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 3)	
	{
		gasmask_tex = s_mask_nm_3.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 4)	
	{
		gasmask_tex = s_mask_nm_4.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 5)	
	{
		gasmask_tex = s_mask_nm_5.Sample(smp_nofilter,I.texcoord);
	}	
	else if (mask_control.x == 6)	
	{
		gasmask_tex = s_mask_nm_6.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 7)	
	{
		gasmask_tex = s_mask_nm_7.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 8)	
	{
		gasmask_tex = s_mask_nm_8.Sample(smp_nofilter,I.texcoord);
	}	
	else if (mask_control.x == 9)	
	{
		gasmask_tex = s_mask_nm_9.Sample(smp_nofilter,I.texcoord);
	}
	else if (mask_control.x == 10)	
	{
		gasmask_tex = s_mask_nm_10.Sample(smp_nofilter,I.texcoord);
	}

	// Prepare refracted texcoord
    float2 refr_tc = I.texcoord.xy + (gasmask_tex.xy * 2.f - 1.f) * GM_DIST_INT;
	
	// Sample scene with refracted texcoord
	float3 image = s_image.Sample(smp_nofilter, refr_tc.xy).xyz;
	
	float3 final_image = image;
	
	// Rain effects share the accumulation and wiping state with curved glass.
	float rainAmount = saturate(visor_rain.x);
	if (mask_control.y == 1.f)
	{
		float4 droplets = s_droplets.Sample(smp_rtlinear, I.texcoord);
		float2 offset = (droplets.xy - (127.f / 255.f)) * def_distort;
		final_image = s_image.SampleLevel(smp_rtlinear, refr_tc - offset * rainAmount, 0.f).xyz;
	}
	else if (mask_control.y == 2.f)
	{
		float2 aspect = float2(screen_res.x / screen_res.y, -1.f);
		float2 normal = DistortUV(I.texcoord, rainAmount, timers.x, aspect);
		float3 rain_col = s_image.SampleLevel(smp_rtlinear, I.texcoord + normal, 0.f).xyz;
		final_image = lerp(final_image, rain_col, rainAmount);
	}

	// Mix gasmask cracks with image
	final_image += gasmask_tex.w * GM_DIFF_INT * final_image;
	
	// Add glass reflection on top
	if (mask_control.z == 1.f)
	{	
		//Prepare aspect-ratio correct TC for attenuation
		float2 vig_tc = I.texcoord.xy;
	    vig_tc -= 0.5f;
		vig_tc.x *= screen_res.x / screen_res.y;
		vig_tc += 0.5f;	
		
		//Get refl attenuation
		float refl_att = smoothstep(0.8f, 1.f, distance(vig_tc, float2(0.5f, 0.5f)));
		
		final_image = lerp(final_image, visor_reflection(final_image, refr_tc.xy), refl_att);
	}

	return float4(final_image, 1.f);
}
