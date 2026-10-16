#ifndef DOF_H_INCLUDED
#define DOF_H_INCLUDED

#ifndef USE_DOF

float3 dof(float2 center)
{
    float3 img = s_image.Sample(smp_rtlinear, center).xyz;
    return img;
}

#else //	USE_DOF

// x - near y - focus z - far w - sky distance
float4 dof_params;
float3 dof_kernel; // x,y - resolution pre-scaled z - just kernel size
Texture2D<float> s_dof_scope_mask;

float DOFFactor(float depth)
{
    float dist_to_focus = depth - dof_params.y;
    float blur_far = saturate(dist_to_focus * rcp(max(dof_params.z - dof_params.y, EPS_S)));
    float blur_near = saturate(dist_to_focus * rcp(min(dof_params.x - dof_params.y, -EPS_S)));
    float blur = blur_near + blur_far;
    blur *= blur;
    return blur;
}

float sampleDepth(float2 center)
{
    // Read raw point-sampled depth: filtering or reprojection loses HUD coverage.
    float P = s_position.SampleLevel(smp_nofilter, center, 0).x;
    // HUD occupies the 0..0.02 viewport range and must stay in focus.
    // Returning focus also prevents HUD colors from contributing to scene blur.
    if (P < 0.02f)
    {
        return dof_params.y;
    }
    return P > 0.9999f ? dof_params.w : (m_P._34 * rcp(P - m_P._33));
}

#define MAXCOF 7.h
#define EPSDEPTH 0.0001h

float3 dof(float2 center)
{
	// The lens samples the sharp scene. Only its surroundings receive weapon DOF.
	if (s_dof_scope_mask.SampleLevel(smp_nofilter, center, 0) > 0.5f)
	{
		return s_image.Sample(smp_rtlinear, center).xyz;
	}
    // Scale tap offsets based on render target size
    float depth = sampleDepth(center);
    float blur = DOFFactor(depth);
    if (blur == 0.0f)
    {
        return s_image.Sample(smp_rtlinear, center).xyz;
    }
    float2 scale = 0.5f * screen_res.zw * dof_kernel.z * blur;

    // poisson
    float2 o[12];
    o[0] = float2(-0.326212f, -0.405810f) * scale;
    o[1] = float2(-0.840144f, -0.073580f) * scale;
    o[2] = float2(-0.695914f, 0.457137f) * scale;
    o[3] = float2(-0.203345f, 0.620716f) * scale;
    o[4] = float2(0.962340f, -0.194983f) * scale;
    o[5] = float2(0.473434f, -0.480026f) * scale;
    o[6] = float2(0.519456f, 0.767022f) * scale;
    o[7] = float2(0.185461f, -0.893124f) * scale;
    o[8] = float2(0.507431f, 0.064425f) * scale;
    o[9] = float2(0.896420f, 0.412458f) * scale;
    o[10] = float2(-0.321940f, -0.932615f) * scale;
    o[11] = float2(-0.791559f, -0.597710f) * scale;

    float3 sum = s_image.Sample(smp_nofilter, center).xyz;
    float contrib = 1.h;

    [unroll]
    for (int i = 0; i < 12; i++)
    {
        float2 tap = center + o[i];
        float3 tap_color = s_image.Sample(smp_nofilter, tap).xyz;
        float tap_depth = sampleDepth(tap);
        float tap_contrib = DOFFactor(tap_depth);
		tap_contrib *= 1.0f - s_dof_scope_mask.SampleLevel(smp_nofilter, tap, 0);
        sum += tap_color * tap_contrib;
        contrib += tap_contrib;
    }

    return float3(sum / contrib);
}

#endif //	USE_DOF
#endif //	DOF_H_INCLUDED
