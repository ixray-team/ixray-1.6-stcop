/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#include "common\common.hlsli"
#include "common\post_cb.hlsli"

Texture2D<float> t_focus_prev : register(t25);

float4 ps_main(PSInputFullscreen I) : SV_Target
{
	float PreviousFocus = t_focus_prev.Load(int3(0, 0, 0));
	bool HasPreviousFocus = isfinite(PreviousFocus) && PreviousFocus > 0.0f;
	float Focus = dof_focus_params.z;
	if (dof_focus_params.y < 0.5f)
	{
		uint Width, Height;
		s_position.GetDimensions(Width, Height);
		float2 HalfTexel = 0.5f / (float2(Width, Height) * view_uv.xy);
		float2 FocusUV = clamp(dof_focus_point.xy, HalfTexel, 1.0f - HalfTexel);
		float RawDepth = s_position.SampleLevel(smp_nofilter, FocusUV * view_uv.xy, 0).x;
		Focus = GbufferGetPointRealUnjitter(FocusUV, RawDepth).z;
		if (!isfinite(RawDepth) || RawDepth < 0.0f || RawDepth > 1.0f || !isfinite(Focus) || Focus <= 0.0f)
		{
			Focus = HasPreviousFocus ? PreviousFocus : dof_focus_params.z;
		}
	}

	Focus = max(Focus, dof_focus_params.w);
	if (!HasPreviousFocus)
	{
		return Focus;
	}

	return max(lerp(PreviousFocus, Focus, saturate(dof_focus_params.x)), dof_focus_params.w);
}
