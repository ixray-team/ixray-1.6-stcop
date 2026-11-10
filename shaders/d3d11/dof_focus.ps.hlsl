#include "common.hlsli"
#include "shared\post_cb.hlsli"

Texture2D<float> t_focus_prev : register(t25);

float4 main(PSInputFullscreen I) : SV_Target
{
	float PreviousFocus = t_focus_prev.Load(int3(0, 0, 0));
	bool HasPreviousFocus = isfinite(PreviousFocus) && PreviousFocus > 0.0f;
	float Focus = dof_focus_params.z;
	if (dof_focus_params.y < 0.5f)
	{
		uint Width, Height;
		s_position.GetDimensions(Width, Height);
		float2 HalfTexel = 0.5f / float2(Width, Height);
		float2 FocusUV = clamp(dof_focus_point.xy, HalfTexel, 1.0f - HalfTexel);
		float RawDepth = s_position.SampleLevel(smp_nofilter, FocusUV, 0).x;
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
