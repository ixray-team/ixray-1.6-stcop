#include "common.hlsli"
#include "shared\post_cb.hlsli"
#include "dof_bokeh.hlsli"

Texture2D<float> s_dof_scope_mask : register(t26);

#ifdef USE_DOF_PHYSICAL
Texture2D<float> t_focus : register(t27);
#endif

float main(PSInputFullscreen I) : SV_Target
{
	float RawDepth = s_position.SampleLevel(smp_nofilter, I.texcoord.xy, 0).x;
	float Depth = GbufferGetPointRealUnjitter(I.texcoord.xy, RawDepth).z;
	if (!isfinite(Depth) || Depth <= 0.0f || s_dof_scope_mask.SampleLevel(smp_nofilter, I.texcoord.xy, 0) > 0.5f)
	{
		return 0.0f;
	}

#ifdef USE_DOF_PHYSICAL
	float FocalLength = max(dof_params.x * 0.001f, EPS_S);
	float FNumber = max(dof_params.y, EPS_S);
	float SensorHeight = max(dof_params.z, EPS_S);
	float Focus = max(t_focus.Load(int3(0, 0, 0)), FocalLength + EPS);
	float CoCDiameter = (FocalLength * FocalLength / FNumber) * ((Depth - Focus) / (Depth * (Focus - FocalLength)));
	float RadiusPixels = 0.5f * CoCDiameter * dof_params.w / SensorHeight;
	return clamp(RadiusPixels, -dof_kernel.z, dof_kernel.z);
#else
	// Preserve the squared classic response and sky substitute for legacy profiles.
	float ClassicDepth = RawDepth > 0.9999f ? dof_classic.w : Depth;
	float Distance = ClassicDepth - dof_classic.y;
	float FarCoC = saturate(Distance / max(dof_classic.z - dof_classic.y, EPS_S));
	float NearCoC = saturate(Distance / min(dof_classic.x - dof_classic.y, -EPS_S));
	float ClassicCoC = sign(Distance) * min(dof_kernel.x * (NearCoC + FarCoC) * (NearCoC + FarCoC), dof_kernel.z);
	return ClassicCoC;
#endif
}
