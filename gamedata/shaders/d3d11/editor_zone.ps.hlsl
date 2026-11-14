#include "common.hlsli"

// Anomaly zone shell of the editors, alpha blended over the lit scene, no depth write:
// a sphere glows at its silhouette (fresnel), a box along its edges; a world grid drifts on the surface,
// a band sweeps the selected zone and the shell glows where it crosses the scene
//	x: 1 for a sphere, 0 for a box
//	y: 1 when selected (brighter, animated)
//	z: time of the animation
float4 zone_params;

struct v2p
{
	float4 HPos : SV_POSITION;
	float4 C : COLOR0;
	float3 PosW : TEXCOORD0;
	float3 PosL : TEXCOORD1;
	float DepthV : TEXCOORD2;
};

float4 main(v2p I) : SV_Target
{
	const float Selected = zone_params.y;
	const float Time = zone_params.z;

	float Shell;
	if (zone_params.x > 0.5f)
	{
		// Identity sphere: the local position is the normal
		const float3 Normal = normalize(mul((float3x3)m_W, I.PosL));
		const float3 View = normalize(eye_position - I.PosW);
		Shell = pow(1.f - abs(dot(Normal, View)), 4.f);
	}
	else
	{
		// Identity box (-0.5..0.5): near an edge two coordinates are near the bounds.
		// The width is in screen pixels so it holds at any distance and box size
		const float3 Dist = 0.5f - abs(I.PosL);
		const float3 Width = fwidth(I.PosL) * 2.5f;
		const float3 Near = 1.f - saturate(Dist / max(Width, 1e-5f));
		const float Edge = max(max(Near.x * Near.y, Near.y * Near.z), Near.x * Near.z);
		Shell = Edge;
	}

	// World grid on the surface, drifting up, kept thin in screen space
	const float3 GridPos = I.PosW * 0.75f + float3(0.f, Time * 0.25f, 0.f);
	const float3 Cell = abs(frac(GridPos) - 0.5f);
	const float3 GridWidth = fwidth(GridPos) * 1.5f;
	const float3 GridLine = 1.f - saturate(Cell / max(GridWidth, 1e-5f));
	const float Grid = max(max(GridLine.x, GridLine.y), GridLine.z);

	// Band sweeping up the selected shell
	const float BandPhase = abs(frac(I.PosW.y * 0.15f - Time * 0.25f) - 0.5f) * 2.f;
	const float Band = pow(saturate(1.f - BandPhase), 16.f) * Selected;

	// Glow where the shell crosses the scene, only for the scene just behind it.
	// Its width grows with the distance, as the depth gets coarser; none on the sky
	const float SceneDepth = s_position.SampleLevel(smp_nofilter, I.HPos.xy * screen_res.zw, 0).x;
	const float ContactWidth = 0.25f + I.DepthV * 0.01f;
	const float Behind = SceneDepth - I.DepthV;
	const float Contact = (SceneDepth > 0.f && Behind >= 0.f) ? 1.f - saturate(Behind / ContactWidth) : 0.f;

	// Far shells fade so the distance does not turn into a wall of color. By the distance, as the
	// editor skips the shells farther than 200 m (CEditShape::IsZoneShellVisible)
	const float Fade = 1.f - saturate((length(I.PosW - eye_position) - 60.f) / 140.f);

	float Alpha = 0.04f + Shell * 0.55f + Grid * lerp(0.05f, 0.15f, Selected) + Band * 0.3f + Contact * 0.5f;
	Alpha *= lerp(0.8f, 1.2f, Selected) * Fade;

	// Brighter core for the lines and the contact, the same hue
	const float3 Color = I.C.rgb * (1.f + (Shell + Contact) * 0.5f);
	return float4(saturate(Color), saturate(Alpha));
}
