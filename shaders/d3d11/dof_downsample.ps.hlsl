// Layer preparation for Frostbite circular convolution and the IXR near/far composite.
// https://media.gdcvault.com/gdc2018/presentations/Garcia_Kleber_CircularDepthOf.pdf
// https://gpuopen.com/manuals/fidelityfx_sdk/techniques/depth-of-field/
#include "common.hlsli"
#include "shared\post_cb.hlsli"
#include "dof_bokeh.hlsli"

Texture2D<float4> t_image : register(t25);
Texture2D<float> t_coc : register(t28);

struct DofDownsampleOutput
{
	float4 Near : SV_Target0;
	float4 Far : SV_Target1;
	float4 Metadata : SV_Target2; // xy - near/far raw depth, zw - near/far radius in output pixels
};

DofDownsampleOutput main(PSInputFullscreen I)
{
	uint2 Pixel = uint2(I.hpos.xy);
	uint2 SourceSize = uint2(dof_downsample_params.xy);
	uint2 TargetSize = uint2(dof_downsample_params.zw);
	uint2 Begin = Pixel * SourceSize / TargetSize;
	uint2 End = (Pixel + 1) * SourceSize / TargetSize;
	float PixelScale = max(dof_downsample_params.x / dof_downsample_params.z, dof_downsample_params.y / dof_downsample_params.w);
	float4 Near = 0.0f;
	float4 Far = 0.0f;
	float2 Depth = 1.0f;
	float2 Radius = 0.0f;
	bool2 HasHud = false;

	[loop]
	for (uint Y = Begin.y; Y < End.y; ++Y)
	{
		[loop]
		for (uint X = Begin.x; X < End.x; ++X)
		{
			float2 UV = (float2(X, Y) + 0.5f) / dof_downsample_params.xy;
			float RawDepth = s_position.SampleLevel(smp_nofilter, UV, 0).x;
			float CoC = t_coc.Load(int3(X, Y, 0));
			float3 Color = t_image.Load(int3(X, Y, 0)).rgb;
			bool IsHud = DofIsHud(RawDepth);
			// Sharp HUD is restored from the full-resolution scene. Putting it in the far
			// field would replace a whole mixed block with weapon color after normalization.
			if (IsHud && CoC == 0.0f)
			{
				continue;
			}
			if (DofIsNear(CoC, RawDepth))
			{
				if (IsHud && !HasHud.x)
				{
					Near = 0.0f;
					Depth.x = 1.0f;
					Radius.x = 0.0f;
					HasHud.x = true;
				}
				if (IsHud == HasHud.x)
				{
					// Split by source radius: the complementary contribution is gathered at full resolution.
					Near += float4(Color, 1.0f) * DofLargeWeight(abs(CoC), PixelScale);
					Depth.x = min(Depth.x, RawDepth);
					Radius.x = max(Radius.x, abs(CoC));
				}
			}
			else
			{
				if (IsHud && !HasHud.y)
				{
					Far = 0.0f;
					Depth.y = 1.0f;
					Radius.y = 0.0f;
					HasHud.y = true;
				}
				if (IsHud == HasHud.y)
				{
					// CoC premultiplication, adapted with a one-output-pixel floor for background fill.
					// A common scale cancels during resolve and avoids inflating HDR intermediates.
					float Weight = max(abs(CoC), 1.0f) / dof_blur_params.w;
					Far += float4(Color, 1.0f) * Weight;
					Depth.y = min(Depth.y, RawDepth);
					Radius.y = max(Radius.y, abs(CoC));
				}
			}
		}
	}

	float InvCount = rcp(float((End.x - Begin.x) * (End.y - Begin.y)));
	DofDownsampleOutput O;
	O.Near = Near * InvCount;
	O.Far = Far * InvCount;
	O.Metadata = float4(Depth, Radius);
	return O;
}
