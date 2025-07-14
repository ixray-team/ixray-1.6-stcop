#include "fluid_common_render.hlsli"

void main(PS_INPUT_RAYDATA_BACK input, out float4 output : SV_Target)
{
    float sceneZ = sceneDepthTex.SampleLevel(samLinearClamp, float2(input.pos.x / RTWidth, input.pos.y / RTHeight), 0).x;
	sceneZ = DepthUnpack.x * rcp(sceneZ - DepthUnpack.y);

    // This value will only remain if no fragments get blended on top in the next pass (front-faces)
    // which would happen if the front faces of the box get clipped by the near plane of the camera
	
    output.xyz = NEARCLIPPED_PIXEL_RAYPOS;
    output.w = min(input.depth, sceneZ);
}