#include "common.hlsli"

Texture2D<float> t_coc_prev : register(t25);
Texture2D<float> t_focus : register(t26);

#include "shared\post_cb.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
{
    //Sample depth buffer
	float depth = GbufferGetPointRealUnjitter(I.texcoord.xy).z;//s_position.Sample(smp_nofilter, I.texcoord.xy).x;
    float focus = t_focus.Sample(smp_nofilter, 0.5f).x;

	float CoC = (((dof_params.x * dof_params.x) / dof_params.y) / ((focus) - dof_params.x)) * 
						(abs(depth - focus) / (depth + (depth==0)));
    return saturate(abs(CoC * dof_params.z));
}