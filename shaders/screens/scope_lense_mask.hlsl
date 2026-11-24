#include "common\common.hlsli"
#include "common\scene_model.hlsli"

void vs_main(PULL_INPUT, out VSOutputFullscreen O)
{
    scene_model model = pull_scene_model(VertexId, DrawId);
    O.hpos = scene_model_project(model, model.vertex.P);
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
	
    O.texcoord = model.vertex.tc.xy;
}

// The HUD's depth in the whole depth's range: drawn from the scene its vertex puts it there
#ifdef SURFACE_CLUSTER
#define SCOPE_HUD_DEPTH(depth) (depth)
#else
#define SCOPE_HUD_DEPTH(depth) ((depth) * 0.02f)
#endif

struct ScopeMaskOutput
{
	float Depth : SV_Depth;
	float DofMask : SV_Target0;
};

ScopeMaskOutput ps_main(in PSInputFullscreen I)
{
	float Depth = s_position.Load(int3(I.hpos.xy, 0)).x;
	float Alpha = m_hud_params.y * m_hud_params.w;
	float Point = SCOPE_HUD_DEPTH(I.hpos.z);
	
#ifdef USE_LENSE_LERP
	I.texcoord.x *= 0.5f;
#endif
	
    float4 t_base = s_base.Sample(smp_base, I.texcoord.xy);
	
	ScopeMaskOutput Result;
	Result.Depth = Depth;
	Result.DofMask = 0.0f;
	if(Depth > Point && Alpha >= 0.5f && t_base.w < 1.0f)
	{
		Result.Depth = 1.0f;
		Result.DofMask = 1.0f;
	}
	
	return Result;
}
