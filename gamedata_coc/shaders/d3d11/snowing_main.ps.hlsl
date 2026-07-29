#include "common.hlsli"
#include "sload.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

RWTexture2D<unorm float4> u_color : register(u0);
RWTexture2D<unorm float4> u_normal : register(u1);
RWTexture2D<unorm float4> u_surface : register(u2);

void main(PSInput _I)
{	
	IXRayGbufferPack O = (IXRayGbufferPack)NULL;
	IXRayMaterial M = (IXRayMaterial)NULL;
	
	uint2 DTid = uint2(_I.hpos.xy);
	
	M.Depth = s_position[DTid];
	M.Point = GbufferGetPointRealJitter(_I.texcoord, M.Depth);
	
	O.Color = u_color[DTid];
	O.Normal = u_normal[DTid];
	O.Material = u_surface[DTid];
	
	GbufferUnpackMaterial(O, M);
	
    p_bumped_new I; IXRayMaterial _M = M;
	
    I.position = float4(M.Point.xyz, 1.0f);

    float3 P = mul(m_invV, I.position);
    float3 N = normalize(mul((float3x3)m_invV, M.Normal.xyz));

	float snow_mask = smoothstep(0.2f, 0.3f, M.Hemi);
	snow_mask *= smoothstep(0.7f, 0.8f, N.y);
	
	bool object_mask = M.MaterialID == OBJECT_ID || M.MaterialID == FOLIAGE_ID;
	
#ifdef IGNORE_SNOW_MASK_ON_TERRAIN
	object_mask = object_mask || M.MaterialID == TERRAIN_ID;
#endif
	
	if(object_mask)
	{
		snow_mask = 0.0f;
	}

   // float3 T, B;
    I.tcdh.xy = P.xz * 0.2f;

 //   build_contangent_frame(P, N, I.tcdh.xy, T, B);
	
	float3 UpVector = abs(N.z) < 0.999f ? float3(0.0f, 0.0f, 1.0f) : float3(1.0f, 0.0f, 0.0f);
	float3 T = normalize(cross(UpVector, N));
	float3 B = cross(N, T);

    float3x3 xform = mul((float3x3)m_V, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z
		)
    );
    I.tcdh.zw = 0.5f;

    I.M1 = xform[0];
    I.M2 = xform[1];
    I.M3 = xform[2];
	
	I.hpos = _I.hpos;
    I.hpos_curr = I.hpos_old = I.hpos;
	
    SloadNew(I, M);

	M.Normal = mul(xform, M.Normal);
	M.Normal = normalize(M.Normal);
	
	M = _M.Lerp(M, snow_mask);

    GbufferPack(O, M);
	
	u_color[DTid] = O.Color;
	u_normal[DTid] = O.Normal;
	u_surface[DTid] = O.Material;
}

