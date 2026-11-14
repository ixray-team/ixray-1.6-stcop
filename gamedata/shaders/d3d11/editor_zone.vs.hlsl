#include "common.hlsli"

// Anomaly zone shell of the editors: identity sphere / box of the shape, in its world transform
struct vf
{
	float4 C : COLOR0;
	float4 P : POSITION;
};

struct v2p
{
	float4 HPos : SV_POSITION;
	float4 C : COLOR0;
	float3 PosW : TEXCOORD0;
	float3 PosL : TEXCOORD1;
	float DepthV : TEXCOORD2;
};

v2p main(vf I)
{
	v2p O;
	O.HPos = mul(m_WVP, I.P);
	O.C = tfactor;
	O.PosW = mul(m_W, I.P);
	O.PosL = I.P.xyz;
	O.DepthV = mul(m_WV, I.P).z;
	return O;
}
