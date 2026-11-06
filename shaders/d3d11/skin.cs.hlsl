#include "shared\vertex.hlsli"

// Job: SrcBase, DstBase, BoneBase, Count | SrcPage << 8. A palette slot is 6 rows: current 0-2, previous 3-5.
StructuredBuffer<uint4> g_SkinData : register(t0);
RWStructuredBuffer<PackedVertex> g_SkinOut : register(u0);

uint EncodeSnorm16(float Value)
{
	return uint(int(round(clamp(Value, -1.0f, 1.0f) * 32767.0f))) & 0xFFFFu;
}

uint EncodeOct(float3 N)
{
	float L1 = dot(abs(N), 1.0f);
	if (!isfinite(L1) || L1 == 0.0f)
	{
		return 0u;
	}
	float2 E = N.xy / L1;
	if (N.z < 0.0f)
	{
		E = (E >= 0.0f ? 1.0f : -1.0f) * (1.0f - abs(E.yx));
	}
	return EncodeSnorm16(E.x) | (EncodeSnorm16(E.y) << 16);
}

[numthreads(64, 1, 1)]
void main(uint3 Group : SV_GroupID, uint Thread : SV_GroupIndex)
{
	uint JobIndex = 1u + Group.y * 65535u + Group.x;
	if (JobIndex > g_SkinData[0].x)
	{
		return;
	}
	uint4 Job = g_SkinData[JobIndex];
	if (Thread >= (Job.w & 0xFFu))
	{
		return;
	}

	PackedVertex Source = LoadPage(Job.w >> 8, Job.x + Thread);
	uint4 Slot = Job.z + ((Source.X >> uint4(0, 8, 16, 24)) & 0xFFu) * 6u;
	uint3 Q = (Source.C >> uint3(0, 8, 16)) & 0xFFu;
	float4 W = float4(float3(Q), 255.0f - float(Q.x + Q.y + Q.z)) * (1.0f / 255.0f);
	float4 M[6];
	[unroll]
	for (uint R = 0; R < 6; ++R)
	{
		M[R] = asfloat(g_SkinData[Slot.x + R]) * W.x + asfloat(g_SkinData[Slot.y + R]) * W.y + asfloat(g_SkinData[Slot.z + R]) * W.z + asfloat(g_SkinData[Slot.w + R]) * W.w;
	}

	float4 P1 = float4(Source.P, 1.0f);
	float3 P = float3(dot(M[0], P1), dot(M[1], P1), dot(M[2], P1));
	float3 Delta = clamp(float3(dot(M[3], P1), dot(M[4], P1), dot(M[5], P1)) - P, -65504.0f, 65504.0f);
	float3x3 Basis = float3x3(M[0].xyz, M[1].xyz, M[2].xyz);
	PulledVertex V = Decode(Source);
	float3 N = mul(Basis, V.N);
	float3 T = mul(Basis, V.T);
	float3 B = mul(Basis, V.B);
	uint Sign = dot(cross(N, T), B) < 0.0f ? 0x8000u : 0u;

	// Hidden bones are zero-scaled: a vanished normal falls back to +Z, a vanished tangent to the basis tangent.
	float L = dot(abs(N), 1.0f);
	N = L > 0.0f ? normalize(N / L) : float3(0.0f, 0.0f, 1.0f);
	uint Oct = EncodeOct(N);
	float3 Raw = DecodeOctRaw(Oct);
	[loop]
	for (uint Step = 0; Step < 8 && abs(abs(Raw.y) - abs(Raw.z)) < 1.0f / 4096.0f; ++Step)
	{
		N.y += N.y >= 0.0f ? 1.0f / 8192.0f : -1.0f / 8192.0f;
		N = normalize(N);
		Oct = EncodeOct(N);
		Raw = DecodeOctRaw(Oct);
	}

	float3 Normal = normalize(Raw);
	float3 T1 = normalize(abs(Raw.y) > abs(Raw.z) ? float3(Raw.y, -Raw.x, 0.0f) : float3(Raw.z, 0.0f, -Raw.x));
	float3 T2 = cross(T1, Normal);
	float A = dot(T, T1);
	float D = dot(T, T2);
	float L1 = abs(A) + abs(D);
	float S = L1 > 0.0f ? A / L1 : 1.0f;
	float Diamond = D >= 0.0f ? 0.25f * (1.0f - S) : 0.5f + 0.25f * (1.0f + S);

	uint3 Half = f32tof16(Delta);
	PackedVertex O;
	O.P = P;
	O.N = Oct;
	O.TF = (Source.TF & 0xFFFF0000u) | uint(round(Diamond * 32767.0f)) | Sign;
	O.UV = Source.UV;
	O.C = Half.x | (Half.y << 16);
	O.X = Half.z;
	g_SkinOut[Job.y + Thread] = O;
}
