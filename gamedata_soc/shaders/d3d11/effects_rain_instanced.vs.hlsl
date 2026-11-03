#include "rain_instanced.hlsli"
struct RainInstance
{
	float3 Position;
	uint Seed;
	float3 Direction;
	float Speed;
};
StructuredBuffer<RainInstance> RainInstances;

RainVertex main(v_TL I, uint InstanceID : SV_InstanceID)
{
	RainInstance Drop = RainInstances[InstanceID];
	float3 Head = mul(m_V, float4(Drop.Position, 1.0f));
	float3 Direction = mul((float3x3)m_V, Drop.Direction);
	float DistanceSquared = dot(Head, Head);
	float3 Ray = DistanceSquared > 1e-6f ? Head * rsqrt(max(DistanceSquared, 1e-6f)) : float3(0, 0, 1);
	float3 Along = -Direction + Ray * dot(Direction, Ray);
	float Len2 = dot(Along, Along);
	float3 Reference = abs(Ray.y) < 0.99f ? float3(0, 1, 0) : float3(1, 0, 0);
	float3 AxisX = normalize(cross(Len2 > 1e-6f ? Along : Reference, Ray));
	float3 AxisY = cross(Ray, AxisX);
	float Radius = lerp(0.5f, 1.0f, RainRandom(Drop.Seed)) * RainShape.z;
	// Looking along the trajectory shows the round cross-section instead of a full-length streak.
	float HalfLength = lerp(min(RainShape.x, RainShape.y), RainShape.y, sqrt(saturate(Len2)));
	// Keep the leading edge at Head for every scale and viewing angle; only the tail changes length.
	float3 Point = Head + Radius * (AxisX * I.P.x * RainShape.x + AxisY * (I.P.y + 1.0f) * HalfLength);
	return RainProject(Point, I.Tex0, AxisX, AxisY, 1.0f);
}
