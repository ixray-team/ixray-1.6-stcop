#include "rain_instanced.hlsli"
struct ImpactInstance
{
	float3 Position;
	float Age;
	float3 Normal;
	float Lifetime;
	uint Seed;
	float Scale;
	float2 Padding;
};
StructuredBuffer<ImpactInstance> RainInstances;

RainVertex main(v_TL I, uint InstanceID : SV_InstanceID)
{
	ImpactInstance Impact = RainInstances[InstanceID / 4u];
	uint Child = InstanceID % 4u;
	float Phase = saturate(Impact.Age / max(Impact.Lifetime, 1e-4f));
	float Scale = Impact.Scale * RainShape.z;
    float3 N = Impact.Normal; //normalize(Impact.Normal);

	float3 Position = Impact.Position;
	float3 AxisX = float3(1, 0, 0);
	float3 AxisY = float3(0, 1, 0);
	float HalfWidth, HalfHeight;
	float Fade = 1.0f - Phase;
	if (Child == 0u)
	{
        float3 ViewNormal = mul((float3x3) m_V, N);
		float Len2 = dot(ViewNormal.xy, ViewNormal.xy);
		float2 Along = Len2 > 1e-6f ? ViewNormal.xy * rsqrt(max(Len2, 1e-6f)) : float2(0, 1);
		AxisY = float3(Along, 0);
		AxisX = float3(Along.y, -Along.x, 0);
		float Height = (0.06f + 0.12f * sin(Phase * PI)) * Scale;
		HalfWidth = 0.045f * (1.0f - 0.5f * Phase) * Scale;
		HalfHeight = lerp(HalfWidth, Height * 0.5f, saturate(sqrt(Len2)));
		//float Support = abs(dot(ViewNormal, AxisX)) * HalfWidth + abs(dot(ViewNormal, AxisY)) * HalfHeight;
        Position += N * (0.005f + Height * 0.5f); //N * (0.005f + max(Support, Height * 0.5f));
    }
	else
	{
		float3 Reference = abs(N.y) < 0.99f ? float3(0, 1, 0) : float3(1, 0, 0);
		float3 Tangent = normalize(cross(Reference, N));
		float3 Bitangent = cross(N, Tangent);
		float Random = Hash(Impact.Seed + Child * 7919u);//RainRandom(Impact.Seed + Child * 7919u); 
		float Angle = (Random + float(Child - 1u) / 3.0f) * (2.0f * PI);
		float SinAngle, CosAngle;
		sincos(Angle, SinAngle, CosAngle);
		float3 Lateral = Tangent * CosAngle + Bitangent * SinAngle;
		float Speed = lerp(0.7f, 1.4f, Random);
		// Gravity is world -Y, including wall/slope contacts. Bound the flight independently of long event lifetimes.
		float FlightTime = min(Impact.Age, 0.3f); // 0.5f
		float3 Displacement = (Lateral * 0.55f + N * Speed) * (FlightTime * Scale);
		Displacement.y -= 4.9f * FlightTime * FlightTime;
		float Height = dot(Displacement, N);
		Position += Displacement + N * 0.005f;
		HalfWidth = lerp(0.012f, 0.025f, Random) * Scale; // 0.05, 0.15
        HalfHeight = HalfWidth * 1.7f; // padding.x / padding.y
		Fade *= saturate(Height * 100.0f) * saturate((0.3f - Impact.Age) * 20.0f); 
	}
	float3 Center = mul(m_V, float4(Position, 1.0f));
	float3 Point = Center + AxisX * I.P.x * HalfWidth + AxisY * I.P.y * HalfHeight;
	return RainProject(Point, I.Tex0, AxisX, AxisY, Fade);
}
