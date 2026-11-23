#ifndef LIGHT_BEAM_HLSLI
#define LIGHT_BEAM_HLSLI

bool LightBeamEnabled()
{
	return Ldynamic_beam_range.z > 0.0f;
}

float LightBeamAttenuation(float3 PointToLight, float LightInvRadius2)
{
	float DistanceSqr = dot(PointToLight, PointToLight);
	float Window = saturate(1.0f - DistanceSqr * LightInvRadius2);
	float Reference = Ldynamic_beam_range.x * Ldynamic_beam_range.x;
	float InverseSquare = min(Reference / max(DistanceSqr, 1e-4f), Ldynamic_beam_range.y);
	return InverseSquare * Window * Window;
}

float LightBeamNormalOffset(float3 PointToLight, float3 Normal)
{
	float Distance = length(PointToLight);
	float Grazing = 1.0f - saturate(dot(Normal, -PointToLight / max(Distance, 1e-4f)));
	return 0.025f + Distance * (0.003f + 0.009f * Grazing);
}

float3 LightBeamProfile(float2 Uv)
{
	float Radius = length(Uv * 2.0f - 1.0f) / max(Ldynamic_beam_range.w, 1e-3f);

	float CoreSize = max(Ldynamic_beam_shape.x, 1e-3f);
	float BeamSize = max(Ldynamic_beam_shape.y, 1e-3f);
	float Softness = saturate(Ldynamic_beam_shape.z);
	float BeamInner = BeamSize * (1.0f - Softness);

	float CoreRatio = Radius / CoreSize;
	float Core = exp2(-CoreRatio * CoreRatio);
	float Beam = 1.0f - smoothstep(BeamInner, BeamSize, Radius);
	float SpillFade = 1.0f - smoothstep(BeamInner, 1.0f, Radius);
	float Spill = SpillFade * SpillFade * Ldynamic_beam_shape.w;

	float RingOffset = (Radius - lerp(BeamInner, BeamSize, 0.35f)) / max(BeamSize * 0.07f, 1e-3f);
	float Ring = exp2(-RingOffset * RingOffset) * Ldynamic_beam_tone.y;

	float EdgeWindow = 1.0f - smoothstep(0.8f, 1.0f, Radius);
	float Profile = (Beam + Core * Ldynamic_beam_tone.x + Spill + Ring) * EdgeWindow;

	float Warmth = saturate(Ldynamic_beam_tone.w) * smoothstep(CoreSize, BeamSize, Radius);
	float3 Tint = lerp(float3(1.0f, 1.0f, 1.0f), float3(1.0f, 0.84f, 0.66f), Warmth);

	return Tint * (Profile * Ldynamic_beam_tone.z);
}

#endif
