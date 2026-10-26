#ifndef DETAIL_WIND_DEFORM_INCLUDED
#define DETAIL_WIND_DEFORM_INCLUDED
float4 BlendGrassState(float4 Previous, float4 Current, float3 PreviousVelocity, float3 CurrentVelocity,
    float Alpha, float Duration, float Limit)
{
    float A2 = Alpha * Alpha, A3 = A2 * Alpha;
    float3 Value = (2*A3-3*A2+1)*Previous.xyz + (A3-2*A2+Alpha)*Duration*PreviousVelocity
        + (-2*A3+3*A2)*Current.xyz + (A3-A2)*Duration*CurrentVelocity;
    float Amount = length(Value.xy) + abs(Value.z);
    Value *= min(1, Limit / max(Amount, .000001));
    return float4(Value, lerp(Previous.w, Current.w, Alpha));
}

float3 WindArcStep(float Step, float2 Tilt)
{
    float Angle = length(Tilt);
    if (Angle < .00001) { return float3(0, Step, 0); }
    float S, C;
    sincos(Angle, S, C);
    float Horizontal = Step * S / Angle;
    return float3(Tilt.x * Horizontal, Step * C, Tilt.y * Horizontal);
}

float3 BendGrassNormal(float3 N, float2 Tilt)
{
    float Angle = length(Tilt);
    float2 TangentXZ = sin(Angle) * Tilt / max(Angle, .000001);
    // Inverse-transpose of the column-preserving deformation Jacobian.
    return normalize(float3(cos(Angle)*N.x, N.y-dot(TangentXZ,N.xz), cos(Angle)*N.z));
}

float2 WindTilt(float U, float4 State, float2 Direction)
{
    float T = saturate(U);
    float Weight = pow(T, max(.65, State.w));
    return (State.xy + Direction * State.z * T) * Weight;
}

float3 BendGrass(float3 Rest, float2 Shape, float4 State, float3x3 Rotation, float Anchor, float2 WorldDirection, out float2 TipTilt)
{
    float Height = max(Shape.y, 0.0001);
    float AnchorY = Shape.x + Height * Anchor;
    if (Rest.y <= AnchorY || State.w < .65) { TipTilt = 0; return Rest; }
    float U = saturate((Rest.y - AnchorY) / max(Height * (1 - Anchor), .0001));
    float3 Centre = float3(0, AnchorY, 0);
    // Transform the external world direction into the instance's local frame.
    float3 LocalDirection = mul(transpose(Rotation), float3(WorldDirection.x, 0, WorldDirection.y));
    float2 Direction = normalize(LocalDirection.xz + float2(.000001, 0));
    float3 LocalBend = mul(transpose(Rotation), float3(State.x, 0, State.y));
    State.xy = LocalBend.xz;
    TipTilt = WindTilt(U, State, Direction);
    float FreeHeight = Height * (1 - Anchor);
    // Match the existing near/distant LOD; both keep constant-length free segments.
    #ifdef USE_TREEWAVE
        const int SegmentCount = 3;
    #else
        const int SegmentCount = 2;
    #endif
    [unroll] for (int Segment = 0; Segment < SegmentCount; ++Segment)
    {
        float Step = clamp(U - Segment / (float)SegmentCount, 0, 1.0 / SegmentCount);
        if (Step > 0)
        {
            float2 Tilt = WindTilt((Segment + .5) / SegmentCount, State, Direction);
            Centre += WindArcStep(Step * FreeHeight, Tilt);
        }
    }
    // Card textures contain multiple stems: translate each rest column along the
    // same arc instead of rotating a wide section and stretching its outer columns.
    return Centre + float3(Rest.x, 0, Rest.z);
}

#endif
