#ifndef DETAIL_TRAMPLE_PRESENTATION_INCLUDED
#define DETAIL_TRAMPLE_PRESENTATION_INCLUDED
struct TramplePresentationEntry
{
    float4 Identity;
    float4 State;
};
StructuredBuffer<TramplePresentationEntry> detail_trample_presentation : register(t1);
uint TrampleIdentityHash(uint4 Identity)
{
    uint Value = 2166136261u;
    [unroll] for (uint I = 0; I < 4; ++I) { Value = (Value ^ Identity[I]) * 16777619u; }
    Value ^= Value >> 16; Value *= 0x7feb352du;
    Value ^= Value >> 15; Value *= 0x846ca68bu;
    return Value ^ (Value >> 16);
}
float2 PresentedTrampleState(float Visual, float Strength, float4 Identity, uint Capacity)
{
    if (Capacity == 0 || (Visual < .001 && Strength < .001)) { return float2(Visual, 1); }
    uint4 Key = asuint(Identity);
    uint Index = TrampleIdentityHash(Key) & (Capacity - 1);
    [loop] for (uint Probe = 0; Probe < Capacity; ++Probe)
    {
        TramplePresentationEntry Entry = detail_trample_presentation[Index];
        if (Entry.State.w == 0) { return float2(Visual, 1); }
        if (all(asuint(Entry.Identity) == Key))
        {
            return lerp(float2(Entry.State.z, Entry.State.w - 1), Entry.State.xy, wind_xz2.x);
        }
        Index = (Index + 1) & (Capacity - 1);
    }
    return float2(Visual, 1);
}

bool TrampleHasEntryPose(float PackedTick, float Profile)
{
    return asuint(PackedTick) == 0xfffffffeu && Profile >= .65;
}
float3 BlendTrailEntryPose(float3 EntryPose, float3 TrailPose, float Progress)
{
    return lerp(EntryPose, TrailPose, saturate(Progress));
}
#endif
