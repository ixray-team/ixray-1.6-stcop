#pragma once

// Shared timing for online decisions and lightweight ALife processing.
namespace CrowBehaviorTiming
{
struct SCrowRange
{
    float low = 0.f, high = 0.f;
    void load(const char* section, const char* name, float a, float b)
    {
        string128 key;
        xr_strconcat(key, name, "_min");
        low = READ_IF_EXISTS(pSettings, r_float, section, key, a);
        xr_strconcat(key, name, "_max");
        high = READ_IF_EXISTS(pSettings, r_float, section, key, b);
        R_ASSERT3(_valid(low) && _valid(high) && low >= 0.f && high >= low && high <= 86400.f,
            "Invalid crow interval", name);
    }
    float sample() const { return high > low ? Random.randF(low, high) : low; }
};
struct SCrowReaction
{
    bool Observed = false, Active = false;
    float Remaining = 0.f;

    bool Update(bool Current, float Dt, const SCrowRange& Enter, const SCrowRange& Leave)
    {
        if (Current != Observed)
        {
            Observed = Current;
            Remaining = Observed == Active ? 0.f : (Observed ? Enter.sample() : Leave.sample());
        }
        if (Observed == Active)
        {
            return false;
        }
        Remaining = std::max(0.f, Remaining - Dt);
        if (Remaining > 0.f)
        {
            return false;
        }
        Active = Observed;
        return true;
    }
};
}
