#pragma once

class XRayRenderObjectSpecific :
    public IRender_ObjectSpecific
{
public:
    XRayRenderObjectSpecific() {}
    ~XRayRenderObjectSpecific() override {}
    void force_mode(u32 mode) override
    {
    }


    float get_luminocity() override
    {
        return 1;
    }


    float get_luminocity_hemi() override
    {
        return 1;
    }


    float* get_luminocity_hemi_cube() override
    {
        static float Result[6] = {};
        return Result;
    }

};