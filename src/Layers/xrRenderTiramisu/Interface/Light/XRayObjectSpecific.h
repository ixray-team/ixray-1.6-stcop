#pragma once

class CDS0_RenderObjectSpecific :
    public IRender_ObjectSpecific
{
public:
    CDS0_RenderObjectSpecific() {}
    ~CDS0_RenderObjectSpecific() override {}
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