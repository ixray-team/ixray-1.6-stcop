#pragma once

class XRayRenderGlow :
    public IRender_Glow
{

public:
    bool IsActive = true;
    void set_active(bool InIsActive) override
    {
        IsActive = InIsActive;
    }


    bool get_active() override
    {
        return IsActive;
    }


    void set_position(const Fvector& P) override
    {
    }


    void set_direction(const Fvector& P) override
    {
    }


    void set_radius(float R) override
    {
    }


    void set_texture(LPCSTR name) override
    {
    }


    void set_color(const Fcolor& C) override
    {
    }


    void set_color(float r, float g, float b) override
    {
    }

};