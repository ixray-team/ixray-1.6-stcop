#pragma once
#include "light.h"

class CLightR_Manager
{
    xr_vector<light*> selected_point;
    xr_vector<light*> selected_spot;
public:
    CLightR_Manager();
    ~CLightR_Manager();
    void add(light* L);
    void render(u32 priority);
    void render_point(u32 priority);
    void render_spot(u32 priority);
};
