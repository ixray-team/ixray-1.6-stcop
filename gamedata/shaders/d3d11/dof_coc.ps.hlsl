#include "common.hlsli"

Texture2D<float> t_coc_prev;
Texture2D<float> t_focus;

float4 dof_params; // x - focallength, y - f-number, z - sensor size

float4 main(PSInputFullscreen I) : SV_Target
{
    float depth = max(GbufferGetPointRealUnjitter(I.texcoord.xy).z, 1e-4);
    float focus = max(t_focus.Sample(smp_nofilter, 0.5).x, 1e-4);
    float coc_prev = t_coc_prev.Sample(smp_nofilter, I.texcoord.xy).x;

    float focal_length = dof_params.x * 0.001f; // convert to meters
    float sensor_size = max(dof_params.z * 0.001, 1e-6);
    float f_stop = max(dof_params.y, 1e-6);
    focus = max(focus, focal_length + 1e-4);

    float coc_m = (focal_length*focal_length / (f_stop * (focus - focal_length))) * abs(depth - focus) / depth;
    float coc_norm = saturate(coc_m / sensor_size);

    float distantCoC =0.0f;
    #ifndef NEW_FOGGIN
        distantCoC = saturate(depth * fog_params.w + fog_params.x);
    #else  //NEW_FOGGIN
        //float a = 1.0f;
        //float b = 0.002f;
        float denom = F_base - exp(-F_dens * (fog_params.z - fog_params.y));
        distantCoC = (F_base - exp(-F_dens * (depth - fog_params.y))) / denom;
        distantCoC = saturate(distantCoC);
    #endif

    coc_norm = max(coc_norm, 0.15 * distantCoC); // distant objects should also be blurred, even if they are in focus
    float a = 1.f - exp(-(timers.x - timers.y) / 1.f); 

    return lerp(coc_norm, coc_prev, a);
}