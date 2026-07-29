#ifndef LMODEL_H
#define LMODEL_H

#include "common.hlsli"

float4 plight_infinity(float m, float3 _point, float3 normal, float3 light_direction)
{
    float3 N = normal; // normal
    float3 V = -normalize(_point); // vector2eye
    float3 L = -light_direction; // vector2light
    float3 H = normalize(L + V); // float-angle-vector
    return tex3D(s_material, float3(dot(L, N), dot(H, N), m)); // sample material
}

float4 plight_local(float m, float3 _point, float3 normal, float3 light_position, float light_range_rsq)
{
    float3 N = normal; // normal
    float3 L2P = _point - light_position; // light2point
    float3 V = -normalize(_point); // vector2eye
    float3 L = -normalize((float3)L2P); // vector2light
    float3 H = normalize(L + V); // float-angle-vector
    float rsqr = dot(L2P, L2P); // distance 2 light (squared)
    float att = saturate(1 - rsqr * light_range_rsq); // q-linear attenuate
    float4 light = tex3D(s_material, float3(dot(L, N), dot(H, N), m)); // sample material
    return att * light;
}

#endif