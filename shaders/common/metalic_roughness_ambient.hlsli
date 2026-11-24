#ifndef metalic_roughness_ambient_h_ixray_included
#define metalic_roughness_ambient_h_ixray_included

#include "common\common.hlsli"
#include "common\metalic_roughness_light.hlsli"

Texture2D s_env_fwd : register(t49);

#include "common\ambient.hlsli"

#endif
