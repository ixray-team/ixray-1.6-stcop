#ifndef COMMON_H
#define COMMON_H

#pragma warning(disable : 4000)
#define NULL (0)

#define EPS_S 0.0000001f
#define EPS   0.0000100f
#define EPS_L 0.0010000f

#define PI 3.141592653589793f

#include "common\common_decl.hlsli"

#include "common\common_defines.hlsli"
#include "common\common_policies.hlsli"
#include "common\common_iostructs.hlsli"
#include "common\common_samplers.hlsli"
#include "common\common_cbuffers.hlsli"
#include "common\common_functions.hlsli"
#include "common\metalic_roughness_base.hlsli"
#include "common\cgim.h"

#endif
