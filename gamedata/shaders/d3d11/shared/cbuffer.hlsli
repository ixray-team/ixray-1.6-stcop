#ifndef CBUFFER_HLSLI
#define CBUFFER_HLSLI

// The *_cb.hlsli blocks are also compiled as C++ structs (xrRenderPC_R4/ShaderConstants.h). There a
// float3 fills a whole register: never follow a float3 with a scalar, and no scalar arrays.
#define CBUFFER(name, slot) cbuffer name : register(b##slot) {
#define CBUFFER_END };

#endif
