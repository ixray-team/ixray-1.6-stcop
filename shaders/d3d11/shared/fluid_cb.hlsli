#ifndef FLUID_CB_HLSLI
#define FLUID_CB_HLSLI

#ifndef __cplusplus
#include "shared\cbuffer.hlsli"
#endif

CBUFFER(CBFluid, 9)
	float textureHeight;
	float textureWidth;
	float textureDepth;
	float modulate;
	float epsilon;
	float timestep;
	float forward;
	float size;
	float4 floatVolumeDim;
	float4 boxLBDcorner;
	float4 boxRTUcorner;
	float4 center;
	float4 splatColor;
	float4 OOBBClipPlane[6];
	float3x4 WorldToLocal;
	float3x4 LocalToWorld;
	float4 MassCenter;
	float4 OOBBWorldAngularVelocity;
	float4 OOBBWorldTranslationVelocity;
	float RTWidth;
	float RTHeight;
	float ZNear;
	float ZFar;
	float4 DiffuseLight;
	float4 DepthUnpack;
	float4x4 WorldViewProjection;
	float4x4 InvWorldViewProjection;
	float4 gridDim;
	float4 recGridDim;
	float maxGridDim;
	float gridScaleFactor;
	float GravityBuoyancy;
	int textureNumber;
	float4 eyeOnGrid;
CBUFFER_END

#endif
