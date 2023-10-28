#pragma once

struct PolyIndexes
{
	int Index1;
	int Index2;
	int Index3;
};

// struct RawVertex
// {
// 	float x;
// 	float y;
// 	float z;
// };

enum HardwareModelType
{
	HM_STANDART,
	HM_OPTIMIZED
};

#ifndef __CUDACC__
#define __device__
#endif

struct HardwareVector
{
	float x;
	float y;
	float z;

	__device__ HardwareVector() {
		x = 0.0f;
		y = 0.0f;
		z = 0.0f;
	}

	__device__ HardwareVector(float InX, float InY, float InZ) {
		x = InX;
		y = InY;
		z = InZ;
	}

#ifdef __CUDACC__

#define EPS_S  0.0000001f

	__device__ HardwareVector Inverted() const {
		HardwareVector Result;
		Result = { -x, -y, -z };
		return Result;
	}

	__device__ float DotProduct(HardwareVector& Another) const {
		return x * Another.x + y * Another.y + z * Another.z;
	}

	__device__ float DistanceSquared(HardwareVector& Another) const {
		return (x - Another.x) * (x - Another.x) + (y - Another.y) * (y - Another.y) + (z - Another.z) * (z - Another.z);
	}

	__device__ HardwareVector Add(HardwareVector& Another) const {
		HardwareVector Result;
		Result = { x + Another.x, y + Another.y, z + Another.z };
		return Result;
	}

	__device__ HardwareVector Subtract(HardwareVector& Another) const {
		HardwareVector Result;
		Result = { x - Another.x, y - Another.y, z - Another.z };
		return Result;
	}

	__device__ void Mad_Self(HardwareVector& Another, float Influence) {
		x += Another.x * Influence;
		y += Another.y * Influence;
		z += Another.z * Influence;
	}

	__device__ void set(float InX, float InY, float InZ) {
		x = InX;
		y = InY;
		z = InZ;
	}

	__device__ void Normalize_Safe() {
		float Magnitude = x * x + y * y + z * z;

		if (Magnitude > EPS_S)
		{
			Magnitude = sqrtf(1 / Magnitude);
			x *= Magnitude;
			y *= Magnitude;
			z *= Magnitude;
		}
	}

#else
	operator Fvector() const {
		Fvector Result;
		Result.x = x;
		Result.y = y;
		Result.z = z;
		return Result;
	}

	HardwareVector(const Fvector& From) {
		x = From.x;
		y = From.y;
		z = From.z;
	}

	HardwareVector(const Fvector2& From) {
		x = From.x;
		y = From.y;
		z = 0.0f;
	}
#endif
};

#ifndef __CUDACC__
#undef __device__
#endif

//not in begin of file, because we need declare HardwareVector first
#include "R_light.h"

struct VertexRay
{
	float Distance;
	bool isDone;
};

struct Ray
{
	static const int format = 1089;

	HardwareVector Origin;
	float tmin;
	HardwareVector Direction;
	float tmax;

#ifndef __CUDACC__
	Ray() {
		tmin = 0.0f;
		tmax = 0.0f;
	}
#endif
};

struct RayRequest
{
	HardwareVector Position;
	HardwareVector Normal;

	void* FaceToSkip;
};

enum class LightCategory
{
	T_RGB = 0,
	T_SUN = 1,
	T_HEMI = 2
};

enum class LightType
{
	T_DIRECT = 0,
	T_POINT,
	T_SECONDARY
};

struct Ray_Detail
{
	LightCategory RayLightCategory;
	LightType RayLightType;
};


struct Hit
{
	static const int format = 0x463;

	float Distance;
	int   triId;
	float u;
	float v;
};

struct xrHardwarePixel
{
	char Alpha;
	char Red;
	char Green;
	char Blue;
};

struct xrHardwareTexture
{
	unsigned int Width;
	unsigned int Height;
	bool IsHaveAlpha;
	xrHardwarePixel* Pixels;
};

struct LightSizeInfo
{
	int RGBLightCount;
	int SunLightCount;
	int HemiLightCount;
};

struct TrisAdditionInfo
{
	HardwareVector TexCoords;
	unsigned int TextureID;
	bool CastShadow;
	u64 FaceID;
};

struct HardwareModel
{
	HardwareVector* Vertexes;
	HardwareVector* VertexNormal;
	TrisAdditionInfo* TrianglesAdditionInfo;
	PolyIndexes* Tris;
	int					VertexCount;
};

struct xrHardwareLCGlobalData
{
	LightSizeInfo* LightSize;
	R_Light* LightData;
	xrHardwareTexture* Textures;

	//NEW DATA!
	HardwareModel RaycastModel;
};