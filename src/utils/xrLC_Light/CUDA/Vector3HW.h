#pragma once
#include <cuda_runtime.h>

#define EPS_L 0.0010000f;
#define EPS_S 0.0000001f

// Hardware Vectors 
struct Hardware_Vector2
{
	float x;
	float y;
 
	__device__ Hardware_Vector2(float2 value)
	{
		x = value.x;
		y = value.y;
 	}

	float2 __device__ getVector2()
	{
		return make_float2(x, y);
	}

	__device__ Hardware_Vector2()
	{
		x = 0;
		y = 0;
 	}

	__device__ Hardware_Vector2(float InX, float InY)
	{
		x = InX;
		y = InY;
	}

	__device__ void set(float InX, float InY)
	{
		x = InX;
		y = InY;
 	}

	// Math

	__device__ float DotProduct(Hardware_Vector2& Another)
	{
		return x * Another.x + y * Another.y;
	}

	__device__ float DistanceSquared(Hardware_Vector2& Another)
	{
		return (x - Another.x) * (x - Another.x) + (y - Another.y) * (y - Another.y);
	}

	__device__ Hardware_Vector2 Add(Hardware_Vector2& Another)
	{
		Hardware_Vector2 Result(0, 0);;
		Result = { x + Another.x, y + Another.y };
		x = Result.x;
		y = Result.y;
		return Result;
	}

	__device__ Hardware_Vector2 Subtract(Hardware_Vector2& Another)
	{
		Hardware_Vector2 Result(0, 0);;
		Result = { x - Another.x, y - Another.y };
		x = Result.x;
		y = Result.y;
		return Result;
	}

	__device__ Hardware_Vector2 Subtract(Hardware_Vector2& p1, Hardware_Vector2& p2)
	{
		Hardware_Vector2 Result(0, 0);;
		Result = { p1.x - p2.x, p1.y - p2.y};
		x = Result.x;
		y = Result.y;
		return Result;
	}


	__device__ void Mad_Self(Hardware_Vector2& Another, float Influence)
	{
		x += Another.x * Influence;
		y += Another.y * Influence;
 	}

	// Invert
 	__device__ Hardware_Vector2 Inverted() const
	{
		return Hardware_Vector2{ -x, -y };
	}

	__device__ void Inverted(Hardware_Vector2 other)
	{
		x = -other.x;
		y = -other.y;
	}


	// Simular
 
	__device__ bool similar(Hardware_Vector2& p, float eu, float ev) const
	{
		return abs(x - p.x) < eu && abs(y - p.y) < ev;
	}

	__device__ bool similar(const Hardware_Vector2& p, float E) const
	{
		return abs(x - p.x) < E && abs(y - p.y) < E;
	};
};
  
struct Hardware_Vector
{
	float x;
	float y;
	float z;
 
	// Cuda Convert
	__device__ Hardware_Vector(float3 value)
	{
		x = value.x;
		y = value.y;
		z = value.z;
	}

	float3 __device__ getVector3()
	{
		return make_float3(x, y, z);
	}
 
	// Oxygen Code
	__device__ Hardware_Vector()
	{
		x = 0;
		y = 0;
		z = 0;
	}

	__device__ Hardware_Vector(float InX, float InY, float InZ)
	{
		x = InX;
		y = InY;
		z = InZ;
	}

	__device__ Hardware_Vector Inverted() const
	{
		return Hardware_Vector{ -x, -y, -z };
	}

	__device__ void Inverted(Hardware_Vector other)
	{
		x = -other.x;  
		y = -other.y;
		z = -other.z;
	}

	__device__ Hardware_Vector CrossProduct(Hardware_Vector& v1, Hardware_Vector& v2)
	{
		Hardware_Vector res(0, 0, 0);
		res.x = v1.y * v2.z - v1.z * v2.y;
		res.y = v1.z * v2.x - v1.x * v2.z;
		res.z = v1.x * v2.y - v1.y * v2.x;
		return res;
	}

	__device__ float DotProduct(Hardware_Vector& Another) const
	{
		return x * Another.x + y * Another.y + z * Another.z;
	}

	__device__ float DistanceSquared(Hardware_Vector& Another) const
	{
		return (x - Another.x) * (x - Another.x) + (y - Another.y) * (y - Another.y) + (z - Another.z) * (z - Another.z);
	}

	__device__ Hardware_Vector Add(Hardware_Vector& Another)
	{
		Hardware_Vector Result(0, 0, 0);;
		Result = { x + Another.x, y + Another.y, z + Another.z };
		x = Result.x;
		y = Result.y;
		z = Result.z;
		return Result;
	}

	__device__ Hardware_Vector Add(float s)
	{
		Hardware_Vector Result(0, 0, 0);;
		Result = { x + s, y + s, z + s };
		x = Result.x;
		y = Result.y;
		z = Result.z;
		return Result;
	}

	__device__ Hardware_Vector Subtract(Hardware_Vector& Another)
	{
		Hardware_Vector Result(0, 0, 0);;
		Result = { x - Another.x, y - Another.y, z - Another.z };
		x = Result.x;
		y = Result.y;
		z = Result.z;
		return Result;
	}

	__device__ Hardware_Vector Subtract(Hardware_Vector& main, Hardware_Vector& Other) 
	{
		Hardware_Vector Result(0, 0, 0);;
		Result = { Other.x - main.x, Other.y - main.y, Other.z - main.z };
		x = Result.x;
		y = Result.y;
		z = Result.z;
		return Result;
	}

	__device__ void Mad_Self(Hardware_Vector& Another, float Influence)
	{
		x += Another.x * Influence;
		y += Another.y * Influence;
		z += Another.z * Influence;
	}

	__device__ void MadOthers(Hardware_Vector& p, Hardware_Vector& d, float Influence)
	{
		x = p.x + d.x * Influence;
		y = p.y + d.y * Influence;
		z = p.z + d.z * Influence;
	}

	__device__ void set(float InX, float InY, float InZ)
	{
		x = InX;
		y = InY;
		z = InZ;
	}

	__device__ void Normalize_Safe()
	{
		float Magnitude = x * x + y * y + z * z;

		if (Magnitude > EPS_S)
		{
			Magnitude = sqrtf(1 / Magnitude);
			x *= Magnitude;
			y *= Magnitude;
			z *= Magnitude;
		}
	}

	__device__ void	from_bary(const Hardware_Vector& V1, const Hardware_Vector& V2, const Hardware_Vector& V3, float u, float v, float w)
	{
		x = V1.x * u + V2.x * v + V3.x * w;
		y = V1.y * u + V2.y * v + V3.y * w;
		z = V1.z * u + V2.z * v + V3.z * w;
 	}

	__device__ void	from_bary(const Hardware_Vector& V1, const Hardware_Vector& V2, const Hardware_Vector& V3, const Hardware_Vector& B)
	{
		from_bary(V1, V2, V3, B.x, B.y, B.z);
 	}
};
 
// Hardware Deflectors Stuff

struct Hardware_Color
{
 	Hardware_Vector rgb;
	float hemi;
	float sun;
 
	__device__ Hardware_Color()
	{
		rgb.set(0,0,0);
 		hemi = 0.0f;
		sun = 0.0f;
 	}
 
	__device__ void mul(float s)
	{
 		rgb.x *= s;
		rgb.y *= s;
		rgb.z *= s;

		hemi *= s;
		sun *= s;
	}

	__device__ void add(float s)
	{
 		rgb.x += s;
		rgb.y += s;
		rgb.z += s;

		hemi += s;
		sun += s;
	}

	__device__ void add(const Hardware_Color& s)
	{
		rgb.x += s.rgb.x;
		rgb.y += s.rgb.y;
		rgb.z += s.rgb.z;

		hemi += s.hemi;
		sun += s.sun;
	}
 
	__device__ void max(const Hardware_Color& s)
	{
		rgb.x = fmaxf(rgb.x, s.rgb.x);
		rgb.y = fmaxf(rgb.y, s.rgb.y);
		rgb.z = fmaxf(rgb.z, s.rgb.z);

		hemi = fmaxf(hemi, s.hemi);
		sun = fmaxf(sun, s.sun);
	}
 
};
 
struct Hardware_Lighting
{
	uint16_t type;				// Type of light source		
	uint16_t light_type;		// RGB, SUN, HEMI
	float3	 diffuse;			// Diffuse color of light	
	float3	 position;			// Position in world space	
	float3	 direction;			// Direction in world space	
	float	 range;				// Cutoff range
	float	 range2;			// ^2
	float	 falloff;			// precalc to make light aqal to zero at light range
	float	 attenuation0;		// Constant attenuation		
	float	 attenuation1;		// Linear attenuation		
	float	 attenuation2;		// Quadratic attenuation	
	float	 energy;			// For radiosity ONLY
};

struct Hardware_Raytask 
{
	float3 Position;
	float3 Direction;
};

// Vertex, Triangle GPU

struct VertexGPU
{
	Hardware_Vector P, N;
	Hardware_Color C;
};

struct _TCF_GPU
{
	Hardware_Vector2			uv[3];

	__device__ void barycentric(Hardware_Vector2& P, float& u, float& v, float& w)
	{
		Hardware_Vector2 	kV02; kV02.Subtract (uv[0], uv[2]);
		Hardware_Vector2 	kV12; kV12.Subtract (uv[1], uv[2]);
		Hardware_Vector2 	kPV2; kPV2.Subtract (P, uv[2]);

		float		fM00		= kV02.DotProduct(kV02);
		float		fM01		= kV02.DotProduct(kV12);
		float		fM11		= kV12.DotProduct(kV12);
		float		fR0			= kV02.DotProduct(kPV2);
		float		fR1			= kV12.DotProduct(kPV2);
		float		fDet		= fM00 * fM11 - fM01 * fM01;

		u = (fM11 * fR0 - fM01 * fR1) / fDet;
		v = (fM00 * fR1 - fM01 * fR0) / fDet;
		w = 1.0f - u - v;
	}

	__device__ void	barycentric(Hardware_Vector2& P, Hardware_Vector& B) { barycentric(P, B.x, B.y, B.z); }
	__device__ bool	isInside(float u, float v, float w) { return (u >= 0 && u <= 1) && (v >= 0 && v <= 1) && (w >= 0 && w <= 1); }
	__device__ bool	isInside(Hardware_Vector& B) { return	isInside(B.x, B.y, B.z); }
	__device__ bool	isInside(Hardware_Vector2& P, Hardware_Vector& B) { barycentric(P, B);	return isInside(B); }

	// bool	similar(const _TCF_GPU& _tc, float eps) const
	// {
	// 	return	uv[0].similar(_tc.uv[0], eps) &&
	// 			uv[1].similar(_tc.uv[1], eps) &&
	// 			uv[2].similar(_tc.uv[2], eps);
	// }

};

struct UVTriGPU : public _TCF_GPU
{
	VertexGPU V[3];
	Hardware_Vector N;
};

// CDeflectorGPU 
struct DeflectorsJitter
{
	Hardware_Vector2	dim, half;
	Hardware_Vector2	JS;
	unsigned int		SamplesCaptured;
};

struct CDeflector_GPU
{
	UVTriGPU*			UVTris;
	unsigned int		UVTrisSize;

	Hardware_Color*		surfaces;
	unsigned int		surfaces_size;

	unsigned char *		marker;
	unsigned int		marker_size;

	unsigned int        Width;
	unsigned int        Height;
	Hardware_Vector		normal;

	DeflectorsJitter	Jitter;
};
 
#include "optix.h"

struct OPTICK_Params
{
	OptixTraversableHandle handle;

	unsigned char		flags;
	Hardware_Raytask* rays;
	Hardware_Color* colors;		// Раньше rays == colors

	Hardware_Lighting* lights;
	int					counts_lights;

	CDeflector_GPU* GpuDeflectors;
	bool				DeflectorsBig;
	int					DeflectorsBigID;
};