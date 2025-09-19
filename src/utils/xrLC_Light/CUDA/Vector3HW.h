#pragma once
#include <cuda_runtime.h>

#define EPS_S  0.0000001f
 

struct HardwareVector
{
	float x;
	float y;
	float z;
 
	// Cuda Convert
	__device__ HardwareVector(float3 value)
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
	__device__ HardwareVector()
	{
		x = 0;
		y = 0;
		z = 0;
	}

	__device__ HardwareVector(float InX, float InY, float InZ)
	{
		x = InX;
		y = InY;
		z = InZ;
	}

	__device__ HardwareVector Inverted() const
	{
		return HardwareVector{ -x, -y, -z };
	}

	__device__ void Inverted(HardwareVector other) 
	{
		x = -other.x;  
		y = -other.y;
		z = -other.z;
	}

	__device__ HardwareVector CrossProduct(HardwareVector& v1, HardwareVector& v2)
	{
		HardwareVector res(0, 0, 0);
		res.x = v1.y * v2.z - v1.z * v2.y;
		res.y = v1.z * v2.x - v1.x * v2.z;
		res.z = v1.x * v2.y - v1.y * v2.x;
		return res;
	}

	__device__ float DotProduct(HardwareVector& Another) const
	{
		return x * Another.x + y * Another.y + z * Another.z;
	}

	__device__ float DistanceSquared(HardwareVector& Another) const
	{
		return (x - Another.x) * (x - Another.x) + (y - Another.y) * (y - Another.y) + (z - Another.z) * (z - Another.z);
	}

	__device__ HardwareVector Add(HardwareVector& Another) const
	{
		HardwareVector Result(0, 0, 0);;
		Result = { x + Another.x, y + Another.y, z + Another.z };
		return Result;
	}

	__device__ HardwareVector Subtract(HardwareVector& Another) const
	{
		HardwareVector Result(0, 0, 0);;
		Result = { x - Another.x, y - Another.y, z - Another.z };
		return Result;
	}

	__device__ HardwareVector Subtract(HardwareVector& main, HardwareVector& Other) const
	{
		HardwareVector Result(0, 0, 0);;
		Result = { Other.x - main.x, Other.y - main.y, Other.z - main.z };
		return Result;
	}

	__device__ void Mad_Self(HardwareVector& Another, float Influence)
	{
		x += Another.x * Influence;
		y += Another.y * Influence;
		z += Another.z * Influence;
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
};

struct hardware_color
{
	union
	{
		uint32_t rgba;
		struct
		{
			uint8_t r, g, b, a;
		};
	};

	float hemi;
	float sun;
	uint32_t _pad;

	__device__ hardware_color()
	{
		rgba = 0;
		hemi = 0.0f;
		sun = 0.0f;
		_pad = 0;
	}

	__device__ void set_rgb_f32(float x, float y, float z)
	{
		r = static_cast<uint8_t>(fminf(fmaxf(x * 255.0f, 0.0f), 255.0f));
		g = static_cast<uint8_t>(fminf(fmaxf(y * 255.0f, 0.0f), 255.0f));
		b = static_cast<uint8_t>(fminf(fmaxf(z * 255.0f, 0.0f), 255.0f));
		a = 0;
	}

	__device__ float3 get_rgb_f32() const
	{
		return make_float3(r / 255.0f, g / 255.0f, b / 255.0f);
	}

	__device__ void mul(float s)
	{
		float3 rgb = get_rgb_f32();
		set_rgb_f32(rgb.x * s, rgb.y * s, rgb.z * s);

		hemi *= s;
		sun *= s;
	}

	__device__ void add(float s)
	{
		float3 rgb = get_rgb_f32();
		rgb = make_float3(rgb.x + s, rgb.y + s, rgb.z + s);
		set_rgb_f32(rgb.x, rgb.y, rgb.z);

		hemi += s;
		sun += s;
	}

	__device__ void add(const hardware_color& s)
	{
		float3 rgb0 = get_rgb_f32();
		float3 rgb1 = s.get_rgb_f32();
		set_rgb_f32(rgb0.x + rgb1.x, rgb0.y + rgb1.y, rgb0.z + rgb1.z);

		hemi += s.hemi;
		sun += s.sun;
	}

	__device__ void scale(int samples)
	{
		float inv = 1.0f / static_cast<float>(samples);
		mul(inv);
	}

	__device__ void max(const hardware_color& s)
	{
		float3 rgb0 = get_rgb_f32();
		float3 rgb1 = s.get_rgb_f32();
		set_rgb_f32(fmaxf(rgb0.x, rgb1.x), fmaxf(rgb0.y, rgb1.y), fmaxf(rgb0.z, rgb1.z));

		hemi = fmaxf(hemi, s.hemi);
		sun = fmaxf(sun, s.sun);
	}

	__device__ void lerp(const hardware_color& A, const hardware_color& B, float s)
	{
		float is = 1.0f - s;
		float3 rgb;
		rgb.x = A.r * is + B.r * s;
		rgb.y = A.g * is + B.g * s;
		rgb.z = A.b * is + B.b * s;
		set_rgb_f32(rgb.x / 255.0f, rgb.y / 255.0f, rgb.z / 255.0f);

		hemi = A.hemi * is + B.hemi * s;
		sun = A.sun * is + B.sun * s;
	}
};

 
struct hardware_lighting
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

struct hardware_raytask 
{
	float3 Position;
	float3 Direction;
};
