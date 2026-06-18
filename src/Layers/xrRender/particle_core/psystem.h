#pragma once
#include "../FVF.h"
// Actually this must be < sqrt(MAXFLOAT) since we store this value squared.
#define P_MAXFLOAT	1.0e16f

#ifdef MAXINT
	#define P_MAXINT	MAXINT
#else
	#define P_MAXINT	0x7fffffff
#endif

#define drand48()		::Random.randF()

namespace PAPI{

	// A single particle
	struct Rotation
	{
		float		x;
		ICF void set(float _x)				{ x = _x;};
		ICF void set(const Rotation& rot_)  { x = rot_.x;};
		ICF	void inertion(const Rotation &p, float v) 
		{
			float inv = 1.f-v;
			x = v*x + inv*p.x;
		};
	};

	/*struct Particle
	{
		enum{
			ANIMATE_CCW	= (1<<0),
		};
		struct LITBUFF { FVF::LIT buff[4]; } buff =
		{
			Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
			Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
			Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
			Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f}
		};
		Fvector pos;
		Fvector posB;
		Fvector posI;
		Fvector vel;
		Fvector velI;
		Fvector rot_vel;
		Fvector rot_velS;
		Fvector size;
		Fvector sizeI;
		Fvector sizeMod{ 1.0f, 1.0f, 1.0f };
		Rotation rot;
		Rotation rotI;
		Fcolor color;
		Fvector4 colorMod{1.0f, 1.0f, 1.0f, 1.0f};
		float age = 0.0f;
		u16 frame = 0;
		Flags16 flags;

		ICF void Reset_modifiers()
		{
			sizeMod = {1.0f, 1.0f, 1.0f};
			colorMod = {1.0f, 1.0f, 1.0f, 1.0f};
		}
	};*/

	struct Particles
	{
		enum{
			ANIMATE_CCW	= (1<<0),
		};
		struct LITBUFF
		{
			FVF::LIT buff[4] =
			{
				Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
				Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
				Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f},
				Fvector{0.f,0.f,0.f},0u,Fvector2{0.f,0.f}
			};
		};
		struct FvectorMod
		{
			Fvector mod = { 1.0f, 1.0f, 1.0f };
		};
		struct Fvector4Mod
		{
			Fvector4 mod = { 1.0f, 1.0f, 1.0f, 1.0f };
		};
		LITBUFF* buff_arr = nullptr;
		Fvector* pos_arr = nullptr;
		Fvector* posB_arr = nullptr;
		Fvector* posI_arr = nullptr;
		Fvector* vel_arr = nullptr;
		Fvector* velI_arr = nullptr;
		Fvector* rot_vel_arr = nullptr;
		Fvector* rot_velS_arr = nullptr;
		Fvector* size_arr = nullptr;
		Fvector* sizeI_arr = nullptr;
		FvectorMod* sizeMod_arr = nullptr;
		Rotation* rot_arr = nullptr;
		Rotation* rotI_arr = nullptr;
		Fcolor* color_arr = nullptr;
		Fvector4Mod* colorMod_arr = nullptr;
		float* age_arr = nullptr;
		u16* frame_arr = nullptr;
		Flags16* flags_arr = nullptr;

		Particles()	= default;

		~Particles()
		{
			Free();
		}

		Particles(const Particles& other) = delete;
		Particles& operator=(const Particles& other) = delete;
		Particles(Particles&& other) noexcept
		{
			buff_arr = other.buff_arr;
			pos_arr = other.pos_arr;
			posB_arr = other.posB_arr;
			posI_arr = other.posI_arr;
			vel_arr = other.vel_arr;
			velI_arr = other.velI_arr;
			rot_vel_arr = other.rot_vel_arr;
			rot_velS_arr = other.rot_velS_arr;
			size_arr = other.size_arr;
			sizeI_arr = other.sizeI_arr;
			sizeMod_arr = other.sizeMod_arr;
			rot_arr = other.rot_arr;
			color_arr = other.color_arr;
			age_arr = other.age_arr;
			frame_arr = other.frame_arr;
			flags_arr = other.flags_arr;
		}
		Particles& operator=(Particles&& other) noexcept
		{
			buff_arr = other.buff_arr;
			pos_arr = other.pos_arr;
			posB_arr = other.posB_arr;
			posI_arr = other.posI_arr;
			vel_arr = other.vel_arr;
			velI_arr = other.velI_arr;
			rot_vel_arr = other.rot_vel_arr;
			rot_velS_arr = other.rot_velS_arr;
			size_arr = other.size_arr;
			sizeI_arr = other.sizeI_arr;
			sizeMod_arr = other.sizeMod_arr;
			rot_arr = other.rot_arr;
			rotI_arr = other.rotI_arr;
			color_arr = other.color_arr;
			colorMod_arr = other.colorMod_arr;
			age_arr = other.age_arr;
			frame_arr = other.frame_arr;
			flags_arr = other.flags_arr;
			other.buff_arr = nullptr;
			other.pos_arr = nullptr;
			other.posB_arr = nullptr;
			other.posI_arr = nullptr;
			other.vel_arr = nullptr;
			other.velI_arr = nullptr;
			other.rot_vel_arr = nullptr;
			other.rot_velS_arr = nullptr;
			other.size_arr = nullptr;
			other.sizeI_arr = nullptr;
			other.sizeMod_arr = nullptr;
			other.rot_arr = nullptr;
			other.rotI_arr = nullptr;
			other.color_arr = nullptr;
			other.colorMod_arr = nullptr;
			other.age_arr = nullptr;
			other.frame_arr = nullptr;
			other.flags_arr = nullptr;
			return *this;
		}

#define ElemSize(X) sizeof(std::remove_pointer_t<decltype(X)>)
		IC void Realloc(size_t Num)
		{
			constexpr size_t buff_elem_size = ElemSize(buff_arr);
			constexpr size_t pos_elem_size = buff_elem_size + ElemSize(pos_arr);
			constexpr size_t posB_elem_size = pos_elem_size + ElemSize(posB_arr);
			constexpr size_t posI_elem_size = posB_elem_size + ElemSize(posI_arr);
			constexpr size_t vel_elem_size = posI_elem_size + ElemSize(vel_arr);
			constexpr size_t velI_elem_size = vel_elem_size + ElemSize(velI_arr);
			constexpr size_t rot_vel_elem_size = velI_elem_size + ElemSize(rot_vel_arr);
			constexpr size_t rot_velS_elem_size = rot_vel_elem_size + ElemSize(rot_velS_arr);
			constexpr size_t size_elem_size = rot_velS_elem_size + ElemSize(size_arr);
			constexpr size_t sizeI_elem_size = size_elem_size + ElemSize(sizeI_arr);
			constexpr size_t sizeMod_elem_size = sizeI_elem_size + ElemSize(sizeMod_arr);
			constexpr size_t rot_elem_size = sizeMod_elem_size + ElemSize(rot_arr);
			constexpr size_t rotI_elem_size = rot_elem_size + ElemSize(rotI_arr);
			constexpr size_t color_elem_size = rotI_elem_size + ElemSize(color_arr);
			constexpr size_t colorMod_elem_size = color_elem_size + ElemSize(colorMod_arr);
			constexpr size_t age_elem_size = colorMod_elem_size + ElemSize(age_arr);
			constexpr size_t frame_elem_size = age_elem_size + ElemSize(frame_arr);
			constexpr size_t flags_elem_size = frame_elem_size + ElemSize(flags_arr);
			
			u8* buff = xr_alloc<u8>(Num*flags_elem_size);
			
			buff_arr = (LITBUFF*)buff;
			pos_arr = (Fvector*)(buff+buff_elem_size*Num);
			posB_arr = (Fvector*)(buff+pos_elem_size*Num);
			posI_arr = (Fvector*)(buff+posB_elem_size*Num);
			vel_arr = (Fvector*)(buff+posI_elem_size*Num);
			velI_arr =(Fvector*)(buff+vel_elem_size*Num);
			rot_vel_arr = (Fvector*)(buff+velI_elem_size*Num);
			rot_velS_arr = (Fvector*)(buff+rot_vel_elem_size*Num);
			size_arr = (Fvector*)(buff+rot_velS_elem_size*Num);
			sizeI_arr = (Fvector*)(buff+size_elem_size*Num);
			sizeMod_arr = (FvectorMod*)(buff+sizeI_elem_size*Num);
			rot_arr = (Rotation*)(buff+sizeMod_elem_size*Num);
			rotI_arr = (Rotation*)(buff+rot_elem_size*Num);
			color_arr = (Fcolor*)(buff+rotI_elem_size*Num);
			colorMod_arr = (Fvector4Mod*)(buff+color_elem_size*Num);
			age_arr = (float*)(buff+colorMod_elem_size*Num);
			frame_arr = (u16*)(buff+age_elem_size*Num);
			flags_arr = (Flags16*)(buff+frame_elem_size*Num);
		}

		IC void Free()
		{
			if (buff_arr)
			{
				VERIFY(buff_arr && pos_arr && posB_arr && posI_arr && vel_arr && velI_arr && rot_vel_arr && rot_velS_arr && size_arr
					&& sizeI_arr && sizeMod_arr && rot_arr && rotI_arr && color_arr && colorMod_arr && age_arr && frame_arr && flags_arr);
				u8* buff_all = (u8*)buff_arr;
				xr_free(buff_all);
			} else
			{
				VERIFY(!(buff_arr || pos_arr || posB_arr || posI_arr || vel_arr || velI_arr || rot_vel_arr || rot_velS_arr || size_arr
					|| sizeI_arr || sizeMod_arr || rot_arr || rotI_arr || color_arr || colorMod_arr || age_arr || frame_arr || flags_arr));
			}
		}

		IC void CopyData(const Particles& Src, size_t ElemNum)
		{
#define DataMemcpy(X) std::memcpy(X, Src.X, ElemSize(X)*ElemNum)
			//DataMemcpy(buff_arr);
			DataMemcpy(pos_arr);
			DataMemcpy(posB_arr);
			DataMemcpy(posI_arr);
			DataMemcpy(vel_arr);
			DataMemcpy(velI_arr);
			DataMemcpy(rot_vel_arr);
			DataMemcpy(rot_velS_arr);
			DataMemcpy(size_arr);
			DataMemcpy(sizeI_arr);
			DataMemcpy(sizeMod_arr);
			DataMemcpy(rot_arr);
			DataMemcpy(rotI_arr);
			DataMemcpy(color_arr);
			DataMemcpy(colorMod_arr);
			DataMemcpy(age_arr);
			DataMemcpy(frame_arr);
			DataMemcpy(flags_arr);
#undef DataMemcpy
		}

		IC void SwapWithLast(size_t i, size_t size)
		{
			if (i == size-1)
			{
				return;
			}
#define Swap(X) X[i] = std::move(X[size-1])
			Swap(pos_arr);
			Swap(posB_arr);
			Swap(posI_arr);
			Swap(vel_arr);
			Swap(velI_arr);
			Swap(rot_vel_arr);
			Swap(rot_velS_arr);
			Swap(size_arr);
			Swap(sizeI_arr);
			Swap(sizeMod_arr);
			Swap(rot_arr);
			Swap(rotI_arr);
			Swap(color_arr);
			Swap(colorMod_arr);
			Swap(age_arr);
			Swap(frame_arr);
			Swap(flags_arr);
#undef Swap
		}

		IC void Add(size_t p_count, const Fvector& pos, const Fvector& posB,
			const Fvector& size, const Fvector& rot, const Fvector& vel, const Fvector& rot_vel,
			u32 color, const float age = 0.0f, u16 frame = 0, u16 flags = 0)
		{
			pos_arr[p_count] = pos;
			posB_arr[p_count] = posB;
			posI_arr[p_count] = pos;
			vel_arr[p_count] = vel;
			velI_arr[p_count] = vel;
			rot_vel_arr[p_count] = rot_vel;
			rot_velS_arr[p_count] = rot_vel;
			size_arr[p_count] = size;
			sizeI_arr[p_count] = size;
			sizeMod_arr[p_count] = FvectorMod();
			rot_arr[p_count].x = rot.x;
			rotI_arr[p_count].x = rot.x;
			color_arr[p_count] = color;
			colorMod_arr[p_count] = Fvector4Mod();
			age_arr[p_count] = age;
			frame_arr[p_count] = frame;
			flags_arr[p_count].assign(flags);
		}

		ICF u8* GetFullBuffer()
		{
			return (u8*)buff_arr;
		}

		static constexpr size_t GetElementSize()
		{
			constexpr size_t buff_elem_size = ElemSize(buff_arr)
				+ ElemSize(pos_arr)
				+ ElemSize(posB_arr)
				+ ElemSize(posI_arr)
				+ ElemSize(vel_arr)
				+ ElemSize(velI_arr)
				+ ElemSize(rot_vel_arr)
				+ ElemSize(rot_velS_arr)
				+ ElemSize(size_arr)
				+ ElemSize(sizeI_arr)
				+ ElemSize(sizeMod_arr)
				+ ElemSize(rot_arr)
				+ ElemSize(rotI_arr)
				+ ElemSize(color_arr)
				+ ElemSize(colorMod_arr)
				+ ElemSize(age_arr)
				+ ElemSize(frame_arr)
				+ ElemSize(flags_arr);
			return buff_elem_size;
		}
#undef ElemSize
	};

	typedef void (* OnBirthParticleCB)	(void* owner, u32 param, PAPI::Particles& P, size_t pID, u32 idx);
	typedef void (* OnDeadParticleCB)	(void* owner, u32 param, PAPI::Particles& P, size_t pID, u32 idx);
	//////////////////////////////////////////////////////////////////////
	// Type codes for domains
	enum PDomainEnum
	{
		PDPoint 	= 0,	// Single point
		PDLine 		= 1,	// Line segment
		PDTriangle 	= 2,	// Triangle
		PDPlane 	= 3,	// Arbitrarily-oriented plane
		PDBox 		= 4,	// Axis-aligned box
		PDSphere 	= 5,	// Sphere
		PDCylinder 	= 6,	// Cylinder
		PDCone 		= 7,	// Cone
		PDBlob 		= 8,	// Gaussian blob
		PDDisc 		= 9,	// Arbitrarily-oriented disc
		PDRectangle = 10,	// Rhombus-shaped planar region
		domain_enum_force_dword = u32(-1)
	};
};
