#pragma once
#include "xrFace.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "xrDeflectorDefs.h"
#include "embree_raytracing/EmbreeRayTrace.h"

// se7kills: Packed Task pool
#include "cuda/xrCuda_PackedLights.h"

class  base_lighting;
extern EmbreeRayTraceModel EmbreeMain;
class  execute_statistics;

class XRLC_LIGHT_API CDeflector
{
public:
   	bool bMerged			= false;
 
	Fvector				normal;
 	xr_vector<UVtri>	UVpolys;

	lm_layer			layer;
	Fsphere				Sphere;

public:

	CDeflector					();
  	~CDeflector					();
 
	void clear_memory() 
	{
		UVpolys.clear();
		UVpolys.shrink_to_fit();

		layer.clear_memory();
	};

	void	OA_SetNormal		(Fvector &_N )	{ normal.set(_N); normal.normalize(); VERIFY(_valid(normal)); }
	bool	OA_Place			(Face *owner);
	void	OA_Place			(vecFace& lst);
	void	OA_Export			();
		
	void	GetRect				(Fvector2 &min, Fvector2 &max);
	u32		GetFaceCount()		{ return (u32)UVpolys.size();	};
		
	void	Light				(CDB::COLLIDER* DB, base_lighting* LightsSelected);
	void	L_Direct			(CDB::COLLIDER* DB, base_lighting* LightsSelected);
 
	u32		weight				() { return layer.Area(); }	
	u16		GetBaseMaterial		() ;

	void	Bounds				(u32 ID, Fbox2& dest)
	{
		UVtri& TC		= UVpolys[ID];
		dest.min.set	(TC.uv[0]);
		dest.max.set	(TC.uv[0]);
		dest.modify		(TC.uv[1]);
		dest.modify		(TC.uv[2]);
	}
	
	void	Bounds_Summary		(Fbox2& bounds)
	{
		bounds.invalidate();
		for (u32 I=0; I<UVpolys.size(); I++)
		{
			Fbox2	B;
			Bounds	(I,B);
			bounds.merge(B);
		}
	}

	void	RemapUV				(xr_vector<UVtri>& dest, u32 base_u, u32 base_v, u32 size_u, u32 size_v, u32 lm_u, u32 lm_v, bool bRotate);
	void	RemapUV				(u32 base_u, u32 base_v, u32 size_u, u32 size_v, u32 lm_u, u32 lm_v, bool bRotate);
 	
	bool	similar				( const CDeflector &D, float eps =EPS ) const;

	// se7kills Подсчитать Размер
	size_t		size_deflector()
	{
		size_t STri = UVpolys.capacity() * sizeof(UVtri);
		size_t SLMLayer = layer.memory_lmap();

		return sizeof(*this) + STri + SLMLayer;
	}

	size_t size_of_lm()
	{
		size_t SLMLayer = layer.memory_lmap();
		return SLMLayer;
	}

	size_t size_of_tris()
	{
		size_t STri = UVpolys.capacity() * sizeof(UVtri);
		return STri;
	}
 
	// Stage 1
	void LightGPU();
	void L_DirectGPU();

	// cuda recvest color reciver
	u32 ProcessedUVColors;
	bool ApplyColors();
 	void ApplyColor(size_t INDEX, base_color_c& C);
 
	// Stage 2
 	void ApplyExpandBordersGPU();


	// Clearing Memory
	void DealocateMemory() { layer.clear_memory(); };
};


typedef xr_vector<UVtri>::iterator UVIt;

extern XRLC_LIGHT_API void		Jitter_Select	(Fvector2* &Jitter, u32& Jcount);
extern void		blit			(u32* dest,		u32 ds_x, u32 ds_y, u32* src,		u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern XRLC_LIGHT_API void		blit			(lm_layer& dst, u32 ds_x, u32 ds_y, lm_layer& src,	u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern void		blit_r			(u32* dest,		u32 ds_x, u32 ds_y, u32* src,		u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern XRLC_LIGHT_API void		blit_r			(lm_layer& dst, u32 ds_x, u32 ds_y, lm_layer& src,	u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern void		lblit			(lm_layer& dst, lm_layer& src, u32 px, u32 py, u32 aREF);


extern void UpdateCurrentPhase(LPCSTR text);

extern XRLC_LIGHT_API void		LightPoint		(CDB::COLLIDER* DB, CDB::MODEL* MDL, base_color_c &C, Fvector &P, Fvector &N, base_lighting& lights, u32 flags, Face* skip);
extern XRLC_LIGHT_API void		LightPointNew	(EmbreeRayTraceModel* MDL, base_color_c& C, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip);
void LightPoint_Jitters(xr_vector<JiterPixel>& world_pos, base_lighting& lights, u32 flags);
void LightPoint_Details(xr_vector<DetailsTask>& world_pos, base_lighting& lights, u32 flags);