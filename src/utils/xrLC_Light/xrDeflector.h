#pragma once
#include "xrFace.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "xrDeflectorDefs.h"
#include "embree_raytracing/EmbreeRayTrace.h"

// se7kills: Packed Task pool
#include "xrDeflectorLight_Packed.h"

class  base_lighting;
class CDeflector;
extern EmbreeData EmbreeMain;
class execute_statistics;




class XRLC_LIGHT_API CDeflector
{

public:
 	bool ApplyLmap = false;
	bool ApplyEdge = false;
	bool ApplyResolution = false;
 	bool bMerged = false;

	u32  ColorsRecvested = 0;
	u32  ColorsApply = 0;
 
	Fvector				normal;
 	xr_vector<UVtri>	UVpolys;

	lm_layer			layer;
	Fsphere				Sphere;

	// se7kills Освещение на GPU
	xrCriticalSection csApply;
	xr_concurrent_unordered_map<size_t, base_color_c>								def_color_map;
	xr_concurrent_unordered_map<size_t, u8>											def_FacesCount;
	 

public:

	CDeflector					();
  	~CDeflector					();
 
	void	OA_SetNormal		(Fvector &_N )	{ normal.set(_N); normal.normalize(); VERIFY(_valid(normal)); }
	BOOL	OA_Place			(Face *owner);
	void	OA_Place			(vecFace& lst);
	void	OA_Export			();
		
	void	GetRect				(Fvector2 &min, Fvector2 &max);
	u32		GetFaceCount()		{ return (u32)UVpolys.size();	};
		
	void	Light				(CDB::COLLIDER* DB, base_lighting* LightsSelected, HASH& H	);
	void	L_Direct			(CDB::COLLIDER* DB, base_lighting* LightsSelected, HASH& H  );
	void	L_Direct_Edge		(CDB::COLLIDER* DB, base_lighting* LightsSelected, Fvector2& p1, Fvector2& p2, Fvector& v1, Fvector& v2, Fvector& N, float texel_size, Face* skip);
	void	L_Calculate			(CDB::COLLIDER* DB, base_lighting* LightsSelected, HASH& H  );
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

	void	RemapUV				(xr_vector<UVtri>& dest, u32 base_u, u32 base_v, u32 size_u, u32 size_v, u32 lm_u, u32 lm_v, BOOL bRotate);
	void	RemapUV				(u32 base_u, u32 base_v, u32 size_u, u32 size_v, u32 lm_u, u32 lm_v, BOOL bRotate);
 	
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


	size_t size_of_colors() const
	{
 		return 0;
	}


	// Stage 1
	void LightGPU( HASH& H);
	void L_DirectGPU( HASH& H);

	// cuda recvest color reciver
	void ApplyColors();
	void ClearResults();
	void ApplyColor(size_t INDEX, base_color_c& C);

	// Stage 2
	void EdgesLighting(HASH& H);
 
	// Stage 3
	void LowerResolutionGPU(HASH& H);
	void ApplyExpadBordersGPU();
};


typedef xr_vector<UVtri>::iterator UVIt;

extern XRLC_LIGHT_API void		Jitter_Select	(Fvector2* &Jitter, u32& Jcount);
extern void		blit			(u32* dest,		u32 ds_x, u32 ds_y, u32* src,		u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern XRLC_LIGHT_API void		blit			(lm_layer& dst, u32 ds_x, u32 ds_y, lm_layer& src,	u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern void		blit_r			(u32* dest,		u32 ds_x, u32 ds_y, u32* src,		u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern XRLC_LIGHT_API void		blit_r			(lm_layer& dst, u32 ds_x, u32 ds_y, lm_layer& src,	u32 ss_x, u32 ss_y, u32 px, u32 py, u32 aREF);
extern void		lblit			(lm_layer& dst, lm_layer& src, u32 px, u32 py, u32 aREF);
extern XRLC_LIGHT_API void		LightPoint		(CDB::COLLIDER* DB, CDB::MODEL* MDL, base_color_c &C, Fvector &P, Fvector &N, base_lighting& lights, u32 flags, Face* skip);
extern XRLC_LIGHT_API BOOL		ApplyBorders	(lm_layer &lm, u32 ref);
extern XRLC_LIGHT_API void		DumpDeflctor	( u32 id );
extern XRLC_LIGHT_API void		DumpDeflctor	( const CDeflector &d );
extern XRLC_LIGHT_API void		DeflectorsStats ();
extern XRLC_LIGHT_API void		DumpDeflctor	( u32 id );

#define rms_zero	((4+g_params().m_lm_rms_zero)/2)
#define rms_shrink	((8+g_params().m_lm_rms)/2)
 