#pragma once
#include "xrFaceDefs.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "../../xrCDB/xrCDB.h"
#include "xrDeflectorDefs.h"

class  base_lighting;
 
class CDeflector;

XRLC_LIGHT_API void IntelEmbereLOAD(bool useForOthers);
XRLC_LIGHT_API void IntelEmbereUNLOAD();
 
class execute_statistics;
class XRLC_LIGHT_API CDeflector
{

public:
 	xr_vector<UVtri>			UVpolys;
	Fvector						normal;
	lm_layer					layer;
	Fsphere						Sphere;
	
	BOOL						bMerged;
public:

						CDeflector					();
 
 						~CDeflector					();
static	CDeflector*		read_create					();	

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
	u16	GetBaseMaterial		() ;

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
	u64		size_deflector()
	{
		u32 STri = UVpolys.capacity() * sizeof(UVtri);
		u32 SLMLayer = layer.memory_lmap();

		return sizeof(*this) + STri + SLMLayer;
	}
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


extern XRLC_LIGHT_API u32		getLMSIZE();
extern XRLC_LIGHT_API void		setLMSIZE(int size);

#define rms_zero	((4+g_params().m_lm_rms_zero)/2)
#define rms_shrink	((8+g_params().m_lm_rms)/2)
 