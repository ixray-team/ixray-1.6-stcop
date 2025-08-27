// DetailManager.h: interface for the CDetailManager class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "../../xrCore/xrPool.h"
#include "DetailFormat.h"
#include "DetailModel.h"
#include "light.h"

#ifdef _EDITOR
//.	#include	"ESceneClassList.h"
	const int	dm_max_decompress	= 14;
	class CCustomObject;
	typedef u32	ObjClassID;

    typedef xr_list<CCustomObject*> 		ObjectList;
    typedef ObjectList::iterator 			ObjectIt;
    typedef xr_map<ObjClassID,ObjectList> 	ObjectMap;
    typedef ObjectMap::iterator 			ObjectPairIt;

#else
	const int	dm_max_decompress	= 7;
#endif

const int		dm_obj_in_slot = 4;
const int		dm_cache1_count = 4;
const int		dm_cache_count = 16;
const float		dm_slot_size = DETAIL_SLOT_SIZE;

extern u32		dm_size;
extern u32 		dm_cache1_line;
extern u32		dm_cache_line;
extern u32		dm_cache_size;
extern float	dm_fade;
extern u32		dm_current_size;
extern u32 		dm_current_cache1_line;
extern u32		dm_current_cache_line;
extern u32		dm_current_cache_size;
extern float	dm_current_fade;
extern float	ps_current_detail_density;

class CDetailManager
{
public:
	struct	SlotPart
	{                              	// 
		u32							id;					// ID модельки
		xr_vector<xr_shared_ptr<CDetail::SlotItem>>			items;              // список кустиков
	};
	enum	SlotType	{
		stReady						= 0,				// Ready to use
		stPending,										// Pending for decompression

		stFORCEDWORD				= 0xffffffff
	};
	struct	Slot
	{								// распакованый слот размером DETAIL_SLOT_SIZE
		struct{
			u32						empty	:1;
			u32						type	:1;
			u32						frame	:30;
		};
		int							sx,sz;				// координаты слота X x Y
		vis_data					vis;				// 
		SlotPart					G[dm_obj_in_slot];	// 

									Slot()				{ frame=0;empty=1; type=stReady; sx=sz=0; vis.clear(); }
	};
    struct 	CacheSlot1
	{
		u32							empty;
    	vis_data 					vis;
        Slot** 						slots[dm_cache_count];
		CacheSlot1()				{empty=1; vis.clear();}
    };
	// swing values
	struct SSwingValue
	{
		float						rot1;
		float						rot2;
		float						amp1;
		float						amp2;
		float						speed;
		void						lerp	(const SSwingValue& v1, const SSwingValue& v2, float factor);
	};
	SSwingValue						swing_desc[2];
	SSwingValue						swing_current; 
	float							m_time_rot_1;
	float							m_time_rot_2;
	float							m_time_pos, m_time_pos_old;
	float							m_global_time_old;
	u32								m_frame_render;
	Fvector4 wave_dir1, wave_dir2, wave_dir1_old, wave_dir2_old;

	int								dither[16][16];

	IReader*						dtFS;
	DetailHeader					dtH;
	DetailSlot*						dtSlots;		// note: pointer into VFS
	DetailSlot						DS_empty;


	int								render_key, calc_key;
#ifndef _EDITOR    
	using DetailIt = xr_vector<CDetail>::iterator;
	xr_vector<CDetail>				objects;
	xrXRC							xrc;
#else
	using DetailIt = xr_vector<CDetail*>::iterator;
	xr_vector<CDetail*>				objects;
#endif

    xr_vector<xr_vector<CacheSlot1>>cache_level1;
    xr_vector<xr_vector<Slot*>>		cache;
    xr_vector<Slot*>				cache_task;
    xr_vector<Slot>					cache_pool;

	int								cache_cx;
	int								cache_cz;

	void							UpdateVisibleM	();

#ifdef _EDITOR
	virtual ObjectList* 			GetSnapList		()=0;
#endif


	u32								hw_BatchSize;
#ifndef USE_DX11	
	ref_geom						hw_Geom;	
	ID3DVertexBuffer*				hw_VB;
	ID3DIndexBuffer*				hw_IB;
#endif
	ref_constant					hwc_consts;
	ref_constant					hwc_wave;
	ref_constant					hwc_wind;
	ref_constant					hwc_array;
	ref_constant					hwc_s_consts;
	ref_constant					hwc_s_xform;
	ref_constant					hwc_s_array;

	void							hw_Load			();
	void							hw_Load_Geom	();
	void							hw_Load_Shaders	();
	void							hw_Unload		();
	void							hw_Render		(light*L=NULL);

#ifdef USE_DX11
	xr_map<u32, ID3D11ShaderResourceView*> detailSRV_map;
	xr_map<u32, ID3D11Buffer*> detailBuffer_map;

	void							hw_Render_dump	(const Fvector4 &consts, const Fvector4 &wave, const Fvector4 &wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light*L=NULL);
#else //USE_DX11
	void							hw_Render_dump	(ref_constant array, u32 var_id, u32 lod_id, light*L=NULL);
#endif


	// get unpacked slot
	DetailSlot&						QueryDB			(int sx, int sz);
    
	void							cache_Initialize();
	void							cache_Update	(Fvector& view);
	void							cache_Task		(int gx, int gz, Slot* D);
	Slot*			cache_Query		(int sx, int sz);
	void							cache_Decompress(Slot* D);
	BOOL							cache_Validate	();
    // cache grid to world
	int								cg2w_X			(int x)			const { return cache_cx-dm_size+x;					}
	int								cg2w_Z			(int z)			const { return cache_cz-dm_size+(dm_cache_line-1-z);	}
    // world to cache grid 
	int								w2cg_X			(int x)			const { return x-cache_cx+dm_size;					}
	int								w2cg_Z			(int z)			const { return cache_cz-dm_size+(dm_cache_line-1-z);	}

	void							Load			();
	void							Unload			();
	void							Render			();

	void							cache_Alloc();
	void							cache_Free();

	CDetailManager					();
	virtual ~CDetailManager			();
};