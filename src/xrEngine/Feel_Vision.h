#pragma once

#include "../xrCore/Collision/xr_collide_defs.h"
#include "Render.h"
#include "pure_relcase.h"

class IRender_Sector;
class CObject;
class ISpatial;

namespace Feel
{
	const float fuzzy_update_vis	= 1000.f;		// speed of fuzzy-logic desisions
	const float fuzzy_update_novis	= 1000.f;		// speed of fuzzy-logic desisions
	const float fuzzy_guaranteed	= 0.001f;		// distance which is supposed 100% visible
	const float lr_granularity		= 0.1f;			// assume similar positions

	class ENGINE_API Vision:
		private pure_relcase
	{
	private:
		xr_vector<CObject*>			seen;
		xr_vector<CObject*>			query;
		xr_vector<CObject*>			diff;
		collide::rq_results			RQR;
		xr_vector<ISpatialShared>	r_spatial;
		CObject*					m_owner;
		CFrustum					Frustum;
		xrSRWLock					lock_query, lock_visible;

		void						o_new		(CObject* E);
		void						o_delete	(CObject* E);
		void						o_trace		(Fvector& P, float dt, float vis_threshold);
	public:
									Vision		(CObject* owner);
		virtual					~	Vision		();
		struct	 feel_visible_Item 
		{
			collide::ray_cache	Cache;
			Fvector				cp_LP;
			Fvector				cp_LR_src;
			Fvector				cp_LR_dst;
			Fvector				cp_LAST;	// last point found to be visible
			CObject*			O;
			float				fuzzy;		// note range: (-1[no]..1[yes])
			float				Cache_vis;
			u16					bone_id;
		};
		xr_vector<feel_visible_Item>	feel_visible;
	public:
		void						feel_vision_clear		();
		void						feel_vision_query		(Fmatrix& mFull);
		void						feel_vision_update		(Fvector& P, float dt, float vis_threshold);
		void						feel_vision_relcase		(CObject* object);
		void						feel_vision_get			(xr_vector<CObject*>& R);
		Fvector						feel_vision_get_vispoint(CObject* _O);
		virtual		bool			feel_vision_isRelevant	(CObject* O)					= 0;
		virtual		float			feel_vision_mtl_transp	(CObject* O, u32 element)		= 0;	
	};
};
