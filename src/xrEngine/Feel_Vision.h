#pragma once

#include "../xrCore/Collision/xr_collide_defs.h"
#include "Render.h"
#include "pure_relcase.h"
#include "xr_object.h"
class IRender_Sector;
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
		CObject const*				m_owner;
		CFrustum					Frustum;
		xrSRWLock					lock_query, lock_visible;
		void						o_new		(CObject* E);
		void						o_delete	(CObject* E);
		void						o_trace		(Fvector& P, float dt, float vis_threshold);
	public:
									Vision		(CObject const* owner);
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
		void						feel_vision_query		(Fmatrix& mFull,	Fvector& P);
		void						feel_vision_update		(CObject* parent,	Fvector& P, float dt, float vis_threshold);
		void				feel_vision_relcase		(CObject* object);
		void feel_vision_get(xr_vector<CObject*>& R)
		{
			R.clear();
			R.reserve(feel_visible.size());
			xrSRWLockGuard guard(&lock_visible, true);
			for (const feel_visible_Item& item : feel_visible)
			{
				if (item.O && !item.O->getDestroy() && positive(item.fuzzy))
					R.push_back(item.O);
			}
		}

		Fvector feel_vision_get_vispoint(CObject* _O)
		{
			static Fvector feel_zero_point = { 0.f,0.f,0.f };
			if (!_O || _O->getDestroy() || feel_visible.empty())
				return feel_zero_point;

			xrSRWLockGuard guard(&lock_visible, true);
			auto it = std::find_if(feel_visible.begin(), feel_visible.end(),
				[_O](const feel_visible_Item& item) {
					return _O == item.O && positive(item.fuzzy);
				});

			if (it != feel_visible.end())
			{
				return it->cp_LAST;
			}

			return feel_zero_point;
		}
		virtual		BOOL			feel_vision_isRelevant	(CObject* O)					= 0;
		virtual		float			feel_vision_mtl_transp	(CObject* O, u32 element)		= 0;	
	};
};
