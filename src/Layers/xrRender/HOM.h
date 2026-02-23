// HOM.h: interface for the CHOM class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrEngine/IGame_Persistent.h"
#include "occRasterizer.h"
class occTri;

class CHOM  
#ifdef DEBUG_DRAW
	: public pureRender
#endif
{
private:
	xrXRC xrc;
	CDB::MODEL* m_pModel;
	CDB::Collector CL;
	xr_vector<u32> adjacency;
	xr_vector<u32> invaltids;
	xr_vector<occTri> m_pTris;
	bool bEnabled;
	Fmatrix m_xform;
	Fmatrix m_xform_01;
#ifdef DEBUG
	u32 tris_in_frame_visible;
	u32 tris_in_frame;
#endif

	volatile u32 MT_frame_rendered;

	void Render_DB(CFrustum& base);
public:
	void Load();
	void Unload();
	void Render(CFrustum& base);

	void occlude(Fbox2& space) {}
	void Disable();
	void Enable();

	void MT_RENDER();

	CDB::MODEL* GetHOMModel() { return m_pModel; }
	xr_vector<u32>* get_invaltids() { return &invaltids; }

	ICF	bool xform_b0(Fvector2& min, Fvector2& max, float& minz, Fmatrix& X, float _x, float _y, float _z)
	{
		float z = _x * X._13 + _y * X._23 + _z * X._33 + X._43;
		if (z < EPS)
		{
			return TRUE;
		}

		float iw = 1.f / (_x * X._14 + _y * X._24 + _z * X._34 + X._44);
		min.x = max.x = (_x * X._11 + _y * X._21 + _z * X._31 + X._41) * iw;
		min.y = max.y = (_x * X._12 + _y * X._22 + _z * X._32 + X._42) * iw;
		minz = 0.f + z * iw;
		return FALSE;
	}

	ICF	bool xform_b1(Fvector2& min, Fvector2& max, float& minz, Fmatrix& X, float _x, float _y, float _z)
	{
		float t;
		float z = _x * X._13 + _y * X._23 + _z * X._33 + X._43;
		if (z < EPS)
		{
			return TRUE;
		}

		float iw = 1.f / (_x * X._14 + _y * X._24 + _z * X._34 + X._44);
		t = (_x * X._11 + _y * X._21 + _z * X._31 + X._41) * iw;
		if (t < min.x) min.x = t; else if (t > max.x) max.x = t;
		t = (_x * X._12 + _y * X._22 + _z * X._32 + X._42) * iw;
		if (t < min.y) min.y = t; else if (t > max.y) max.y = t;
		t = 0.f + z * iw;

		if (t < minz)
			minz = t;
		return false;
	}

	ICF bool _visible(Fbox& B, Fmatrix& m_xform_01)
	{
		// Find min/max points of xformed-box
		Fvector2	min, max;
		float		z;
		if (xform_b0(min, max, z, m_xform_01, B.min.x, B.min.y, B.min.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.min.x, B.min.y, B.max.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.max.x, B.min.y, B.max.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.max.x, B.min.y, B.min.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.min.x, B.max.y, B.min.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.min.x, B.max.y, B.max.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.max.x, B.max.y, B.max.z)) return true;
		if (xform_b1(min, max, z, m_xform_01, B.max.x, B.max.y, B.min.z)) return true;
		return !!Raster.test(min.x, min.y, max.x, max.y, z);
	}

	ICF bool visible(Fbox3& B)
	{
		if (!bEnabled)
			return true;

		if (B.contains(Device.vCameraPosition))
			return true;

		return _visible(B, m_xform_01);
	}

	ICF bool visible(Fsphere& S)
	{
		Fbox B; B.setb(S.P, Fvector().set(S.R, S.R, S.R));
		return visible(B);
	}

	ICF bool visible(Fbox2& B, float depth)
	{
		if (!bEnabled)
			return true;

		return Raster.test(B.min.x, B.min.y, B.max.x, B.max.y, depth);
	}

	ICF bool visible(vis_data& vis)
	{
		if (Device.dwFrame < vis.hom_frame)	return true;				// not at this time :)
		if (!bEnabled)						return true;				// return - everything visible

		// Now, the test time comes
		// 0. The object was hidden, and we must prove that each frame	- test		| frame-old, tested-new, hom_res = false;
		// 1. The object was visible, but we must to re-check it		- test		| frame-new, tested-???, hom_res = true;
		// 2. New object slides into view								- delay test| frame-old, tested-old, hom_res = ???;
		u32 frame_current = Device.dwFrame;
		// u32	frame_prev		= frame_current-1;

#ifdef DEBUG
		Device.Statistic->RenderCALC_HOM.Begin();
#endif
		bool result = _visible(vis.box, m_xform_01);
		u32  delay = 1;
		if (result)
		{
			// visible	- delay next test
			delay = ::Random.randI(5 * 2, 5 * 5);
		}
		else {
			// hidden	- shedule to next frame
		}
		vis.hom_frame = frame_current + delay;
		vis.hom_tested = frame_current;
#ifdef DEBUG
		Device.Statistic->RenderCALC_HOM.End();
#endif

		return result;
	}

	ICF bool visible(sPoly& P)
	{
		if (!bEnabled)
		{
			return true;
		}

		// Find min/max points of xformed-box
		Fvector2 min, max;
		float z;

		if (xform_b0(min, max, z, m_xform_01, P.front().x, P.front().y, P.front().z))
		{
			return true;
		}

		for (u32 it = 1; it < P.size(); it++)
		{
			if (xform_b1(min, max, z, m_xform_01, P[it].x, P[it].y, P[it].z))
			{
				return true;
			}
		}

		return Raster.test(min.x, min.y, max.x, max.y, z);
	}

	CHOM();
	~CHOM();

#ifdef DEBUG_DRAW
	virtual void OnRender();
#endif
#ifdef DEBUG
	void stats();
#endif
};