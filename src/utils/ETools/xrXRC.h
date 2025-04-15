// xrXRC.h: interface for the xrXRC class.
//
//////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrCDB/xrCDB.h"

#ifndef ETOOLS_API
#	ifdef ETOOLS_EXPORTS
#		define ETOOLS_API __declspec( dllexport )
#	else
#		define ETOOLS_API __declspec( dllimport )
#	endif
#endif

class ETOOLS_API xrXRC
{
public:
	IC CDB::COLLIDER* collider();
	IC void ray_options(DWORD f)
	{
		collider()->ray_options(f);
	}

	IC void ray_query(const CDB::MODEL* m_def, const Fvector& r_start, const Fvector& r_dir, float r_range)
	{
		collider()->ray_query(m_def, r_start, r_dir, r_range);
	}
	IC void ray_query(const Fmatrix& inv_parent, const CDB::MODEL* m_def, const Fvector& r_start, const Fvector& r_dir, float r_range)
	{
		// transform
		Fvector S, D;
		inv_parent.transform_tiny(S, r_start);
		inv_parent.transform_dir(D, r_dir);
		ray_query(m_def, S, D, r_range);
	}

	IC void box_options(DWORD f)
	{
		collider()->box_options(f);
	}
	IC void box_query(const CDB::MODEL* m_def, const Fvector& b_center, const Fvector& b_dim)
	{
		collider()->box_query(m_def, b_center, b_dim);
	}
	IC void box_query(const Fmatrix& inv_parent, const CDB::MODEL* m_def, const Fbox& src)
	{
		Fbox dest;
		dest.xform(src, inv_parent);
		Fvector c, d;
		dest.getcenter(c);
		dest.getradius(d);
		box_query(m_def, c, d);
	}

	IC void frustum_options(DWORD f)
	{
		collider()->frustum_options(f);
	}
	IC void frustum_query(const CDB::MODEL* m_def, const CFrustum& F)
	{
		collider()->frustum_query(m_def, F);
	}

	IC CDB::RESULT* r_begin() { return collider()->r_begin(); };
	IC CDB::RESULT* r_end() { return collider()->r_end(); };
	IC void			r_free() { collider()->r_free(); }
	IC int			r_count() { return collider()->r_count(); };
	IC void			r_clear() { collider()->r_clear(); };
};

ENGINE_API extern xrXRC XRC;