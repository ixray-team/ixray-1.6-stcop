#include "stdafx.h"
#include "LightSpot.h"

void CLight_Compute_XFORM_and_VIS::compute_xf_spot(light* L)
{
	// Build EYE-space xform
	Fvector						L_dir, L_up, L_right, L_pos;
	L_dir.set(L->direction);			L_dir.normalize();

	if (L->right.square_magnitude() > EPS) {
		// use specified 'up' and 'right', just enshure ortho-normalization
		L_right.set(L->right);				L_right.normalize();
		L_up.crossproduct(L_dir, L_right);		L_up.normalize();
		L_right.crossproduct(L_up, L_dir);			L_right.normalize();
	}
	else {
		// auto find 'up' and 'right' vectors
		L_up.set(0, 1, 0);				if (_abs(L_up.dotproduct(L_dir)) > .99f)	L_up.set(0, 0, 1);
		L_right.crossproduct(L_up, L_dir);			L_right.normalize();
		L_up.crossproduct(L_dir, L_right);		L_up.normalize();
	}
	L_pos.set(L->position);

	// make N pixel border
	L->X.S.view.build_camera_dir(L_pos, L_dir, L_up);

	// _min(L->cone + deg2rad(4.5f), PI*0.98f) - Here, it is needed to enlarge the shadow map frustum to include also 
	// displaced pixels and the pixels neighbor to the examining one.
	L->X.S.project.build_projection(_min(L->cone + deg2rad(5.f), PI * 0.98f), 1.f, L->virtual_size, L->range + EPS_S);
	L->X.S.combine.mul(L->X.S.project, L->X.S.view);
}