#include "stdafx.h"
#include "R_Backend_hemi.h"

R_hemi::R_hemi()
{
	m_tfactor.set(1, 1, 1, 1);
	unmap();
}

void R_hemi::unmap()
{
	c_pos_faces = 0;
	c_neg_faces = 0;
	c_material	= 0;
	c_lit_color = 0;
	c_lit_dir = 0;
	c_selection = 0;
	c_tfactor = 0;
}

void	R_hemi::set_pos_faces		(float posx, float posy, float posz)
{
	if (c_pos_faces) RCache.set_c(c_pos_faces, posx, posy, posz, 0);
}
void	R_hemi::set_neg_faces		(float negx, float negy, float negz)
{
	if (c_neg_faces) RCache.set_c(c_neg_faces, negx, negy, negz, 0);
}

void	R_hemi::set_material		(float x, float y, float z, float w)
{
	if (c_material) RCache.set_c(c_material, x, y, z, w);
}

void R_hemi::set_lit_color(Fvector color, Fvector dir) 
{
	RCache.xforms.m_v.transform_tiny(dir);

	if (c_lit_color) 
	{
		RCache.set_c(c_lit_color, color.x, color.y, color.z, 0);
	}

	if (c_lit_dir) 
	{
		RCache.set_c(c_lit_dir, dir.x, dir.y, dir.z, 0);
	}
}
void R_hemi::set_tfactor(float x, float y, float z, float w)
{
	m_tfactor.set(x, y, z, w);

	if (c_tfactor) 
	{
		RCache.set_c(c_tfactor, x, y, z, w);
	}
}

void R_hemi::set_tfactor(u32 tfactor)
{
	static Fcolor temp; temp.set(tfactor);
	set_tfactor(temp.r, temp.g, temp.b, temp.a);
}

void R_hemi::set_selection(float& x, float& y, float& z, float& w)
{
	m_selection.set(x, y, z, w);

	if (c_selection)
	{
		RCache.set_c(c_selection, m_selection);
	}
}

void R_hemi::set_selection(Fvector4& factor)
{
	m_selection.set(factor);

	if (c_selection)
	{
		RCache.set_c(c_selection, m_selection);
	}
}

void R_hemi::set_selection(u32 tfactor)
{
	static Fcolor temp; temp.set(tfactor);
	set_selection(temp.r, temp.g, temp.b, temp.a);
}

void R_hemi::set_c_tfactor(RHIShaderConstant* C)
{
	c_tfactor = C;

	if (C)
	{
		RCache.set_c(C, m_tfactor);
	}
}

void R_hemi::set_c_selection(RHIShaderConstant* C)
{
	c_selection = C;

	if (C)
	{
		RCache.set_c(C, m_selection);
	}
}