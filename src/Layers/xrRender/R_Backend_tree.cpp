#include "stdafx.h"


#include "R_Backend_tree.h"
#include "../xrRenderDX10/dx10FixedConstants.h"

R_tree::R_tree()
{
	unmap();
}

void	R_tree::unmap		()
{
	c_m_xform_v	= nullptr;
	c_m_xform	= nullptr;
	c_consts	= nullptr;
	c_wave		= nullptr;
	c_wind		= nullptr;
	c_c_scale	= nullptr;
	c_c_bias	= nullptr;
	c_c_sun		= nullptr;

	c_consts_old = nullptr;
	c_wave_old   = nullptr;
	c_wind_old   = nullptr;
}

void	R_tree::set_m_xform_v	(Fmatrix& mat)
{
	if (c_m_xform_v) RCache.set_c(c_m_xform_v, mat);
	FixedConstants::SetTreeXformV(mat);
}

void	R_tree::set_m_xform	(Fmatrix& mat)
{
	if (c_m_xform) RCache.set_c(c_m_xform, mat);
	FixedConstants::SetTreeXform(mat);
}

void	R_tree::set_consts	(float x, float y, float z, float w)
{
	if (c_consts) RCache.set_c(c_consts, x, y, z, w);
	FixedConstants::SetTreeConsts(x,y,z,w);
}

void	R_tree::set_wave	(Fvector4& vec)
{
	if (c_wave) RCache.set_c(c_wave, vec);
	FixedConstants::SetTreeWave(vec);
}

void	R_tree::set_wind	(Fvector4& vec)
{
	if (c_wind) RCache.set_c(c_wind, vec);
	FixedConstants::SetTreeWind(vec);
}

void	R_tree::set_consts_old (float x, float y, float z, float w)
{
	if (c_consts_old) RCache.set_c(c_consts_old, x, y, z, w);
	FixedConstants::SetTreeConstsOld(x,y,z,w);
}

void	R_tree::set_wave_old (Fvector4& vec)
{
	if (c_wave_old) RCache.set_c(c_wave_old, vec);
	FixedConstants::SetTreeWaveOld(vec);
}

void	R_tree::set_wind_old (Fvector4& vec)
{
	if (c_wind_old) RCache.set_c(c_wind_old, vec);
	FixedConstants::SetTreeWindOld(vec);
}

void	R_tree::set_c_scale	(float x, float y, float z, float w)
{
	if (c_c_scale) RCache.set_c(c_c_scale, x, y, z, w);
	FixedConstants::SetTreeCScale(x,y,z,w);
}

void	R_tree::set_c_bias	(float x, float y, float z, float w)
{
	if (c_c_bias) RCache.set_c(c_c_bias, x, y, z, w);
	FixedConstants::SetTreeCBias(x,y,z,w);
}

void	R_tree::set_c_sun	(float x, float y, float z, float w)
{
	if (c_c_sun) RCache.set_c(c_c_sun, x, y, z, w);
	FixedConstants::SetTreeCSun(x,y,z,w);
}
