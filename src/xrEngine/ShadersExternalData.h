#pragma once

class ShadersExternalData
{
public:
	Fmatrix m_script_params;
	Fvector4 hud_params;     // [zoom_rotate_factor, secondVP_zoom_factor, NULL, NULL]
	Fvector4 m_blender_mode; // x\y = [0 - default, 1 - night vision, 2 - thermo vision, ... common.h]

	ShadersExternalData()
	{
		m_script_params = Fmatrix();
		hud_params.set(0.f, 0.f, 0.f, 0.f);
		m_blender_mode.set(0.f, 0.f, 0.f, 0.f);
	}
};