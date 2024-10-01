#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_puddles()
{
	if(!FS.exist("$level$","level.puddles") || g_pGamePersistent->Environment().wetness_factor == 0.0f)
		return;

	//Set the rendertarget and DSV
	u_setrt(rt_Generic_0, 0, 0, RDepth);

	//XFORM, init it to identity
	Fmatrix m_world, m_scale;
	m_world.set(Fidentity);

	//Read the puddles file
	string_path ini_file;
	FS.update_path(ini_file,"$level$", "level.puddles");
	CInifile ini(ini_file);
	CInifile::Root& sects = ini.sections();

	if (!sects.empty())
	{
		for(u32 i = 0; i < sects.size(); i++)
		{
			//Read params
			Fvector position;
			position.set(ini.r_fvector3(sects[i]->Name, "position")); //Position of the quad in world space
			float max_height = ini.r_float(sects[i]->Name, "max_height"); //Defines max height of the puddle (position.y + max_height * rain)
			Fvector2 size_xz = ini.r_fvector2(sects[i]->Name, "size_xz"); //Size of the quad (position.x * size.x, position.y, position.z * size.z)
			float rotation = deg2rad(ini.r_float(sects[i]->Name, "rotation")); //Rotation of the quad around Y axis. In degrees

			//Frustum culling
			if(!RImplementation.ViewBase.testSphere_dirty(position, std::max(size_xz.x, size_xz.y)))
				continue;

			//World matrix
			m_scale.scale(size_xz.x, 1.0, size_xz.y); //Set up scale matrix
			m_world.rotateY(rotation); //Rotation
			m_world.mulB_43(m_scale); //Scale
			m_world.translate_over(position); //Translation

			//Set xform, shader, and constants
			RCache.set_xform_world(m_world);
			RCache.set_Shader(s_puddles);
			RCache.set_CullMode(CULL_CCW);
			RCache.set_c("puddle_constants", g_pGamePersistent->Environment().wetness_factor * max_height);

			//Draw the quad
			RCache.Render_noIA(6);
		}
	}
}