#pragma once

#include "xrEngine/Shader_xrLC.h"
#include "xrCore/SharedMaterialLibrary.h"

IC void post_process_materials(const Shader_xrLC_LIB	&shaders, const xr_vector<b_shader> &shader_compile, xr_vector<b_material> &materials )
{
 	for (auto& M : materials)
	{
		if (65535==M.shader_xrlc)	{
			// No compiler shader
			M.reserved	= u16(-1);
			// clMsg	(" *  %20s",shader_render[M.shader].name);
		} else {
			// clMsg	(" *  %20s / %-20s",shader_render[M.shader].name, shader_compile[M.shader_xrlc].name);
			int id = shaders.GetID(shader_compile[M.shader_xrlc].name);
			if (id<0) {
				Msg	("ERROR: Shader '%s' not found in library",shader_compile[M.shader].name);
				R_ASSERT(id>=0);
			}
			M.reserved = u16(id);
		}
	}
}

IC void post_process_materials_shared(const Shader_xrLC_LIB	&shaders, xr_vector<b_material_shared> &materials )
{
	for (auto& M : materials)
	{
		auto Data = CSharedMaterialLibrary::Instance().GetData(M.Name);
		int id = shaders.GetID(Data->m_ShaderXRLCName.c_str());

		if (I_ASSERT_M(id>=0, "ERROR: Shader '%s' not found in library", Data->m_ShaderXRLCName.c_str())) {
			M.reserved = u16(id);
		} else
		{
			clMsg("ERROR: Shader '%s' not found in library",Data->m_ShaderXRLCName.c_str());
			M.reserved = u16(-1);
		}
	}
}
