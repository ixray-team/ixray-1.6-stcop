#include "Stdafx.h"
#include "SharedMaterialLibrary.h"

CSharedMaterialLibrary::CSharedMaterialLibrary()
{
	xr_stack_string_path path;
	FS.update_path(path, "$game_config$", "SharedMaterials.ltx");
	auto MaterialsIni = CInifile(path.c_str());
	for(auto& Sect : MaterialsIni.sections())
	{
		auto Data = new SSurfaceData();
		Data->m_Name = MaterialsIni.r_string(Sect.Name.c_str(), "NameRaw");
		Data->m_Texture = MaterialsIni.r_string(Sect.Name.c_str(), "Texture");
		Data->m_ShaderName = MaterialsIni.r_string(Sect.Name.c_str(), "Shader");
		Data->m_ShaderXRLCName = MaterialsIni.r_string(Sect.Name.c_str(), "ShaderXRLC");
		Data->m_GameMtlName = MaterialsIni.r_string(Sect.Name.c_str(), "GameMtl");
		Data->m_Flags.assign(MaterialsIni.r_u32(Sect.Name.c_str(), "Flags"));
		m_Data[Data->m_Name] = xr_unique_ptr<SSurfaceData>(Data);
	}
}

CSharedMaterialLibrary& CSharedMaterialLibrary::Instance()
{
	static CSharedMaterialLibrary instance;
	return instance;
}

SSurfaceData* CSharedMaterialLibrary::GetData(shared_str Name)
{
	auto Slot = m_Data.find(Name);
	if(Slot == m_Data.end())
	{
		return nullptr;
	}
	return Slot->second.get();
}

void CSharedMaterialLibrary::MakeSharedCopy(SSurfaceData* Data)
{
	if(!IVERIFY(Data) || !IVERIFY(!GetData(Data->m_Name)))
	{
		return;
	}
	auto Slot = m_Data.try_emplace(Data->m_Name, xr_make_unique<SSurfaceData>());
	IVERIFY(Slot.second);
	*(Slot.first->second) = *Data;
	Save();
}

void CSharedMaterialLibrary::Save()
{
	xr_stack_string_path path;
	FS.update_path(path, "$game_config$", "SharedMaterials.ltx");
	auto MaterialsIni = CInifile(path.c_str(), false, false);
	for(auto& elem : m_Data)
	{
		xr_stack_string256 SectName = elem.first.c_str();
		if(MaterialsIni.section_exist(SectName.c_str()))
		{
			int i = 0;
			xr_stack_string256 SectNumberName;
			do
			{
				SectNumberName = SectName;
				SectNumberName.append(std::to_string(i++));
				
			} while(MaterialsIni.section_exist(SectName.c_str()));
			SectName = SectNumberName;
		}
		MaterialsIni.w_string(SectName.c_str(), "NameRaw", elem.first.c_str());
		MaterialsIni.w_string(SectName.c_str(), "Texture", elem.second->m_Texture.c_str());
		MaterialsIni.w_string(SectName.c_str(), "Shader", elem.second->m_ShaderName.c_str());
		MaterialsIni.w_string(SectName.c_str(), "ShaderXRLC", elem.second->m_ShaderXRLCName.c_str());
		MaterialsIni.w_string(SectName.c_str(), "GameMtl", elem.second->m_GameMtlName.c_str());
		MaterialsIni.w_u32(SectName.c_str(), "Flags", elem.second->m_Flags.get());
	}
}