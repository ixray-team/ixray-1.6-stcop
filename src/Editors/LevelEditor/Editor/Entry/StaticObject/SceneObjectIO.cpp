#include "stdafx.h"


#define SCENEOBJ_CURRENT_VERSION		0x0012

#define SCENEOBJ_CHUNK_VERSION		  	0x0900
#define SCENEOBJ_CHUNK_REFERENCE     	0x0902
#define SCENEOBJ_CHUNK_PLACEMENT     	0x0904
#define SCENEOBJ_CHUNK_FLAGS			0x0905
#define SCENEOBJ_CHUNK_SURFACE			0x0906

bool CSceneObject::ResolveReference(const char* Name)
{
    if (SetReference(Name))
    {
        return true;
    }

    ELog.Msg(mtError, "CSceneObject: '%s' not found in library", Name);
    bool bRes = false;

    xr_string _new_name;
    if (Scene->GetSubstObjectName(Name, _new_name))
    {
        xr_string _message;
        _message = "Object [" + xr_string(Name) + "] not found. Relace it with [" + _new_name + "] or select other from library?";
        if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, _message.c_str()))
        {
            bRes = SetReference(_new_name.c_str());
        }
    }

    if (!bRes && !Scene->isSkipCantFindDialog())
    {
        if (ELog.DlgMsg(mtSkip, "CSceneObject: '%s' not found in library", Name) == mrSkip)
        {
            Scene->setSkipCantFindDialog(true);
        }
    }
    else if (!bRes)
    {
        Msg("! CSceneObject: '%s' not found in library", Name);
    }

    Scene->Modified();
    return bRes;
}

void CSceneObject::ApplySurfaceOverrides()
{
    for (const SSurfaceOverride& Override : PendingSurfaces)
    {
        CSurface* Surf = nullptr;
        for (CSurface* It : m_Surfaces)
        {
            if (It->m_pData.second->m_Name == Override.Name.c_str())
            {
                Surf = It;
                break;
            }
        }

        if (!Surf)
        {
            continue;
        }

        if (!Surf->IsVoid())
        {
            Surf->OnDeviceDestroy();
        }

        Surf->SetShader(Override.Shader.c_str());
        Surf->SetShaderXRLC(Override.ShaderXRLC.c_str());
        Surf->SetGameMtl(Override.GameMtl.c_str());
        Surf->SetTexture(Override.Texture.c_str());
        Surf->m_VMap = Override.VMap.c_str();
        Surf->OnDeviceCreate();
    }

    PendingSurfaces.clear();
}

void CSceneObject::CaptureSurfaceOverrides(xr_vector<SSurfaceOverride>& Dest) const
{
    if (ReferencePending)
    {
        Dest = PendingSurfaces;
        return;
    }

    Dest.clear();
    for (const CSurface* Surf : m_Surfaces)
    {
        Dest.push_back({ Surf->_Name(), Surf->_ShaderName(), Surf->_ShaderXRLCName(), Surf->_GameMtlName(), Surf->_Texture(), Surf->m_VMap.c_str() });
    }
}

bool CSceneObject::GetStreamBox(Fbox& Box)
{
    if (!ReferencePending)
    {
        return GetBox(Box);
    }

    if (!StreamBoxValid)
    {
        StreamBoxValid = true;
        if (GWorldPartition.PeekReferenceBounds(m_ReferenceName.c_str(), StreamBox))
        {
            StreamBox.xform(_Transform());
        }
        else
        {
            StreamBox.invalidate();
        }
    }

    Box = StreamBox;
    return Box.min.x <= Box.max.x;
}

bool CSceneObject::LoadReference()
{
    UpdateTransform(true);
    if (GWorldPartition.ShouldDeferReference(m_ReferenceName.c_str(), _Transform()))
    {
        ReferencePending = true;
        return true;
    }

    ReferencePending = false;
    const bool bRes = ResolveReference(m_ReferenceName.c_str());
    ApplySurfaceOverrides();
    return bRes;
}

bool CSceneObject::StreamIn()
{
	PROF_EVENT("CSceneObject::StreamIn")
    if (!ReferencePending)
    {
        return true;
    }

    ReferencePending = false;
    const bool bRes = ResolveReference(m_ReferenceName.c_str());
    ApplySurfaceOverrides();
    UpdateTransform(true);
    return bRes;
}

void CSceneObject::PinStreaming()
{
    StreamPinned = true;
    StreamIn();
}

void CSceneObject::StreamOut()
{
	PROF_EVENT("CSceneObject::StreamOut")
    if (ReferencePending || StreamPinned || !m_pReference)
    {
        return;
    }

    if (m_Flags.test(flUseSurface))
    {
        CaptureSurfaceOverrides(PendingSurfaces);
    }
    else
    {
        PendingSurfaces.clear();
    }

    for (CEditableMesh* Mesh : m_pReference->m_Meshes)
    {
        Mesh->RemoveColor(this);
    }

    for (CSurface* Surf : m_Surfaces)
    {
        Surf->OnDeviceDestroy();
        xr_delete(Surf);
    }
    m_Surfaces.clear();

    Lib.RemoveEditObject(m_pReference);
    m_pReference = nullptr;
    m_TBBox.invalidate();
    ReferencePending = true;
}

bool CSceneObject::LoadLTX(CInifile& ini, const char* sect_name)
{
    u32 version = ini.r_u32(sect_name, "version");

    CCustomObject::LoadLTX(ini, sect_name);

    m_ReferenceName = ini.r_string(sect_name, "reference_name");

    m_Flags.assign(ini.r_u32(sect_name, "flags"));

    PendingSurfaces.clear();
    if (m_Flags.test(flUseSurface))
    {
        SIniFileStream ini_stream;
        ini_stream.ini = &ini;
        ini_stream.sect = sect_name;
        ini_stream.move_begin();
        if (ini.line_exist(sect_name, ini_stream.gen_name()))
        {
            ini_stream.move_begin();
            u32 Size;
            ini_stream.r_u32(Size);
            for (u32 i = 0; i < Size; i++)
            {
                SSurfaceOverride& Override = PendingSurfaces.emplace_back();
                ini_stream.r_string(Override.Name);
                ini_stream.r_string(Override.Shader);
                ini_stream.r_string(Override.ShaderXRLC);
                ini_stream.r_string(Override.GameMtl);
                ini_stream.r_string(Override.Texture);
                ini_stream.r_string(Override.VMap);
            }
        }
    }

    const bool bRes = LoadReference();

    IsLoaded = true;

    return bRes;
}

void CSceneObject::SaveLTX(CInifile& ini, const char* sect_name)
{
	CCustomObject::SaveLTX		(ini, sect_name);

    ini.w_u32					(sect_name, "version", SCENEOBJ_CURRENT_VERSION);

    // reference object version; a streamed out object keeps its name
    ini.w_string				(sect_name, "reference_name", m_ReferenceName.c_str());

	ini.w_u32					(sect_name, "flags", m_Flags.get());
    if (m_Flags.test(flUseSurface))
    {
        xr_vector<SSurfaceOverride> Surfaces;
        CaptureSurfaceOverrides(Surfaces);

        SIniFileStream 			ini_stream;
        ini_stream.ini = &ini;
        ini_stream.sect = sect_name;
        ini_stream.move_begin();
        ini_stream.w_u32(Surfaces.size());
        for (const SSurfaceOverride& Surf : Surfaces)
        {
            ini_stream.w_stringZ(Surf.Name.c_str());
            ini_stream.w_stringZ(Surf.Shader.c_str());
            ini_stream.w_stringZ(Surf.ShaderXRLC.c_str());
            ini_stream.w_stringZ(Surf.GameMtl.c_str());
            ini_stream.w_stringZ(Surf.Texture.c_str());
            ini_stream.w_stringZ(Surf.VMap.c_str());
        }
    }
}


bool CSceneObject::LoadStream(IReader& F)
{
    u16 version = 0;
    string1024 buf;
    R_ASSERT(F.r_chunk(SCENEOBJ_CHUNK_VERSION,&version));

    if (version==0x0010)
    {
        R_ASSERT(F.find_chunk(SCENEOBJ_CHUNK_PLACEMENT));
        F.r_fvector3(FPosition);
        F.r_fvector3(FRotation);
        F.r_fvector3(FScale);
    }

    CCustomObject::LoadStream(F);

    R_ASSERT(F.find_chunk(SCENEOBJ_CHUNK_REFERENCE));
    if(version<=0x0011)
    {
        F.r_u32();
        F.r_u32();
    }
    F.r_stringZ	(buf,sizeof(buf));
    m_ReferenceName = buf;

    // flags
    if (F.find_chunk(SCENEOBJ_CHUNK_FLAGS)){
        m_Flags.assign(F.r_u32());
    }

    PendingSurfaces.clear();
    if (m_Flags.test(flUseSurface) && F.find_chunk(SCENEOBJ_CHUNK_SURFACE))
    {
        u32 Size = F.r_u32();
        for (u32 i = 0; i < Size; i++)
        {
            SSurfaceOverride& Override = PendingSurfaces.emplace_back();
            F.r_stringZ(Override.Name);
            F.r_stringZ(Override.Shader);
            F.r_stringZ(Override.ShaderXRLC);
            F.r_stringZ(Override.GameMtl);
            F.r_stringZ(Override.Texture);
            F.r_stringZ(Override.VMap);
        }
    }

    return LoadReference();
}

void CSceneObject::SaveStream(IWriter& F)
{
	CCustomObject::SaveStream(F);

	F.open_chunk	(SCENEOBJ_CHUNK_VERSION);
	F.w_u16			(SCENEOBJ_CURRENT_VERSION);
	F.close_chunk	();

    // reference object version; a streamed out object keeps its name
    F.open_chunk	(SCENEOBJ_CHUNK_REFERENCE);
    F.w_stringZ		(m_ReferenceName);
    F.close_chunk	();

    F.open_chunk	(SCENEOBJ_CHUNK_FLAGS);
	F.w_u32			(m_Flags.flags);
    F.close_chunk	();

    if (m_Flags.test(flUseSurface))
    {
        xr_vector<SSurfaceOverride> Surfaces;
        CaptureSurfaceOverrides(Surfaces);

        F.open_chunk(SCENEOBJ_CHUNK_SURFACE);

        F.w_u32(Surfaces.size());
        for (const SSurfaceOverride& Surf : Surfaces)
        {
            F.w_stringZ(Surf.Name.c_str());
            F.w_stringZ(Surf.Shader.c_str());
            F.w_stringZ(Surf.ShaderXRLC.c_str());
            F.w_stringZ(Surf.GameMtl.c_str());
            F.w_stringZ(Surf.Texture.c_str());
            F.w_stringZ(Surf.VMap.c_str());
        }
        F.close_chunk();
    }
}
