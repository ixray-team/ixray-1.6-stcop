#pragma once

struct XRCORE_API SSurfaceData
{
    enum EFlags{
        sf2Sided		= (1<<0),
    };
    shared_str m_Name;
    shared_str m_Texture;
    shared_str m_ShaderName;
    shared_str m_ShaderXRLCName;
    shared_str m_GameMtlName = "default";
    Flags32 m_Flags{};
};

class XRCORE_API CSharedMaterialLibrary
{
    xr_hash_map<shared_str, xr_unique_ptr<SSurfaceData>> m_Data;

    CSharedMaterialLibrary();
public:
    static CSharedMaterialLibrary& Instance();

    CSharedMaterialLibrary(const CSharedMaterialLibrary&) = delete;
    CSharedMaterialLibrary(CSharedMaterialLibrary&&) = delete;
    CSharedMaterialLibrary& operator=(const CSharedMaterialLibrary&) = delete;
    CSharedMaterialLibrary& operator=(CSharedMaterialLibrary&&) = delete;
    ~CSharedMaterialLibrary() = default;

    SSurfaceData* GetData(shared_str Name);
    void MakeSharedCopy(SSurfaceData* Data);
    const auto& GetAllData() const {return m_Data;}

    void Save();
};