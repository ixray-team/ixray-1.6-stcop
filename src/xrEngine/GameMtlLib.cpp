//---------------------------------------------------------------------------
#include "stdafx.h"
#include "GameMtlLib.h"
CGameMtlLibrary GMLib;

CGameMtlLibrary::CGameMtlLibrary()
{
    material_index = 0;
    material_pair_index = 0;
    material_count = 0;
    PGMLib = &GMLib;
}

SGameMtl* CGameMtlLibrary::GetMaterialByIdx(u16 idx)
{
    if (idx >= materials.size())
    {
        if (idx != 65535)
            Msg("Material [%d] not found in library! ", (int)idx);
        return materials[0];
    }

    return materials[idx];
}

EGameMtlVersion SGameMtl::Load(IReader& fs)
{
    EGameMtlVersion vers{ GAMEMTL_VERSION_SOC };

	R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_MAIN));
	ID						= fs.r_u32();
    fs.r_stringZ			(m_Name);

    if (fs.find_chunk(GAMEMTL_CHUNK_DESC)){
		fs.r_stringZ		(m_Desc);
    }
    
	R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_FLAGS));
    Flags.assign			(fs.r_u32());

	R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_PHYSICS));
    fPHFriction				= fs.r_float();
    fPHDamping				= fs.r_float();
    fPHSpring				= fs.r_float();
    fPHBounceStartVelocity 	= fs.r_float();
    fPHBouncing				= fs.r_float();

	R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_FACTORS));
    fShootFactor			= fs.r_float();
    fBounceDamageFactor		= fs.r_float();
    fVisTransparencyFactor	= fs.r_float();
    fSndOcclusionFactor		= fs.r_float();

    if (fs.find_chunk(GAMEMTL_CHUNK_FLOTATION))
    {
        fFlotationFactor = fs.r_float();
    }

    if (fs.find_chunk(GAMEMTL_CHUNK_INJURIOUS))
    {
        fInjuriousSpeed = fs.r_float();
    }
    
    if (fs.find_chunk(GAMEMTL_CHUNK_DENSITY))
    {
        fDensityFactor = fs.r_float();
        vers = GAMEMTL_VERSION_CS;
    }
    else // St4lker0k765: for some reason, in SoC shoot factor is reversed
    {
        fShootFactor = 1.0f - fShootFactor;
    }

    if (fs.find_chunk(GAMEMTL_CHUNK_FACTORS_MP))
    {
        fShootFactorMP = fs.r_float();
        vers = GAMEMTL_VERSION_COP;
    }
    else
    {
        fShootFactorMP = fShootFactor;
    }
	
	if (fs.find_chunk(GAMEMTL_CHUNK_INJURIOUS_CALLBACK))
	{
		fs.r_stringZ(m_DangerTouchType);
	}
	return vers;
}

void CGameMtlLibrary::Load()
{
	string_path			name;
	if (!FS.exist(name,	_game_data_,GAMEMTL_FILENAME)){
        Msg("! Can't find game material file: %s",name);
    	return;
    }

    R_ASSERT			(material_pairs.empty());
    R_ASSERT			(materials.empty());

	IReader*	F		= FS.r_open(name);
    IReader& fs			= *F;

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_VERSION));
    const auto file_version = static_cast<EGameMtlVersion>(fs.r_u16());

    if (file_version != GAMEMTL_VERSION_COP)
    {
        Msg("CGameMtlLibrary: unsupported version [%u]. Library can't load.", file_version);
        FS.r_close(F);
        return;
    }

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_AUTOINC));
    material_index		= fs.r_u32();
    material_pair_index	= fs.r_u32();

    materials.clear		();
    material_pairs.clear();
    auto detected_version{ GAMEMTL_VERSION_SOC };

    IReader* OBJ 		= fs.open_chunk(GAMEMTLS_CHUNK_MTLS);
    if (OBJ) {
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count,O)) {
        	SGameMtl*	M = new SGameMtl ();
            const auto version = M->Load(*O);
            detected_version = std::max(detected_version, version);
            materials.push_back(M);
        }
        OBJ->close		();
    }
    m_version = detected_version;

    OBJ 				= fs.open_chunk(GAMEMTLS_CHUNK_MTLS_PAIR);
    if (OBJ){
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count,O)) {
        	SGameMtlPair* M	= new SGameMtlPair (this);
	        M->Load		(*O);
        	material_pairs.push_back(M);
        }
        OBJ->close		();
    }

	material_count		= (u32)materials.size();
    material_pairs_rt.resize(material_count*material_count,0);
    for (GameMtlPairIt p_it=material_pairs.begin(); material_pairs.end() != p_it; ++p_it){
		SGameMtlPair* S	= *p_it;
    	int idx0		= GetMaterialIdx(S->mtl0)*material_count+GetMaterialIdx(S->mtl1);
    	int idx1		= GetMaterialIdx(S->mtl1)*material_count+GetMaterialIdx(S->mtl0);
	    material_pairs_rt[idx0]=S;
	    material_pairs_rt[idx1]=S;
    }

	FS.r_close		(F);

	::Sound->OcclusionMaterialCallback = xr_delegate<float(u16)>(+[](u16 MtlID)->float
	{
		static bool UseMaterialOCC = EngineExternal()[EEngineExternalSound::MaterialOCC];

		if (UseMaterialOCC)
		{
			const SGameMtl* MtlIter = GMLib.GetMaterialByIdx((int)MtlID);
			return MtlIter->fSndOcclusionFactor;
		}

		return psSoundOcclusionScale;
	});
}

#ifdef DEBUG
const char* SGameMtlPair::dbg_Name()
{
	static string256 nm;
	SGameMtl* M0 = GMLib.GetMaterialByID(GetMtl0());
	SGameMtl* M1 = GMLib.GetMaterialByID(GetMtl1());
	xr_sprintf(nm,sizeof(nm),"Pair: %s - %s",*M0->m_Name,*M1->m_Name);
	return nm;
}
#endif
