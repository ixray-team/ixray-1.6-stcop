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

void SGameMtl::Load(IReader& fs)
{
    R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_MAIN));
    ID = fs.r_u32();
    fs.r_stringZ(m_Name);

    if (fs.find_chunk(GAMEMTL_CHUNK_DESC))
    {
        fs.r_stringZ(m_Desc);
    }

    R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_FLAGS));
    Flags.assign(fs.r_u32());

    R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_PHYSICS));
    fPHFriction = fs.r_float();
    fPHDamping = fs.r_float();
    fPHSpring = fs.r_float();
    fPHBounceStartVelocity = fs.r_float();
    fPHBouncing = fs.r_float();

    R_ASSERT(fs.find_chunk(GAMEMTL_CHUNK_FACTORS));
    fShootFactor = fs.r_float();
    fBounceDamageFactor = fs.r_float();
    fVisTransparencyFactor = fs.r_float();
    fSndOcclusionFactor = fs.r_float();

    if (fs.find_chunk(GAMEMTL_CHUNK_FACTORS_MP))
    {
        fShootFactorMP = fs.r_float();
    }
    else
    {
        fShootFactorMP = fShootFactor;
    }

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
    }
}

static FS_FileSet NewMTLs = {};

void CGameMtlLibrary::Load(const shared_str& filename)
{
    string_path name;
    if (!FS.exist(name, _game_data_, *filename))
    {
        Msg("! Can't find game material file: %s", name);
        return;
    }

    IReader* F = FS.r_open(name);
    IReader& fs = *F;

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_VERSION));
    u16 version = fs.r_u16();

    if (GAMEMTL_CURRENT_VERSION != version)
    {
        Log("CGameMtlLibrary: invalid version. Library can't load.");
        FS.r_close(F);
        return;
    }

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_AUTOINC));
    material_index = fs.r_u32();
    material_pair_index = fs.r_u32();

    IReader* OBJ = fs.open_chunk(GAMEMTLS_CHUNK_MTLS);
    if (OBJ)
    {
        u32 count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count, O))
        {
            IReader temp = *O;
            temp.find_chunk(GAMEMTL_CHUNK_MAIN);
            temp.skip<u32>();
            shared_str read_name;
            temp.r_stringZ(read_name);
            bool need_rewrite = false;
            SGameMtl* M = nullptr;
            for (auto& mat : materials)
            {
                if (mat->m_Name == read_name)
                {
                    M = mat;
                    need_rewrite = true;
                    break;
                }
            }

            if (need_rewrite)
            {
                M->Load(*O);
            }
            else
            {
                M = new SGameMtl();
                M->Load(*O);
                materials.push_back(M);
            }
        }
        OBJ->close();
    }

    OBJ = fs.open_chunk(GAMEMTLS_CHUNK_MTLS_PAIR);
    if (OBJ)
    {
        u32 count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count, O))
        {
            IReader temp = *O;
            temp.find_chunk(GAMEMTLPAIR_CHUNK_PAIR);
            u32 mtl_0 = temp.r_u32();
            u32 mtl_1 = temp.r_u32();
            bool need_rewrite = false;
            SGameMtlPair* M = nullptr;
            for (auto& mat : material_pairs)
            {
                if (mat->GetMtl0() == mtl_0 && mat->GetMtl1() == mtl_1)
                {
                    M = mat;
                    need_rewrite = true;
                    break;
                }
            }

            if (need_rewrite)
            {
                M->Load(*O);
            }
            else
            {
                M = new SGameMtlPair(this);
                M->Load(*O);
                material_pairs.push_back(M);
            }
        }
        OBJ->close();
    }

    FS.r_close(F);
}

void CGameMtlLibrary::Load()
{
    R_ASSERT(material_pairs.empty());
    R_ASSERT(materials.empty());

    materials.clear();
    material_pairs.clear();

    Load(GAMEMTL_FILENAME);

    if (NewMTLs.empty())
    {
        FS.file_list(NewMTLs, _game_data_, FS_ListFiles, R"(gamemtl_*.xr)");
    }

    for (auto& mtl : NewMTLs)
    {
        Load(mtl.name.c_str());
    }

    material_count = (u32)materials.size();
    material_pairs_rt.resize(material_count * material_count, 0);
    for (GameMtlPairIt p_it = material_pairs.begin(); material_pairs.end() != p_it; ++p_it)
    {
        SGameMtlPair* S = *p_it;
        int idx0 = GetMaterialIdx(S->mtl0) * material_count + GetMaterialIdx(S->mtl1);
        int idx1 = GetMaterialIdx(S->mtl1) * material_count + GetMaterialIdx(S->mtl0);
        material_pairs_rt[idx0] = S;
        material_pairs_rt[idx1] = S;
    }
}

#ifdef GM_NON_GAME
SGameMtlPair::~SGameMtlPair		()
{
}                
void SGameMtlPair::Load(IReader& fs)
{
	shared_str				buf;

	R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_PAIR));
	mtl0				= fs.r_u32();
	mtl1				= fs.r_u32();
	ID					= fs.r_u32();
	ID_parent			= fs.r_u32();
    u32 own_mask		= fs.r_u32(); 
    if (GAMEMTL_NONE_ID==ID_parent) OwnProps.one	();
    else							OwnProps.assign	(own_mask);

	R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_BREAKING));
	fs.r_stringZ		(buf); 	BreakingSounds	= buf.size()?*buf:"";

	R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_STEP));
	fs.r_stringZ		(buf);	StepSounds		= buf.size()?*buf:"";

	R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_COLLIDE));
	fs.r_stringZ		(buf);	CollideSounds	= buf.size()?*buf:"";
	fs.r_stringZ		(buf);	CollideParticles= buf.size()?*buf:"";
	fs.r_stringZ		(buf);	CollideMarks	= buf.size()?*buf:"";
}
#endif

#ifdef DEBUG
LPCSTR SGameMtlPair::dbg_Name()
{
	static string256 nm;
	SGameMtl* M0 = GMLib.GetMaterialByID(GetMtl0());
	SGameMtl* M1 = GMLib.GetMaterialByID(GetMtl1());
	xr_sprintf(nm,sizeof(nm),"Pair: %s - %s",*M0->m_Name,*M1->m_Name);
	return nm;
}
#endif
