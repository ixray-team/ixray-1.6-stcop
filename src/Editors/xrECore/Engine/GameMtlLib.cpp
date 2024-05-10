#pragma error
//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "GameMtlLib.h"

CGameMtlLibrary GMLib;

void SGameMtl::Load(IReader& fs)
{
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

	if(fs.find_chunk(GAMEMTL_CHUNK_FLOTATION))
	    fFlotationFactor	= fs.r_float();

    if(fs.find_chunk(GAMEMTL_CHUNK_INJURIOUS))
    	fInjuriousSpeed		= fs.r_float();
}

void CGameMtlLibrary::Load()
{
	string_path			name;
	if (!FS.exist(name,	_game_data_,GAMEMTL_FILENAME)){
    	Log				("! Can't find game material file: ",name);
    	return;
    }

    R_ASSERT			(material_pairs.empty());
    R_ASSERT			(materials.empty());
    		
	IReader*	F		= FS.r_open(name);
    IReader& fs			= *F;

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_VERSION));
    u16 version			= fs.r_u16();
    if (GAMEMTL_CURRENT_VERSION!=version){
        Log				("CGameMtlLibrary: invalid version. Library can't load.");
		FS.r_close		(F);
    	return;
    }

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_AUTOINC));
    material_index		= fs.r_u32();
    material_pair_index	= fs.r_u32();

    materials.clear		();
    material_pairs.clear();

    IReader* OBJ 		= fs.open_chunk(GAMEMTLS_CHUNK_MTLS);
    if (OBJ) {
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count,O)) {
        	SGameMtl*	M = xr_new<SGameMtl> ();
	        M->Load		(*O);
        	materials.push_back(M);
        }
        OBJ->close		();
    }

    OBJ 				= fs.open_chunk(GAMEMTLS_CHUNK_MTLS_PAIR);
    if (OBJ){
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count,O)) {
        	SGameMtlPair* M	= xr_new<SGameMtlPair> (this);
	        M->Load		(*O);
        	material_pairs.push_back(M);
        }
        OBJ->close		();
    }

#ifndef _EDITOR
	material_count		= (u32)materials.size();
    material_pairs_rt.resize(material_count*material_count,0);
    for (GameMtlPairIt p_it=material_pairs.begin(); material_pairs.end() != p_it; ++p_it){
		SGameMtlPair* S	= *p_it;
    	int idx0		= GetMaterialIdx(S->mtl0)*material_count+GetMaterialIdx(S->mtl1);
    	int idx1		= GetMaterialIdx(S->mtl1)*material_count+GetMaterialIdx(S->mtl0);
	    material_pairs_rt[idx0]=S;
	    material_pairs_rt[idx1]=S;
    }
#endif

/*
	for (GameMtlPairIt p_it=material_pairs.begin(); material_pairs.end() != p_it; ++p_it){
		SGameMtlPair* S	= *p_it;
		for (int k=0; k<S->StepSounds.size(); k++){
			Msg("%40s - 0x%x", S->StepSounds[k].handle->file_name(), S->StepSounds[k].g_type);
		}
	}
*/
	FS.r_close		(F);
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
	sprintf(nm,"Pair: %s - %s",*M0->m_Name,*M1->m_Name);
	return nm;
}
#endif

void SGameMtl::Save(IWriter& fs)
{
    Flags.set(flSlowDown, !fis_zero(1.f - fFlotationFactor, EPS_L));
    Flags.set(flShootable, fis_zero(fShootFactor, EPS_L));
    Flags.set(flTransparent, fis_zero(fVisTransparencyFactor, EPS_L));
    Flags.set(flInjurious, !fis_zero(fInjuriousSpeed, EPS_L));

    fs.open_chunk(GAMEMTL_CHUNK_MAIN);
    fs.w_u32(ID);
    fs.w_stringZ(m_Name);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_DESC);
    fs.w_stringZ(m_Desc);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_FLAGS);
    fs.w_u32(Flags.get());
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_PHYSICS);
    fs.w_float(fPHFriction);
    fs.w_float(fPHDamping);
    fs.w_float(fPHSpring);
    fs.w_float(fPHBounceStartVelocity);
    fs.w_float(fPHBouncing);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_FACTORS);
    fs.w_float(fShootFactor);
    fs.w_float(fBounceDamageFactor);
    fs.w_float(fVisTransparencyFactor);
    fs.w_float(fSndOcclusionFactor);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_FACTORS_MP);
    fs.w_float(fShootFactorMP);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_FLOTATION);
    fs.w_float(fFlotationFactor);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_INJURIOUS);
    fs.w_float(fInjuriousSpeed);
    fs.close_chunk();

    fs.open_chunk(GAMEMTL_CHUNK_DENSITY);
    fs.w_float(fDensityFactor);
    fs.close_chunk();

}

IC SGameMtlPair* GetLastParentValue(SGameMtlPair* who, u32 flag)
{
    if (!who)					return 0;
    if ((GAMEMTL_NONE_ID == who->GetParent()) || (who->OwnProps.is(flag))) return who;
    else						return GetLastParentValue(who->m_Owner->GetMaterialPair(who->GetParent()), flag);
}


void SGameMtlPair::Save(IWriter& fs)
{
    fs.open_chunk(GAMEMTLPAIR_CHUNK_PAIR);
    fs.w_u32(mtl0);
    fs.w_u32(mtl1);
    fs.w_u32(ID);
    fs.w_u32(ID_parent);
    fs.w_u32(OwnProps.get());
    fs.close_chunk();

    // copy from parent
    if (ID_parent != GAMEMTL_NONE_ID) {
        SGameMtlPair* P;
        if ((0 != (P = GetLastParentValue(this, flBreakingSounds))) && (P != this))
            BreakingSounds = P->BreakingSounds;
        if ((0 != (P = GetLastParentValue(this, flStepSounds))) && (P != this))
            StepSounds = P->StepSounds;
        if ((0 != (P = GetLastParentValue(this, flCollideSounds))) && (P != this))
            CollideSounds = P->CollideSounds;
        if ((0 != (P = GetLastParentValue(this, flCollideParticles))) && (P != this))
            CollideParticles = P->CollideParticles;
        if ((0 != (P = GetLastParentValue(this, flCollideMarks))) && (P != this))
            CollideMarks = P->CollideMarks;
    }
    /*
        else{
            OwnProps.zero();
            if (!BreakingSounds.IsEmpty())	OwnProps.set(flBreakingSounds,TRUE);
            if (!StepSounds.IsEmpty())		OwnProps.set(flStepSounds,TRUE);
            if (!CollideSounds.IsEmpty())	OwnProps.set(flCollideSounds,TRUE);
            if (!CollideParticles.IsEmpty())OwnProps.set(flCollideParticles,TRUE);
            if (!CollideMarks.IsEmpty())	OwnProps.set(flCollideMarks,TRUE);
        }
    */
    // save    
    fs.open_chunk(GAMEMTLPAIR_CHUNK_BREAKING);
    fs.w_stringZ(BreakingSounds);
    fs.close_chunk();

    fs.open_chunk(GAMEMTLPAIR_CHUNK_STEP);
    fs.w_stringZ(StepSounds);
    fs.close_chunk();

    fs.open_chunk(GAMEMTLPAIR_CHUNK_COLLIDE);
    fs.w_stringZ(CollideSounds);
    fs.w_stringZ(CollideParticles);
    fs.w_stringZ(CollideMarks);
    fs.close_chunk();
}

GameMtlPairIt CGameMtlLibrary::GetMaterialPairIt(int id)
{
    for (GameMtlPairIt it = material_pairs.begin(); it != material_pairs.end(); it++)
        if ((*it)->ID == id) return it;
    return material_pairs.end();
}
SGameMtlPair* CGameMtlLibrary::GetMaterialPair(int id)
{
    GameMtlPairIt it = GetMaterialPairIt(id);
    return it != material_pairs.end() ? *it : 0;
}
GameMtlPairIt CGameMtlLibrary::GetMaterialPairIt(int mtl0, int mtl1)
{
    for (GameMtlPairIt it = material_pairs.begin(); it != material_pairs.end(); it++)
        if ((*it)->IsPair(mtl0, mtl1)) return it;
    return material_pairs.end();
}
SGameMtlPair* CGameMtlLibrary::GetMaterialPair(int mtl0, int mtl1)
{
    GameMtlPairIt it = GetMaterialPairIt(mtl0, mtl1);
    return it != material_pairs.end() ? *it : 0;
}
SGameMtlPair* CGameMtlLibrary::GetMaterialPair(LPCSTR name)
{
    if (name && name[0]) {
        int mtl0, mtl1;
        NameToMtlPair(name, mtl0, mtl1);
        GameMtlPairIt it = GetMaterialPairIt(mtl0, mtl1);
        return it != material_pairs.end() ? *it : 0;
    }
    return 0;
}

void CGameMtlLibrary::NameToMtlPair(LPCSTR name, int& mtl0, int& mtl1)
{
    string256 		buf0, buf1;
    if (_GetItemCount(name, '\\') < 2) {
        mtl0 = GAMEMTL_NONE_ID;
        mtl1 = GAMEMTL_NONE_ID;
        return;
    }
    _GetItem(name, 0, buf0, '\\');
    _GetItem(name, 1, buf1, '\\');
    _ChangeSymbol(buf0, '/', '\\');
    _ChangeSymbol(buf1, '/', '\\');
    SGameMtl* M0 = GetMaterial(buf0);	mtl0 = M0 ? M0->GetID() : GAMEMTL_NONE_ID;
    SGameMtl* M1 = GetMaterial(buf1);	mtl1 = M1 ? M1->GetID() : GAMEMTL_NONE_ID;
}