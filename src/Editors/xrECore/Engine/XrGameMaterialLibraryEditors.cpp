//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop
#include "../Editor/UI_MainCommand.h"
#include "XrGameMaterialLibraryEditors.h"
//#include "../include/xrapi/xrapi.h"
ECORE_API XrGameMaterialLibraryEditors* GameMaterialLibraryEditors = nullptr;
void XrGameMaterialLibraryEditors::CopyMtlPairs(SGameMtl* from, SGameMtl* to)
{
    for (GameMtlIt m1_it = materials.begin(); m1_it != materials.end(); ++m1_it)
    {
        SGameMtl* M1 = *m1_it;
        SGameMtlPair* p_from = GetMaterialPair(from->GetID(), M1->GetID());
        SGameMtlPair* p_to = GetMaterialPair(to->GetID(), M1->GetID());

        if (p_from && p_to)
            static_cast<SGameMtlPairEditor*> (p_to)->CopyFrom(static_cast<SGameMtlPairEditor*> (p_from));
    }

}

BOOL XrGameMaterialLibraryEditors::UpdateMtlPairs(SGameMtl* src)
{
    BOOL bRes = FALSE;
    SGameMtl* M0 = src;
    for (GameMtlIt m1_it = materials.begin(); m1_it != materials.end(); ++m1_it)
    {
        SGameMtl* M1 = *m1_it;
        GameMtlPairIt p_it = GetMaterialPairIt(M0->GetID(), M1->GetID());
        if ((!M0->Flags.is(SGameMtlEditor::flDynamic)) && (!M1->Flags.is(SGameMtlEditor::flDynamic)))
        {
            R_ASSERT(p_it == material_pairs.end());
            continue;
        }
        else {
            if (p_it == material_pairs.end())
            {
                // create pair
                CreateMaterialPair(M0->GetID(), M1->GetID(), 0);
                bRes = TRUE;
            }
        }
    }
    return bRes;
}

BOOL XrGameMaterialLibraryEditors::UpdateMtlPairs()
{
    BOOL bRes = FALSE;
    for (GameMtlIt m0_it = materials.begin(); m0_it != materials.end(); m0_it++)
        if (UpdateMtlPairs(*m0_it)) bRes = TRUE;
    return bRes;
}


XrGameMaterialLibraryEditors::XrGameMaterialLibraryEditors()
{
}

XrGameMaterialLibraryEditors::~XrGameMaterialLibraryEditors()
{
}

SGameMtl* XrGameMaterialLibraryEditors::AppendMaterial(SGameMtl* parent)
{
    SGameMtl* M = xr_new<SGameMtlEditor>();
    if (parent)
        *M = *parent;//base params

    M->ID = material_index++;

    materials.push_back(M);
    UpdateMtlPairs(M);
    if (parent)	CopyMtlPairs(parent, M);
    return 					M;
}
void XrGameMaterialLibraryEditors::RemoveMaterial(LPCSTR name)
{
    // find material
    GameMtlIt 	rem_it = GetMaterialIt(name);
    R_ASSERT(rem_it != materials.end());
    // remove dependent pairs
    RemoveMaterialPair((*rem_it)->GetID());
    // destroy material
    xr_delete(*rem_it);
    materials.erase(rem_it);
}
//------------------------------------------------------------------------------
// material library routines
//------------------------------------------------------------------------------
LPCSTR XrGameMaterialLibraryEditors::MtlPairToName(int mtl0, int mtl1)
{
    static string512 buf;
    SGameMtl* M0 = GetMaterialByID(mtl0);	R_ASSERT(M0);
    SGameMtl* M1 = GetMaterialByID(mtl1);	R_ASSERT(M1);
    string256 buf0, buf1;
    strcpy(buf0, *M0->m_Name);	_ChangeSymbol(buf0, '\\', '/');
    strcpy(buf1, *M1->m_Name);	_ChangeSymbol(buf1, '\\', '/');
    sprintf(buf, "%s \\ %s", buf0, buf1);
    return buf;
}
void XrGameMaterialLibraryEditors::NameToMtlPair(LPCSTR name, int& mtl0, int& mtl1)
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
void XrGameMaterialLibraryEditors::MtlNameToMtlPair(LPCSTR name, int& mtl0, int& mtl1)
{
    string256 buf;
    SGameMtl* M0 = GetMaterial(_GetItem(name, 0, buf, ','));	R_ASSERT(M0); 	mtl0 = M0->GetID();
    SGameMtl* M1 = GetMaterial(_GetItem(name, 1, buf, ','));	R_ASSERT(M1);	mtl1 = M1->GetID();
}

SGameMtlPair* XrGameMaterialLibraryEditors::CreateMaterialPair(int m0, int m1, SGameMtlPair* parent)
{
    SGameMtlPairEditor* M = xr_new<SGameMtlPairEditor>(this);
    if (parent) {
        M->ID_parent = parent->ID;
        M->OwnProps.zero();
    }
    M->ID = material_pair_index++;
    M->SetPair(m0, m1);
    material_pairs.push_back(M);
    return 		M;
}
SGameMtlPair* XrGameMaterialLibraryEditors::AppendMaterialPair(int m0, int m1, SGameMtlPair* parent)
{
    SGameMtlPair* S = GetMaterialPair(m0, m1);
    if (!S) {
        return CreateMaterialPair(m0, m1, parent);
    }
    else {
        return 		S;
    }
}
void XrGameMaterialLibraryEditors::RemoveMaterialPair(LPCSTR name)
{
    int mtl0, mtl1;
    NameToMtlPair(name, mtl0, mtl1);
    RemoveMaterialPair(mtl0, mtl1);
}
void XrGameMaterialLibraryEditors::RemoveMaterialPair(GameMtlPairIt rem_it)
{
    if (rem_it == material_pairs.end()) return;
    // delete parent dependent
    for (GameMtlPairIt it = material_pairs.begin(); it != material_pairs.end(); it++)
        if ((*it)->ID_parent == (*rem_it)->ID) {
            // transfer parented props to child
           static_cast<SGameMtlPairEditor*> (*it)->TransferFromParent(static_cast<SGameMtlPairEditor*> (*rem_it));
            // reset parenting
            (*it)->ID_parent = -1;
        }
    // erase from list and remove physically
    xr_delete(*rem_it);
    material_pairs.erase(rem_it);
}
void XrGameMaterialLibraryEditors::RemoveMaterialPair(int mtl)
{
    for (int i = 0; i < (int)material_pairs.size(); i++) {
        GameMtlPairIt it = material_pairs.begin() + i;
        if (((*it)->mtl0 == mtl) || ((*it)->mtl1 == mtl)) {
            RemoveMaterialPair(it);
            i--;
        }
    }
}
void XrGameMaterialLibraryEditors::RemoveMaterialPair(int mtl0, int mtl1)
{
    GameMtlPairIt 	rem_it = GetMaterialPairIt(mtl0, mtl1);
    if (rem_it == material_pairs.end()) return;
    RemoveMaterialPair(rem_it);
}
GameMtlPairIt XrGameMaterialLibraryEditors::GetMaterialPairIt(int id)
{
    for (GameMtlPairIt it = material_pairs.begin(); it != material_pairs.end(); it++)
        if ((*it)->ID == id) return it;
    return material_pairs.end();
}
SGameMtlPairEditor* XrGameMaterialLibraryEditors::GetMaterialPair(int id)
{
    GameMtlPairIt it = GetMaterialPairIt(id);
    return static_cast<SGameMtlPairEditor*>(it != material_pairs.end() ? *it : 0);
}
GameMtlPairIt XrGameMaterialLibraryEditors::GetMaterialPairIt(u16 mtl0, u16 mtl1)
{
    for (GameMtlPairIt it = material_pairs.begin(); it != material_pairs.end(); it++)
        if ((*it)->IsPair(mtl0, mtl1)) return it;
    return material_pairs.end();
}
SGameMtlPair* XrGameMaterialLibraryEditors::GetMaterialPair(u16 mtl0, u16 mtl1)
{
    GameMtlPairIt it = GetMaterialPairIt(mtl0, mtl1);
    return it != material_pairs.end() ? *it : 0;
}
SGameMtlPairEditor* XrGameMaterialLibraryEditors::GetMaterialPair(LPCSTR name)
{
    if (name && name[0]) {
        int mtl0, mtl1;
        NameToMtlPair(name, mtl0, mtl1);
        GameMtlPairIt it = GetMaterialPairIt(mtl0, mtl1);
        return static_cast<SGameMtlPairEditor*>(it != material_pairs.end() ? *it : 0);
    }
    return 0;
}

void XrGameMaterialLibraryEditors::Load()
{
    string_path			name;
    if (!FS.exist(name, _game_data_, GAMEMTL_FILENAME)) {
        Log("! Can't find game material file: ", name);
        return;
    }

    R_ASSERT(material_pairs.empty());
    R_ASSERT(materials.empty());

    IReader* F = FS.r_open(name);
    IReader& fs = *F;

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_VERSION));
    u16 version = fs.r_u16();
    if (GAMEMTL_CURRENT_VERSION != version) {
        Log("CGameMtlLibrary: invalid version. Library can't load.");
        FS.r_close(F);
        return;
    }

    R_ASSERT(fs.find_chunk(GAMEMTLS_CHUNK_AUTOINC));
    material_index = fs.r_u32();
    material_pair_index = fs.r_u32();

    materials.clear();
    material_pairs.clear();

    IReader* OBJ = fs.open_chunk(GAMEMTLS_CHUNK_MTLS);
    if (OBJ) {
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count, O)) {
            SGameMtl* M = xr_new<SGameMtlEditor>();
            M->Load(*O);
            materials.push_back(M);
        }
        OBJ->close();
    }

    OBJ = fs.open_chunk(GAMEMTLS_CHUNK_MTLS_PAIR);
    if (OBJ) {
        u32				count;
        for (IReader* O = OBJ->open_chunk_iterator(count); O; O = OBJ->open_chunk_iterator(count, O)) {
            SGameMtlPair* M = xr_new<SGameMtlPairEditor>(this);
            M->Load(*O);
            material_pairs.push_back(M);
        }
        OBJ->close();
    }

    FS.r_close(F);
}

bool XrGameMaterialLibraryEditors::Save()
{
    R_ASSERT(FALSE == UpdateMtlPairs());
    // save
    CMemoryWriter fs;
    fs.open_chunk(GAMEMTLS_CHUNK_VERSION);
    fs.w_u16(GAMEMTL_CURRENT_VERSION);
    fs.close_chunk();

    fs.open_chunk(GAMEMTLS_CHUNK_AUTOINC);
    fs.w_u32(material_index);
    fs.w_u32(material_pair_index);
    fs.close_chunk();

    fs.open_chunk(GAMEMTLS_CHUNK_MTLS);
    int count = 0;
    for (GameMtlIt m_it = materials.begin(); m_it != materials.end(); m_it++) {
        fs.open_chunk(count++);
        (*m_it)->Save(fs);
        fs.close_chunk();
    }
    fs.close_chunk();

    fs.open_chunk(GAMEMTLS_CHUNK_MTLS_PAIR);
    count = 0;
    for (GameMtlPairIt p_it = material_pairs.begin(); p_it != material_pairs.end(); p_it++) {
        fs.open_chunk(count++);
        (*p_it)->Save(fs);
        fs.close_chunk();
    }
    fs.close_chunk();

    string_path fn;
    FS.update_path(fn, _game_data_, GAMEMTL_FILENAME);
    return fs.save_to(fn);
}
//------------------------------------------------------------------------------
// material routines
//------------------------------------------------------------------------------
void SGameMtlEditor::FillProp(PropItemVec& items, ListItem* owner)
{
    PropValue* V = 0;
    PHelper().CreateRText(items, "Desc", &m_Desc);
    // flags                                                      	
    V = PHelper().CreateFlag32(items, "Flags\\Dynamic", &Flags, flDynamic);	V->Owner()->Enable(FALSE);
    PHelper().CreateFlag32(items, "Flags\\Passable", &Flags, flPassable);
    if (Flags.is(flDynamic))
        PHelper().CreateFlag32(items, "Flags\\Breakable", &Flags, flBreakable);
    PHelper().CreateFlag32(items, "Flags\\Bounceable", &Flags, flBounceable);
    PHelper().CreateFlag32(items, "Flags\\Skidmark", &Flags, flSkidmark);
    PHelper().CreateFlag32(items, "Flags\\Bloodmark", &Flags, flBloodmark);
    PHelper().CreateFlag32(items, "Flags\\Climable", &Flags, flClimable);
    PHelper().CreateFlag32(items, "Flags\\Liquid", &Flags, flLiquid);
    PHelper().CreateFlag32(items, "Flags\\Suppress Shadows", &Flags, flSuppressShadows);
    PHelper().CreateFlag32(items, "Flags\\Suppress Wallmarks", &Flags, flSuppressWallmarks);
    PHelper().CreateFlag32(items, "Flags\\Actor Obstacle", &Flags, flActorObstacle);
    PHelper().CreateFlag32(items, "Flags\\Bullet No Ricoshet", &Flags, flNoRicoshet);
    // physics part
    PHelper().CreateFloat(items, "Physics\\Friction", &fPHFriction, 0.f, 100.f, 0.001f, 3);
    PHelper().CreateFloat(items, "Physics\\Damping", &fPHDamping, 0.001f, 100.f, 0.001f, 3);
    PHelper().CreateFloat(items, "Physics\\Spring", &fPHSpring, 0.001f, 100.f, 0.001f, 3);
    PHelper().CreateFloat(items, "Physics\\Bounce start vel", &fPHBounceStartVelocity, 0.f, 100.f, 0.01f, 2);
    PHelper().CreateFloat(items, "Physics\\Bouncing", &fPHBouncing, 0.f, 1.f, 0.001f, 3);
    // factors
    PHelper().CreateFloat(items, "Factors\\Bounce Damage", &fBounceDamageFactor, 0.f, 100.f, 0.1f, 1);
    PHelper().CreateFloat(items, "Factors\\Injurious", &fInjuriousSpeed, 0.f, 10000.f);
    PHelper().CreateFloat(items, "Factors\\Shooting (1-went through)", &fShootFactor);
    PHelper().CreateFloat(items, "Factors\\Shooting MP (1-went through)", &fShootFactorMP);
    PHelper().CreateFloat(items, "Factors\\Transparency (1-full transp)", &fVisTransparencyFactor);
    PHelper().CreateFloat(items, "Factors\\Sound occlusion (1-full hear)", &fSndOcclusionFactor);
    PHelper().CreateFloat(items, "Factors\\Flotation (1-full passable)", &fFlotationFactor);

    PHelper().CreateFloat(items, "Factors\\Density Factor", &fDensityFactor, 0.0f, 1000.0f, 1.0f, 1);
}


//------------------------------------------------------------------------------
// material pair routines
//------------------------------------------------------------------------------
void  SGameMtlPairEditor::OnFlagChange(PropValue* sender)
{
    bool bChecked = sender->Owner()->m_Flags.is(PropItem::flCBChecked);
    u32 mask = 0;
    if (sender == propBreakingSounds)			mask = flBreakingSounds;
    else if (sender == propStepSounds)		mask = flStepSounds;
    else if (sender == propCollideSounds)		mask = flCollideSounds;
    else if (sender == propCollideParticles)	mask = flCollideParticles;
    else if (sender == propCollideMarks)		mask = flCollideMarks;
    else THROW;

    OwnProps.set(mask, bChecked);
    sender->Owner()->m_Flags.set(PropItem::flDisabled, !bChecked);

    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

IC u32 SetMask(u32 mask, Flags32 flags, u32 flag)
{
    return mask ? (mask | (flags.is(flag) ? PropItem::flCBChecked : PropItem::flDisabled)) : 0;
}

IC SGameMtlPairEditor* GetLastParentValue(SGameMtlPairEditor* who, u32 flag)
{
    if (!who)					return 0;
    if ((GAMEMTL_NONE_ID == who->GetParent()) || (who->OwnProps.is(flag))) return who;
    else						return GetLastParentValue(GameMaterialLibraryEditors->GetMaterialPair(who->GetParent()), flag);
}

IC BOOL ValidateParent(SGameMtlPair* who, SGameMtlPair* parent)
{
    if (!parent)				return TRUE;
    if (who == parent)			return FALSE;
    else						return ValidateParent(who, GameMaterialLibraryEditors->GetMaterialPair(parent->GetParent()));
}

BOOL SGameMtlPairEditor::SetParent(int parent)
{
    int ID_parent_save = ID_parent;
    ID_parent = parent;

    for (GameMtlPairIt it = GameMaterialLibraryEditors->FirstMaterialPair(); it != GameMaterialLibraryEditors->LastMaterialPair(); it++) {
        if (!ValidateParent(*it, GameMaterialLibraryEditors->GetMaterialPair((*it)->GetParent()))) {
            ID_parent = ID_parent_save;
            return FALSE;
        }
    }
    // all right
    if (GAMEMTL_NONE_ID == ID_parent) {
        OwnProps.one();
    }
    else {
        OwnProps.zero();
        OwnProps.set(flBreakingSounds, BreakingSounds.size());
        OwnProps.set(flStepSounds, StepSounds.size());
        OwnProps.set(flCollideSounds, CollideSounds.size());
        OwnProps.set(flCollideParticles, CollideParticles.size());
        OwnProps.set(flCollideMarks, CollideMarks.size());
    }
    return TRUE;
}

void  SGameMtlPairEditor::FillChooseMtl(ChooseItemVec& items, void* param)
{
    for (GameMtlIt m0_it = GameMaterialLibraryEditors->FirstMaterial(); m0_it != GameMaterialLibraryEditors->LastMaterial(); m0_it++) {
        SGameMtl* M0 = *m0_it;
        for (GameMtlIt m1_it = GameMaterialLibraryEditors->FirstMaterial(); m1_it != GameMaterialLibraryEditors->LastMaterial(); m1_it++) {
            SGameMtl* M1 = *m1_it;
            GameMtlPairIt p_it = GameMaterialLibraryEditors->GetMaterialPairIt(M0->GetID(), M1->GetID());
            if (p_it != GameMaterialLibraryEditors->LastMaterialPair())
                items.push_back(SChooseItem(GameMaterialLibraryEditors->MtlPairToName(M0->GetID(), M1->GetID()), ""));
        }
    }
}

void  SGameMtlPairEditor::OnParentClick(ButtonValue* V, bool& bModif, bool& bSafe)
{
    bModif = false;
    switch (V->btn_num)
    {
    case 0:
    {
        LPCSTR MP = 0;
        SGameMtlPair* P = GameMaterialLibraryEditors->GetMaterialPair(ID_parent);
        xr_string nm = P ? GameMaterialLibraryEditors->MtlPairToName(P->GetMtl0(), P->GetMtl1()) : NONE_CAPTION;
        UIChooseForm::SelectItem(smCustom, 1, (nm == NONE_CAPTION) ? 0 : nm.c_str(), TOnChooseFillItems(this, &SGameMtlPairEditor::FillChooseMtl));
        m_EditParent = true;
    }break;
    }
}

void  SGameMtlPairEditor::OnCommandClick(ButtonValue* V, bool& bModif, bool& bSafe)
{
    bModif = false;
    switch (V->btn_num) {
    case 0: {
        SGameMtlPair* P = GameMaterialLibraryEditors->GetMaterialPair(ID_parent);
        xr_string nm = P ? GameMaterialLibraryEditors->MtlPairToName(P->GetMtl0(), P->GetMtl1()) : NONE_CAPTION;
        UIChooseForm::SelectItem(smCustom, 128, 0, TOnChooseFillItems(this, &SGameMtlPairEditor::FillChooseMtl));
        m_EditCommand = true;
    }
          break;
    }
}

void SGameMtlPairEditor::FillProp(PropItemVec& items)
{
    PropValue::TOnChange OnChange = 0;
    u32 show_CB = 0;
    SGameMtlPair* P = 0;
    if (ID_parent != GAMEMTL_NONE_ID) {
        OnChange.bind(this, &SGameMtlPairEditor::OnFlagChange);
        show_CB = PropItem::flShowCB;
        P = GameMaterialLibraryEditors->GetMaterialPair(ID_parent);
    }
    ButtonValue* B;
    B = PHelper().CreateButton(items, "Command", "Set As Parent To...", 0);
    B->OnBtnClickEvent.bind(this, &SGameMtlPairEditor::OnCommandClick);
    B = PHelper().CreateButton(items, "Parent", P ? GameMaterialLibraryEditors->MtlPairToName(P->GetMtl0(), P->GetMtl1()) : NONE_CAPTION, 0);
    B->OnBtnClickEvent.bind(this, &SGameMtlPairEditor::OnParentClick);

    propBreakingSounds = PHelper().CreateChoose(items, "Breaking Sounds", &BreakingSounds, smSoundSource, 0, 0, GAMEMTL_SUBITEM_COUNT);
    propStepSounds = PHelper().CreateChoose(items, "Step Sounds", &StepSounds, smSoundSource, 0, 0, GAMEMTL_SUBITEM_COUNT + 2);
    propCollideSounds = PHelper().CreateChoose(items, "Collide Sounds", &CollideSounds, smSoundSource, 0, 0, GAMEMTL_SUBITEM_COUNT);
    propCollideParticles = PHelper().CreateChoose(items, "Collide Particles", &CollideParticles, smParticles, 0, 0, GAMEMTL_SUBITEM_COUNT);
    propCollideMarks = PHelper().CreateChoose(items, "Collide Marks", &CollideMarks, smTexture, 0, 0, GAMEMTL_SUBITEM_COUNT);

    propBreakingSounds->Owner()->m_Flags.assign(SetMask(show_CB, OwnProps, flBreakingSounds));
    propStepSounds->Owner()->m_Flags.assign(SetMask(show_CB, OwnProps, flStepSounds));
    propCollideSounds->Owner()->m_Flags.assign(SetMask(show_CB, OwnProps, flCollideSounds));
    propCollideParticles->Owner()->m_Flags.assign(SetMask(show_CB, OwnProps, flCollideParticles));
    propCollideMarks->Owner()->m_Flags.assign(SetMask(show_CB, OwnProps, flCollideMarks));

    propBreakingSounds->OnChangeEvent = OnChange;
    propStepSounds->OnChangeEvent = OnChange;
    propCollideSounds->OnChangeEvent = OnChange;
    propCollideParticles->OnChangeEvent = OnChange;
    propCollideMarks->OnChangeEvent = OnChange;

    if (show_CB)
    {
        SGameMtlPairEditor* O;
        if (0 != (O = GetLastParentValue(this, flBreakingSounds)))	BreakingSounds = O->BreakingSounds;
        if (0 != (O = GetLastParentValue(this, flStepSounds))) 		StepSounds = O->StepSounds;
        if (0 != (O = GetLastParentValue(this, flCollideSounds))) 	CollideSounds = O->CollideSounds;
        if (0 != (O = GetLastParentValue(this, flCollideParticles))) CollideParticles = O->CollideParticles;
        if (0 != (O = GetLastParentValue(this, flCollideMarks))) 	CollideMarks = O->CollideMarks;
    }
}

void SGameMtlPairEditor::TransferFromParent(SGameMtlPairEditor* parent)
{
    R_ASSERT(parent);
    if (!OwnProps.is(flBreakingSounds))		BreakingSounds = parent->BreakingSounds;
    if (!OwnProps.is(flStepSounds))			StepSounds = parent->StepSounds;
    if (!OwnProps.is(flCollideSounds))		CollideSounds = parent->CollideSounds;
    if (!OwnProps.is(flCollideParticles))	CollideParticles = parent->CollideParticles;
    if (!OwnProps.is(flCollideMarks))		CollideMarks = parent->CollideMarks;
}

void SGameMtlPairEditor::CopyFrom(SGameMtlPairEditor* parent)
{
    R_ASSERT(parent);
    OwnProps = parent->OwnProps;
    ID_parent = parent->ID_parent;

    BreakingSounds = parent->BreakingSounds;


    StepSounds = parent->StepSounds;

    CollideSounds = parent->CollideSounds;

    CollideParticles = parent->CollideParticles;

    CollideMarks = parent->CollideMarks;
}


SGameMtlPairEditor::SGameMtlPairEditor(CGameMtlLibrary* owner):SGameMtlPair(owner)
{
    m_EditParent = false;
    m_EditCommand = false;
}


SGameMtlEditor::SGameMtlEditor()
{
}

SGameMtlEditor::~SGameMtlEditor()
{
}

//------------------------------------------------------------------------------
// IO - routines
//------------------------------------------------------------------------------
void SGameMtlEditor::Save(IWriter& fs)
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

void SGameMtlPairEditor::Load(IReader& fs)
{
    shared_str				buf;
    
    R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_PAIR));
    mtl0 = fs.r_u32();
    mtl1 = fs.r_u32();
    ID = fs.r_u32();
    ID_parent = fs.r_u32();
    OwnProps.assign(fs.r_u32());

    R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_BREAKING));
    fs.r_stringZ(buf); 	BreakingSounds = *buf;

    R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_STEP));
    fs.r_stringZ(buf);	StepSounds = *buf;

    R_ASSERT(fs.find_chunk(GAMEMTLPAIR_CHUNK_COLLIDE));
    fs.r_stringZ(buf);	CollideSounds = *buf;
    fs.r_stringZ(buf);	CollideParticles = *buf;
    fs.r_stringZ(buf);	CollideMarks = *buf;
}

void SGameMtlPairEditor::Save(IWriter& fs)
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
        SGameMtlPairEditor* P;
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

void  SGameMtlPairEditor::OnDrawUI()
{
    if (m_EditParent)
    {
        bool change = true;
        shared_str result;
        if (UIChooseForm::GetResult(change, result))
        {
            if (change)
            {
                int m0, m1;
                GameMaterialLibraryEditors->NameToMtlPair(result.c_str(), m0, m1);
                SGameMtlPair* p = GameMaterialLibraryEditors->GetMaterialPair(m0, m1); VERIFY(p);
                if (!SetParent(p->GetID()))
                {
                    ELog.DlgMsg(mtError, "Pair can't inherit from self.");
                }
                else
                {
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }

            }
            else
            {
                SetParent(GAMEMTL_NONE_ID);
                ExecCommand(COMMAND_UPDATE_PROPERTIES);
            }
            m_EditParent = false;
        }
        UIChooseForm::Update();
    }
    if (m_EditCommand)
    {
        bool change = true;
        shared_str result;
        if (UIChooseForm::GetResult(change, result))
        {
            if (change)
            {
                AStringVec lst;
                _SequenceToList(lst, result.c_str());
                for (AStringIt it = lst.begin(); it != lst.end(); it++)
                {
                    int m0, m1;
                    GameMaterialLibraryEditors->NameToMtlPair(it->c_str(), m0, m1);
                    SGameMtlPairEditor* p =static_cast<SGameMtlPairEditor*>( GameMaterialLibraryEditors->GetMaterialPair(m0, m1)); VERIFY(p);
                    if (!p->SetParent(GetID()))
                    {
                        ELog.DlgMsg(mtError, "Pair can't inherit from self.");
                    }
                }
                ExecCommand(COMMAND_UPDATE_PROPERTIES);
            }
            m_EditCommand = FALSE;
        }
        UIChooseForm::Update();
    }
}

SGameMtlPairEditor::~SGameMtlPairEditor()
{
}
