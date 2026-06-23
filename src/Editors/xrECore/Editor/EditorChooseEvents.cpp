#include "stdafx.h"
#include "EditorChooseEvents.h"

namespace ChoseEvents
{
    void FillEntity(ChooseItemVec& items, void* param)
    {
        for (CInifile::Sect& sect : pSettings->sections())
        {
            str_c val;
            if (sect.line_exist("$spawn", &val))
            {
                items.emplace_back(*sect.Name, "");
            }
        }
    }
    //---------------------------------------------------------------------------
    void SelectSoundSource(SChooseItem* item, PropItemVec& info_items)
    {
        choose_snd->stop();
        choose_snd->create(item->name.c_str(), st_Effect, sg_Undefined);
        choose_snd->play(nullptr, sm_2D);
    }
    void CloseSoundSource()
    {
        choose_snd->destroy();
    }
    void FillSoundSource(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (SndLib->GetGameSounds(lst))
        {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    //---------------------------------------------------------------------------
    void FillSoundEnv(ChooseItemVec& items, void* param)
    {
        AStringVec lst;
        if (SndLib->GetSoundEnvs(lst)) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.c_str(), "");
            }
        }
    }
    //---------------------------------------------------------------------------
    void FillObject(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (Lib.GetObjects(lst)) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    void SelectObject(SChooseItem* item, PropItemVec& info_items)
    {
        EObjectThumbnail* thm = new EObjectThumbnail(*item->name);
        if (thm->Valid())
        {
            thm->FillInfo(info_items);
        }
        xr_delete(thm);
    }
    void UpdateObjectTHM(str_c name, IRHISurface*&ID)
    {
        EObjectThumbnail* thm = new EObjectThumbnail(name);
        if (thm->Valid())
        {
            thm->Update(ID);
        }
        else if (ID)
        {
            IM_TEXTURE_RELEASE(ID);
            ID = nullptr;
        }
        xr_delete(thm);
    }
    //---------------------------------------------------------------------------
    void FillGroup(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (FS.file_list(lst, _groups_, FS_ListFiles | FS_ClampExt, "*.group")) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    void SelectGroup(SChooseItem* item, PropItemVec& info_items)
    {
        EGroupThumbnail* thm = new EGroupThumbnail(*item->name);
        if (thm->Valid())
        {
            thm->FillInfo(info_items);
        }
        xr_delete(thm);
    }
    void UpdateGroupTHM(str_c name, IRHISurface*& ID)
    {
        EGroupThumbnail* thm = new EGroupThumbnail(name);
        if (thm->Valid())
        {
            thm->Update(ID);
        }
        else if(ID)
        {
            IM_TEXTURE_RELEASE(ID);
            ID = nullptr;
        }
        xr_delete(thm);
    }
    //---------------------------------------------------------------------------
    void FillVisual(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (FS.file_list(lst, _game_meshes_, FS_ListFiles | FS_ClampExt, "*.ogf")) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    void SelectVisual(SChooseItem* item, PropItemVec& info_items)
    {
    }
    //---------------------------------------------------------------------------
    void FillGameObjectMots(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (FS.file_list(lst, _game_meshes_, FS_ListFiles | FS_ClampExt, "*.omf")) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    void SelectGameObjectMots(SChooseItem* item, PropItemVec& info_items)
    {
    }
    //---------------------------------------------------------------------------
    void FillGameAnim(ChooseItemVec& items, void* param)
    {
        FS_FileSet lst;
        if (FS.file_list(lst, "$game_anims$", FS_ListFiles, "*.anm,*.anms"))
        {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }
    //---------------------------------------------------------------------------
    void FillLAnim(ChooseItemVec& items, void* param)
    {
        LAItemVec& lst = LALib.Objects();
        for (auto& elem : lst)
        {
            items.emplace_back(*elem->cName, "");
        }
    }

    void UpdateLAnim(str_c Name, IRHISurface*& Texture)
    {
        CLAItem* Item = LALib.FindItem(Name);
        if (!Item)
        {
            return;
        }

        RHITextureDesc Desc;
        Desc.Width = THUMB_WIDTH;
        Desc.Height = THUMB_HEIGHT;
        Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
        Desc.MipLevels = 1;
        Desc.ArraySize = 1;
        Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
        Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

        if (Texture)
        {
            if (Texture->GetWidth() != Desc.Width ||
                Texture->GetHeight() != Desc.Height ||
                Texture->GetFormat() != Desc.Format)
            {
                Texture->Release();
                Texture = nullptr;
            }
        }

        xr_vector<u32> Pixels(THUMB_WIDTH * THUMB_HEIGHT);

        int Frame = 0;
        for (u32 Y = 0; Y < THUMB_HEIGHT; ++Y)
        {
            for (u32 X = 0; X < THUMB_WIDTH; ++X)
            {
                u32 Color = Item->CalculateRGB(EDevice->fTimeGlobal, Frame);
                Color = subst_alpha(Color, 0xFF); // фиксируем альфу
                Pixels[Y * THUMB_WIDTH + X] = Color;
            }
        }

        // Подготовка подресурса
        RHISubResource SubResource{};
        SubResource.Width = THUMB_WIDTH;
        SubResource.Height = THUMB_HEIGHT;
        SubResource.TextureFormat = Desc.Format;
        SubResource.RowPitch = THUMB_WIDTH * 4;
        SubResource.Data = Pixels.data();

        if (!Texture)
        {
            Texture = GRHI->CreateTexture2D(Desc, SubResource);
        }
        else
        {
            RHIBox box;
            box.left = 0;
            box.top = 0;
            box.front = 0;
            box.right = THUMB_WIDTH;
            box.bottom = THUMB_HEIGHT;
            box.back = 1;

            Texture->UpdateData(0, 0, &SubResource, box);
        }
    }

    //---------------------------------------------------------------------------
    void FillEShader(ChooseItemVec& items, void* param)
    {
        for (auto& elem : EDevice->Resources->_GetBlenders())
        {
            items.emplace_back(elem.first, "");
        }
    }
    //---------------------------------------------------------------------------
    void FillCShader(ChooseItemVec& items, void* param)
    {
        for (auto& elem : EDevice->ShaderXRLC.Library())
        {
            items.emplace_back(elem.Name, "");
        }
    }
    //---------------------------------------------------------------------------
    void FillPE(ChooseItemVec& items, void* param)
    {
        for(auto& elem : ::RImplementation.PSLibrary.VecPEDs())
        {
            items.emplace_back(*elem->m_Name, "EFFECT");
        }
    }
    //---------------------------------------------------------------------------
    void FillPAC(ChooseItemVec& items, void* param)
    {
        for (auto elem : RImplementation.PSLibrary.VecPACDs())
        {
            items.emplace_back(elem->getName(), "ANIM_CURVE");
        }
    }
    //---------------------------------------------------------------------------
    void FillSharedMaterials(ChooseItemVec& items, void* param)
    {
        for (auto& elem : CSharedMaterialLibrary::Instance().GetAllData())
        {
            items.emplace_back(elem.first.c_str(), "SHARED_MATERIAL");
        }
    }
    void FillSharedMaterialsInfo(SChooseItem* item, PropItemVec& info_items)
    {
        if (item->name.size()) {
            auto Elem = CSharedMaterialLibrary::Instance().GetData(item->name);
            if(IVERIFY(Elem))
            {
                PHelper().CreateCaption(info_items, "Texture", Elem->m_Texture);
                PHelper().CreateCaption(info_items, "Game Shader", Elem->m_ShaderName);
                PHelper().CreateCaption(info_items, "Compile Shader", Elem->m_ShaderXRLCName);
                PHelper().CreateCaption(info_items, "Game Material", Elem->m_GameMtlName);
                PHelper().CreateCaption(info_items, "2 Sided", Elem->m_Flags.is(SSurfaceData::sf2Sided)?"on":"off");
            }
        }
    }
    //---------------------------------------------------------------------------
    void FillParticles(ChooseItemVec& items, void* param)
    {
        for (auto& elem : ::RImplementation.PSLibrary.VecPEDs())
        {
            items.emplace_back(*elem->m_Name, "EFFECT");
        }
        for (auto& elem : ::RImplementation.PSLibrary.VecPGDs())
        {
            items.emplace_back(*elem->m_Name, "GROUP");
        }
    }

    void SelectPE(SChooseItem* item, PropItemVec& info_items)
    {
        u32 i = 0;
        PHelper().CreateCaption(info_items, "", "used in groups");
        for (auto def : ::RImplementation.PSLibrary.VecPGDs())
        {
            for (auto& elem : def->m_Effects)
            {
                if (elem->m_EffectName == item->name)
                {
                    string64 str;
                    xr_sprintf(str, sizeof(str), "%d", ++i);
                    PHelper().CreateCaption(info_items, str, def->m_Name);
                }
            }
        }
    }

    void SelectPG(SChooseItem* item, PropItemVec& info_items)
    {
        u32 i = 0;
        PHelper().CreateCaption(info_items, "", "using effects");
        for (const auto def : ::RImplementation.PSLibrary.VecPGDs())
        {
            if (def->m_Name == item->name)
            {
                for (auto& elem : def->m_Effects)
                {
                    string64 str;
                    xr_sprintf(str, sizeof(str), "%d", ++i);
                    PHelper().CreateCaption(info_items, str, elem->m_EffectName);
                }
                break;
            }
        }
    }

    //---------------------------------------------------------------------------
    void FillTexture(ChooseItemVec& items, void* param)
    {
        FS_FileSet	lst;
        if (ImageLib.GetTextures(lst)) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }

    void UpdateTextureTHM(str_c name, IRHISurface*&Texture)
    {
        if (name && name[0]) {
            ETextureThumbnail* thm = new ETextureThumbnail(name);
            if (thm->Valid())
            {
                thm->Update(Texture);
            }
            xr_delete(thm);
        }
    }

    //---------------------------------------------------------------------------
    void FillTextureRaw(ChooseItemVec& items, void* param)
    {
        FS_FileSet	lst;
        if (ImageLib.GetTexturesRaw(lst)) {
            for (auto& elem : lst)
            {
                items.emplace_back(elem.name.c_str(), "");
            }
        }
    }

    void UpdateTextureTHMRaw(str_c name, IRHISurface*& ID)
    {
        if (name && name[0]) {
            ETextureThumbnail* thm = new ETextureThumbnail(name);
            if (thm->Valid())
            {
                thm->Update(ID);
            }
            xr_delete(thm);
        }
    }

    void SelectTexture(SChooseItem* item, PropItemVec& info_items)
    {
        if (item->name.size()) {
            ETextureThumbnail* thm = new ETextureThumbnail(*item->name);
            if (thm->Valid())
            {
                thm->FillInfo(info_items);
            }
            xr_delete(thm);
        }
    }
    void SelectTextureRaw(SChooseItem* item, PropItemVec& info_items)
    {
        if (item->name.size()) {
            ETextureThumbnail* thm = new ETextureThumbnail(*item->name);
            if (thm->Valid())
            {
                thm->FillInfo(info_items);
            }
            xr_delete(thm);
        }
    }
    //---------------------------------------------------------------------------
    void FillGameMaterial(ChooseItemVec& items, void* param)
    {
        for (auto& elem : PGMLib->GetMaterials())
        {
            items.emplace_back(*elem->m_Name, "");
        }
    }
    //---------------------------------------------------------------------------

    void FillSkeletonAnims(ChooseItemVec& items, void* param)
    {
        IRenderVisual* V = ::Render->model_Create((str_c)param);
        if (PKinematicsAnimated(V)) {
            u32 cnt = PKinematicsAnimated(V)->LL_MotionsSlotCount();
            for (u32 k = 0; k < cnt; k++) {
                for (auto& Motion : *PKinematicsAnimated(V)->LL_Motions(k)) {
                    bool bFound = false;
                    for (auto& item : items)
                    {
                        if (item.name == Motion.first)
                        {
                            bFound = true;
                            break;
                        }
                    }
                    if (!bFound)
                    {
                        items.emplace_back(*Motion.first, "");
                    }
                }
            }
        }
        ::Render->model_Delete(V);
    }

    void FillSkeletonBones(ChooseItemVec& items, void* param)
    {
        IRenderVisual* V = ::Render->model_Create((str_c)param);
        if (PKinematics(V))
        {
            for (auto& elem : *PKinematics(V)->LL_Bones())
            {
                items.emplace_back(*elem.first, "");
            }
        }
        ::Render->model_Delete(V);
    }

    void FillSkeletonBonesObject(ChooseItemVec& items, void* param)
    {
        CEditableObject* eo = (CEditableObject*)param;
        for (auto elem : eo->Bones())
        {
            items.emplace_back(elem->Name().c_str(), "");
        }
    }

}//namespace

void FillChooseEvents()
{
    UIChooseForm::AppendEvents(smDisabled, "Just Preview", nullptr, nullptr, nullptr, nullptr, SChooseEvents::flDisabled);
    UIChooseForm::AppendEvents(smSoundSource, "Select Sound Source", ChoseEvents::FillSoundSource, ChoseEvents::SelectSoundSource, nullptr, ChoseEvents::CloseSoundSource, 0);
    UIChooseForm::AppendEvents(smSoundEnv, "Select Sound Environment", ChoseEvents::FillSoundEnv, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smObject, "Select Library Object", ChoseEvents::FillObject, ChoseEvents::SelectObject, ChoseEvents::UpdateObjectTHM, nullptr, 0);
    UIChooseForm::AppendEvents(smGroup, "Select Group", ChoseEvents::FillGroup, ChoseEvents::SelectGroup, ChoseEvents::UpdateGroupTHM, nullptr, 0);
    UIChooseForm::AppendEvents(smEShader, "Select Engine Shader", ChoseEvents::FillEShader, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smCShader, "Select Compiler Shader", ChoseEvents::FillCShader, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smPE, "Select Particle Effect", ChoseEvents::FillPE, nullptr/*ChoseEvents::SelectPE*/, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smParticles, "Select Particle System", ChoseEvents::FillParticles, nullptr/*ChoseEvents::SelectPG*/, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smTextureRaw, "Select Source Texture", ChoseEvents::FillTextureRaw, ChoseEvents::SelectTextureRaw, ChoseEvents::UpdateTextureTHMRaw, nullptr, SChooseEvents::flClearTexture);
    UIChooseForm::AppendEvents(smTexture, "Select Texture", ChoseEvents::FillTexture, ChoseEvents::SelectTexture, ChoseEvents::UpdateTextureTHM, nullptr, SChooseEvents::flClearTexture);
    UIChooseForm::AppendEvents(smEntityType, "Select Entity", ChoseEvents::FillEntity, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smLAnim, "Select Light Animation", ChoseEvents::FillLAnim, nullptr, ChoseEvents::UpdateLAnim, nullptr, SChooseEvents::flAnimated);
    UIChooseForm::AppendEvents(smVisual, "Select Visual", ChoseEvents::FillVisual, ChoseEvents::SelectVisual, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smSkeletonAnims, "Select Skeleton Animation", ChoseEvents::FillSkeletonAnims, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smSkeletonBones, "Select Skeleton Bones", ChoseEvents::FillSkeletonBones, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smSkeletonBonesInObject, "Select Skeleton Bones", ChoseEvents::FillSkeletonBonesObject, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smGameMaterial, "Select Game Material", ChoseEvents::FillGameMaterial, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smGameAnim, "Select Animation", ChoseEvents::FillGameAnim, nullptr, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smGameSMotions, "Select Game Object Motions", ChoseEvents::FillGameObjectMots, ChoseEvents::SelectGameObjectMots, nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smPAC, "Select Animation Curve", ChoseEvents::FillPAC, nullptr,nullptr, nullptr, 0);
    UIChooseForm::AppendEvents(smSharedMaterial, "Select Shared Material", ChoseEvents::FillSharedMaterials, ChoseEvents::FillSharedMaterialsInfo, nullptr, nullptr, SChooseEvents::flClearTexture);
    choose_snd = new ref_sound();
}

void ClearChooseEvents()
{
	UIChooseForm::ClearEvents	();
    xr_delete					(choose_snd);
}
