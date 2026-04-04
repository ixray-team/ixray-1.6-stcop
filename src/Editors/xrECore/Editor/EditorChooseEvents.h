#pragma once

#include "SoundManager.h"
#include "Library.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/LightAnimLibrary.h"
#include "../../Layers/xrRender/SkeletonAnimated.h"
#include "../../Layers/xrRender/ResourceManager.h"
#include "../../Layers/xrRender/ParticleEffect.h"
#include "../../Layers/xrRender/ParticleGroup.h"
#include "../../Layers/xrRender/ParticleAnimCurve.h"
#include "../../xrEngine/defines.h"
#include "EditObject.h"
inline ref_sound* choose_snd;

namespace ChoseEvents
{
    void FillEntity(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void SelectSoundSource(SChooseItem* item, PropItemVec& info_items);
    void CloseSoundSource();
    void FillSoundSource(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillSoundEnv(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillObject(ChooseItemVec& items, void* param);
    void SelectObject(SChooseItem* item, PropItemVec& info_items);
    void UpdateObjectTHM(LPCSTR name, IRHISurface*&ID);
    //---------------------------------------------------------------------------
    void FillGroup(ChooseItemVec& items, void* param);
    void SelectGroup(SChooseItem* item, PropItemVec& info_items);
    void UpdateGroupTHM(LPCSTR name, IRHISurface*& ID);
    //---------------------------------------------------------------------------
    void FillVisual(ChooseItemVec& items, void* param);
    void SelectVisual(SChooseItem* item, PropItemVec& info_items);
    //---------------------------------------------------------------------------
    void FillGameObjectMots(ChooseItemVec& items, void* param);
    void SelectGameObjectMots(SChooseItem* item, PropItemVec& info_items);
    //---------------------------------------------------------------------------
    void FillGameAnim(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillLAnim(ChooseItemVec& items, void* param);
    void UpdateLAnim(LPCSTR Name, IRHISurface*& Texture);
    //---------------------------------------------------------------------------
    void FillEShader(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillCShader(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillPE(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillPAC(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillSharedMaterials(ChooseItemVec& items, void* param);
    void FillSharedMaterialsInfo(SChooseItem* item, PropItemVec& info_items);
    //---------------------------------------------------------------------------
    void FillParticles(ChooseItemVec& items, void* param);
    void SelectPE(SChooseItem* item, PropItemVec& info_items);
    void SelectPG(SChooseItem* item, PropItemVec& info_items);
    //---------------------------------------------------------------------------
    void FillTexture(ChooseItemVec& items, void* param);
    void UpdateTextureTHM(const char* name, IRHISurface*&Texture);
    //---------------------------------------------------------------------------
    void FillTextureRaw(ChooseItemVec& items, void* param);
    void UpdateTextureTHMRaw(const char* name, IRHISurface*& ID);
    void SelectTexture(SChooseItem* item, PropItemVec& info_items);
    void SelectTextureRaw(SChooseItem* item, PropItemVec& info_items);
    //---------------------------------------------------------------------------
    void FillGameMaterial(ChooseItemVec& items, void* param);
    //---------------------------------------------------------------------------
    void FillSkeletonAnims(ChooseItemVec& items, void* param);
    void FillSkeletonBones(ChooseItemVec& items, void* param);
    void FillSkeletonBonesObject(ChooseItemVec& items, void* param);
}//namespace

ECORE_API void FillChooseEvents();
ECORE_API void ClearChooseEvents();

//---------------------------------------------------------------------------
