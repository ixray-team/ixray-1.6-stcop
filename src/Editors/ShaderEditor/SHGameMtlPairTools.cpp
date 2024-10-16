//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "SHGameMtlPairTools.h"
#include "UI_ShaderTools.h"
#include "../xrEProps/folderlib.h"
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
CSHGameMtlPairTools::CSHGameMtlPairTools(const ISHInit& init):ISHTools(init)
{
    m_MtlPair 			= 0;
    m_GameMtlTools		= 0;
}

CSHGameMtlPairTools::~CSHGameMtlPairTools()
{
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::OnFrame()
{
	inherited::OnFrame();
}
void CSHGameMtlPairTools::OnDrawUI()
{
    if (m_MtlPair)static_cast<SGameMtlPairEditor*>(m_MtlPair)->OnDrawUI();
}
//---------------------------------------------------------------------------

bool CSHGameMtlPairTools::OnCreate()
{
	m_GameMtlTools		= STools->FindTools(aeMtl); R_ASSERT(m_GameMtlTools);
    Load();
    return true;
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::OnDestroy()
{
    m_bModified = FALSE;
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::Reload()
{
	// mtl
    ResetCurrentItem();
    // mtl pair
    m_GameMtlTools->ResetCurrentItem();
    // load
    Load();
    // mtl pair
	m_GameMtlTools->FillItemList();
    FillItemList		();
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::FillItemList()
{
	ListItemsVec items;
    for (GameMtlIt m0_it=GameMaterialLibraryEditors->FirstMaterial(); m0_it!=GameMaterialLibraryEditors->LastMaterial(); m0_it++){
        SGameMtl* M0 		= *m0_it;
	    for (GameMtlIt m1_it=GameMaterialLibraryEditors->FirstMaterial(); m1_it!=GameMaterialLibraryEditors->LastMaterial(); m1_it++){
            SGameMtl* M1 	= *m1_it;
            GameMtlPairIt p_it = GameMaterialLibraryEditors->GetMaterialPairIt(M0->GetID(),M1->GetID());
            if (p_it!=GameMaterialLibraryEditors->LastMaterialPair())
                LHelper().CreateItem(items, GameMaterialLibraryEditors->MtlPairToName(M0->GetID(),M1->GetID()),0);
        }
    }

    // Sort motions 
    std::sort(items.begin(), items.end(), [](ListItem* ItemA, ListItem* ItemB)
    {
        xr_string NameA = ItemA->Key();
        xr_string NameB = ItemB->Key();
        return NameA < NameB;
    });

	Ext.m_Items->AssignItems(items);
	m_MtlPair=0;
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::Load()
{
    m_bLockUpdate		= TRUE;

    ResetCurrentItem	();

    m_bLockUpdate		= FALSE;
}
//---------------------------------------------------------------------------

bool CSHGameMtlPairTools::Save()
{
    m_bLockUpdate		= TRUE;

    // save
    string_path 		fn;
    FS.update_path		(fn,_game_data_,GAMEMTL_FILENAME);
    EFS.MarkFile		(fn,false);
    bool bRes			= GameMaterialLibraryEditors->Save();
    m_bLockUpdate		= FALSE;
    if (bRes)			m_bModified	= FALSE;
    return bRes;
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::RealUpdateList()
{
	FillItemList			();
}
//------------------------------------------------------------------------------

void CSHGameMtlPairTools::RealUpdateProperties()
{
	PropItemVec items;

    if (m_MtlPair)
        dynamic_cast<SGameMtlPairEditor*>(m_MtlPair)->FillProp(items);

    Ext.m_ItemProps->ClearProperties();
    Ext.m_ItemProps->AssignItems(items);
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::ApplyChanges(bool bForced)
{
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::OnActivate()
{
    FillItemList();
    inherited::OnActivate		();
    m_StoreFlags				= Ext.m_Items->m_Flags.get();
    Ext.m_Items->m_Flags.assign		(0);
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::OnDeactivate()
{
    inherited::OnDeactivate		();
    Ext.m_Items->m_Flags.assign(m_StoreFlags);
}

void CSHGameMtlPairTools::SetCurrentItem(LPCSTR name, bool bView)
{
    if (m_bLockUpdate) return;
	SGameMtlPair* S=GameMaterialLibraryEditors->GetMaterialPair(name);
    // set material
	if (m_MtlPair!=S){
        m_MtlPair = S;
	    ExecCommand(COMMAND_UPDATE_PROPERTIES);
	 	if (bView) ViewSetCurrentItem(name);
   }
}
//---------------------------------------------------------------------------

void CSHGameMtlPairTools::ResetCurrentItem()
{
	m_MtlPair	= 0;
}
//---------------------------------------------------------------------------

