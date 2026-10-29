//---------------------------------------------------------------------------
#include "stdafx.h"

#include "SHGameMtlTools.h"
#include "../../Public/PropertiesListHelper.h"
#include "FolderLib.h"
#include "UI_shadertools.h"
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
CSHGameMtlTools::CSHGameMtlTools(const ISHInit& init):ISHTools(init)
{
    m_CreatingMtl = false;
    m_Mtl 				= 0;
    m_GameMtlPairTools	= 0;
}

CSHGameMtlTools::~CSHGameMtlTools()
{
}
//---------------------------------------------------------------------------

void CSHGameMtlTools::OnActivate()
{
    // fill items
    FillItemList		();

  /*  Ext.m_Items->SetOnModifiedEvent		(fastdelegate::bind<TOnModifiedEvent>(this,&CSHGameMtlTools::Modified));*/
    Ext.m_Items->SetOnItemCloneEvent    ({this,  &CSHGameMtlTools::OnCloneItem});
    Ext.m_Items->SetOnItemCreaetEvent   ({this,&CSHGameMtlTools::OnCreateItem});
    Ext.m_Items->SetOnItemRenameEvent	({this,&CSHGameMtlTools::OnRenameItem});
    Ext.m_Items->SetOnItemRemoveEvent	({this,&CSHGameMtlTools::OnRemoveItem});
    inherited::OnActivate		();
}

void CSHGameMtlTools::OnDeactivate()
{
    inherited::OnDeactivate		();
}

void CSHGameMtlTools::OnFrame()
{
	inherited::OnFrame();
    if (m_CreatingMtl)
    {
        bool change = true;
        shared_str result;
        if (UIChooseForm::GetResult(change, result))
        {
            if (change)
            {
                AppendItem(m_CreatingMtlPath.c_str(),stricmp( result.c_str(),"dynamic")==0);
            }
            m_CreatingMtl = false;
        }

    }
}
void CSHGameMtlTools::OnDrawUI()
{
    if (m_CreatingMtl )
    {
        UIChooseForm::Update();
    }
}
//---------------------------------------------------------------------------

bool CSHGameMtlTools::OnCreate()
{
	m_GameMtlPairTools	= STools->FindTools(aeMtlPair); R_ASSERT(m_GameMtlPairTools);
    // The Level Editor loads the library at startup and the scene holds its materials
    if (GameMaterialLibraryEditors->CountMaterial() == 0)
    {
        Load();
    }
    return true;
}

void CSHGameMtlTools::OnDestroy()
{
    m_bModified = false;
}

void CSHGameMtlTools::Reload()
{
	// mtl
    ResetCurrentItem();
    UndoClear();
    // mtl pair
    m_GameMtlPairTools->ResetCurrentItem();
    m_GameMtlPairTools->UndoClear();
    // load
    Load();
    FillItemList		();
}

void CSHGameMtlTools::FillItemList()
{

	ListItemsVec items;
    for (GameMtlIt m_it= GameMaterialLibraryEditors->FirstMaterial(); m_it!= GameMaterialLibraryEditors->LastMaterial(); m_it++)
        LHelper().CreateItem(items,*(*m_it)->m_Name,0);
    // assign items
	Ext.m_Items->AssignItems(items,0,false);
}

void CSHGameMtlTools::Load()
{
    m_bLockUpdate		= true;

    GameMaterialLibraryEditors->Unload	();
    GameMaterialLibraryEditors->Load		();
    ResetCurrentItem();

	m_bLockUpdate		= false;
}

bool CSHGameMtlTools::Save()
{
	ResetCurrentItem	();
    m_bLockUpdate		= true;

    // save
    string_path 		fn;
    FS.update_path		(fn,_game_data_,GAMEMTL_FILENAME);
    EFS.MarkFile		(fn,false);
    bool bRes			= GameMaterialLibraryEditors->Save();
    
	m_bLockUpdate		= false;

    if (bRes) 			m_bModified	= false;
    return bRes;
}

SGameMtl* CSHGameMtlTools::FindItem(const char* name)
{
	if (name && name[0]){
    	return GameMaterialLibraryEditors->GetMaterial(name);
    }else return 0;
}

void CSHGameMtlTools::FillChooseMtlType(ChooseItemVec& items, void* param)
{
    items.push_back(SChooseItem("Dynamic",	"Dynamic material"));
    items.push_back(SChooseItem("Static",	"Static material"));
}

void CSHGameMtlTools::AppendItem(const char* path, const char* parent_name)
{
	SGameMtl* parent 	= FindItem(parent_name);
    if (!parent)
    {
        UIChooseForm::SelectItem(smCustom, 1, 0, TOnChooseFillItems(this, &CSHGameMtlTools::FillChooseMtlType), 0, 0, 0, 0);
        m_CreatingMtl = true;
        m_CreatingMtlPath = path;
    }
    else
    {
        AppendItem(path, false, parent);
    }
   
}

void CSHGameMtlTools::AppendItem(const char* path, bool dynamic, SGameMtl* parent )
{
    SGameMtl* S = GameMaterialLibraryEditors->AppendMaterial(parent);
    m_LastSelection = path;
    S->m_Name = m_LastSelection.c_str();
    if (!parent)		S->Flags.set(SGameMtl::flDynamic, dynamic);
    STools->UpdateList();
    STools->UpdateProperties();
    Modified();
}

void CSHGameMtlTools::OnRenameItem(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type)
{
	if (type==TYPE_OBJECT){
        SGameMtl* S = FindItem(old_full_name); R_ASSERT(S);
        S->m_Name		= new_full_name;
        if (S==m_Mtl){
            STools->UpdateProperties	();
            STools->UpdateList	();
            m_LastSelection = m_Mtl->m_Name.c_str();

            m_Mtl = 0;
        }

       
    }
}

void CSHGameMtlTools::OnRemoveItem(UIItemListForm::Node& node/*, const char* name, EItemType type*/)
{
    string_path path;
    if (node.Path.size())
    {
        xr_strconcat(path, node.Path.c_str(), "\\", node.Name.c_str());
    } else
    {
        xr_strcpy(path, node.Name.c_str());
    }
    const char* name = path;
	if (node.IsObject()){
        R_ASSERT(name && name[0]);
        if (m_Mtl&&m_Mtl->m_Name == name)
        {
            m_Mtl = 0;
            STools->UpdateProperties(true);
        }
        GameMaterialLibraryEditors->RemoveMaterial(name);
    }
}

void CSHGameMtlTools::SetCurrentItem(const char* name, bool bView)
{
    if (m_bLockUpdate) return;

	SGameMtl* S = FindItem(name);
    // load material
	if (m_Mtl!=S){
        m_Mtl = S;
        UndoTrack();
	    STools->UpdateProperties();
	 	if (bView) ViewSetCurrentItem(name);
   	}
}

void CSHGameMtlTools::ResetCurrentItem()
{
	m_Mtl=0;
}

bool CSHGameMtlTools::SaveUndoItem(shared_str& Name, CMemoryWriter& Writer)
{
    if (m_Mtl == nullptr)
    {
        return false;
    }

    Name = m_Mtl->m_Name;
    m_Mtl->Save(Writer);
    return true;
}

bool CSHGameMtlTools::LoadUndoItem(const shared_str& Name, IReader& Reader)
{
    SGameMtl* S = FindItem(Name.c_str());
    if (S == nullptr)
    {
        return false;
    }

    S->Load(Reader);
    SetCurrentItem(Name.c_str(), true);
    return true;
}
//---------------------------------------------------------------------------

void CSHGameMtlTools::RealUpdateList()
{
	FillItemList			();
}
//------------------------------------------------------------------------------

void CSHGameMtlTools::RealUpdateProperties()
{
	PropItemVec items;
    if (m_Mtl)
    	static_cast<SGameMtlEditor*>( m_Mtl)->FillProp	(items,m_CurrentItem);

    Ext.m_ItemProps->ClearProperties();
    Ext.m_ItemProps->AssignItems		(items);
    Ext.m_ItemProps->SetModifiedEvent	(TOnModifiedEvent(this,&CSHGameMtlTools::Modified));
}
//---------------------------------------------------------------------------

void CSHGameMtlTools::ApplyChanges(bool bForced)
{
}
//---------------------------------------------------------------------------


