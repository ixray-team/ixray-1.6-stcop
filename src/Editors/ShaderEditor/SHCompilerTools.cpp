//---------------------------------------------------------------------------

#include "stdafx.h"


#include "SHCompilerTools.h"
#include "ui_shadermain.h"
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
CSHCompilerTools::CSHCompilerTools(const ISHInit& init) :ISHTools(init)
{
    m_Shader = 0;
}

CSHCompilerTools::~CSHCompilerTools() 
{
}
//---------------------------------------------------------------------------

void CSHCompilerTools::OnActivate()
{
    // fill items
    FillItemList();

    //Ext.m_Items->SetOnModifiedEvent		(TOnModifiedEvent(this,&CSHCompilerTools::Modified));
    Ext.m_Items->SetOnItemCreaetEvent({this, &CSHCompilerTools::OnCreateItem});
    Ext.m_Items->SetOnItemCloneEvent({this, &CSHCompilerTools::OnCloneItem});
    Ext.m_Items->SetOnItemRenameEvent({this, &CSHCompilerTools::OnRenameItem});
    Ext.m_Items->SetOnItemRemoveEvent({this, &CSHCompilerTools::OnRemoveItem});

    inherited::OnActivate();
}
void CSHCompilerTools::OnDeactivate()
{
    inherited::OnDeactivate();
}

void CSHCompilerTools::OnFrame()
{
    inherited::OnFrame();
}
//---------------------------------------------------------------------------

bool CSHCompilerTools::OnCreate()
{
    Load();
    return true;
}

void CSHCompilerTools::OnDestroy()
{
    m_bModified = false;
}

void CSHCompilerTools::ApplyChanges(bool bForced)
{
}

void CSHCompilerTools::Reload()
{
    ResetCurrentItem();
    Load();
    FillItemList();
}

void CSHCompilerTools::Load()
{
    string_path fn;
    FS.update_path(fn, _game_data_, "shaders_xrlc.xr");

    if (FS.exist(fn))
    {
        m_Library.Load(fn);
    }
    else {
        ELog.DlgMsg(mtInformation, "Can't find file '%s'", fn);
    }
}

bool CSHCompilerTools::Save()
{
    ApplyChanges();

    // save
    string_path fn;
    FS.update_path(fn, _game_data_, "shaders_xrlc.xr");

    EFS.MarkFile(fn, false);
    bool bRes = m_Library.Save(fn);

    if (bRes) 
        m_bModified = false;

    return bRes;
}

Shader_xrLC* CSHCompilerTools::FindItem(const char* name)
{
    if (name && name[0])
    {
        return m_Library.Get(name);
    }
    else return 0;
}

void CSHCompilerTools::AppendItem(const char* path, const char* parent_name)
{
    Shader_xrLC* parent = FindItem(parent_name);
    xr_string pref = path;
    Shader_xrLC* S = m_Library.Append(parent);
    m_LastSelection = pref;
    strcpy(S->Name, m_LastSelection.c_str());
    ExecCommand(COMMAND_UPDATE_LIST);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
    Modified();

}

void CSHCompilerTools::OnRenameItem(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type)
{
    if (type == TYPE_OBJECT)
    {
        ApplyChanges();
        Shader_xrLC* S = FindItem(old_full_name); R_ASSERT(S);
        strcpy(S->Name, new_full_name);
        if (S == m_Shader) {
            ExecCommand(COMMAND_UPDATE_PROPERTIES);
            ExecCommand(COMMAND_UPDATE_LIST);

            m_LastSelection = m_Shader->Name;

            m_Shader = 0;
        }
    }
}


void CSHCompilerTools::OnRemoveItem(UIItemListForm::Node& node/*, const char* name, EItemType type*/)
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
    if (node.IsObject()) {
        R_ASSERT(name && name[0]);
        if (m_Shader && stricmp(m_Shader->Name, name) == 0)
        {
            m_Shader = 0;
            Tools->UpdateProperties(true);
        }
        m_Library.Remove(name);
    }
}

void CSHCompilerTools::SetCurrentItem(const char* name, bool bView)
{
    if (m_bLockUpdate) return;

    Shader_xrLC* S = FindItem(name);
    // load shader
    if (m_Shader != S) 
    {
        m_Shader = S;
        ExecCommand(COMMAND_UPDATE_PROPERTIES);
        if (bView)	ViewSetCurrentItem(name);
    }
}

void CSHCompilerTools::ResetCurrentItem()
{
    m_Shader = 0;
}

void CSHCompilerTools::FillItemList()
{
    ListItemsVec items;
    auto& lst = m_Library.Library();
    for (auto& elem : lst)
    {
	    LHelper().CreateItem(items, elem.Name, 0);
    }

    Ext.m_Items->AssignItems(items, nullptr);
}

void CSHCompilerTools::RealUpdateList()
{
    FillItemList();
}

void CSHCompilerTools::RealUpdateProperties()
{
    PropItemVec items;
    if (m_Shader)
    {
        Shader_xrLC& L = *m_Shader;
        PHelper().CreateFloat(items, "Translucency", &L.vert_translucency);
        PHelper().CreateFloat(items, "Ambient", &L.vert_ambient);
        PHelper().CreateFloat(items, "LM density", &L.lm_density, 0.01f, 20.f);

        PHelper().CreateFlag32(items, "Flags\\Collision", &L.m_Flags, Shader_xrLC::flCollision);
        PHelper().CreateFlag32(items, "Flags\\Rendering", &L.m_Flags, Shader_xrLC::flRendering);
        PHelper().CreateFlag32(items, "Flags\\OptimizeUV", &L.m_Flags, Shader_xrLC::flOptimizeUV);
        PHelper().CreateFlag32(items, "Flags\\Vertex light", &L.m_Flags, Shader_xrLC::flLIGHT_Vertex);
        PHelper().CreateFlag32(items, "Flags\\Cast shadow", &L.m_Flags, Shader_xrLC::flLIGHT_CastShadow);
    }

    Ext.m_ItemProps->ClearProperties();
    Ext.m_ItemProps->AssignItems(items);
    Ext.m_ItemProps->SetModifiedEvent(TOnModifiedEvent(this, &CSHCompilerTools::Modified));
}