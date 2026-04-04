#include "stdafx.h"
#include "UISharedMaterialsLibrary.h"
#include "imgui_internal.h"

UISharedMaterialsLibrary* UISharedMaterialsLibrary::Form = nullptr;

UISharedMaterialsLibrary::UISharedMaterialsLibrary()
{
    m_ObjectList = new UIItemListForm();
    InitObjects();
    m_ObjectList->SetOnItemFocusedEvent({this, &UISharedMaterialsLibrary::OnItemFocused});
    m_ObjectList->SetOnItemUnfocusedEvent({this, &UISharedMaterialsLibrary::OnItemUnfocused});
    m_ObjectList->m_Flags.set(UIItemListForm::fMultiSelect, true);
    
    PreviewProps = new UIPropertiesForm();
    PreviewProps->DisableSearch(true);
    
    SearchList.SetOnItemFocusedEvent({this, &UISharedMaterialsLibrary::OnItemFocused});
    SearchList.SetOnItemUnfocusedEvent({this, &UISharedMaterialsLibrary::OnItemUnfocused});
}

UISharedMaterialsLibrary::~UISharedMaterialsLibrary()
{
    xr_delete(PreviewProps);
    xr_delete(m_ObjectList);
    //xr_delete(InternalProps);
}

void UISharedMaterialsLibrary::Update()
{
    if (!Form)
    {
        return;
    }

    if (!Form->IsClosed())
    {
        Form->Draw();
    }
    else
    {
        Close();
    }
}

void UISharedMaterialsLibrary::Show()
{
    //UI->BeginEState(esEditLibrary);

    bool NeedToPush = false;

    if (!Form)
    {
        Form = new UISharedMaterialsLibrary();
        NeedToPush = true;
    }
    else
    {
        NeedToPush = !Form->bOpen;
        Form->bOpen = true;
    }

    if (NeedToPush)
    {
        Form->InitObjects();
        UI->Push(Form, false);
        //modif_map.clear();
    }
}

void UISharedMaterialsLibrary::Close()
{
	Form->bOpen = false;
}

void UISharedMaterialsLibrary::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(550, 650));

    if (!ImGui::Begin("Shared Material Library", &bOpen))
    {
        ImGui::PopStyleVar(1);
        ImGui::End();
        return;
    }

    {
	    ImGui::BeginGroup();

	    if (ImGui::BeginChild("Left", ImVec2(-220, -ImGui::GetFrameHeight() - 4), true))
	    {
	        DrawObjects();
	    }

	    ImGui::EndChild();
	    ImGui::SetNextItemWidth(-200);
		ImGui::Text(" Items count: %u", ActualItemList().m_Items.size());
	    ImGui::EndGroup();
    }

    ImGui::SameLine();
    DrawRightBar();

    ImGui::PopStyleVar(1);
    ImGui::End();
}

void UISharedMaterialsLibrary::DrawObjects()
{
    ImGui::BeginChild("Object List");

    // Поиск
    string256 buf;
    xr_strcpy(buf, SearchQuery.c_str());
    ImGui::SetNextItemWidth(-1);
    if (ImGui::InputTextWithHint("##value", "Search...", buf, sizeof(buf)))
    {
        SearchQuery = buf;

        if (SearchQuery.empty())
        {
            SearchList.ClearList();
        }
        else
        {
            ListItemsVec Filtered;
            for (ListItem* Item : m_ObjectList->GetItems())
            {
                if (strstr(Item->Key(), SearchQuery.c_str()))
                {
                    ListItem* CopyItem = new ListItem(Item->Type());
                    *CopyItem = *Item;
                    Filtered.push_back(CopyItem);
                }
            }
            SearchList.AssignItems(Filtered, nullptr, true, false);
        }
    }

    if (GUIManager->SearchIcon)
    {
        ImVec2 IconSize = { 12,12 };

        ImGui::SameLine();
        ImVec2 cursorPos = ImGui::GetCursorPos();
        ImGui::SetCursorPos(ImVec2(cursorPos.x - IconSize.x - 10.f, 1 + cursorPos.y + (IconSize.y / 4)));

        ImGui::Image(GUIManager->SearchIcon, IconSize);
    }
    ImGui::Separator();

    ActualItemList().Draw();

    ImGui::Separator();
    ImGui::EndChild();

    if (ImGui::IsItemHovered())
    {
        ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
}

void UISharedMaterialsLibrary::InitObjects()
{
    m_ObjectList->ClearList();
    ListItemsVec items;
    for(auto& elem : CSharedMaterialLibrary::Instance().GetAllData())
    {
        LHelper().CreateItem(items, elem.first.c_str(), 0, ListItem::flDrawThumbnail, nullptr);
    }
    m_ObjectList->AssignItems(items);
}

void UISharedMaterialsLibrary::DrawRightBar()
{
    if (ImGui::BeginChild("Right", ImVec2(0, 0)))
    {
        ImGui::Image(m_RealTexture ? m_RealTexture->get_SRView()->GetRawSRV() : EDevice->texture_null->get_SRView()->GetRawSRV(), ImVec2(200, 200));

        PreviewProps->Draw();

        if(ImGui::Button("Save", ImVec2(-1, 0)))
        {
            CSharedMaterialLibrary::Instance().Save();
        }
        if(ImGui::Button("Reload", ImVec2(-1, 0)))
        {
            InitObjects();
        }
    }
    ImGui::EndChild();
}

void UISharedMaterialsLibrary::OnItemFocused(ListItem* item)
{
    m_RealTexture = nullptr;
    PreviewProps->ClearProperties();
    m_Current = nullptr;

    if (item)
    {
        PropItemVec Info;

        m_Current = item->Key();

        auto Data = CSharedMaterialLibrary::Instance().GetData(m_Current);
        if(IVERIFY(Data))
        {
            PHelper().CreateChoose(Info, "Texture", &Data->m_Texture, smTexture);
            PHelper().CreateChoose(Info, "Game Shader", &Data->m_ShaderName, smEShader);
            PHelper().CreateChoose(Info, "Compile Shader", &Data->m_ShaderXRLCName, smCShader);
            PHelper().CreateChoose(Info, "Game Material", &Data->m_GameMtlName, smGameMaterial);
            PHelper().CreateFlag32(Info, "2 Sided", &Data->m_Flags, SSurfaceData::sf2Sided);
            
            PreviewProps->AssignItems(Info);

            auto m_Thm = ImageLib.CreateThumbnail(Data->m_Texture.c_str(), EImageThumbnail::ETTexture);
            if(IVERIFY(m_Thm))
            {
                IRHISurface* Surface = nullptr;
                m_Thm->Update(Surface);
                m_RealTexture = new CTexture();
                m_RealTexture->surface_set(Surface);
                Surface->Release();
                xr_delete(m_Thm);
            }
            
            /*CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Tex"), &s->m_pData.second->m_Texture, smTexture);
            CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

            CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Shader"), &s->m_pData.second->m_ShaderName, smEShader);
            CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

            CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Compile"), &s->m_pData.second->m_ShaderXRLCName, smCShader);
            CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);
				
            CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Mtl"), &s->m_pData.second->m_GameMtlName, smGameMaterial);
            CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);
            CV->OnAfterEditEvent.bind(this, &CSceneObject::AfterEditGameMtl);*/
        }
        
        /*EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(m_Current, EImageThumbnail::ETObject);

        if (m_Thm && m_Thm->_FaceCount() != 0 && m_Thm->_VertexCount() != 0)
        {
            IRHISurface* Surface = nullptr;
            m_Thm->Update(Surface);

            m_RealTexture = new CTexture();
            m_RealTexture->surface_set(Surface);
            Surface->Release();

            m_Thm->FillInfo(Info);
            PreviewProps->AssignItems(Info);
        }
        else
        {
            PHelper().CreateCaption(Info, "Faces", "THM not found");
            PHelper().CreateCaption(Info, "Vertexes", "THM not found");
            PreviewProps->AssignItems(Info);
        }*/

        /*if (m_Preview)
        {
            FocusedItems = ActualItemList().m_SelectedItems;
            SelectionToReference(&FocusedItems);
        }

        if (bShowProps)
            OnPropertiesClick();*/
    }
    else
    {
        //bShowProps = false;
    }

    UI->RedrawScene();
}

void UISharedMaterialsLibrary::OnItemUnfocused(ListItem* item)
{
    /*if (!m_Preview)
        return;

    if (item != nullptr)
    {
        auto Iter = std::find(FocusedItems.begin(), FocusedItems.end(), item);
		
        if (Iter != FocusedItems.end())
        {
            FocusedItems.erase(Iter);
            SelectionToReference(&FocusedItems);
        }
    }*/
}
