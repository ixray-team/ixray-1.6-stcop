#include "stdafx.h"
#include "IconsFontAwesome6.h"

UIParticlesTool::UIParticlesTool()
{
    m_Current = nullptr;
    m_ParticlesList = new UIItemListForm();
    m_ParticlesList->SetOnItemFocusedEvent({this, &UIParticlesTool::OnItemFocused});
    ListItemsVec items;
    for (PS::PEDIt E = ::RImplementation.PSLibrary.FirstPED(); E != ::RImplementation.PSLibrary.LastPED(); E++) {
        ListItem* I = LHelper().CreateItem(items, *(*E)->m_Name, 0, 0, *E);
        I->SetIcon(1);
    }
    for (PS::PGDIt G = ::RImplementation.PSLibrary.FirstPGD(); G != ::RImplementation.PSLibrary.LastPGD(); G++) {
        ListItem* I = LHelper().CreateItem(items, *(*G)->m_Name, 0, 0, *G);
        I->SetIcon(2);
    }

    m_ParticlesList->AssignItems(items);
}

UIParticlesTool::~UIParticlesTool()
{
    xr_delete(m_ParticlesList);
}

void UIParticlesTool::Draw()
{
	const float TableRowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			if (XRay::ImGui::BeginTable("##particles_tools", 4, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
												ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);				ImGui::TableSetupColumn("-", ImGuiTableColumnFlags_WidthFixed);																					ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthFixed);													ImGui::TableSetupColumn("---", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn(); XRay::ImGui::TextFramed("Ref's Select: ");		XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button("+", { TableRowHeight, TableRowHeight })) { SelByRef(true); }; ImGui::SameLine();		XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button("-", { TableRowHeight, TableRowHeight })) { SelByRef(false); };
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn(); XRay::ImGui::TextFramed("Selected:       ");
				XRay::ImGui::TableNextColumn();
				if (XRay::ImGui::Button(ICON_FA_PLAY, { TableRowHeight, TableRowHeight }))
				{
					ObjectIt _F = Scene->FirstObj(OBJCLASS_PS);
					ObjectIt _E = Scene->LastObj(OBJCLASS_PS);
					for (; _F != _E; _F++) {
						if ((*_F)->Visible() && (*_F)->Selected())
							((EParticlesObject*)(*_F))->Play();
					}
				}
				XRay::ImGui::TableNextColumn();
				if ((XRay::ImGui::Button(ICON_FA_STOP, { TableRowHeight, TableRowHeight })))
				{
					ObjectIt _F = Scene->FirstObj(OBJCLASS_PS);
					ObjectIt _E = Scene->LastObj(OBJCLASS_PS);
					for (; _F != _E; _F++) {
						if ((*_F)->Visible() && (*_F)->Selected())
							((EParticlesObject*)(*_F))->Stop();
					}
				}
				XRay::ImGui::EndTable();
			}
			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}

void UIParticlesTool::DrawObjectsList()
{
    if (ImGui::Begin("Edit group items"))
    {
        m_ParticlesList->Draw();
    }
    ImGui::End();
}

void UIParticlesTool::SelByRef(bool flag)
{
    if (m_Current) 
    {
        ObjectIt _F = Scene->FirstObj(OBJCLASS_PS);
        ObjectIt _E = Scene->LastObj(OBJCLASS_PS);
        for (; _F != _E; _F++) {
            if ((*_F)->Visible()) {
                EParticlesObject* _O = (EParticlesObject*)(*_F);
                if (_O->RefCompare(m_Current)) _O->Select(flag);
            }
        }
    }
}

void UIParticlesTool::OnItemFocused(ListItem* item)
{
    m_Current = nullptr;
    if (item)
    {
        m_Current = item->Key();
    }
}
