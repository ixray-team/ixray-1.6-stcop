#include "stdafx.h"

UIWayTool::UIWayTool()
{
	m_WayMode = true;
	m_AutoLink = true;
}

UIWayTool::~UIWayTool()
{
}

void UIWayTool::Draw()
{
	const float	ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;
	bool WayMode = m_WayMode;
	bool WayPoint = !m_WayMode;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::ToggleButton("Way Mode", &WayMode, { SizeX, 0 })) {
				LTools->SetTarget(OBJCLASS_WAY, 0);
				m_WayMode = true;
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::ToggleButton("Way Point", &WayPoint, { SizeX, 0 })) {
				LTools->SetTarget(OBJCLASS_WAY, 1);
				m_WayMode = false;
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Link Command"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

			if (XRay::ImGui::ToggleButton("Auto Link", &m_AutoLink)) {}
			if (XRay::ImGui::Button("Create 1-Link", { SizeX, 0 }))
			{
				if (m_WayMode) {
					ELog.DlgMsg(mtInformation, "Before editing enter Point Mode.");
					return;
				}
				bool bRes = false;
				ObjectList lst;
				Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				// remove links
				for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
					((CWayObject*)(*it))->RemoveLink();
					bRes |= ((CWayObject*)(*it))->Add1Link();
				}
				if (bRes) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Convert to 1-Link", { SizeX, 0 }))
			{
				ObjectList lst;
				int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				for (ObjectIt it = lst.begin(); it != lst.end(); it++)
					((CWayObject*)(*it))->Convert1Link();
				if (cnt) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}

			if (XRay::ImGui::Button("Create 2-Link", { SizeX, 0 }))
			{
				if (m_WayMode) {
					ELog.DlgMsg(mtInformation, "Before editing enter Point Mode.");
					return;
				}
				bool bRes = false;
				ObjectList lst;
				Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				for (ObjectIt it = lst.begin(); it != lst.end(); it++)
					bRes |= ((CWayObject*)(*it))->Add2Link();
				if (bRes) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Convert to 2-Link", { SizeX, 0 }))
			{
				ObjectList lst;
				int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				for (ObjectIt it = lst.begin(); it != lst.end(); it++)
					((CWayObject*)(*it))->Convert2Link();
				if (cnt) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}

			if (XRay::ImGui::Button("Invert Link", { SizeX, 0 }))
			{
				if (m_WayMode) {
					ELog.DlgMsg(mtInformation, "Before editing enter Point Mode.");
					return;
				}
				ObjectList lst;
				int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				for (ObjectIt it = lst.begin(); it != lst.end(); it++)
					((CWayObject*)(*it))->InvertLink();
				if (cnt) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Remove Link", { SizeX, 0 }))
			{
				if (m_WayMode) {
					ELog.DlgMsg(mtInformation, "Before editing enter Point Mode.");
					return;
				}
				ObjectList lst;
				int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
				for (ObjectIt it = lst.begin(); it != lst.end(); it++)
					((CWayObject*)(*it))->RemoveLink();
				if (cnt) Scene->UndoSave();
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
			}

			XRay::ImGui::EndExpand();
		}
		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}

}
