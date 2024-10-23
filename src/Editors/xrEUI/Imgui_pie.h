
namespace ImGui
{
	IMGUI_API bool BeginPiePopup(const char* pName, int iMouseButton = 0);
	IMGUI_API void EndPiePopup();

	IMGUI_API bool PieMenuItem(const char* pName, bool bEnabled = true);
	IMGUI_API bool BeginPieMenu(const char* pName, bool bEnabled = true);
	IMGUI_API void EndPieMenu();
}

//ПРИМЕР ИСПОЛЬЗОВАНИЯ
	/*{
	* std::string sText;
		if (ImGui::IsWindowHovered() && ImGui::IsMouseClicked(1))
		{
			ImGui::OpenPopup("PieMenu");
		}

		if (ImGui::BeginPiePopup("PieMenu", 1))
		{
			if (ImGui::BeginPieMenu("Visiblity"))
			{
				if (ImGui::BeginPieMenu(" Hide "))
				{
					if (ImGui::PieMenuItem("Selected")) sText = "TestSub";
					if (ImGui::PieMenuItem("Unselected")) sText = "TestSub";
					if (ImGui::PieMenuItem("All")) sText = "TestSub";
					ImGui::EndPieMenu();
				}

				if (ImGui::PieMenuItem("Unhide All")) sText = "TestSub";
				ImGui::EndPieMenu();
			}
			if (ImGui::BeginPieMenu("Locking"))
			{
				if (ImGui::BeginPieMenu("Lock"))
				{
					if (ImGui::PieMenuItem("Selection")) sText = "TestSub";
					if (ImGui::PieMenuItem("Unselected")) sText = "TestSub";
					if (ImGui::PieMenuItem("All")) sText = "TestSub";
					ImGui::EndPieMenu();
				}
				if (ImGui::BeginPieMenu("Unock"))
				{
					if (ImGui::PieMenuItem("Selection")) sText = "TestSub";
					if (ImGui::PieMenuItem("Unselected")) sText = "TestSub";
					if (ImGui::PieMenuItem("All")) sText = "TestSub";
					ImGui::EndPieMenu();
				}
				ImGui::EndPieMenu();
			}
			if (ImGui::BeginPieMenu("Edit"))
			{
				if (ImGui::PieMenuItem("Copy")) sText = "TestSub";
				if (ImGui::PieMenuItem("Paste")) sText = "TestSub";
				if (ImGui::PieMenuItem("Dublicate")) sText = "TestSub";
				if (ImGui::PieMenuItem("Cut")) sText = "TestSub";
				if (ImGui::PieMenuItem("Delete")) sText = "TestSub";
				ImGui::EndPieMenu();
			}
			if (ImGui::PieMenuItem("Properties")) sText = "TestSub";
			ImGui::EndPiePopup();
		}
	}*/