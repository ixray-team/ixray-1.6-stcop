//---------------------------------------------------------------------------
#include "stdafx.h"
#include "UIIConPicker.h"
UIIconPicker* UIIconPicker::Form = nullptr;

UIIconPicker::UIIconPicker()
{

	Icons["ed_nodata"] = { EDevice->Resources->_CreateTexture("ed\\ed_nodata"),	true };
}

UIIconPicker::~UIIconPicker()
{
	Icons.clear();
}

bool UIIconPicker::ShowIcons() {
	const int button_size = 60;
	const int padding = 4;

	FS_FileSetIt it = texture_map.begin();
	FS_FileSetIt _E = texture_map.end();

	for (; it != _E; ++it) {
		float available_width = ImGui::GetContentRegionAvail().x;

		if (available_width < button_size) {
			ImGui::NewLine();
		}

		if (ImGui::ImageButton("##ibUIIconPicker001",Icons[it->name]->pSurface, ImVec2(button_size, button_size))) {
			ImGui::Text("Button %d pressed", 0);
			EPrefs->custom_icons[file_path.c_str()] = it->name;
			return true;
		}

		ImGui::SameLine(0, padding);
	}

	return false;
}

void UIIconPicker::Draw()
{
	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused();

	{
		ImGui::BeginGroup();
		ImGui::BeginChild("Left", ImVec2(0, -ImGui::GetFrameHeight() - 4));
		{
			if (ShowIcons())
			{
				ExecCommand(COMMAND_ICON_LOAD);
				HideLib();
			}

			if (!IsDocked)
				IsDocked = ImGui::IsWindowDocked();
			if (!IsFocused)
				IsFocused = ImGui::IsWindowFocused();
		}
		ImGui::EndChild();

		if (ImGui::Button("Close"))
		{
			HideLib();
		}
		
		if (EPrefs->custom_icons.contains(file_path.c_str())) 
		{
			ImGui::SameLine();
			if (ImGui::Button("Default Icon"))
			{
				EPrefs->custom_icons.erase(file_path.c_str());
				ExecCommand(COMMAND_ICON_REMOVE, file_path);
				HideLib();
			}
		}
		ImGui::EndGroup();
	}
}

void UIIconPicker::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			Form->BeginDraw();
			ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(600, 400));
			if (ImGui::Begin("Icon Picker", nullptr, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoScrollbar))
			{
				Form->Draw();
			}
			ImGui::PopStyleVar(1);
			ImGui::End();
			Form->EndDraw();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIIconPicker::Show(const xr_string file_path)
{
	if (Form == nullptr) {
		Form = new UIIconPicker();
	}
	Form->file_path = file_path;
	Form->InitItemList();
}

void UIIconPicker::InitItemList()
{
	string_path Path = {};

	FS.update_path(Path, _game_textures_, "");
	sprintf(Path, "%s%s", Path, "ed\\content_browser\\");
	FS.file_list(texture_map, Path, FS_ListFiles | FS_ClampExt, "*.dds");

	FS_FileSetIt it = texture_map.begin();
	FS_FileSetIt _E = texture_map.end();
	for (; it != _E; it++)
	{
		InitPreviewTexture(it->name, Path);
	}
}

void UIIconPicker::InitPreviewTexture(const xr_string& Tag, const string_path& Path)
{
	if (Icons.contains(Tag))
		return;

	xr_string NewPath = Path + Tag;
	Icons[Tag] = EDevice->Resources->_CreateTexture(NewPath.c_str());
	Icons[Tag]->Load();

	if (!Icons[Tag]->pSurface) {
		Icons[Tag] = Icons["ed_nodata"];
	}
}

void UIIconPicker::HideLib()
{
	bOpen = false;
	ImGui::CloseCurrentPopup();
}
