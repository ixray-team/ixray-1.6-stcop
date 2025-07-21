#include "stdafx.h"
#include "ContentView.h"

#include "../../Nodes/UIDialogsView.h"
#include "../../../../utils/xrDXT/xrDXT.h"
#include "../../../xrECore/Editor/ParticleEffectActions.h"

#include "Viewports/ViewportMesh.h"

CContentView* GContentView = nullptr;

CContentView::CContentView():
	WatcherPtr(nullptr)
{
	string_path Dir = {};
	FS.update_path(Dir, "$fs_root$", "");

	RootDir = xr_path(Dir).xstring();
	CurrentDir = RootDir;
	CopiedObjects.clear();
	ExtDesc.clear();
	IsCutting = false;

	FS.update_path(Dir, "$logs$", "");
	LogsDir = Dir;
}

void CContentView::Draw()
{
	if (IsWndDestroyed)
		return;

	if (ImGui::Begin("Content Browser"))
	{
		DrawHeader();

		if ((NeedRescan || Files.empty()) && !IsFindResult && !IsSpawnElement && !IsParticles)
		{
			RescanDirectory();
			NeedRescan = false;
		}

		if (ImGui::BeginChild("##contentbrowserscroll"))
		{
			const size_t IterCount = (ImGui::GetWindowSize().x / (BtnSize.x + 15)) - 1;
			size_t HorBtnIter = 0;
			xr_string NextDir = CurrentDir;

			if (ImGui::IsWindowHovered(ImGuiHoveredFlags_None) && ImGui::IsMouseReleased(1) && !ImGui::IsAnyItemHovered())
			{
				if (!xr_path(CurrentDir).has_root_path() && !IsSpawnElement)
					ImGui::OpenPopup("##contentbrowsercontext");
				SelectedObjects.clear();
			}
			else if (!RenameObject.Focus && ImGui::IsMouseClicked(0))
			{
				RenameObject.Active = false;
			}
			else if (ImGui::IsWindowHovered(ImGuiHoveredFlags_None) && ImGui::IsMouseReleased(0) && !ImGui::IsAnyItemHovered())
			{
				SelectedObjects.clear();
			}

			DrawFormContext();

			if ((!RootDir.Contains(CurrentDir) && !IsSpawnElement) || IsFindResult)
			{
				DrawOtherDir(HorBtnIter, IterCount, NextDir);
			}
			else if (IsParticles)
			{
				DrawParticlesDir(HorBtnIter, IterCount);
			}
			else if (IsSpawnElement)
			{
				DrawISEDir(HorBtnIter, IterCount);
			}
			else
			{
				DrawRootDir(HorBtnIter, IterCount, NextDir);
			}

			CurrentDir = NextDir;
			xr_strlwr(CurrentDir);
		}

		if (CurrentItemHint.Active)
		{
			ImGui::SetCursorPos(CurrentItemHint.Pos);
			ImGui::Button(CurrentItemHint.Name.c_str());
			CurrentItemHint.Active = false;
		}

		ImGui::EndChild();
	}

	ImGui::End();

	ThmPropWnd.Draw();
}

void CContentView::DrawHeader()
{
	BtnSize = (ViewMode == EViewMode::Tile) ? ImVec2(64.f, 64.f) : ImVec2(32.f, 32.f);
	if (ImGui::Button("root"))
	{
		CurrentDir = RootDir;
		IsSpawnElement = false;
		IsFindResult = false;
		std::memset(FindStr, 0, sizeof(FindStr));
		VirtualPath.clear();
	}

	TextHeight = ImGui::CalcTextSize("1").y;

	ImGui::SameLine();
	ImGui::Text("/");

	auto DrawByPathLambda = [&](const xr_string& ViewDir)
	{
		auto Pathes = ViewDir.Split('\\');

		for (const xr_string& Path : Pathes)
		{
			ImGui::SameLine();
			if (ImGui::Button(Platform::ANSI_TO_UTF8(Path).data()))
			{
				xr_string NewPath = "";
				for (const xr_string& LocPath : Pathes)
				{
					NewPath += LocPath;

					if (LocPath == Path)
						break;

					NewPath += "\\";
				}

				if (IsSpawnElement)
				{
					VirtualPath = NewPath;
					RescanISEDirectory(VirtualPath);
				}
				else if (IsParticles)
				{
					VirtualPath = NewPath;
					RescanParticlesDirectory(VirtualPath);
				}
				else
				{
					CurrentDir = NewPath;
					RescanDirectory();
				}
			}

			ImGui::SameLine();
			ImGui::Text("/");
		}
	};

	if (IsSpawnElement || IsParticles)
	{
		ImGui::SameLine();
		shared_str DirPartialName = IsSpawnElement ? "Spawn Element" : "Particles";

		if (ImGui::Button(*DirPartialName))
		{
			VirtualPath.clear();

			if (IsSpawnElement)
			{
				RescanISEDirectory(VirtualPath);
			}
			else
			{
				RescanParticlesDirectory(VirtualPath);
			}
		}
		ImGui::SameLine();
		ImGui::Text("/");

		if (!VirtualPath.empty())
		{
			DrawByPathLambda(VirtualPath);
		}
	}
	else if (CurrentDir != RootDir)
	{
		DrawByPathLambda(CurrentDir);
	}


	int FindStartPosX = (int)ImGui::GetWindowSize().x;

	float w = 0;

	if (FindStartPosX > 400)
	{
		ImGui::SameLine();
		int FindSizeX = FindStartPosX / 3.5f;
		FindStartPosX -= FindSizeX;

		ImGui::SetCursorPosX(FindStartPosX);
		
		w = FindSizeX - 35;
	}
	else
	{
		w = FindStartPosX - 45;
	}

	IconData* IconPtr = &GetTexture("search");
	ImVec2 IconSize{ 0,0 };

	//Varian 1
	/*if (IconPtr->Icon)
	{
		IconSize={ 16,16 };
		ImGui::Image(IconPtr->Icon->pSurface, IconSize);
		ImGui::SameLine();
	}*/

	ImGui::SetNextItemWidth(w - IconSize.x*1.5f);

	if (ImGui::InputTextWithHint("##Search", "Search", FindStr, sizeof(FindStr)))
	{
		FindFile();
	}

	//Varian 2
	if (IconPtr->Icon)
	{
		IconSize = { 12,12 };

		ImGui::SameLine();
		ImVec2 cursorPos = ImGui::GetCursorPos();
		ImGui::SetCursorPos(ImVec2(cursorPos.x - IconSize.x-10.f, cursorPos.y+(IconSize.y/4)));

		ImGui::Image(IconPtr->Icon->pSurface, IconSize);
	}

	ImGui::SameLine();

	if (ImGui::BeginPopupContextItem("MenuCBPpp"))
	{
		if (ImGui::Checkbox("Show THM", &IsThmMode) && !IsSpawnElement)
		{
			RescanDirectory();
		}
		ImGui::Separator();

		if (ImGui::BeginMenu("View mode"))
		{
			if (ImGui::MenuItem("Tile"))
			{
				ViewMode = EViewMode::Tile;
			}
			if (ImGui::MenuItem("List"))
			{
				ViewMode = EViewMode::List;
			}

			ImGui::EndMenu();
		}

		ImGui::EndPopup();
	}
	/*
	Exception thrown: read access violation.
this->MenuIcon.p_ was nullptr.
	*/
	if (MenuIcon && ImGui::ImageButton("##MenuCB", MenuIcon->pSurface, { 15, 15 }))
	{
		ImGui::OpenPopup("MenuCBPpp");
	}
	ImGui::Separator();
}

void CContentView::FindFile()
{
	xr_string ParseStr = IsUTF8(FindStr) ? Platform::UTF8_to_CP1251(FindStr) : FindStr;
	size_t Len = ParseStr.length();
	if (Len > 2)
	{
		IsFindResult = true;
		if (CurrentDir == RootDir && !IsSpawnElement)
		{
			IsDelWatcher = true;
			xr_delete(WatcherPtr);

			ClearFileList();
			for (const auto& file : xr_dir_recursive_iter { CurrentDir.data() })
			{
				if (file.is_directory())
					continue;

				const xr_string& FName = file.path().filename().string().data();
				if (FName.Contains(ParseStr) && CheckFile(FName))
				{
					Files.push_back({ file, false });
				}
			}

			auto TempPath = ScanConfigs("");
			ScanConfigsRecursive(TempPath, ParseStr);
		}
		else if (IsSpawnElement)
		{
			ClearFileList();

			auto TempPath = ScanConfigs("");
			ScanConfigsRecursive(TempPath, ParseStr);
		}
		else
		{
			IsDelWatcher = true;
			xr_delete(WatcherPtr);

			ClearFileList();
			for (const xr_dir_entry& file : xr_dir_recursive_iter { CurrentDir.data() })
			{
				if (file.is_directory())
					continue;

				const xr_string& FName = xr_path(file.path()).xfilename();
				if (FName.Contains(ParseStr) && CheckFile(FName))
				{
					Files.push_back({ file, false });
				}
			}
		}
	}
	else if (Len == 0)
	{
		IsFindResult = false;

		if (IsSpawnElement)
		{
			RescanISEDirectory(VirtualPath);
		}
		else
		{
			RescanDirectory();
		}
	}
}

void CContentView::ScanConfigsRecursive(xr_map<xr_string, CContentView::FileOptData>& TempPath, const xr_string& ParseStr)
{
	for (auto& [Name, DirOpt] : TempPath)
	{
		if (DirOpt.IsDir)
		{
			auto RecFiles = ScanConfigs(Name);
			ScanConfigsRecursive(RecFiles, ParseStr);
		}

		if (Name.Contains(ParseStr) && !DirOpt.IsDir)
		{
			Files.push_back(DirOpt);
		}
	}
}

void CContentView::DrawISEDir(size_t& HorBtnIter, const size_t IterCount)
{
	if (DrawItem({ "..", true }, HorBtnIter, IterCount))
	{
		if (VirtualPath.empty())
		{
			IsSpawnElement = false;
			VirtualPath = "";
			ClearFileList();
		}
		else
		{
			xr_string Validate = VirtualPath;
			if (Validate.ends_with('\\'))
			{
				Validate = Validate.erase(Validate.length() - 1);
			}
			xr_path ISEFS = Validate;

			if (ISEFS.has_parent_path())
			{
				RescanISEDirectory(ISEFS.parent_path().string().data());
				VirtualPath = ISEFS.parent_path().string().data();
			}
			else
			{
				VirtualPath = "";
				RescanISEDirectory("");
			}
		}
	}

	for (const FileOptData& Data : Files)
	{
		if (DrawItem(Data, HorBtnIter, IterCount))
		{
			if (Data.IsDir)
			{
				const xr_string& CopyFileName = Data.File.xstring();
				RescanISEDirectory(CopyFileName);
			}

			break;
		}
	}
}

void CContentView::DrawParticlesDir(size_t& HorBtnIter, const size_t IterCount)
{
	// Draw ".." button to go up one level
	if (DrawItem({ "..", true }, HorBtnIter, IterCount))
	{
		xr_path TryVirtualPath = VirtualPath;
		if (VirtualPath.empty())
		{
			IsParticles = false;
		}
		else if (TryVirtualPath.has_parent_path())
		{
			VirtualPath = TryVirtualPath.parent_path().string().c_str();
		}
		else
		{
			VirtualPath = "";
		}

		ClearFileList();

		if (IsParticles)
		{
			RescanParticlesDirectory(VirtualPath);
		}
	}

	// Draw particle items
	for (const FileOptData& Data : Files)
	{
		if (DrawItem(Data, HorBtnIter, IterCount))
		{
			// Handle particle item double-click
			if (!Data.IsDir) // Particles are files, not directories
			{
				// You might want to implement particle editing functionality here
				// For example:
				// ExecCommand(COMMAND_EDIT_PARTICLE, Data.File.xstring());
			}
			else
			{
				VirtualPath = Data.File;
				RescanParticlesDirectory(Data.File);
			}
		}
	}
}

void CContentView::DrawRootDir(size_t& HorBtnIter, const size_t& IterCount, xr_string& NextDir)
{
	string_path FSEntry = {};
	auto PathClickLambda = [&FSEntry, &HorBtnIter, &IterCount, &NextDir, this]()
	{
		xr_string Validate = FSEntry;
		if (Validate.ends_with('\\'))
		{
			Validate = Validate.erase(Validate.length() - 1);
		}

		ImGui::BeginDisabled(!FS.TryLoad(FSEntry));

		if (DrawItem({ Validate.c_str(), true }, HorBtnIter, IterCount))
		{
			NextDir = FSEntry;
			if (NextDir.ends_with('\\'))
			{
				NextDir = NextDir.erase(NextDir.length() - 1);
			}
			ClearFileList();
		}

		ImGui::EndDisabled();
	};

	IsSpawnElement = false;

	FS.update_path(FSEntry, "$server_data_root$", "");
	PathClickLambda();

	FS.update_path(FSEntry, "$import$", "");
	PathClickLambda();

	FS.update_path(FSEntry, "$game_data$", "");
	PathClickLambda();

	if (DrawItem({ "Particles", true }, HorBtnIter, IterCount))
	{
		RescanParticlesDirectory("");
	}

	if (DrawItem({ "Spawn Elements", true }, HorBtnIter, IterCount))
	{
		RescanISEDirectory("");
	}
}

void CContentView::RescanISEDirectory(const xr_string& StartPath)
{
	ClearFileList();

	if (!StartPath.empty() && StartPath != VirtualPath)
	{
		if (!VirtualPath.empty() && !VirtualPath.ends_with('\\'))
			VirtualPath += "\\";

		VirtualPath += StartPath + '\\';
	}

	auto TempPath = ScanConfigs(StartPath);

	for (auto& [Name, DirOpt] : TempPath)
	{
		if (DirOpt.IsDir)
		{
			Files.push_back(DirOpt);
		}
	}

	for (auto& [Name, DirOpt] : TempPath)
	{
		if (!DirOpt.IsDir)
		{
			Files.push_back(DirOpt);
		}
	}

	if (VirtualPath.empty())
	{
		Files.push_back({ xr_string(xr_string(ENVMOD_CHOOSE_NAME) + ".ise") , false, ENVMOD_CHOOSE_NAME });
		Files.push_back({ xr_string(xr_string(RPOINT_CHOOSE_NAME) + ".ise") , false, RPOINT_CHOOSE_NAME });
	}

	IsSpawnElement = true;
}

void CContentView::RescanParticlesDirectory(const xr_string& path)
{
	ClearFileList();

	xr_vector<xr_string> Directories;

	{
		PS::PEDIt Pe = RImplementation.PSLibrary.FirstPED();
		PS::PEDIt Ee = RImplementation.PSLibrary.LastPED();
		for (; Pe != Ee; Pe++)
		{
			xr_path PEPath = *(*Pe)->m_Name;
			if (PEPath.has_parent_path() && !path.empty())
			{
				xr_string ParentPath = PEPath.parent_path().string().c_str();
				if (ParentPath == path)
				{
					FileOptData FileInfo;
					FileInfo.File = *(*Pe)->m_Name;
					FileInfo.File.replace_extension("pe");

					Files.push_back(FileInfo);
				}
				else if (!path.empty() && ParentPath.contains(path))
				{
					auto Iter = std::find(Directories.begin(), Directories.end(), ParentPath);
					if (Iter == Directories.end())
					{
						FileOptData FileInfo;
						FileInfo.File = ParentPath;
						FileInfo.IsDir = true;
						Directories.push_back(ParentPath);

						Files.push_back(FileInfo);
					}
				}
			}
			else if (path.empty() && !PEPath.has_parent_path())
			{
				FileOptData FileInfo;
				FileInfo.File = *(*Pe)->m_Name;
				Files.push_back(FileInfo);
			}
			else if (path.empty() && PEPath.has_parent_path())
			{
				xr_path TestDir = PEPath.parent_path();
				while (TestDir.has_parent_path())
				{
					TestDir = TestDir.parent_path();
				}

				auto Iter = std::find(Directories.begin(), Directories.end(), TestDir.xfilename());
				if (Iter == Directories.end())
				{
					FileOptData FileInfo;
					FileInfo.File = TestDir;
					FileInfo.IsDir = true;
					Directories.push_back(TestDir.xfilename());

					Files.push_back(FileInfo);
				}
			}
		}
	}
	{
		PS::PGDIt Pg = RImplementation.PSLibrary.FirstPGD();
		PS::PGDIt Eg = RImplementation.PSLibrary.LastPGD();
		for (; Pg != Eg; Pg++)
		{
			xr_path PEPath = *(*Pg)->m_Name;
			if (PEPath.has_parent_path() && !path.empty())
			{
				xr_string ParentPath = PEPath.parent_path().string().c_str();
				if (ParentPath == path)
				{
					FileOptData FileInfo;
					FileInfo.File = *(*Pg)->m_Name;
					FileInfo.File.replace_extension("pg");
					Files.push_back(FileInfo);
				}
				else if (!path.empty() && ParentPath.contains(path))
				{
					auto Iter = std::find(Directories.begin(), Directories.end(), ParentPath);
					if (Iter == Directories.end())
					{
						FileOptData FileInfo;
						FileInfo.File = ParentPath;
						FileInfo.IsDir = true;
						Directories.push_back(ParentPath);

						Files.push_back(FileInfo);
					}
				}
			}
			else if (path.empty() && !PEPath.has_parent_path())
			{
				FileOptData FileInfo;
				FileInfo.File = *(*Pg)->m_Name;
				Files.push_back(FileInfo);
			}
			else if (path.empty() && PEPath.has_parent_path())
			{
				xr_path TestDir = PEPath.parent_path();
				while (TestDir.has_parent_path())
				{
					TestDir = TestDir.parent_path();
				}

				auto Iter = std::find(Directories.begin(), Directories.end(), TestDir.xfilename());
				if (Iter == Directories.end())
				{
					FileOptData FileInfo;
					FileInfo.File = TestDir;
					FileInfo.IsDir = true;
					Directories.push_back(TestDir.xfilename());

					Files.push_back(FileInfo);
				}
			}
		}
	}

	IsParticles = true;
}

void CContentView::DrawOtherDir(size_t& HorBtnIter, const size_t IterCount, xr_string& NextDir)
{
	xr_path FilePath = CurrentDir;
	if (!IsFindResult && DrawItem({ "..", true }, HorBtnIter, IterCount))
	{
		NextDir = FilePath.parent_path().string().data();
		if (FilePath.parent_path().is_absolute() && !NextDir.Contains(RootDir) || NextDir.empty())
		{
			NextDir = RootDir;
		}
		ClearFileList();
	}

	for (FileOptData FilePath : Files)
	{
		if (FilePath.IsDir)
		{
			if (DrawItem(FilePath, HorBtnIter, IterCount))
			{
				NextDir = FilePath.File.xstring();
				if (NextDir.ends_with('\\'))
				{
					//NextDir = NextDir.erase(NextDir.length() - 1);
					NextDir.pop_back();
				}
				ClearFileList();
				break;
			}
		}
		else
		{
			if (DrawItem(FilePath, HorBtnIter, IterCount))
			{
				if (FilePath.File.extension() == ".xml")
				{
					xr_string FileName = FilePath.File.xfilename();
					FileName = FileName.substr(0, FileName.size() - 4);

					auto Iter = std::find(GameDialogs.begin(), GameDialogs.end(), FileName);
					if (Iter != GameDialogs.end())
					{
						CUIDialogView::OpenFile(FilePath.File.xfilename());
					}
				}
				if (FilePath.File.extension() == ".thm")
				{
					ThmPropWnd.Load(FilePath.File);
					ThmPropWnd.Show();
				}
				else if (FilePath.File.extension() == ".tga")
				{
					string_path fn = {};
					FS.update_path(fn, _textures_, "");
					xr_string OldPath = FilePath.File;

					auto CharIndex = OldPath.find(fn);
					if (CharIndex != xr_string::npos)
					{
						xr_string NewPath = OldPath.substr(OldPath.find(fn) + xr_strlen(fn));
						NewPath = NewPath.substr(0, NewPath.find_last_of("."));
						ExecCommand(COMMAND_IMAGE_EDITOR_SELECT, NewPath, false);
					}
					else
					{
						FS.update_path(fn, _import_, "");
						CharIndex = OldPath.find(fn);

						if (CharIndex != xr_string::npos)
						{
							xr_string NewPath = OldPath.substr(OldPath.find(fn) + xr_strlen(fn));
							ExecCommand(COMMAND_IMAGE_EDITOR_SELECT, NewPath, true);
						}
					}
				}
			}
		}
	}
}

void CContentView::ClearFileList()
{
	Files.clear();
}

void CContentView::RescanDirectory()
{
	IsDelWatcher = true;
	xr_delete(WatcherPtr);

	ClearFileList();
	for (const auto& file : xr_dir_iter{ CurrentDir.data() })
	{
		if (std::filesystem::is_directory(file))
		{
			Files.push_back({ file, true });
		}
	}
	for (const xr_dir_entry& file : xr_dir_iter { CurrentDir.data() })
	{
		if (!file.is_directory() && CheckFile(file))
		{
			Files.push_back({ file, false });
		}
	}

	WatcherPtr = new filewatch::FileWatch<std::string>
	(
		CurrentDir.data(),
		[this](const std::string&, const filewatch::Event)
		{
			NeedRescan = true;
		}
	);
}

void CContentView::Destroy()
{
	MenuIcon.destroy();
	Icons.clear();

	IsWndDestroyed = true;
}

void CContentView::ResetBegin() {
}

void CContentView::ResetEnd() {
}

void CContentView::LoadCustomIcons()
{
	if (EPrefs->custom_icons.size() == 0)
		return;

	for (auto el : EPrefs->custom_icons)
	{
		if (Icons.contains(el.second.c_str()))
		{
			Icons[el.first.c_str()] = Icons[el.second.c_str()];
			Icons[el.first.c_str()].UseButtonColor = true;
		}
		else
		{
			string_path Path = {};
			sprintf(Path, "%s%s", "ed\\content_browser\\", el.second);
			Icons[el.first.c_str()] = { EDevice->Resources->_CreateTexture(Path),	true };
		}

	}
	
}

void CContentView::RemoveCustomIcon(const xr_string& icon)
{
	if (Icons.contains(icon))
		Icons.erase(icon);
}

void CContentView::Init()
{
	Icons["Folder"] = {EDevice->Resources->_CreateTexture("ed\\content_browser\\folder"),	true};
	Icons[".."]		= {EDevice->Resources->_CreateTexture("ed\\content_browser\\folder"),	true};
	Icons["thm"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\thm"),		true};
	Icons["logs"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\log"),		true};
	Icons["ogg"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\ogg"),		true};
	Icons["level"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\level"),	true};
	Icons["wav"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\wav"),		true};
	Icons["object"] = {EDevice->Resources->_CreateTexture("ed\\content_browser\\object"),	true};
	Icons["image"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\image"),	true};
	Icons["seq"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\seq"),		true};
	Icons["tga"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\tga"),		true};
	Icons["file"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\file"),		true};
	Icons["exe"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\exe"),		true};
	Icons["cmd"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\cmd"),		true};
	Icons["dll"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\dll"),		true};
	Icons["backup"] = {EDevice->Resources->_CreateTexture("ed\\content_browser\\backup"),	true};
	Icons["env_mod"]= {EDevice->Resources->_CreateTexture("ed\\content_browser\\env_mod"),	true};
	Icons["dialogs"] = { EDevice->Resources->_CreateTexture("ed\\content_browser\\dialogs"),true};
	Icons["multi"]	= {EDevice->Resources->_CreateTexture("ed\\content_browser\\multi"),	true};

	Icons["search"]= {EDevice->Resources->_CreateTexture("ed\\content_browser\\search"),	false};

	MenuIcon = EDevice->Resources->_CreateTexture("ed\\bar\\menu");

	LoadCustomIcons();
	LoadExtDest();

	xr_string Files = pSettings->r_string("dialogs", "files");
	Files.RemoveWhitespaces();
	GameDialogs = Files.Split(',');
}

void CContentView::LoadExtDest()
{
	ExtDesc["_dir"] = "Directory";
	ExtDesc[".dds"] = "Texture Asset";
	ExtDesc[".tga"] = "Raw Texture Asset";
	ExtDesc[".png"] = "Image";
	ExtDesc[".object"] = "Object Asset";
	ExtDesc[".group"] = "Group object Asset";
	ExtDesc[".r16"] = "HeightMap Asset";
	ExtDesc[".ogf"] = "Object";
	ExtDesc[".wav"] = "Raw Sound";
	ExtDesc[".ogg"] = "Sound Asset";
	ExtDesc[".ise"] = "Spawn Component";
	ExtDesc[".skl"] = "Raw Single Animation Asset";
	ExtDesc[".skls"] = "Raw Animations Asset";
	ExtDesc[".omf"] = "Animations Asset";
	ExtDesc["_script_ltx"] = "Logic Preference";
	ExtDesc[".ltx"] = "Config";
	ExtDesc[".script"] = "Lua Script";
	ExtDesc["dialogs"] = "Dialog Description";
}

bool CContentView::DrawItem(const FileOptData& FilePath, size_t& HorBtnIter, const size_t IterCount)
{
	bool IsClicked = false;

	IsClicked = DrawItemN(FilePath, HorBtnIter, IterCount);

	if (IsClicked)
		SelectedObjects.clear();

	return IsClicked;
}

void CContentView::AcceptDragDropAction(const CContentView::FileOptData& InitFileName)
{
	if (/*!InitFileName.IsDir ||*/ (InitFileName.File == ".." && CurrentDir.find_last_of("/\\") == xr_string::npos) || IsSpawnElement || !ImGui::BeginDragDropTarget())
	{
		return;
	}
	
	auto ImData = ImGui::AcceptDragDropPayload("TEST");

	if (ImData == nullptr)
		ImData = ImGui::AcceptDragDropPayload("FLDR");

	if (ImData == nullptr)
		ImData = ImGui::AcceptDragDropPayload("OTHR");

	if (ImData != nullptr)
	{
		if (ImData != nullptr)
			Data = *(DragDropData*)ImData->Data;

		if (Data.FileName != InitFileName.File.xstring()) //На всякий случай
		{
			CutAction(/*Data.FileName*/);
			PasteAction(InitFileName.File);
		}
	}

	ImGui::EndDragDropTarget();
}

bool CContentView::BeginDragDropAction(xr_path& FilePath, xr_string& FileName, const CContentView::FileOptData& InitFileName, CContentView::IconData* IconPtr)
{
	/*
		Разделение на 3 типа dnd:

			TEST - ".object", ".group", ".ise"
					Объекты, которые принимает вьюпорт

			FLDR - Только папки.
					Для работы только в Content View

			OTHR - Все иные объекты.
					В дальнейшем можно как-нибудь использовать.
					Или просто удалить совместив с FLDR.
	*/

	bool WeCanDrag = false;

	
	if (FilePath.has_extension()) //File DnD
	{
		xr_string Extension = FilePath.extension().string().c_str();
		WeCanDrag = Extension == ".object" || Extension == ".group" || Extension == ".r16" || Extension == ".ise" || Extension == ".dti" || Extension == ".rai";

		if (!ImGui::BeginDragDropSource())
		{
			return false;
		}

		if (WeCanDrag)
		{
			if (IsSpawnElement || FilePath.xstring().ends_with(".ise"))
			{
				if (InitFileName.ISESect.size() > 0)
				{
					Data.FileName = InitFileName.ISESect.c_str();
				}
			}
			else
			{
				Data.FileName = FilePath;
			}

			xr_string PayloadName = "TEST";
			if (SelectedObjects.size() != 1)
			{
				PayloadName = "OTHR";
			}
			
			if (FilePath.xstring().ends_with(".dti"))
			{
				PayloadName += "#dti";
			}
			
			if (FilePath.xstring().ends_with(".rai"))
			{
				PayloadName += "#rai";
			}

			ImGui::SetDragDropPayload(PayloadName.c_str(), &Data, sizeof(DragDropData));
		}
		else 
		{
			Data.FileName = FilePath;
			ImGui::SetDragDropPayload("OTHR", &Data, sizeof(DragDropData));
		}
	}
	else
	{
		if (
				FilePath == ".." || 
				!InitFileName.IsDir ||
				FilePath.xstring().find_last_of("/\\") == xr_string::npos ||
				!ImGui::BeginDragDropSource()
		   )
		{
			return false;
		}

		Data.FileName = FilePath;
		ImGui::SetDragDropPayload("FLDR", &Data, sizeof(DragDropData));
	}

	xr_string LabelText = FilePath.has_extension() ? FileName.substr(0, FileName.length() - FilePath.extension().string().length()).c_str() : FileName.c_str();
	if (SelectedObjects.size() == 1) 
	{
		ImGui::ImageButton(FilePath.xfilename().c_str(), IconPtr->Icon->pSurface, BtnSize);
		ImGui::Text(LabelText.data());
	}
	else 
	{
		ImGui::ImageButton(FilePath.xfilename().c_str(), Icons["multi"].Icon->pSurface, BtnSize);
		ImGui::Text("%d objects", SelectedObjects.size());
	}
	
	ImGui::EndDragDropSource();
	return true; 
}

bool CContentView::DrawItemHelper(xr_path& FilePath, xr_string& FileName, const CContentView::FileOptData& InitFileName, CContentView::IconData* IconPtr, bool isSelected)
{
	if (!DrawContext(FilePath))
	{
		if (ViewMode == EViewMode::Tile && ImGui::IsItemHovered())
		{
			ImVec2 DrawHintPos = ImGui::GetMousePos() - ImGui::GetWindowPos() + ImVec2{ ImGui::GetScrollX(), ImGui::GetScrollY() };
			DrawHintPos.y -= 15;
			CurrentItemHint = { Platform::ANSI_TO_UTF8(FileName) ,DrawHintPos, true };
		}
	}

	if (!isSelected)
		AcceptDragDropAction(InitFileName);

	return BeginDragDropAction(FilePath, FileName, InitFileName, IconPtr);
}

bool CContentView::DrawItemN(const FileOptData& InitFileName, size_t& HorBtnIter, const size_t IterCount)
{
	if (InitFileName.File.empty())
		return false;

	const ImGuiStyle& style = ImGui::GetStyle();

	xr_path FilePath = InitFileName.File;
	xr_string FileName = FilePath.xfilename();
	bool inSelectedList = false;
	bool isClicked = false;

	bool isRenaming = RenameObject.Path == FilePath;

	if (FileName.empty())
		return false;

	if (ViewMode == EViewMode::List)
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	const ImVec2& CursorPos = ImGui::GetCursorPos();
	auto InvalidateLambda = [&FileName, this, &CursorPos, &HorBtnIter, IterCount]()
		{
			if (HorBtnIter != IterCount)
			{
				ImGui::SetCursorPosY(CursorPos.y);
				ImGui::SetCursorPosX(CursorPos.x + 15 + BtnSize.x);
				HorBtnIter++;
			}
			else
			{
				HorBtnIter = 0;
			}
		};
	int padding = 10;

	ImVec2 ImageSize = BtnSize;

	ImVec2 buttonSize
	{
		ImageSize.x + padding,
		ImageSize.y + (ViewMode == EViewMode::Tile ? padding + 10 : padding)
	};

	IconData* IconPtr = nullptr;

	if (InitFileName.IsDir)
	{
		IconPtr = &GetTexture("Folder");
	}
	else if (InitFileName.ISESect.size() > 0)
	{
		xr_string HackName = InitFileName.ISESect.c_str(); HackName += ".ise";
		IconPtr = &GetTexture(HackName.c_str());
	}
	else
		IconPtr = &GetTexture(FilePath);

	if (!IconPtr->Icon)
		return false;

	ImVec4* colors = ImGui::GetStyle().Colors;
	ImVec4 IconColor = IconPtr->UseButtonColor ? colors[ImGuiCol_CheckMark] : ImVec4(1, 1, 1, 1);

	xr_string ButtonId = "##";
	ButtonId += FileName;

	ImVec2 availableSpace = ImGui::GetContentRegionAvail();

	if (ViewMode == EViewMode::List || Contains(buttonSize))
	{
		if (!SelectedObjects.empty() && std::find(SelectedObjects.begin(), SelectedObjects.end(), FilePath) != SelectedObjects.end())
		{
			inSelectedList = true;
			ImGui::PushStyleColor(ImGuiCol_Button, colors[ImGuiCol_ButtonActive]);
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, colors[ImGuiCol_ButtonActive]);
			ImGui::PushStyleColor(ImGuiCol_ButtonActive, colors[ImGuiCol_ButtonActive]);
		}

		ImGui::BeginGroup();
		{
			if (ViewMode == EViewMode::List)
				ImGui::Dummy(ImVec2(0, padding / 2));
			
			if (ImGui::Button(ButtonId.c_str(), ImVec2(buttonSize.x, (isRenaming && ViewMode == EViewMode::Tile) ? buttonSize.y - 20: buttonSize.y)))
			//if (ImGui::Button(ButtonId.c_str(), ImVec2(buttonSize.x, buttonSize.y-20)))
			{
				ImGuiIO& io = ImGui::GetIO();

				if (!io.KeyCtrl)
				{
					SelectedObjects.clear();
				}

				if (inSelectedList && io.KeyCtrl)
					SelectedObjects.erase(std::find(SelectedObjects.begin(), SelectedObjects.end(), FilePath));
				else if (FileName != "..")
					SelectedObjects.push_back(FilePath);
			}

			isClicked = ImGui::IsMouseDoubleClicked(0) && ImGui::IsItemActive();

			DrawItemHelper(FilePath, FileName, InitFileName, IconPtr, inSelectedList);

			ImVec2 cursorPos = ImGui::GetItemRectMin();
			ImVec2 imagePos = ImVec2(
				cursorPos.x + (buttonSize.x - ImageSize.x) / 2,
				cursorPos.y + (ViewMode == EViewMode::Tile ? 0 : (buttonSize.y - ImageSize.y) / 2)
			);
			ImGui::SetCursorScreenPos(imagePos);
			
			if (std::find(CopiedObjects.begin(), CopiedObjects.end(), FilePath) != CopiedObjects.end() && IsCutting || ImGui::IsItemActive() &&
				ImGui::IsMouseDragging(ImGuiMouseButton_Left) && ImGui::GetDragDropPayload() != nullptr)
			{
				if (!inSelectedList)
				{
					if (SelectedObjects.size() != 0)
						SelectedObjects.clear();

					SelectedObjects.push_back(FilePath);
				}
				IconColor.w = 0.3;
			}

			ImGui::Image(IconPtr->Icon->pSurface, ImageSize, ImVec2(0, 0), ImVec2(1, 1), IconColor);

			/*
				Два варианта
					Во втором попытка уменьшить обращения к ImGui API увел. шагом проходки
			*/
			ImVec2 textSize;
			xr_string LabelText = FilePath.has_extension()
				? FileName.substr(0, FileName.length() - FilePath.extension().string().length())
				: FileName;
			if (!isRenaming)
			{
				if (ViewMode == EViewMode::Tile)
				{
#if 1
					textSize = ImGui::CalcTextSize(Platform::ANSI_TO_UTF8(LabelText).data());
					float textWidth = textSize.x;

					while (textWidth > buttonSize.x - padding)
					{
						LabelText = LabelText.substr(0, LabelText.length() - 4) + "..";
						textWidth = ImGui::CalcTextSize(Platform::ANSI_TO_UTF8(LabelText).data()).x;
					}
#else
					float maxTextWidth = buttonSize.x - padding;
					float textWidth = ImGui::CalcTextSize(LabelText.c_str()).x;

					if (textWidth > maxTextWidth)
					{
						size_t trimSize = LabelText.length();
						while (textWidth > maxTextWidth && trimSize > 2)
						{
							trimSize -= 2;
							LabelText = LabelText.substr(0, trimSize) + "..";
							textWidth = ImGui::CalcTextSize(LabelText.c_str()).x;
						}
					}

#endif
					textSize.x = textWidth;
				}
				else
					textSize = ImGui::CalcTextSize(Platform::ANSI_TO_UTF8(LabelText).data());
			}
			
			float TextPosY = ImageSize.y;

			xr_string ExtDescription = "";

			if (ViewMode == EViewMode::List)
			{
				if (InitFileName.IsDir)
				{
					ExtDescription = ExtDesc["_dir"];
				}
				else if (InitFileName.File.xstring().ends_with(".xml"))
				{
					xr_string FileName = InitFileName.File.xfilename();
					FileName = FileName.substr(0, FileName.size() - 4);

					auto Iter = std::find(GameDialogs.begin(), GameDialogs.end(), FileName);
					if (Iter != GameDialogs.end())
					{
						ExtDescription = ExtDesc["dialogs"];
					}
				}
				else if (FilePath.extension().string() == ".ltx")
				{
					xr_string PathName = FilePath;
					ExtDescription = (PathName.Contains("scripts\\") ? ExtDesc["_script_ltx"] : ExtDesc[".ltx"]) ;
				}
				else
					ExtDescription = ExtDesc[FilePath.extension().string().c_str()];

				TextPosY = ImageSize.y < 18 ? (buttonSize.y - textSize.y) / 2 : (padding)/2;
			}

			ImVec2 scrPos = ImVec2(
				cursorPos.x + (ViewMode == EViewMode::Tile ? (buttonSize.x - textSize.x) / 2 : padding * 2 + ImageSize.x),
				cursorPos.y + TextPosY
			);

			ImGui::SetCursorScreenPos(scrPos);


			if (isRenaming)
			{
				if (RenameObject.Active)
				{
					if (RenameObject.SetText)
					{
						RenameObject.SetText = false;
						RenameObject.RenameBuf = Platform::ANSI_TO_UTF8(LabelText);
						ImGui::SetKeyboardFocusHere();

						SelectedObjects.clear();
						SelectedObjects.push_back(FilePath);
					}

					ImGuiIO& io = ImGui::GetIO();

					if (ViewMode == EViewMode::Tile)
					{
						ImGui::SetCursorPosX(CursorPos.x);
						ImGui::SetNextItemWidth(buttonSize.x);
					}

					if (ImGui::InputText("##ren", RenameObject.RenameBuf.data(), 255, ImGuiInputTextFlags_EnterReturnsTrue))
						RenameObject.Active = false;

					if (io.KeysDown[ImGuiKey_Escape])
						RenameActionEnd();

					RenameObject.Focus = ImGui::IsItemHovered();
				}
				else
				{
					if (strcmp(Platform::ANSI_TO_UTF8(LabelText).c_str(), RenameObject.RenameBuf.c_str()))
						RenameAction(FilePath, RenameObject.RenameBuf.c_str());

					RenameActionEnd();
				}
			}
			else
			{
				//ImGui::SetItemAllowOverlap();
				ImGui::Text("%s", LabelText.c_str());

				if (ImGui::IsMouseReleased(0) && ImGui::IsItemHovered())
				{
					if (FilePath.xstring() != ".." && !FilePath.parent_path().empty() && !IsSpawnElement)
						RenameActionActivate(FilePath);
				}
			}

			if (ViewMode == EViewMode::List) {

				ImVec4 TooltipTextColor = ImGui::GetStyle().Colors[ImGuiCol_Text];
				TooltipTextColor.w *= 0.5f;

				ImGui::SetCursorScreenPos(ImVec2(scrPos.x, ImGui::GetCursorScreenPos().y));

				if (ExtDescription.empty())
				{
					
					ExtDescription = InitFileName.File.extension().string().c_str();
					ExtDescription += " File";
					
				}

				if (!isRenaming)
				{
					float inputHeight = ImGui::GetFrameHeight();
					float textHeight = ImGui::GetTextLineHeight();
					float textOffset = (inputHeight - textHeight) ;

					ImGui::SetCursorPosY(ImGui::GetCursorPosY() + textOffset);
				}

				ImGui::TextColored(TooltipTextColor, ExtDescription.c_str());

				ImGui::SetCursorScreenPos(ImVec2(
					cursorPos.x,
					cursorPos.y + buttonSize.y
				));
				ImGui::Dummy(ImVec2(0, padding/2));
				ImGui::Separator();
			}
		}
		ImGui::EndGroup();

		if (inSelectedList)
			ImGui::PopStyleColor(3);
	}
	else
	{
		ImGui::Button(ButtonId.c_str(), buttonSize);
		InvalidateLambda();
		return false;
	}

	if (ViewMode == EViewMode::Tile)
	{
		InvalidateLambda();

		if (availableSpace.x - ImGui::GetCursorPosX() - buttonSize.x > buttonSize.x + style.ItemSpacing.x)
			ImGui::SameLine();
	}
	else if (ViewMode == EViewMode::List)
		ImGui::PopStyleVar();

	return isClicked;
}

bool CContentView::Contains(const ImVec2& ButtonSize)
{
	float ScrollValue = ImGui::GetScrollY();
	float CursorPosY = ImGui::GetCursorPosY();

	bool IsNotAfter = CursorPosY < ScrollValue + ImGui::GetWindowSize().y;
	bool IsNotBefor = CursorPosY > ScrollValue - ButtonSize.y;
	return IsNotAfter && IsNotBefor;
}


bool CContentView::CheckFile(const xr_path& File) const
{
	bool TestTHM = IsThmMode || (File.has_extension() && File.extension().string() != ".thm");
	bool TestWinTrash = File.xfilename() != "desktop.ini";

	return TestTHM && TestWinTrash;
}

bool CContentView::DrawFormContext()
{
	if (!ImGui::BeginPopupContextItem("##contentbrowsercontext"))
	{
		return false;
	}

	ImGui::BeginDisabled(CopiedObjects.empty());
	if (ImGui::MenuItem("Paste"))
	{
		PasteAction(CurrentDir);
	}
	ImGui::EndDisabled();

	ImGui::Separator();

	if (ImGui::MenuItem("Create Folder"))
	{
		CreateAction();
	}

	ImGui::EndPopup();
	return true;
}

bool CContentView::DrawContext(const xr_path& Path)
{
	if (Path.xstring() == ".." || Path.parent_path().empty() || !ImGui::BeginPopupContextItem())
	{
		return false;
	}

	if (Path.has_extension() && Path.extension().string() == ".object")
	{
		if (ImGui::MenuItem("Open"))
		{
			CViewportMesh* MeshView = new CViewportMesh;
			MeshView->OpenModel(Path);

			UI->Push(MeshView);
		}
	}
	if (Path.has_extension() && Path.extension().string() == ".level")
	{
		if (ImGui::MenuItem("Open"))
		{
			UI->SetStatus("Level loading...");
			ExecCommand(COMMAND_CLEAR);
			FS.TryLoad(Path.xstring());
			IReader* R = FS.r_open(Path.xstring().c_str());
			if (!R)
			{
				ImGui::EndPopup();
				return false;
			}
			char ch;
			R->r(&ch, sizeof(ch));
			bool is_ltx = (ch == '[');
			FS.r_close(R);
			bool res;
			LTools->m_LastFileName = Path.xstring();

			if (is_ltx)
				Scene->LoadLTX(Path.xstring().c_str(), false);
			else
				Scene->Load(Path.xstring().c_str(), false);
		}
		ImGui::Separator();
	}

	if (Path.has_extension() && Path.extension().string() == ".wav")
	{
		if (ImGui::MenuItem("Open"))
		{
			ExecCommand(COMMAND_SOUND_EDITOR, xr_path(Path.stem()).xstring());
		}

		ImGui::Separator();
	}


	if (ImGui::MenuItem("Cut"))
	{
		if (SelectedObjects.empty())
			SelectedObjects.push_back(Path);

		CutAction();
	}

	if (ImGui::MenuItem("Copy"))
	{
		if (SelectedObjects.empty())
			SelectedObjects.push_back(Path);

		CopyAction();
	}

	if (ImGui::MenuItem("Rename"))
	{
		RenameActionActivate(Path);
	}

	if (ImGui::MenuItem("Delete"))
	{
		if (SelectedObjects.empty())
		{
			DeleteAction(Path);
		}
		else
		{
			for (const xr_path& obj : SelectedObjects)
			{
				DeleteAction(obj);
			}
			SelectedObjects.clear();
		}
	}

	ImGui::Separator();

	if (ImGui::BeginMenu("Properties"))
	{
		if (ImGui::MenuItem("Change icon"))
		{
			ExecCommand(COMMAND_ICON_PICKER, Path.xstring());
		}

		ImGui::EndMenu();
	}

	bool ShowConvert = false;

	const xr_set<xr_string> supportedExtensionsConvert = { ".dds", ".tga", ".png", ".wav" };

	if (Path.has_extension() && supportedExtensionsConvert.count(xr_path(Path.extension()).xstring()) > 0)
	{
		ImGui::Separator();

		if (auto ex = xr_path(Path.extension()).xstring(); ImGui::BeginMenu("Convert"))
		{
			if ((ex == ".dds" || ex == ".png") && ImGui::MenuItem("TGA"))
			{
				xr_path OutFile = Path;
				OutFile.replace_extension(".tga");

				DXTUtils::Converter::MakeTGA(Path, OutFile);

			}

			if ((ex == ".dds" || ex == ".tga") && ImGui::MenuItem("PNG"))
			{
				xr_path OutFile = Path;
				OutFile.replace_extension(".png");

				DXTUtils::Converter::MakePNG(Path, OutFile);
			}

			if (ex == ".wav" && ImGui::MenuItem("OGG"))
			{
				xr_path OutFile = Path;
				OutFile.replace_extension(".ogg");
				
				xr_string stem = xr_path(Path.stem());
				ESoundThumbnail* pTHM = new ESoundThumbnail(stem.c_str());
				
				SndLib->MakeGameSound(pTHM, Path.xstring().c_str(), OutFile.xstring().c_str());
				xr_delete(pTHM);
			}

			ImGui::EndMenu();
		}
	}

	ImGui::EndPopup();
	return true;
}

CContentView::IconData & CContentView::GetTexture(const xr_string & IconPath)
{
	if (Icons.contains(IconPath))
		return Icons[IconPath];

	if (IconPath.Contains(".~"))
		return Icons["backup"];

	if (IconPath.ends_with(".ltx"))
		return Icons["thm"];
	
	if (IconPath.ends_with(".ogg"))
		return Icons["ogg"];
	
	if (IconPath.ends_with(".level"))
		return Icons["level"];
	
	if (IconPath.ends_with(".wav"))
		return Icons["wav"];
	
	if (IconPath.ends_with(".xml"))
	{
		xr_string FileName = xr_path(IconPath).xfilename();
		FileName = FileName.substr(0, FileName.size() - 4);

		auto Iter = std::find(GameDialogs.begin(), GameDialogs.end(), FileName);
		if (Iter != GameDialogs.end())
		{
			return Icons["dialogs"];
		}
	}

	if (IconPath.ends_with(".seq"))
		return Icons["seq"];

	if (IconPath.ends_with(".dll"))
		return Icons["dll"];

	if (IconPath.ends_with(".exe"))
		return Icons["exe"];

	if (IconPath.ends_with("$") && IconPath.starts_with("$"))
		return Icons["Folder"];

	if (IconPath.ends_with(".cmd") ||
		IconPath.ends_with(".bat"))
		return Icons["cmd"];
	
	if (IconPath.Contains(LogsDir))
		return Icons["logs"];

	if (!Icons.contains(IconPath))
	{
		if (IconPath.ends_with(".ise"))
		{
			if (IconPath == "$env_mod.ise")
			{
				return Icons["env_mod"];
			}

			ESceneSpawnTool* SpTool = (ESceneSpawnTool*)Scene->GetTool(OBJCLASS_SPAWNPOINT);
			xr_string ValidPath = IconPath;
			ValidPath = ValidPath.erase(ValidPath.length() - 4);

			if (pSettings->line_exist(ValidPath.data(), "$ed_icon"))
			{
				Icons[IconPath] = { EDevice->Resources->_CreateTexture(pSettings->r_string_wb(ValidPath.data(), "$ed_icon").c_str()), false };
				Icons[IconPath].Icon->Load();
			}
			else
			{
				Icons[IconPath] = Icons["file"];
			}
		}
		else if (IconPath.ends_with(".object"))
		{
			string_path fn = {};
			FS.update_path(fn, _objects_, fn);
			Icons[IconPath] = Icons["object"];

			if(IconPath.find(fn) != xr_string::npos) {
				xr_string NewPath = IconPath.substr(IconPath.find(fn) + xr_strlen(fn));

				EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(NewPath.data(), EImageThumbnail::ETObject);
				CTexture* TempTexture = new CTexture();
				m_Thm->Update(TempTexture->pSurface);

				if(TempTexture->pSurface != nullptr) {
					Icons[IconPath] = {TempTexture, false};
				}
				else {
					xr_delete(TempTexture);
				}
			}
		}
		else if (IconPath.ends_with(".group"))
		{
			string_path fn = {};
			FS.update_path(fn, _groups_, "");
			Icons[IconPath] = Icons["object"];

			if (IconPath.find(fn) != xr_string::npos) {
				xr_string NewPath = IconPath.substr(IconPath.find(fn) + xr_strlen(fn));

				EGroupThumbnail* m_Thm = new EGroupThumbnail(NewPath.data());
				//EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(NewPath.data(), EImageThumbnail::ETTexture);
				CTexture* TempTexture = new CTexture();
				m_Thm->Update(TempTexture->pSurface);

				if (TempTexture->pSurface != nullptr) {
					Icons[IconPath] = { TempTexture, false };
				}
				else {
					xr_delete(TempTexture);
				}
			}
		}
		else if (IconPath.ends_with(".png") || IconPath.ends_with(".tga"))
		{
			U8Vec Pixels = DXTUtils::GitPixels(IconPath.c_str(), BtnSize.x, BtnSize.y);
			if (!Pixels.empty())
			{
				CTexture* TempTexture = new CTexture();
				ID3DTexture2D* pTexture = nullptr;
				Icons[IconPath] = { TempTexture, false };
				R_CHK(REDevice->CreateTexture(BtnSize.x, BtnSize.x, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &pTexture, 0));
				{
					D3DLOCKED_RECT rect;
					R_CHK(pTexture->LockRect(0, &rect, 0, D3DLOCK_DISCARD));
					memcpy(rect.pBits, Pixels.data(), Pixels.size());
					R_CHK(pTexture->UnlockRect(0));

					TempTexture->pSurface = pTexture;
				}
			}
			else if (IconPath.ends_with(".tga"))
			{
				Icons[IconPath] = Icons["tga"];
			}
		}
		else if(IconPath.ends_with(".dds")) 
		{
			xr_string NewPath = IconPath.substr(0, IconPath.length() - 4);

			Icons[IconPath] = {EDevice->Resources->_CreateTexture(NewPath.c_str()), false};
			Icons[IconPath].Icon->Load();

			if(!Icons[IconPath].Icon->pSurface) {
				Icons[IconPath] = Icons["image"];
			}
		}
		else
		{
			Icons[IconPath] = Icons["file"];
		}
	}

	return Icons[IconPath];
}

xr_map<xr_string, CContentView::FileOptData> CContentView::ScanConfigs(const xr_string& StartPath)
{
	xr_map<xr_string, FileOptData> TempPath;
	CInifile::Root& data = ((CInifile*)pSettings)->sections();

	for (CInifile::RootIt it = data.begin(); it != data.end(); it++)
	{
		LPCSTR val;
		if ((*it)->line_exist("$spawn", &val))
		{
			shared_str caption = pSettings->r_string_wb((*it)->Name, "$spawn");
			shared_str sect = (*it)->Name;
			if (caption.size())
			{
				xr_string FileName = caption.c_str();

				if (!FileName.Contains(StartPath) && !StartPath.empty())
					continue;

				if (FileName == StartPath)
					continue;

				if (StartPath.empty())
				{
					size_t DirStart = FileName.find('\\');

					if (DirStart != xr_string::npos)
					{
						xr_string DirName = FileName.substr(0, DirStart);
						if (TempPath.contains(DirName))
							continue;

						TempPath[DirName] = { DirName.c_str(), true };
						continue;
					}
				}
				else
				{
					xr_string Delimer = StartPath;
					if (!Delimer.ends_with('\\'))
					{
						Delimer += '\\';
					}

					size_t DirStart = FileName.find(Delimer);

					if (DirStart != xr_string::npos)
					{
						xr_string DirName = FileName.substr(DirStart + Delimer.length());
						if (TempPath.contains(DirName))
							continue;

						int DirIter = DirName.find('\\');
						if (DirIter != xr_string::npos)
						{
							xr_string ExtractedDirName = DirName.substr(0, DirIter);
							TempPath[ExtractedDirName] = { ExtractedDirName.c_str(), true };
						}
						else
						{
							TempPath[DirName] = { (DirName + ".ise").c_str(), false, sect };
						}
					}

					continue;
				}

				TempPath[FileName] = { (FileName + ".ise").c_str(), false, sect };
			}
		}
	}

	return std::move(TempPath);
}

#pragma region ObjectActions

void CContentView::CheckFileNameRecursive(xr_path& FilePath, const xr_string& postfix) const
{
	xr_path NewFileName = FilePath.stem();
	NewFileName += " - ";
	NewFileName += postfix.c_str();
	NewFileName += FilePath.extension();

	FilePath.replace_filename(NewFileName);
	if (std::filesystem::exists(FilePath))
	{
		CheckFileNameRecursive(FilePath, postfix);
	}

	return;
}

void CContentView::PasteAction(const xr_string& Path) /*const*/
{
	xr_path OutDir;
	for (auto obj : CopiedObjects)
	{
		OutDir = ((Path == "..") ? CurrentDir / xr_path(Path) : xr_path(Path)) / obj.xfilename().c_str();

		if (obj == OutDir || std::filesystem::exists(OutDir))
		{
			if (IsCutting)
				continue;

			CheckFileNameRecursive(OutDir, "Copy");
		}

		if (std::filesystem::is_directory(obj))
		{
			std::filesystem::copy(obj, OutDir, std::filesystem::copy_options::recursive);
		}
		else
		{
			std::filesystem::copy(obj, OutDir);

			if (auto ThmFile = obj; ShouldTheFileHaveTHM(obj) &&
				ThmFile.extension() != ".thm" && std::filesystem::exists(ThmFile.replace_extension(".thm")))
			{
				std::filesystem::copy(ThmFile, OutDir.replace_extension(".thm"));
			}
		}
		if (IsCutting)
			DeleteAction(obj);
	}

	if (IsCutting)
		IsCutting = false;

	CopiedObjects.clear();
	FS.rescan_path(OutDir.parent_path().string().c_str(), true);
}

void CContentView::DeleteAction(const xr_path& Path) /*const*/
{
	if (std::filesystem::is_directory(Path))
	{
		std::filesystem::remove_all(Path);
	}
	else
	{
		std::filesystem::remove(Path);

		if (auto ThmFile = Path; ShouldTheFileHaveTHM(Path) && 
			Path.extension() != ".thm" && std::filesystem::exists(ThmFile.replace_extension(".thm")))
		{
			std::filesystem::remove(ThmFile);
		}
	}

	// For some reason, FS does not want to register that the file has been deleted. \
				Temporarily removed the "const" and made the Rescan Directory();

		//FS.rescan_path(Path.parent_path().string().c_str() , true);
	RescanDirectory();
}

void CContentView::CopyAction(/*const xr_path& Path*/)
{
	CopiedObjects = SelectedObjects;
	IsCutting = false;
}

void CContentView::CutAction(/*const xr_path& Path*/) 
{
	CopyAction();
	IsCutting = true;
}

void CContentView::RenameAction(const xr_path& FilePath, const xr_string NewName)
{
	xr_path TempFileName = xr_path(FilePath).replace_filename(std::filesystem::path(std::tmpnam(nullptr)).stem());
	xr_path NewFileName = FilePath;
	NewFileName.replace_filename(NewName.c_str());
	NewFileName.replace_extension(FilePath.extension());

	std::filesystem::rename(FilePath, TempFileName);

	if (std::filesystem::exists(NewFileName))
	{
		CheckFileNameRecursive(NewFileName, "Copy");
	}

	std::filesystem::rename(TempFileName, NewFileName);

	if (!std::filesystem::is_directory(NewFileName) && ShouldTheFileHaveTHM(FilePath))
	{
		if (auto ThmFile = FilePath; ThmFile.extension() != ".thm" && std::filesystem::exists(ThmFile.replace_extension(".thm")))
		{
			std::filesystem::rename(ThmFile, NewFileName.replace_extension(".thm"));
		}
	}

	FS.rescan_path(NewFileName.parent_path().string().c_str(), true);

}

void CContentView::RenameActionActivate(const xr_path& Path)
{
	RenameObject.SetText = true;
	RenameObject.Focus = false;
	RenameObject.Active = true;
	RenameObject.RenameBuf.clear();
	RenameObject.Path = Path;
}

void CContentView::RenameActionEnd()
{
	RenameObject.Active = false;
	RenameObject.RenameBuf.clear();
	RenameObject.Path.clear();
}

bool CContentView::ShouldTheFileHaveTHM(const xr_path& file) const
{
	if (!file.has_extension())
		return false;

	if (auto e = file.extension();
		e == ".group" || e == ".object" || e == ".dds" || e == ".tga")
		return true;

	return (false);
}

void CContentView::CreateAction() /*const*/
{
	xr_path OutDir = CurrentDir / xr_path("Folder");

	if (std::filesystem::exists(OutDir))
	{
		CheckFileNameRecursive(OutDir, "New");
	}

	std::filesystem::create_directory(OutDir);

	RenameActionActivate(OutDir);

	// For some reason, FS does not want to register that the file has been deleted. \
				Temporarily removed the "const" and made the Rescan Directory();
	RescanDirectory();
}
#pragma endregion
