#include "stdafx.h"
#include "ContentView.h"

CContentView::CContentView()
{
	string_path Dir = {};
	FS.update_path(Dir, "$fs_root$", "");

	CurrentDir = Dir;
	RootDir = Dir;

	FS.update_path(Dir, "$logs$", "");
	LogsDir = Dir;
}

void CContentView::Draw()
{
	if (ImGui::Begin("Content Browser"))
	{
		const size_t IterCount = (ImGui::GetWindowSize().x / (BtnSize.x + 15)) - 1;
		size_t HorBtnIter = 0;
		xr_string NextDir = CurrentDir;

		if (CurrentDir != RootDir)
		{
			std::filesystem::path FilePath = CurrentDir.c_str();
			if (DrawItem("..", HorBtnIter, IterCount))
			{
				NextDir = FilePath.parent_path().string().data();
			}
		}

		for (std::filesystem::path FilePath : std::filesystem::directory_iterator{ CurrentDir.data() })
		{
			if (std::filesystem::is_directory(FilePath))
			{
				if (DrawItem(FilePath.string().c_str(), HorBtnIter, IterCount))
				{
					NextDir = FilePath.string().data();
				}
			}
		}

		for (std::filesystem::path FilePath : std::filesystem::directory_iterator{ CurrentDir.data() })
		{
			if (!std::filesystem::is_directory(FilePath) && FilePath.has_extension() && FilePath.extension().string() != ".thm")
			{
				DrawItem(FilePath.string().c_str(), HorBtnIter, IterCount);
			}
		}

		CurrentDir = NextDir;
	}
	ImGui::End();
}

void CContentView::Destroy()
{
	Icons.clear();
}

void CContentView::Init()
{
	Icons["Folder"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\folder");
	Icons[".."] = EDevice->Resources->_CreateTexture("ed\\content_browser\\folder");
	Icons["thm"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\thm");
	Icons["logs"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\log");
	Icons["ogg"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\ogg");
	Icons["wav"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\wav");
	Icons["object"] = EDevice->Resources->_CreateTexture("ed\\content_browser\\object");
}

bool CContentView::DrawItem(const xr_string& InitFileName, size_t& HorBtnIter, const size_t IterCount)
{
	std::filesystem::path FilePath = InitFileName.c_str();
	float YPos = ImGui::GetCursorPosY();
	float XPos = ImGui::GetCursorPosX();

	xr_string FileName = FilePath.filename().string().data();

	ref_texture& Icon = std::filesystem::is_directory(FilePath) ? GetTexture("Folder") : GetTexture(FilePath.string().data());
	bool OutValue = ImGui::ImageButton(FilePath.filename().string().c_str(), Icon->pSurface, BtnSize);

	xr_string LabelText = FilePath.has_extension() ? FileName.substr(0, FileName.length() - FilePath.extension().string().length() - 1).c_str() : FileName.c_str();
	//ImGui::Text(LabelText.data());
	if (ImGui::BeginDragDropSource())
	{
		Data.FileName = FilePath.string().c_str();
		ImGui::SetDragDropPayload("TEST", &Data, sizeof(DragDropData));
		ImGui::ImageButton(FilePath.filename().string().c_str(), Icon->pSurface, BtnSize);
		ImGui::Text(LabelText.data());
		ImGui::EndDragDropSource();
	}
	else
	{
		float TextPixels = ImGui::CalcTextSize(LabelText.data()).x;

		while (TextPixels > BtnSize.x)
		{
			LabelText = LabelText.substr(0, LabelText.length() - 4) + "..";
			TextPixels = ImGui::CalcTextSize(LabelText.data()).x;
		}

		ImGui::SetCursorPosX(XPos + (((10 + BtnSize.x) - TextPixels) / 2));
		ImGui::Text(LabelText.data());
	}

	if (HorBtnIter != IterCount)
	{
		ImGui::SetCursorPosY(YPos);
		ImGui::SetCursorPosX(XPos + 15 + BtnSize.x);
		HorBtnIter++;
	}
	else
	{
		HorBtnIter = 0;
	}
	return OutValue;
}

ref_texture& CContentView::GetTexture(const xr_string& IconPath)
{
	if (IconPath.ends_with(".ltx"))
		return Icons["thm"];
	
	if (IconPath.ends_with(".ogg"))
		return Icons["ogg"];
	
	if (IconPath.ends_with(".wav"))
		return Icons["wav"];
	
	if (IconPath.Contains(LogsDir))
		return Icons["logs"];

	if (!Icons.contains(IconPath))
	{
		if (IconPath.ends_with(".object"))
		{
			string_path fn = {};
			FS.update_path(fn, _objects_, fn);
			xr_string NewPath = IconPath.substr(IconPath.find(fn) + xr_strlen(fn));
			//NewPath = NewPath.substr(0, NewPath.length() - 4);

			EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(NewPath.data(), EImageThumbnail::ETObject);
			CTexture* TempTexture = new CTexture();
			m_Thm->Update(TempTexture->pSurface);

			if (TempTexture->pSurface == nullptr)
			{
				xr_delete(TempTexture);
				Icons[IconPath] = Icons["object"];
			}
			else
			{
				Icons[IconPath] = TempTexture;
			}
		}
		else if (IconPath.ends_with(".dds"))
		{
			//size_t Iter = IconPath.find(GamedataTextureDir);
			//xr_string NewPath = IconPath.substr(Iter + GamedataTextureDir.length());
			xr_string NewPath = IconPath.substr(0, NewPath.length() - 4);
			Icons[IconPath] = EDevice->Resources->_CreateTexture(NewPath.c_str());
			Icons[IconPath]->Load();
		}
		else
		{
			Icons[IconPath] = EDevice->Resources->_CreateTexture(IconPath.c_str());
		}
	}

	return Icons[IconPath];
}
