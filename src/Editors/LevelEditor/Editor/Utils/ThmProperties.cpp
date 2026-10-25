#include "stdafx.h"
#include "ThmProperties.h"

#include "../xrECore/Editor/EThumbnail.h"

CUIThmProperties::CUIThmProperties()
{
	bOpen = false;
}

void CUIThmProperties::Load(const xr_path& File)
{
	xr_delete(ThmPtr);
	FileName = File.xstring();
	mProps.ClearProperties();
	HasPreview = false;

	IReader* F = FS.r_open(FileName.c_str());
	R_ASSERT(F->find_chunk(THM_CHUNK_TYPE));
	ECustomThumbnail::THMType FileType = ECustomThumbnail::THMType(F->r_u32());
	F->close();

	PropItemVec Vec;
	auto LoadThmInfoLambda = [&]<class T>(T*)
	{
		ThmPtr = CreateThumbnail(FileName.c_str(), FileType);
		T* ThmPtrCasted = (T*)ThmPtr;
		
		ThmPtrCasted->Load(FileName.c_str(), "");

		if constexpr (std::is_same_v<T, ETextureThumbnail>)
		{
			ThmPtrCasted->FillProp(Vec, &CUIThmProperties::OnPropChange);
			HasPreview = ThmPtrCasted->IsClass(ECustomThumbnail::ETTexture);
		}
		else
		{
			ThmPtrCasted->FillProp(Vec);
		}
	};

	switch (FileType)
	{
	case ECustomThumbnail::THMType::ETGroup:	LoadThmInfoLambda((EGroupThumbnail*)nullptr); break;
	case ECustomThumbnail::THMType::ETObject:	LoadThmInfoLambda((EObjectThumbnail*)nullptr); break;
	case ECustomThumbnail::THMType::ETSound:
	case ECustomThumbnail::THMType::ETTexture:	LoadThmInfoLambda((ETextureThumbnail*)nullptr); break;
	default:									Msg("! Unsupported thm file!");
	}

	mProps.AssignItems(Vec);

	if (HasPreview)
	{
		Preview.SetTexture(CMaterialPreview::GetTextureName(FileName));
	}
	else
	{
		Preview.Clear();
	}
}

void CUIThmProperties::Show()
{
	bOpen = true;
	ImGui::SetWindowFocus("Thumbnail View");
}

void CUIThmProperties::Draw()
{
	if (!bOpen)
		return;

	BeginDraw();

	if (ImGui::Begin("Thumbnail View", &bOpen))
	{
		ImGui::Text(FileName.c_str());
		ImGui::Separator();

		if (HasPreview && ThmPtr != nullptr)
		{
			Preview.Draw(((ETextureThumbnail*)ThmPtr)->_Format(), ImVec2(0.f, PreviewHeight));

			// Splitter between the preview and the properties
			ImGui::InvisibleButton("##PreviewSplitter", ImVec2(-1.f, 6.f));
			if (ImGui::IsItemActive())
			{
				PreviewHeight = clampr(PreviewHeight + ImGui::GetIO().MouseDelta.y, 64.f, std::max(64.f, ImGui::GetWindowHeight() - 200.f));
			}
			if (ImGui::IsItemHovered() || ImGui::IsItemActive())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNS);
			}
		}

		ImVec2 WndSize = ImGui::GetContentRegionAvail();
		WndSize.y -= ImGui::GetFrameHeightWithSpacing() + ImGui::GetStyle().ItemSpacing.y;
		if (ImGui::BeginChild("#TVHelper", WndSize))
		{
			mProps.Draw();
		}
		ImGui::EndChild();
	}
	ImGui::Separator();

	if (ImGui::Button("Save"))
	{
		ThmPtr->Save(0, "");
		FS.set_file_age(FileName.c_str(), xr_chrono_to_time_t(std::chrono::system_clock::now()));
	}
	ImGui::End();

	EndDraw();
}

void CUIThmProperties::OnPropChange(PropValue* prop)
{
}
