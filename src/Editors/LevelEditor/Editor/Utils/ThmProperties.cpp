#include "stdafx.h"
#include "ThmProperties.h"

#include "../xrECore/Editor/EThumbnail.h"

void CUIThmProperties::Load(const xr_path& File)
{
	xr_delete(ThmPtr);
	FileName = File.xstring();
	mProps.ClearProperties();

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
}

void CUIThmProperties::Show()
{
	bOpen = true;
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

		ImVec2 WndSize = ImGui::GetWindowSize();
		WndSize.y -= 100;
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
	}
	ImGui::End();

	EndDraw();
}

void CUIThmProperties::OnPropChange(PropValue* prop)
{
}
