#include "stdafx.h"
#include "UIUVView.h"
#include "../xrECore/Editor/EditMesh.h"
#include "../xrECore/Editor/EditObject.h"

CUIUVView::CUIUVView()
{
	SelectedObject = nullptr;
	CurrentSurface = nullptr;
	Zoom = 1.0f;
	PanOffset = ImVec2(0, 0);
	NeedUpdate = false;
	bOpen = false;

	Texture = new CTexture;
}

CUIUVView::~CUIUVView()
{
}

void CUIUVView::Show(bool State)
{
	bOpen = State;
}

void CUIUVView::SetSurface(CSurface* surf, CEditableObject* obj)
{
	if (CurrentSurface == surf && SelectedObject == obj)
		return;

	CurrentSurface = surf;
	SelectedObject = obj;
	NeedUpdate = true;
	Zoom = 1.0f;
	PanOffset = ImVec2(0, 0);
}

void CUIUVView::UpdateTexture()
{
	if (!CurrentSurface || !SelectedObject)
	{
		return;
	}

	if (CurrentSurface->_Texture() == nullptr)
	{
		return;
	}

	xr_string TexName = CurrentSurface->_Texture();
	TexName += ".dds";

	string_path FullPath = {};
	FS.update_path(FullPath, _game_textures_, TexName.c_str());

	SrcData = DXTUtils::GitPixels(FullPath);
	if (SrcData.W == 0 || SrcData.H == 0)
	{
		return;
	}

	RHITextureDesc Desc;
	Desc.Width = SrcData.W;
	Desc.Height = SrcData.H;
	Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

	xr_vector<u8> Pixels(SrcData.W * SrcData.H * 4);
	for (size_t Y = 0; Y < SrcData.H; ++Y)
	{
		for (size_t X = 0; X < SrcData.W; ++X)
		{
			size_t Idx = (Y * SrcData.W + X) * 4;
			Pixels[Idx + 0] = SrcData.P[Idx + 2]; // B
			Pixels[Idx + 1] = SrcData.P[Idx + 1]; // G
			Pixels[Idx + 2] = SrcData.P[Idx + 0]; // R
			Pixels[Idx + 3] = SrcData.P[Idx + 3]; // A
		}
	}

	RHISubResource SubResource{};
	SubResource.DataSize = SrcData.W * 4;
	SubResource.Data = Pixels.data();

	IRHISurface* Surf = GRHI->CreateTexture2D(Desc, SubResource);
	Texture->surface_set(Surf);
	Surf->Release();

	NeedUpdate = false;
}

void CUIUVView::Draw()
{
	if (!bOpen)
	{
		return;
	}

	if (NeedUpdate)
	{
		UpdateTexture();
	}

	ImGui::SetNextWindowSizeConstraints(ImVec2(400, 300), ImVec2(FLT_MAX, FLT_MAX));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));

	if (ImGui::Begin("UV View", &bOpen))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_ChildRounding, 0.0f);
		if (ImGui::BeginChild("##UVViewTopPanel", {0, 26}))
		{
			ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(4, 0));
			ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 0));

			ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
			ImGui::Text("Zoom:");
			ImGui::SameLine();
			ImGui::SetNextItemWidth(120.0f);
			ImGui::SliderFloat("##UVZoom", &Zoom, 0.1f, 10.0f, "%.1f");

			ImGui::SameLine(0, 10.0f);
			if (ImGui::Button("Reset", ImVec2(50, 20)))
			{
				Zoom = 1.0f;
				PanOffset = ImVec2(0, 0);
			}

			if (CurrentSurface)
			{
				ImGui::SameLine(0, 10.0f);
				ImGui::Text("Surface: %s", CurrentSurface->_Name());
				ImGui::SameLine(0, 10.0f);
				ImGui::Text("Texture: %s", CurrentSurface->_Texture());
			}

			ImGui::PopStyleVar(2);
		}
		ImGui::EndChild();
		ImGui::PopStyleVar();

		ImVec2 Avail = ImGui::GetContentRegionAvail();
		ImVec2 CanvasPos = ImGui::GetCursorScreenPos();

		ImDrawList* BgDrawList = ImGui::GetWindowDrawList();
		BgDrawList->AddRectFilled(CanvasPos, ImVec2(CanvasPos.x + Avail.x, CanvasPos.y + Avail.y), IM_COL32(40, 40, 40, 255));

		if (Texture && Texture->get_SRView() != nullptr && Texture->get_SRView()->GetRawSRV())
		{
			ImVec2 TexSize((float)SrcData.W, (float)SrcData.H);

			float ScaleX = Avail.x / TexSize.x;
			float ScaleY = Avail.y / TexSize.y;
			float BaseScale = std::min(ScaleX, ScaleY);

			// Apply zoom
			float CurrentScale = BaseScale * Zoom;

			ImVec2 DrawSize(TexSize.x * CurrentScale, TexSize.y * CurrentScale);
			ImVec2 CenterOffset((Avail.x - DrawSize.x) * 0.5f, (Avail.y - DrawSize.y) * 0.5f);

			// Apply pan offset (scaled with zoom)
			ImVec2 DrawOrigin(CanvasPos.x + CenterOffset.x + PanOffset.x, CanvasPos.y + CenterOffset.y + PanOffset.y);

			ImGui::SetCursorScreenPos(DrawOrigin);
			ImGui::Image((void*)Texture->get_SRView()->GetRawSRV(), DrawSize);

			ImDrawList* DrawList = ImGui::GetWindowDrawList();
			DrawUVWireframe(DrawList, DrawOrigin, DrawSize, CurrentScale, TexSize);
		}
		else
		{
			ImGui::SetCursorScreenPos(CanvasPos);
			ImGui::Dummy(Avail);
			ImDrawList* DrawList = ImGui::GetWindowDrawList();
			DrawList->AddText(ImVec2(CanvasPos.x + 10, CanvasPos.y + 10), IM_COL32(200, 200, 200, 255), "No texture loaded. Select a surface in Object Properties.");
		}
	}
	ImGui::End();
	ImGui::PopStyleVar();
}

void CUIUVView::DrawUVWireframe(ImDrawList* DrawList, const ImVec2& Origin, const ImVec2& DrawSize, float scale, const ImVec2& TexSize)
{
	if (!SelectedObject || !CurrentSurface)
	{
		return;
	}

	const ImU32 WireColor = IM_COL32(0, 255, 0, 200);
	const float LineWidth = 1.0f;

	for (CEditableMesh* Mesh : SelectedObject->Meshes())
	{
		const SurfFaces& SurfFaces = Mesh->GetSurfFaces();
		auto Iter = SurfFaces.find(CurrentSurface);
		if (Iter == SurfFaces.end())
		{
			continue;
		}

		const IntVec& FaceList = Iter->second;
		for (int FaceIdx : FaceList)
		{
			const Fvector2* tc[3];
			Mesh->GetFaceTC(FaceIdx, tc);

			for (int Kord = 0; Kord < 3; ++Kord)
			{
				const Fvector2& UV0 = *tc[Kord];
				const Fvector2& UV1 = *tc[(Kord + 1) % 3];

				ImVec2 Point0(Origin.x + (UV0.x * DrawSize.x), Origin.y + (UV0.y * DrawSize.y));
				ImVec2 Point1(Origin.x + (UV1.x * DrawSize.x), Origin.y + (UV1.y * DrawSize.y));

				DrawList->AddLine(Point0, Point1, WireColor, LineWidth);
			}
		}
	}
}