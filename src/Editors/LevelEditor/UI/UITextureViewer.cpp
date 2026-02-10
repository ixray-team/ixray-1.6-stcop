#include "stdafx.h"
#include "UITextureViewer.h"
#include "../../xrEUI/IconsFontAwesome6.h"

CUITextureViewer::CUITextureViewer()
{
	Texture = new CTexture;
	Zoom = 1.0f;
}

CUITextureViewer::~CUITextureViewer()
{
}

void CUITextureViewer::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImGui::SetNextWindowSizeConstraints(ImVec2(650, 512), ImVec2(FLT_MAX, FLT_MAX));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	if (ImGui::Begin(("Texture Viewer##" + CurrentFileName).c_str(), &bOpen))
	{
		DrawView();
	}
	ImGui::End();
	ImGui::PopStyleVar();

}

void CUITextureViewer::DrawView()
{
	static const ImVec2 buttonSize(22, 22);

	auto DrawButtonLambda = [&](const char* label, u8 bit, ImVec4 color)
	{
		bool active = (ChannelMask & bit) != 0;
		ImVec4 col = active ? color : ImVec4(0.3f, 0.3f, 0.3f, 1.0f);
		ImVec4 textCol = active ? ImVec4(0, 0, 0, 1) : ImVec4(1, 1, 1, 1);

		ImGui::PushStyleColor(ImGuiCol_Button, col);
		ImGui::PushStyleColor(ImGuiCol_Text, textCol);
		if (ImGui::Button(label, buttonSize))
		{
			if (active) ChannelMask &= ~bit;
			else        ChannelMask |= bit;
			UpdateTexture();
		}
		ImGui::PopStyleColor(2);

		ImGui::SameLine(0.0f, 0.0f);
	};

	ImGui::PushStyleVar(ImGuiStyleVar_ChildRounding, 0.0f);
	if (ImGui::BeginChild("##TextureViewerTopPanel", {0, 22}))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);
		DrawButtonLambda("R", Channel_R, ImVec4(1, 0, 0, 1));
		DrawButtonLambda("G", Channel_G, ImVec4(0, 1, 0, 1));
		DrawButtonLambda("B", Channel_B, ImVec4(0, 0, 1, 1));
		DrawButtonLambda("A", Channel_A, ImVec4(1, 1, 1, 1));

		ImVec4 col = GrayMode ? ImVec4(0.7f, 0.7f, 0.7f, 1.0f) : ImVec4(0.3f, 0.3f, 0.3f, 1.0f);
		ImGui::SameLine(0.0f, 10.0f);
		ImGui::PushStyleColor(ImGuiCol_Button, col);
		if (ImGui::Button("Gray", { 35, 22 }))
		{
			GrayMode = !GrayMode;
			UpdateTexture();
		}
		ImGui::PopStyleColor();

		ImGui::SameLine(0.0f, 10.0f);
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 1);

		float sliderHeight = 6.0f;

		// перед слайдером
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

		// просто задаём через SetNextItemWidth и потом меняем FrameHeight через Style
		ImGuiStyle& style = ImGui::GetStyle();
		float oldFrameHeight = style.FramePadding.y;
		style.FramePadding.y = sliderHeight * 0.5f;

		ImGui::SetNextItemWidth(100.0f);
		ImGui::SliderFloat("##Zoom", &Zoom, 0.1f, 5.0f, "%.1f");

		style.FramePadding.y = oldFrameHeight;
		ImGui::PopStyleVar(2);

		ImGui::SameLine();
		ImGui::Text(ICON_FA_MAGNIFYING_GLASS);
		ImGui::SameLine();
		ImGui::Text(*SrcData.Format);

		ImGui::SameLine();
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 2);
		if (!CurrentFileName.empty())
		{
			ImVec2 WindowPos = ImGui::GetWindowPos();
			ImVec2 WindowSize = ImGui::GetWindowSize();
			ImVec2 TextSize = ImGui::CalcTextSize(CurrentFileName.c_str());

			ImGui::SetCursorPosX(WindowSize.x - TextSize.x - 10);
			ImGui::Text(CurrentFileName.c_str());
		}
		ImGui::PopStyleVar();
	}
	ImGui::EndChild();
	ImGui::PopStyleVar();

	if (Texture && Texture->get_SRView()->GetRawSRV())
	{
		ImVec2 avail = ImGui::GetContentRegionAvail();
		ImVec2 textureSize((float)SrcData.W, (float)SrcData.H);

		textureSize.x *= Zoom;
		textureSize.y *= Zoom;

		if (textureSize.x > avail.x || textureSize.y > avail.y)
		{
			float scaleX = avail.x / textureSize.x;
			float scaleY = avail.y / textureSize.y;
			float scale = std::min(scaleX, scaleY);

			textureSize.x *= scale;
			textureSize.y *= scale;
		}

		ImVec2 pos = ImGui::GetCursorScreenPos();
		pos.x += (avail.x - textureSize.x) * 0.5f;
		pos.y += (avail.y - textureSize.y) * 0.5f;

		ImGui::SetCursorScreenPos(pos);
		ImGui::Image((void*)Texture->get_SRView()->GetRawSRV(), textureSize);
	}
}

void CUITextureViewer::LoadFromFile(const xr_path& File)
{
	CurrentFileName = File.xfilename();
	SrcData = DXTUtils::GitPixels(File.xstring().c_str());

	RHITextureDesc Desc;
	Desc.Width = SrcData.W;
	Desc.Height = SrcData.H;
	Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

	xr_vector<u8> Pixels(SrcData.W * SrcData.H * 4);
	for (size_t y = 0; y < SrcData.H; ++y)
	{
		for (size_t x = 0; x < SrcData.W; ++x)
		{
			size_t idx = (y * SrcData.W + x) * 4;
			Pixels[idx + 0] = SrcData.P[idx + 2]; // B
			Pixels[idx + 1] = SrcData.P[idx + 1]; // G
			Pixels[idx + 2] = SrcData.P[idx + 0]; // R
			Pixels[idx + 3] = SrcData.P[idx + 3]; // A
		}
	}

	RHISubResource SubResource{};
	SubResource.Width = SrcData.W;
	SubResource.Height = SrcData.H;
	SubResource.TextureFormat = Desc.Format;
	SubResource.RowPitch = SrcData.W * 4;
	SubResource.Data = Pixels.data();

	IRHISurface* Surf = GRHI->CreateTexture2D(Desc, SubResource);
	Texture->surface_set(Surf);
	Surf->Release();

	UpdateTexture();
}

void CUITextureViewer::UpdateTexture()
{
	if (!Texture || !Texture->pSurface)
	{
		return;
	}

	IRHISurface* Surf = Texture->pSurface;
	u32 Width = Surf->GetWidth();
	u32 Height = Surf->GetHeight();

	u8* Data = static_cast<u8*>(Surf->Lock(0, nullptr));
	if (!Data)
	{
		return;
	}

	bool FullMask = (ChannelMask == (Channel_R | Channel_G | Channel_B | Channel_A));

	for (size_t y = 0; y < Height; ++y)
	{
		u8* Row = Data + y * Width * 4;
		for (size_t x = 0; x < Width; ++x)
		{
			size_t idx = (y * Width + x) * 4;
			uint8_t r = SrcData.P[idx + 0];
			uint8_t g = SrcData.P[idx + 1];
			uint8_t b = SrcData.P[idx + 2];
			uint8_t a = SrcData.P[idx + 3];

			uint8_t R, G, B, A;

			if (FullMask)
			{
				R = b;
				G = g;
				B = r;
				A = a;
			}
			else if (GrayMode)
			{
				uint8_t v = 0;
				if (ChannelMask & Channel_R) v = b;
				if (ChannelMask & Channel_G) v = g;
				if (ChannelMask & Channel_B) v = r;
				if (ChannelMask & Channel_A) v = a;

				R = G = B = v;
				A = 255;
			}
			else
			{
				R = (ChannelMask & Channel_R) ? b : 0;
				G = (ChannelMask & Channel_G) ? g : 0;
				B = (ChannelMask & Channel_B) ? r : 0;
				A = (ChannelMask & Channel_A) ? a : 255;
			}

			Row[x * 4 + 0] = B;
			Row[x * 4 + 1] = G;
			Row[x * 4 + 2] = R;
			Row[x * 4 + 3] = A;
		}
	}

	Surf->Unlock();
}