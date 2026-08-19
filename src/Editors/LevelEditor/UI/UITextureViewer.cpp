#include "stdafx.h"
#include "UITextureViewer.h"
#include "../../xrEUI/IconsFontAwesome6.h"

CUITextureViewer::CUITextureViewer()
{
	Zoom = 1.0f;
}

CUITextureViewer::~CUITextureViewer()
{
	UI->DestroyImGuiTexture(EditorTexture);
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
			if (active)
			{
				ChannelMask &= ~bit;
			}
			else
			{
				ChannelMask |= bit;
			}
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
		if (ImGui::Button("Gray", {35, 22}))
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

	const ImTextureID TextureId = EditorTexture.IsValid()
									  ? UI->GetImGuiTexture(EditorTexture)
									  : nullptr;
	if (TextureId)
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
		ImGui::Image(TextureId, textureSize);
	}
}

void CUITextureViewer::LoadFromFile(const xr_path& File)
{
	CurrentFileName = File.xfilename();
	SrcData = DXTUtils::GitPixels(File.xstring().c_str());
	UpdateTexture();
}

void CUITextureViewer::UpdateTexture()
{
	if (SrcData.P.empty() || SrcData.W == 0 || SrcData.H == 0)
	{
		return;
	}

	const u32 Width = SrcData.W;
	const u32 Height = SrcData.H;
	xr_vector<u8> Pixels(
		static_cast<size_t>(Width) * Height * 4
	);
	const bool FullMask =
		ChannelMask == (Channel_R | Channel_G | Channel_B | Channel_A);

	for (size_t y = 0; y < Height; ++y)
	{
		for (size_t x = 0; x < Width; ++x)
		{
			const size_t Index = (y * Width + x) * 4;
			const u8 Red = SrcData.P[Index + 0];
			const u8 Green = SrcData.P[Index + 1];
			const u8 Blue = SrcData.P[Index + 2];
			const u8 Alpha = SrcData.P[Index + 3];

			u8 OutputRed = 0;
			u8 OutputGreen = 0;
			u8 OutputBlue = 0;
			u8 OutputAlpha = 255;

			if (FullMask)
			{
				OutputRed = Blue;
				OutputGreen = Green;
				OutputBlue = Red;
				OutputAlpha = Alpha;
			}
			else if (GrayMode)
			{
				u8 Value = 0;
				if (ChannelMask & Channel_R)
				{
					Value = Blue;
				}
				if (ChannelMask & Channel_G)
				{
					Value = Green;
				}
				if (ChannelMask & Channel_B)
				{
					Value = Red;
				}
				if (ChannelMask & Channel_A)
				{
					Value = Alpha;
				}

				OutputRed = Value;
				OutputGreen = Value;
				OutputBlue = Value;
			}
			else
			{
				OutputRed = (ChannelMask & Channel_R) ? Blue : 0;
				OutputGreen = (ChannelMask & Channel_G) ? Green : 0;
				OutputBlue = (ChannelMask & Channel_B) ? Red : 0;
				OutputAlpha = (ChannelMask & Channel_A) ? Alpha : 255;
			}

			Pixels[Index + 0] = OutputBlue;
			Pixels[Index + 1] = OutputGreen;
			Pixels[Index + 2] = OutputRed;
			Pixels[Index + 3] = OutputAlpha;
		}
	}

	// GPU resource принадлежит выбранному editor backend. Окно просмотра
	// больше не создаёт и не блокирует legacy D3D surface напрямую.
	(void)UI->UpdateImGuiTexture(
		EditorTexture,
		Pixels.data(),
		Width,
		Height,
		Width * 4,
		++TextureRevision,
		CurrentFileName.c_str(),
		EEditorTextureFormat::Bgra8Unorm
	);
}
