#include "stdafx.h"
#include "UITextureViewer.h"
#include "../../xrEUI/IconsFontAwesome7.h"

static constexpr float MinZoom = 0.05f;
static constexpr float MaxZoom = 32.f;
static constexpr float CheckerCell = 8.f;
static constexpr float PixelGridMinZoom = 4.f;

static void DrawChecker(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max)
{
	DrawList->AddRectFilled(Min, Max, IM_COL32(56, 56, 56, 255));

	int Row = 0;
	for (float Y = Min.y; Y < Max.y; Y += CheckerCell, ++Row)
	{
		for (float X = Min.x + ((Row & 1) ? CheckerCell : 0.f); X < Max.x; X += CheckerCell * 2.f)
		{
			DrawList->AddRectFilled({ X, Y }, { std::min(X + CheckerCell, Max.x), std::min(Y + CheckerCell, Max.y) }, IM_COL32(84, 84, 84, 255));
		}
	}
}

// Toggle button that shows its state with the channel color
static bool ToggleButton(const char* Label, bool Active, const ImVec4& ActiveColor, const char* Tooltip)
{
	ImVec4 Color = Active ? ActiveColor : ImGui::GetStyleColorVec4(ImGuiCol_FrameBg);
	ImVec4 Hovered = Color;
	Hovered.x = std::min(Hovered.x + 0.1f, 1.f);
	Hovered.y = std::min(Hovered.y + 0.1f, 1.f);
	Hovered.z = std::min(Hovered.z + 0.1f, 1.f);

	ImGui::PushStyleColor(ImGuiCol_Button, Color);
	ImGui::PushStyleColor(ImGuiCol_ButtonHovered, Hovered);
	ImGui::PushStyleColor(ImGuiCol_ButtonActive, Hovered);
	if (Active && ActiveColor.x + ActiveColor.y + ActiveColor.z > 1.5f)
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.f, 0.f, 0.f, 1.f));
	}
	else
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(Active ? ImGuiCol_Text : ImGuiCol_TextDisabled));
	}

	const float Size = ImGui::GetFrameHeight();
	const bool Pressed = ImGui::Button(Label, { Size, Size });
	ImGui::PopStyleColor(4);

	if (Tooltip != nullptr)
	{
		ImGui::SetItemTooltip("%s", Tooltip);
	}

	return Pressed;
}

static bool IconButton(const char* Label, const char* Tooltip)
{
	const float Size = ImGui::GetFrameHeight();
	const bool Pressed = ImGui::Button(Label, { Size, Size });
	ImGui::SetItemTooltip("%s", Tooltip);
	return Pressed;
}

static void StatusSeparator()
{
	ImGui::SameLine(0.f, 8.f);
	ImGui::TextDisabled("|");
	ImGui::SameLine(0.f, 8.f);
}

static void ToolbarSeparator()
{
	ImGui::SameLine();
	ImVec2 Pos = ImGui::GetCursorScreenPos();
	const float Height = ImGui::GetFrameHeight();
	ImGui::GetWindowDrawList()->AddLine({ Pos.x, Pos.y + 3.f }, { Pos.x, Pos.y + Height - 3.f }, ImGui::GetColorU32(ImGuiCol_Separator));
	ImGui::Dummy({ 1.f, Height });
	ImGui::SameLine();
}

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

	const xr_string Title = xr_string(ICON_FA_IMAGE " ") + CurrentFileName + "###TextureViewer" + CurrentFileName;

	ImGui::SetNextWindowSize(ImVec2(720, 640), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints(ImVec2(420, 320), ImVec2(FLT_MAX, FLT_MAX));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	if (ImGui::Begin(Title.c_str(), &bOpen))
	{
		DrawView();
	}
	ImGui::End();
	ImGui::PopStyleVar();
}

void CUITextureViewer::DrawToolbar()
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	const float Height = ImGui::GetFrameHeight() + Style.WindowPadding.y * 2.f;

	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(8.f, Style.WindowPadding.y));
	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImGui::GetStyleColorVec4(ImGuiCol_MenuBarBg));
	ImGui::BeginChild("##TextureViewerToolbar", { 0.f, Height }, ImGuiChildFlags_AlwaysUseWindowPadding, ImGuiWindowFlags_NoScrollbar);
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(2.f, Style.ItemSpacing.y));

	struct ChannelDesc
	{
		const char* Label;
		u8 Bit;
		ImVec4 Color;
		const char* Tooltip;
	};

	static const ChannelDesc Channels[] =
	{
		{ "R", Channel_R, ImVec4(0.86f, 0.24f, 0.24f, 1.f), "Red channel\nRight click: show only this channel" },
		{ "G", Channel_G, ImVec4(0.30f, 0.78f, 0.34f, 1.f), "Green channel\nRight click: show only this channel" },
		{ "B", Channel_B, ImVec4(0.27f, 0.47f, 0.92f, 1.f), "Blue channel\nRight click: show only this channel" },
		{ "A", Channel_A, ImVec4(0.85f, 0.85f, 0.85f, 1.f), "Alpha channel\nRight click: show only this channel" },
	};

	for (const ChannelDesc& Channel : Channels)
	{
		if (ToggleButton(Channel.Label, (ChannelMask & Channel.Bit) != 0, Channel.Color, Channel.Tooltip))
		{
			ChannelMask ^= Channel.Bit;
			UpdateTexture();
		}

		if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
		{
			ChannelMask = Channel.Bit;
			UpdateTexture();
		}

		ImGui::SameLine();
	}

	const ImVec4 Accent = ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive);
	if (ToggleButton(ICON_FA_CIRCLE_HALF_STROKE, GrayMode, Accent, "Grayscale: show the selected channel as gray"))
	{
		GrayMode = !GrayMode;
		UpdateTexture();
	}

	ImGui::SameLine();
	if (ToggleButton(ICON_FA_CHESS_BOARD, ShowChecker, Accent, "Checkerboard behind transparent pixels"))
	{
		ShowChecker = !ShowChecker;
	}

	ImGui::SameLine();
	if (ToggleButton(ICON_FA_BORDER_ALL, ShowPixelGrid, Accent, "Pixel grid (shown from 400% zoom)"))
	{
		ShowPixelGrid = !ShowPixelGrid;
	}

	ToolbarSeparator();

	if (IconButton(ICON_FA_MAGNIFYING_GLASS_MINUS, "Zoom out"))
	{
		SetZoom(Zoom / 1.25f);
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(56.f);
	float Percent = Zoom * 100.f;
	if (ImGui::DragFloat("##Zoom", &Percent, 1.f, MinZoom * 100.f, MaxZoom * 100.f, "%.0f%%", ImGuiSliderFlags_AlwaysClamp))
	{
		SetZoom(Percent / 100.f);
	}
	ImGui::SetItemTooltip("Zoom. Mouse wheel over the image zooms at the cursor");

	ImGui::SameLine();
	if (IconButton(ICON_FA_MAGNIFYING_GLASS_PLUS, "Zoom in"))
	{
		SetZoom(Zoom * 1.25f);
	}

	ImGui::SameLine();
	if (ToggleButton(ICON_FA_EXPAND, FitToView, Accent, "Fit to window (double click on the image)"))
	{
		FitToView = true;
	}

	ImGui::SameLine();
	if (ImGui::Button("1:1", { 0.f, ImGui::GetFrameHeight() }))
	{
		SetZoom(1.f);
		Pan = { 0.f, 0.f };
	}
	ImGui::SetItemTooltip("Actual pixels");

	ImGui::PopStyleVar();
	ImGui::EndChild();
	ImGui::PopStyleColor();
	ImGui::PopStyleVar();
}

void CUITextureViewer::DrawCanvas(const ImVec2& Size)
{
	if (!Texture || !Texture->pSurface || SrcData.W == 0 || SrcData.H == 0)
	{
		ImGui::Dummy(Size);
		return;
	}

	ImGuiIO& IO = ImGui::GetIO();
	const ImVec2 CanvasMin = ImGui::GetCursorScreenPos();
	const ImVec2 CanvasMax = { CanvasMin.x + Size.x, CanvasMin.y + Size.y };
	const ImVec2 CanvasCenter = { CanvasMin.x + Size.x * 0.5f, CanvasMin.y + Size.y * 0.5f };

	ImGui::InvisibleButton("##TextureViewerCanvas", Size, ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonMiddle);
	const bool IsHovered = ImGui::IsItemHovered();

	if (FitToView)
	{
		Zoom = std::clamp(std::min(Size.x / SrcData.W, Size.y / SrcData.H) * 0.95f, MinZoom, MaxZoom);
		Pan = { 0.f, 0.f };
	}

	if (IsHovered && IO.MouseWheel != 0.f)
	{
		const float NewZoom = std::clamp(Zoom * powf(1.2f, IO.MouseWheel), MinZoom, MaxZoom);
		// Keeps the image point under the cursor in place
		const ImVec2 Point = { (IO.MousePos.x - CanvasCenter.x - Pan.x) / Zoom, (IO.MousePos.y - CanvasCenter.y - Pan.y) / Zoom };
		Pan = { IO.MousePos.x - CanvasCenter.x - Point.x * NewZoom, IO.MousePos.y - CanvasCenter.y - Point.y * NewZoom };
		Zoom = NewZoom;
		FitToView = false;
	}

	if (ImGui::IsItemActive() && (ImGui::IsMouseDragging(ImGuiMouseButton_Left, 0.f) || ImGui::IsMouseDragging(ImGuiMouseButton_Middle, 0.f)))
	{
		Pan.x += IO.MouseDelta.x;
		Pan.y += IO.MouseDelta.y;
		FitToView = false;
	}

	if (IsHovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		FitToView = true;
	}

	const ImVec2 ImageSize = { SrcData.W * Zoom, SrcData.H * Zoom };
	const ImVec2 ImageMin = { floorf(CanvasCenter.x + Pan.x - ImageSize.x * 0.5f), floorf(CanvasCenter.y + Pan.y - ImageSize.y * 0.5f) };
	const ImVec2 ImageMax = { ImageMin.x + ImageSize.x, ImageMin.y + ImageSize.y };

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->PushClipRect(CanvasMin, CanvasMax, true);
	DrawList->AddRectFilled(CanvasMin, CanvasMax, IM_COL32(24, 24, 24, 255));

	const ImVec2 VisibleMin = { std::max(ImageMin.x, CanvasMin.x), std::max(ImageMin.y, CanvasMin.y) };
	const ImVec2 VisibleMax = { std::min(ImageMax.x, CanvasMax.x), std::min(ImageMax.y, CanvasMax.y) };
	if (ShowChecker && VisibleMin.x < VisibleMax.x && VisibleMin.y < VisibleMax.y)
	{
		DrawChecker(DrawList, VisibleMin, VisibleMax);
	}

	DrawList->AddImage((ImTextureID)Texture->get_SRView()->GetRawSRV(), ImageMin, ImageMax);

	// Denser lines would hide the pixels
	if (ShowPixelGrid && Zoom >= PixelGridMinZoom && VisibleMin.x < VisibleMax.x && VisibleMin.y < VisibleMax.y)
	{
		const ImU32 GridColor = IM_COL32(0, 0, 0, 110);
		const int FirstX = (int)ceilf((VisibleMin.x - ImageMin.x) / Zoom);
		const int LastX = (int)floorf((VisibleMax.x - ImageMin.x) / Zoom);
		for (int X = FirstX; X <= LastX; ++X)
		{
			const float LineX = floorf(ImageMin.x + X * Zoom);
			DrawList->AddLine({ LineX, VisibleMin.y }, { LineX, VisibleMax.y }, GridColor);
		}

		const int FirstY = (int)ceilf((VisibleMin.y - ImageMin.y) / Zoom);
		const int LastY = (int)floorf((VisibleMax.y - ImageMin.y) / Zoom);
		for (int Y = FirstY; Y <= LastY; ++Y)
		{
			const float LineY = floorf(ImageMin.y + Y * Zoom);
			DrawList->AddLine({ VisibleMin.x, LineY }, { VisibleMax.x, LineY }, GridColor);
		}
	}
	DrawList->AddRect({ ImageMin.x - 1.f, ImageMin.y - 1.f }, { ImageMax.x + 1.f, ImageMax.y + 1.f }, ImGui::GetColorU32(ImGuiCol_Border));
	DrawList->PopClipRect();

	HoveredX = -1;
	HoveredY = -1;
	if (IsHovered)
	{
		const int X = (int)floorf((IO.MousePos.x - ImageMin.x) / Zoom);
		const int Y = (int)floorf((IO.MousePos.y - ImageMin.y) / Zoom);
		if (X >= 0 && Y >= 0 && X < (int)SrcData.W && Y < (int)SrcData.H)
		{
			HoveredX = X;
			HoveredY = Y;
		}
	}
}

void CUITextureViewer::DrawStatusBar()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(8.f, 3.f));
	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImGui::GetStyleColorVec4(ImGuiCol_MenuBarBg));
	ImGui::BeginChild("##TextureViewerStatus", { 0.f, ImGui::GetTextLineHeight() + 6.f }, ImGuiChildFlags_AlwaysUseWindowPadding, ImGuiWindowFlags_NoScrollbar);

	ImGui::Text("%u x %u", SrcData.W, SrcData.H);
	StatusSeparator();
	ImGui::TextDisabled("%s", SrcData.Format.size() ? *SrcData.Format : "-");
	StatusSeparator();
	ImGui::TextDisabled("%.0f%%", Zoom * 100.f);

	if (HoveredX >= 0)
	{
		// The source pixels are stored as BGRA
		const size_t Index = ((size_t)HoveredY * SrcData.W + HoveredX) * 4;
		const u8 R = SrcData.P[Index + 2];
		const u8 G = SrcData.P[Index + 1];
		const u8 B = SrcData.P[Index + 0];
		const u8 A = SrcData.P[Index + 3];

		StatusSeparator();
		ImGui::Text("%d, %d", HoveredX, HoveredY);
		ImGui::SameLine(0.f, 8.f);

		const float Swatch = ImGui::GetTextLineHeight();
		ImGui::ColorButton("##Pixel", ImVec4(R / 255.f, G / 255.f, B / 255.f, A / 255.f), ImGuiColorEditFlags_AlphaPreviewHalf | ImGuiColorEditFlags_NoTooltip, { Swatch, Swatch });
		ImGui::SameLine(0.f, 6.f);
		ImGui::Text("R %3u  G %3u  B %3u  A %3u", R, G, B, A);
	}

	ImGui::EndChild();
	ImGui::PopStyleColor();
	ImGui::PopStyleVar();
}

void CUITextureViewer::DrawView()
{
	DrawToolbar();

	const float StatusHeight = ImGui::GetTextLineHeight() + 6.f;
	ImVec2 CanvasSize = ImGui::GetContentRegionAvail();
	CanvasSize.y = std::max(CanvasSize.y - StatusHeight, 1.f);
	CanvasSize.x = std::max(CanvasSize.x, 1.f);

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.f, 0.f));
	DrawCanvas(CanvasSize);
	DrawStatusBar();
	ImGui::PopStyleVar();
}

void CUITextureViewer::SetZoom(float NewZoom)
{
	const float Clamped = std::clamp(NewZoom, MinZoom, MaxZoom);
	if (Zoom > 0.f)
	{
		// Zooms around the canvas center
		Pan.x *= Clamped / Zoom;
		Pan.y *= Clamped / Zoom;
	}

	Zoom = Clamped;
	FitToView = false;
}

void CUITextureViewer::LoadFromFile(const xr_path& File)
{
	CurrentFileName = File.xfilename();
	SrcData = DXTUtils::GitPixels(File.xstring().c_str());
	if (SrcData.W == 0 || SrcData.H == 0)
	{
		Msg("! Texture Viewer: cannot load %s", File.xstring().c_str());
		return;
	}

	RHITextureDesc Desc;
	Desc.Width = SrcData.W;
	Desc.Height = SrcData.H;
	Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	// D3D11 refuses a dynamic texture without CPU write access
	Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;

	RHISubResource SubResource{};
	SubResource.Width = SrcData.W;
	SubResource.Height = SrcData.H;
	SubResource.TextureFormat = Desc.Format;
	SubResource.RowPitch = SrcData.W * 4;
	SubResource.DataSize = SrcData.W * 4;
	SubResource.Data = SrcData.P.data();

	IRHISurface* Surf = GRHI->CreateTexture2D(Desc, SubResource);
	if (Surf == nullptr)
	{
		Msg("! Texture Viewer: cannot create texture for %s (%ux%u)", File.xstring().c_str(), SrcData.W, SrcData.H);
		return;
	}

	Texture->surface_set(Surf);
	Surf->Release();

	FitToView = true;
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

	const bool FullMask = (ChannelMask == (Channel_R | Channel_G | Channel_B | Channel_A));

	for (size_t y = 0; y < Height; ++y)
	{
		u8* Row = Data + y * Width * 4;
		for (size_t x = 0; x < Width; ++x)
		{
			// The source pixels are stored as BGRA, the texture is RGBA
			const size_t idx = (y * Width + x) * 4;
			const u8 r = SrcData.P[idx + 2];
			const u8 g = SrcData.P[idx + 1];
			const u8 b = SrcData.P[idx + 0];
			const u8 a = SrcData.P[idx + 3];

			u8 R, G, B, A;

			if (FullMask)
			{
				R = r;
				G = g;
				B = b;
				A = a;
			}
			else if (GrayMode)
			{
				u8 v = 0;
				if (ChannelMask & Channel_R) v = r;
				if (ChannelMask & Channel_G) v = g;
				if (ChannelMask & Channel_B) v = b;
				if (ChannelMask & Channel_A) v = a;

				R = G = B = v;
				A = 255;
			}
			else
			{
				R = (ChannelMask & Channel_R) ? r : 0;
				G = (ChannelMask & Channel_G) ? g : 0;
				B = (ChannelMask & Channel_B) ? b : 0;
				A = (ChannelMask & Channel_A) ? a : 255;
			}

			Row[x * 4 + 0] = R;
			Row[x * 4 + 1] = G;
			Row[x * 4 + 2] = B;
			Row[x * 4 + 3] = A;
		}
	}

	Surf->Unlock();
}
