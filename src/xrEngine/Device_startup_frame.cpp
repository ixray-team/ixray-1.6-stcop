#include "stdafx.h"
#include "device.h"
#include "EngineIcons.h"
#include "EngineStartupFont.h"
#include <lunasvg.h>
#include <algorithm>
#include <cmath>

int CRenderDevice::s_StartupProgress = 0;
xr_string CRenderDevice::s_StartupStatus = "";
float CRenderDevice::s_StartupAnimationTime = 0.0f;
u64 CRenderDevice::s_StartupLastTick = 0;
bool CRenderDevice::s_StartupActive = true;

void CRenderDevice::SetStartupStatus(int Progress, const char* Status)
{
	s_StartupProgress = Progress;
	if (Status != nullptr)
	{
		s_StartupStatus = Status;
	}

	if (DevicePtr != nullptr && GRHI != nullptr && GRHI->DevicePtr != nullptr)
	{
		DevicePtr->DrawStartupFrame();
	}
	else
	{
		SDL_PumpEvents();
	}
}

void CRenderDevice::UpdateStartupFrame()
{
	if (DevicePtr != nullptr && GRHI != nullptr && GRHI->DevicePtr != nullptr)
	{
		DevicePtr->DrawStartupFrame();
	}
	else
	{
		SDL_PumpEvents();
	}
}

struct StartupCanvas
{
	u32 Width = 0;
	u32 Height = 0;
	xr_vector<u32> Pixels;

	void Resize(u32 W, u32 H, u32 ClearColor)
	{
		Width = W;
		Height = H;
		Pixels.assign(W * H, ClearColor);
	}

	void PutPixel(u32 X, u32 Y, u32 Color)
	{
		if (X < Width && Y < Height)
		{
			Pixels[Y * Width + X] = Color;
		}
	}

	void FillRect(u32 X, u32 Y, u32 W, u32 H, u32 Color)
	{
		const u32 XEnd = std::min(X + W, Width);
		const u32 YEnd = std::min(Y + H, Height);
		for (u32 Py = Y; Py < YEnd; ++Py)
		{
			u32* Row = &Pixels[Py * Width];
			for (u32 Px = X; Px < XEnd; ++Px)
			{
				Row[Px] = Color;
			}
		}
	}

	void DrawRectOutline(u32 X, u32 Y, u32 W, u32 H, u32 Color)
	{
		FillRect(X, Y, W, 1, Color);
		if (H > 1)
			FillRect(X, Y + H - 1, W, 1, Color);
		FillRect(X, Y, 1, H, Color);
		if (W > 1)
			FillRect(X + W - 1, Y, 1, H, Color);
	}

	void DrawChar8x16(u32 X, u32 Y, char C, u32 Color, u32 Scale)
	{
		const u8 Code = static_cast<u8>(C);
		if (Code < 32 || Code > 126)
		{
			return;
		}
		const u8* Glyph = StartupFont8x16[Code - 32];
		for (u32 Row = 0; Row < 16; ++Row)
		{
			const u8 RowBits = Glyph[Row];
			for (u32 Col = 0; Col < 8; ++Col)
			{
				if (RowBits & (1 << (7 - Col)))
				{
					if (Scale == 1)
					{
						PutPixel(X + Col, Y + Row, Color);
					}
					else
					{
						FillRect(X + Col * Scale, Y + Row * Scale, Scale, Scale, Color);
					}
				}
			}
		}
	}

	void DrawString(u32 X, u32 Y, const char* Str, u32 Color, u32 Scale)
	{
		if (!Str)
			return;
		u32 CurX = X;
		while (*Str)
		{
			DrawChar8x16(CurX, Y, *Str, Color, Scale);
			CurX += 8 * Scale;
			++Str;
		}
	}
};

static void CopyStartupSvg(IRHISurface* Target, const char* Svg, u32 Size, u32 PosX, u32 PosY)
{
	const ERHI_FORMAT Format = Target->GetFormat();
	const bool IsRGBA = Format == ERHI_FORMAT::R8G8B8A8_UNORM || Format == ERHI_FORMAT::R8G8B8A8_UNORM_SRGB;
	if (Size == 0 || PosX + Size > Target->GetWidth() || PosY + Size > Target->GetHeight())
	{
		return;
	}

	auto Document = lunasvg::Document::loadFromData(Svg);
	if (!Document)
	{
		return;
	}

	lunasvg::Bitmap Bitmap = Document->renderToBitmap(Size, Size);
	u32* Pixels = (u32*)Bitmap.data();
	for (u32 Idx = 0; Idx < Size * Size; ++Idx)
	{
		u32 Pixel = Pixels[Idx];
		if (IsRGBA)
		{
			Pixel = (Pixel & 0x0000FF00) | ((Pixel & 0xFF) << 16) | ((Pixel >> 16) & 0xFF);
		}
		Pixels[Idx] = Pixel | 0xFF000000;
	}

	RHISubResource SubResource{};
	SubResource.Data = Pixels;
	SubResource.DataSize = Size * Size * 4;
	SubResource.Width = Size;
	SubResource.Height = Size;
	SubResource.RowPitch = Size * 4;
	SubResource.TextureFormat = Format;

	const RHIBox Box = {PosX, PosY, 0, PosX + Size, PosY + Size, 1};
	Target->UpdateData(0, 0, &SubResource, Box);
}

static void RenderInfoBox(IRHISurface* Target, u32 Width, u32 Height, u32 PosX, u32 PosY, float Dpi, bool IsRGBA, int Progress, const char* Status)
{
	StartupCanvas Canvas;
	Canvas.Resize(Width, Height, 0xFF000000);

	const u32 FontScale = (Dpi >= 1.5f) ? 2 : 1;
	const u32 StatusLen = Status ? u32(xr_strlen(Status)) : 0;
	const u32 TextW = StatusLen * 8 * FontScale;
	const u32 TextX = (Width > TextW) ? (Width - TextW) / 2 : 0;
	const u32 TextY = u32(2.0f * Dpi);

	auto ToColor = [IsRGBA](u8 R, u8 G, u8 B) -> u32
	{
		if (IsRGBA)
			return (0xFF000000 | (B << 16) | (G << 8) | R);
		return (0xFF000000 | (R << 16) | (G << 8) | B);
	};

	const u32 TextColor = ToColor(225, 225, 225);
	const u32 BarBgColor = ToColor(30, 30, 30);
	const u32 BarBorderColor = ToColor(55, 55, 55);
	const u32 BarFillColor = ToColor(58, 134, 255);
	const u32 PercentColor = ToColor(140, 140, 140);

	if (Status && StatusLen > 0)
	{
		Canvas.DrawString(TextX, TextY, Status, TextColor, FontScale);
	}

	// Progress bar
	const u32 BarW = std::min(Width - u32(40.0f * Dpi), u32(360.0f * Dpi));
	u32 BarH = u32(6.0f * Dpi);
	if (BarH < 4)
		BarH = 4;
	const u32 BarX = (Width > BarW) ? (Width - BarW) / 2 : 0;
	const u32 BarY = TextY + 16 * FontScale + u32(8.0f * Dpi);

	Canvas.FillRect(BarX, BarY, BarW, BarH, BarBgColor);
	Canvas.DrawRectOutline(BarX > 0 ? BarX - 1 : 0, BarY > 0 ? BarY - 1 : 0, BarW + 2, BarH + 2, BarBorderColor);

	const int ClampedProgress = std::clamp(Progress, 0, 100);
	const u32 FillW = (BarW * ClampedProgress) / 100;
	if (FillW > 0)
	{
		Canvas.FillRect(BarX, BarY, FillW, BarH, BarFillColor);
	}

	// Percentage string
	string32 PercentStr;
	xr_sprintf(PercentStr, "%d%%", ClampedProgress);
	const u32 PercentLen = u32(xr_strlen(PercentStr));
	const u32 PercentW = PercentLen * 8;
	const u32 PercentX = (Width > PercentW) ? (Width - PercentW) / 2 : 0;
	const u32 PercentY = BarY + BarH + u32(8.0f * Dpi);
	Canvas.DrawString(PercentX, PercentY, PercentStr, PercentColor, 1);

	RHISubResource SubResource{};
	SubResource.Data = Canvas.Pixels.data();
	SubResource.DataSize = Canvas.Pixels.size() * 4;
	SubResource.Width = Width;
	SubResource.Height = Height;
	SubResource.RowPitch = Width * 4;
	SubResource.TextureFormat = Target->GetFormat();

	const RHIBox Box = {PosX, PosY, 0, PosX + Width, PosY + Height, 1};
	Target->UpdateData(0, 0, &SubResource, Box);
}

void CRenderDevice::DrawStartupFrame(bool WithWindowButtons)
{
	if (GRHI == nullptr || GRHI->DevicePtr == nullptr)
	{
		return;
	}

	IRHIRenderTargetView* BackBuffer = GRHI->DevicePtr->SwapChainRTV;
	if (BackBuffer == nullptr)
	{
		return;
	}

	IRHISurface* Target = BackBuffer->GetSurface();
	if (Target == nullptr)
	{
		return;
	}

	const ERHI_FORMAT Format = Target->GetFormat();
	const bool IsBGRA = Format == ERHI_FORMAT::B8G8R8A8_UNORM || Format == ERHI_FORMAT::B8G8R8A8_UNORM_SRGB;
	const bool IsRGBA = Format == ERHI_FORMAT::R8G8B8A8_UNORM || Format == ERHI_FORMAT::R8G8B8A8_UNORM_SRGB;

	if (!IsBGRA && !IsRGBA)
	{
		return;
	}

	// Update delta time and animation time
	const u64 CurrentTick = SDL_GetTicks();
	if (s_StartupLastTick == 0)
	{
		s_StartupLastTick = CurrentTick;
	}
	float Dt = float(CurrentTick - s_StartupLastTick) / 1000.0f;
	s_StartupLastTick = CurrentTick;
	Dt = std::clamp(Dt, 0.0f, 0.05f);
	s_StartupAnimationTime += Dt;

	// Hardware clear of backbuffer
	const float Black[4] = {0.f, 0.f, 0.f, 1.f};
	GRHI->ClearTarget(BackBuffer, Black);

	const u32 Width = Target->GetWidth();
	const u32 Height = Target->GetHeight();

	float Dpi = 1.0f;
	if (g_AppInfo.Window)
	{
		Dpi = SDL_GetWindowDisplayScale(g_AppInfo.Window);
		if (Dpi <= 0.0f)
		{
			Dpi = 1.0f;
		}
	}

	// Smooth cyclical pulsation animation using delta time
	const float PulsingScale = 1.0f + 0.08f * std::sin(s_StartupAnimationTime * 2.8f);
	const u32 BaseLogoSize = std::min(u32(256.0f * Dpi), std::min(Width, Height));
	const u32 Spacing = u32(16.0f * Dpi);
	const u32 InfoBoxH = u32(85.0f * Dpi);
	const u32 TotalH = BaseLogoSize + Spacing + InfoBoxH;
	const u32 StartY = (Height > TotalH) ? (Height - TotalH) / 2 : 0;

	// Stable anchor positions independent of dynamic LogoSize
	const u32 LogoCenterX = Width / 2;
	const u32 LogoCenterY = StartY + BaseLogoSize / 2;

	u32 LogoSize = u32(float(BaseLogoSize) * PulsingScale);
	if (LogoSize > Width)
		LogoSize = Width;
	if (LogoSize > Height)
		LogoSize = Height;

	const u32 LogoX = (LogoCenterX >= LogoSize / 2) ? (LogoCenterX - LogoSize / 2) : 0;
	const u32 LogoY = (LogoCenterY >= LogoSize / 2) ? (LogoCenterY - LogoSize / 2) : 0;

	// 1. Draw animated SVG logo (centered around stable anchor)
	CopyStartupSvg(Target, IX_RAY_LOGO, LogoSize, LogoX, LogoY);

	// 2. Draw info box below logo (status text, progress bar, percentage) at strictly fixed position
	const u32 InfoBoxW = std::min(Width, u32(480.0f * Dpi));
	const u32 InfoBoxX = (Width > InfoBoxW) ? (Width - InfoBoxW) / 2 : 0;
	const u32 InfoBoxY = StartY + BaseLogoSize + Spacing;

	if (InfoBoxW > 0 && InfoBoxH > 0 && InfoBoxY + InfoBoxH <= Height)
	{
		RenderInfoBox(Target, InfoBoxW, InfoBoxH, InfoBoxX, InfoBoxY, Dpi, IsRGBA, s_StartupProgress, s_StartupStatus.c_str());
	}

	// 3. Optional window control buttons (minimize, maximize, close)
	if (WithWindowButtons && Width > 160)
	{
		const u32 ButtonW = u32(52.f * Dpi);
		const u32 ButtonH = u32(26.f * Dpi);
		const u32 IconSize = u32(10.f * Dpi);

		const char* Icons[] = {IX_MIN_ICON, IX_MAX_ICON, IX_CLOSE_ICON};
		for (u32 Idx = 0; Idx < 3; ++Idx)
		{
			const u32 ButtonX = Width - ButtonW * (3 - Idx);
			CopyStartupSvg(Target, Icons[Idx], IconSize, ButtonX + (ButtonW - IconSize) / 2, (ButtonH - IconSize) / 2);
		}
	}

	GRHI->Present();

	// Process window events to keep the application responsive
	SDL_PumpEvents();
}
