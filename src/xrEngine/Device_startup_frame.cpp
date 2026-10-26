#include "stdafx.h"
#include "device.h"
#include "Autotest.h"
#include "EngineIcons.h"
#include "EngineStartupFont.h"
#include <lunasvg.h>
#include <algorithm>
#include <cmath>
#include <future>

int CRenderDevice::s_StartupProgress = 0;
xr_string CRenderDevice::s_StartupStatus = "";
float CRenderDevice::s_StartupAnimationTime = 0.0f;
u64 CRenderDevice::s_StartupLastTick = 0;
bool CRenderDevice::s_StartupActive = true;
static float s_DisplayedProgress = 0.0f;
extern volatile bool quiting;

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

void CRenderDevice::PumpStartupFrames(u32 FrameCount)
{
	if (DevicePtr == nullptr || GRHI == nullptr || GRHI->DevicePtr == nullptr)
	{
		SDL_PumpEvents();
		return;
	}

	for (u32 i = 0; i < FrameCount; ++i)
	{
		const u64 FrameStart = SDL_GetTicks();

		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			if (Event.type == SDL_EVENT_QUIT)
			{
				s_StartupActive = false;
				extern volatile bool quiting;
				quiting = true;
				return;
			}
		}

		DevicePtr->DrawStartupFrame(DevicePtr->IsEditorMode());

		const u64 Elapsed = SDL_GetTicks() - FrameStart;
		constexpr u64 TargetFrameMs = 16;
		if (!Autotest::Active() && Elapsed < TargetFrameMs)
		{
			SDL_Delay(static_cast<u32>(TargetFrameMs - Elapsed));
		}
	}
}

void CRenderDevice::FinishStartup()
{
	SetStartupStatus(100, "Ready");
	if (!Autotest::Active())
	{
		while (s_DisplayedProgress < 99.5f && !quiting)
		{
			PumpStartupFrames(1);
		}
		s_DisplayedProgress = 100.0f;
		PumpStartupFrames(6);
	}
	else
	{
		s_DisplayedProgress = 100.0f;
		PumpStartupFrames(1);
	}
	s_StartupActive = false;
}

void CRenderDevice::StartupFrameWaitTask(std::function<void()> TaskFunc, int TargetProgress, const char* TargetStatus)
{
	if (TargetStatus != nullptr || TargetProgress >= 0)
	{
		SetStartupStatus(TargetProgress >= 0 ? TargetProgress : s_StartupProgress, TargetStatus);
	}

	if (!TaskFunc)
	{
		return;
	}

	std::future<void> Future = std::async(std::launch::async, std::move(TaskFunc));

	while (Future.wait_for(std::chrono::milliseconds(0)) != std::future_status::ready)
	{
		const u64 FrameStart = SDL_GetTicks();

		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			if (Event.type == SDL_EVENT_QUIT)
			{
				s_StartupActive = false;
				extern volatile bool quiting;
				quiting = true;
				break;
			}
		}

		if (DevicePtr != nullptr && GRHI != nullptr && GRHI->DevicePtr != nullptr)
		{
			DevicePtr->DrawStartupFrame(DevicePtr->IsEditorMode());
		}
		else
		{
			SDL_PumpEvents();
		}

		const u64 Elapsed = SDL_GetTicks() - FrameStart;
		constexpr u64 TargetFrameMs = 16;
		if (!Autotest::Active() && Elapsed < TargetFrameMs)
		{
			SDL_Delay(static_cast<u32>(TargetFrameMs - Elapsed));
		}
	}

	Future.get();
	SDL_PumpEvents();
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

static const xr_vector<u32>& GetCachedSvgBitmap(const char* Svg, u32 Size, bool IsRGBA)
{
	static xr_hash_map<u64, xr_vector<u32>> s_BitmapCache;
	const u64 Key = (u64(reinterpret_cast<uintptr_t>(Svg)) ^ (u64(Size) << 32) ^ (IsRGBA ? 0x8000000000000000ULL : 0));

	auto It = s_BitmapCache.find(Key);
	if (It != s_BitmapCache.end())
	{
		return It->second;
	}

	auto Doc = lunasvg::Document::loadFromData(Svg);
	xr_vector<u32>& Out = s_BitmapCache[Key];
	if (!Doc)
	{
		Out.assign(Size * Size, 0xFF000000);
		return Out;
	}

	lunasvg::Bitmap Bmp = Doc->renderToBitmap(Size, Size);
	Out.resize(Size * Size);
	const u32* Pixels = reinterpret_cast<const u32*>(Bmp.data());
	for (u32 i = 0; i < Size * Size; ++i)
	{
		u32 Pixel = Pixels[i];
		if (IsRGBA)
		{
			Pixel = (Pixel & 0x0000FF00) | ((Pixel & 0xFF) << 16) | ((Pixel >> 16) & 0xFF);
		}
		Out[i] = Pixel | 0xFF000000;
	}

	return Out;
}

static void BilinearScale(const u32* Src, u32 SrcW, u32 SrcH, u32* Dst, u32 DstW, u32 DstH)
{
	if (SrcW == 0 || SrcH == 0 || DstW == 0 || DstH == 0)
	{
		return;
	}

	const float XRatio = float(SrcW) / float(DstW);
	const float YRatio = float(SrcH) / float(DstH);

	for (u32 Y = 0; Y < DstH; ++Y)
	{
		const float SrcY = (float(Y) + 0.5f) * YRatio - 0.5f;
		const int Y0 = std::clamp(int(std::floor(SrcY)), 0, int(SrcH - 1));
		const int Y1 = std::clamp(Y0 + 1, 0, int(SrcH - 1));
		const float YWeight = std::clamp(SrcY - float(Y0), 0.0f, 1.0f);
		const float InvYWeight = 1.0f - YWeight;

		const u32* Row0 = &Src[Y0 * SrcW];
		const u32* Row1 = &Src[Y1 * SrcW];
		u32* DstRow = &Dst[Y * DstW];

		for (u32 X = 0; X < DstW; ++X)
		{
			const float SrcX = (float(X) + 0.5f) * XRatio - 0.5f;
			const int X0 = std::clamp(int(std::floor(SrcX)), 0, int(SrcW - 1));
			const int X1 = std::clamp(X0 + 1, 0, int(SrcW - 1));
			const float XWeight = std::clamp(SrcX - float(X0), 0.0f, 1.0f);
			const float InvXWeight = 1.0f - XWeight;

			const u32 P00 = Row0[X0];
			const u32 P01 = Row0[X1];
			const u32 P10 = Row1[X0];
			const u32 P11 = Row1[X1];

			const float W00 = InvXWeight * InvYWeight;
			const float W01 = XWeight * InvYWeight;
			const float W10 = InvXWeight * YWeight;
			const float W11 = XWeight * YWeight;

			const u32 B = static_cast<u32>((P00 & 0xFF) * W00 + (P01 & 0xFF) * W01 + (P10 & 0xFF) * W10 + (P11 & 0xFF) * W11);
			const u32 G = static_cast<u32>(((P00 >> 8) & 0xFF) * W00 + ((P01 >> 8) & 0xFF) * W01 + ((P10 >> 8) & 0xFF) * W10 + ((P11 >> 8) & 0xFF) * W11);
			const u32 R = static_cast<u32>(((P00 >> 16) & 0xFF) * W00 + ((P01 >> 16) & 0xFF) * W01 + ((P10 >> 16) & 0xFF) * W10 + ((P11 >> 16) & 0xFF) * W11);
			const u32 A = static_cast<u32>(((P00 >> 24) & 0xFF) * W00 + ((P01 >> 24) & 0xFF) * W01 + ((P10 >> 24) & 0xFF) * W10 + ((P11 >> 24) & 0xFF) * W11);

			DstRow[X] = (A << 24) | (R << 16) | (G << 8) | B;
		}
	}
}

static void DrawAnimatedStartupLogo(IRHISurface* Target, u32 Size, u32 PosX, u32 PosY)
{
	if (Size == 0 || PosX + Size > Target->GetWidth() || PosY + Size > Target->GetHeight())
	{
		return;
	}

	const ERHI_FORMAT Format = Target->GetFormat();
	const bool IsRGBA = Format == ERHI_FORMAT::R8G8B8A8_UNORM || Format == ERHI_FORMAT::R8G8B8A8_UNORM_SRGB;

	static xr_vector<u32> s_MasterLogoPixels;
	static u32 s_MasterLogoSize = 0;
	static bool s_MasterLogoIsRGBA = false;
	static xr_vector<u32> s_ScaledLogoPixels;

	const u32 RequiredMasterSize = std::max(512u, Size * 2);
	if (s_MasterLogoSize < RequiredMasterSize || s_MasterLogoIsRGBA != IsRGBA)
	{
		auto Doc = lunasvg::Document::loadFromData(IX_RAY_LOGO);
		if (Doc)
		{
			s_MasterLogoSize = RequiredMasterSize;
			s_MasterLogoIsRGBA = IsRGBA;
			lunasvg::Bitmap Bmp = Doc->renderToBitmap(s_MasterLogoSize, s_MasterLogoSize);
			s_MasterLogoPixels.resize(s_MasterLogoSize * s_MasterLogoSize);
			const u32* Pixels = reinterpret_cast<const u32*>(Bmp.data());
			for (u32 i = 0; i < s_MasterLogoSize * s_MasterLogoSize; ++i)
			{
				u32 Pixel = Pixels[i];
				if (IsRGBA)
				{
					Pixel = (Pixel & 0x0000FF00) | ((Pixel & 0xFF) << 16) | ((Pixel >> 16) & 0xFF);
				}
				s_MasterLogoPixels[i] = Pixel | 0xFF000000;
			}
		}
	}

	if (s_MasterLogoPixels.empty())
	{
		return;
	}

	s_ScaledLogoPixels.resize(Size * Size);
	BilinearScale(s_MasterLogoPixels.data(), s_MasterLogoSize, s_MasterLogoSize, s_ScaledLogoPixels.data(), Size, Size);

	RHISubResource SubResource{};
	SubResource.Data = s_ScaledLogoPixels.data();
	SubResource.DataSize = Size * Size * 4;
	SubResource.Width = Size;
	SubResource.Height = Size;
	SubResource.RowPitch = Size * 4;
	SubResource.TextureFormat = Format;

	const RHIBox Box = {PosX, PosY, 0, PosX + Size, PosY + Size, 1};
	Target->UpdateData(0, 0, &SubResource, Box);
}

static void CopyStartupSvg(IRHISurface* Target, const char* Svg, u32 Size, u32 PosX, u32 PosY)
{
	if (Size == 0 || PosX + Size > Target->GetWidth() || PosY + Size > Target->GetHeight())
	{
		return;
	}

	const ERHI_FORMAT Format = Target->GetFormat();
	const bool IsRGBA = Format == ERHI_FORMAT::R8G8B8A8_UNORM || Format == ERHI_FORMAT::R8G8B8A8_UNORM_SRGB;

	const xr_vector<u32>& Pixels = GetCachedSvgBitmap(Svg, Size, IsRGBA);

	RHISubResource SubResource{};
	SubResource.Data = const_cast<u32*>(Pixels.data());
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
	static xr_vector<u32> s_CachedInfoBoxPixels;
	static u32 s_CachedW = 0;
	static u32 s_CachedH = 0;
	static float s_CachedDpi = 0.0f;
	static bool s_CachedIsRGBA = false;
	static int s_CachedProgress = -1;
	static xr_string s_CachedStatus = "";

	const char* ValidStatus = Status ? Status : "";
	const bool NeedRedraw = (Width != s_CachedW || Height != s_CachedH || Dpi != s_CachedDpi ||
		IsRGBA != s_CachedIsRGBA || Progress != s_CachedProgress || s_CachedStatus != ValidStatus);

	if (NeedRedraw)
	{
		s_CachedW = Width;
		s_CachedH = Height;
		s_CachedDpi = Dpi;
		s_CachedIsRGBA = IsRGBA;
		s_CachedProgress = Progress;
		s_CachedStatus = ValidStatus;

		StartupCanvas Canvas;
		Canvas.Resize(Width, Height, 0xFF000000);

		const u32 FontScale = (Dpi >= 1.5f) ? 2 : 1;
		const u32 StatusLen = u32(xr_strlen(ValidStatus));
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

		if (StatusLen > 0)
		{
			Canvas.DrawString(TextX, TextY, ValidStatus, TextColor, FontScale);
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

		s_CachedInfoBoxPixels = std::move(Canvas.Pixels);
	}

	RHISubResource SubResource{};
	SubResource.Data = s_CachedInfoBoxPixels.data();
	SubResource.DataSize = s_CachedInfoBoxPixels.size() * 4;
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
	const float RealDt = float(CurrentTick - s_StartupLastTick) / 1000.0f;
	s_StartupLastTick = CurrentTick;

	// Cap animation advance per frame so that if a synchronous operation or pause
	// took 100ms or 200ms, the animation DOES NOT JUMP forward abruptly!
	// It smoothly advances by at most 1 frame (~0.020s)!
	const float AnimDt = std::clamp(RealDt, 0.001f, 0.020f);
	s_StartupAnimationTime += AnimDt;

	// Smoothly interpolate displayed progress towards s_StartupProgress
	if (s_DisplayedProgress < float(s_StartupProgress))
	{
		const float Diff = float(s_StartupProgress) - s_DisplayedProgress;
		const float Step = std::max(Diff * std::clamp(RealDt * 8.0f, 0.05f, 0.5f), RealDt * 15.0f);
		s_DisplayedProgress = std::min(s_DisplayedProgress + Step, float(s_StartupProgress));
	}
	else if (s_DisplayedProgress > float(s_StartupProgress))
	{
		s_DisplayedProgress = float(s_StartupProgress);
	}

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
	const float PulsingScale = 1.0f + 0.10f * std::sin(s_StartupAnimationTime * 3.2f);
	const u32 BaseLogoSize = std::min(u32(256.0f * Dpi), std::min(Width, Height));
	const u32 Spacing = u32(16.0f * Dpi);
	const u32 InfoBoxH = u32(85.0f * Dpi);
	const u32 TotalH = BaseLogoSize + Spacing + InfoBoxH;
	const u32 StartY = (Height > TotalH) ? (Height - TotalH) / 2 : 0;

	// Stable anchor positions independent of dynamic LogoSize
	const u32 LogoCenterX = Width / 2;
	const u32 LogoCenterY = StartY + BaseLogoSize / 2;

	u32 LogoSize = u32(std::round(float(BaseLogoSize) * PulsingScale));
	if (LogoSize > Width)
		LogoSize = Width;
	if (LogoSize > Height)
		LogoSize = Height;

	const u32 LogoX = (LogoCenterX >= LogoSize / 2) ? (LogoCenterX - LogoSize / 2) : 0;
	const u32 LogoY = (LogoCenterY >= LogoSize / 2) ? (LogoCenterY - LogoSize / 2) : 0;

	// 1. Draw animated SVG logo (centered around stable anchor)
	DrawAnimatedStartupLogo(Target, LogoSize, LogoX, LogoY);

	// 2. Draw info box below logo (status text, progress bar, percentage) at strictly fixed position
	const u32 InfoBoxW = std::min(Width, u32(480.0f * Dpi));
	const u32 InfoBoxX = (Width > InfoBoxW) ? (Width - InfoBoxW) / 2 : 0;
	const u32 InfoBoxY = StartY + BaseLogoSize + Spacing;

	if (InfoBoxW > 0 && InfoBoxH > 0 && InfoBoxY + InfoBoxH <= Height)
	{
		const int DisplayedIntProgress = int(std::round(s_DisplayedProgress));
		RenderInfoBox(Target, InfoBoxW, InfoBoxH, InfoBoxX, InfoBoxY, Dpi, IsRGBA, DisplayedIntProgress, s_StartupStatus.c_str());
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
