#include "stdafx.h"
#include "CrashWindow.h"

#include <algorithm>
#include <bit>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

#if defined(_MSC_VER)
#	pragma warning(push)
#	pragma warning(disable : 4244 4456 4457 4505)
#elif defined(__clang__)
#	pragma clang diagnostic push
#	pragma clang diagnostic ignored "-Wunused-function"
#	pragma clang diagnostic ignored "-Wsign-conversion"
#endif

#define STBTT_STATIC
#define STB_TRUETYPE_IMPLEMENTATION
#include "../3rd-party/imgui/imstb_truetype.h"

#if defined(_MSC_VER)
#	pragma warning(pop)
#elif defined(__clang__)
#	pragma clang diagnostic pop
#endif

namespace CrashWindow
{
namespace
{
	constexpr int WindowWidth = 920;
	constexpr int WindowHeight = 560;

	constexpr float WinBarHeight = 32.0f;
	constexpr float WinBarButtonW = 44.0f;

	constexpr float LogoClipWidth = 280.0f;
	constexpr float RightX = 294.0f;
	constexpr float RightMargin = 54.0f;
	constexpr float RightW = static_cast<float>(WindowWidth) - RightX - RightMargin;

	constexpr float TitleX = RightX;
	constexpr float TitleY = 82.0f;

	constexpr float BoxX = RightX;
	constexpr float BoxY = 130.0f;
	constexpr float BoxW = RightW;
	constexpr float BoxH = 312.0f;
	constexpr float BoxPad = 14.0f;
	constexpr float ScrollbarW = 10.0f;

	constexpr float ButtonY = BoxY + BoxH + 12.0f;
	constexpr float ButtonW = 196.0f;
	constexpr float ButtonH = 36.0f;

	struct PointF
	{
		float X;
		float Y;
	};

	struct GlyphInfo
	{
		SDL_FRect SrcRect = {};
		float OffsetX = 0.0f;
		float OffsetY = 0.0f;
		float Advance = 0.0f;
		bool bValid = false;
	};

	struct BakedFont
	{
		static constexpr int FirstChar = 32;
		static constexpr int LastChar = 126;
		static constexpr int CharCount = LastChar - FirstChar + 1;

		SDL_Texture* Texture = nullptr;
		GlyphInfo Glyphs[CharCount] = {};
		float Ascent = 0.0f;
		float LineHeight = 18.0f;
		float MonoAdvance = 8.0f;
		bool bLoaded = false;

		void Destroy()
		{
			if (Texture != nullptr)
			{
				SDL_DestroyTexture(Texture);
				Texture = nullptr;
			}
			bLoaded = false;
		}
	};

	struct EditorState
	{
		std::vector<std::string> Lines;
		int CursorLine = 0;
		int CursorCol = 0;
		int SelAnchorLine = 0;
		int SelAnchorCol = 0;
		bool bHasSelection = false;
		bool bSelectingMouse = false;
		int ScrollY = 0;
		int ScrollX = 0;
		float CharAdvance = 8.0f;
		float LineHeight = 18.0f;
		bool bFocused = true;
		Uint64 CopiedUntilTicks = 0;
		bool bHoveredCopy = false;
		bool bHoveredQuit = false;
		bool bPressedCopy = false;
		bool bPressedQuit = false;
		bool bHoveredMin = false;
		bool bHoveredClose = false;
		bool bPressedMin = false;
		bool bPressedClose = false;
		bool bDraggingVScroll = false;
		bool bDraggingHScroll = false;
		float DragScrollOffset = 0.0f;
	};

	struct WinBarHitTestContext
	{
		SDL_FRect WinBarRect;
		SDL_FRect MinRect;
		SDL_FRect CloseRect;
	};

	bool PointInRect(float X, float Y, const SDL_FRect& R)
	{
		return X >= R.x && X <= (R.x + R.w) && Y >= R.y && Y <= (R.y + R.h);
	}

	SDL_HitTestResult SDLCALL WinBarHitTest(SDL_Window*, const SDL_Point* Area, void* Data)
	{
		const auto* Ctx = static_cast<const WinBarHitTestContext*>(Data);
		if (Ctx == nullptr || Area == nullptr)
		{
			return SDL_HITTEST_NORMAL;
		}
		const float X = static_cast<float>(Area->x);
		const float Y = static_cast<float>(Area->y);
		if (PointInRect(X, Y, Ctx->WinBarRect) && !PointInRect(X, Y, Ctx->MinRect) && !PointInRect(X, Y, Ctx->CloseRect))
		{
			return SDL_HITTEST_DRAGGABLE;
		}
		return SDL_HITTEST_NORMAL;
	}

	bool ReadFileBytes(const char* Path, std::vector<u8>& OutBytes)
	{
		if (Path == nullptr || Path[0] == '\0')
		{
			return false;
		}
		std::FILE* File = std::fopen(Path, "rb");
		if (File == nullptr)
		{
			return false;
		}
		if (std::fseek(File, 0, SEEK_END) != 0)
		{
			std::fclose(File);
			return false;
		}
		const long Size = std::ftell(File);
		if (Size <= 0 || Size > 32 * 1024 * 1024)
		{
			std::fclose(File);
			return false;
		}
		std::fseek(File, 0, SEEK_SET);
		OutBytes.resize(static_cast<size_t>(Size));
		const size_t ReadCount = std::fread(OutBytes.data(), 1, static_cast<size_t>(Size), File);
		std::fclose(File);
		if (ReadCount != static_cast<size_t>(Size))
		{
			OutBytes.clear();
			return false;
		}
		return true;
	}

	bool ReadFsFontBytes(const char* Subpath, std::vector<u8>& OutBytes)
	{
		if (xr_FS == nullptr || !FS.path_exist("$game_data$") || Subpath == nullptr)
		{
			return false;
		}
		if (IReader* Reader = FS.r_open("$game_data$", Subpath))
		{
			const int Size = Reader->length();
			if (Size > 0 && Size <= 32 * 1024 * 1024)
			{
				OutBytes.resize(static_cast<size_t>(Size));
				Reader->r(OutBytes.data(), Size);
				FS.r_close(Reader);
				return true;
			}
			FS.r_close(Reader);
		}
		return false;
	}

	bool LoadMonospaceFontData(std::vector<u8>& OutBytes, bool bMediumWeight)
	{
		// 1. Prefer game-shipped Roboto Mono / Consolas via FS (works for both loose gamedata and .db packs).
		if (bMediumWeight)
		{
			static constexpr const char* MediumFsSubpaths[] = {
				"fonts\\eng\\robotomono-medium.ttf",
				"fonts\\eng\\robotomono-bold.ttf",
				"fonts\\eng\\consolab.ttf",
				"fonts\\eng\\robotomono.ttf",
				"fonts\\editors\\robotomono.ttf",
				"fonts\\eng\\consola.ttf",
			};
			for (const char* Subpath : MediumFsSubpaths)
			{
				if (ReadFsFontBytes(Subpath, OutBytes))
				{
					return true;
				}
			}
		}
		else
		{
			static constexpr const char* RegularFsSubpaths[] = {
				"fonts\\eng\\robotomono.ttf",
				"fonts\\editors\\robotomono.ttf",
				"fonts\\eng\\consola.ttf",
				"fonts\\eng\\robotomono-medium.ttf",
			};
			for (const char* Subpath : RegularFsSubpaths)
			{
				if (ReadFsFontBytes(Subpath, OutBytes))
				{
					return true;
				}
			}
		}

		// 2. Relative loose files and OS monospace font fallbacks
		if (bMediumWeight)
		{
			static constexpr const char* MediumPaths[] = {
				"gamedata/fonts/eng/robotomono-medium.ttf",
				"gamedata/fonts/eng/robotomono-bold.ttf",
				"gamedata/fonts/eng/consolab.ttf",
				"gamedata/fonts/eng/robotomono.ttf",
				"gamedata/fonts/editors/robotomono.ttf",
				"gamedata/fonts/eng/consola.ttf",
#if defined(IXR_WINDOWS)
				"C:\\Windows\\Fonts\\consolab.ttf",
				"C:\\Windows\\Fonts\\consola.ttf",
				"C:\\Windows\\Fonts\\lucon.ttf",
				"C:\\Windows\\Fonts\\courbd.ttf",
#elif defined(IXR_LINUX)
				"/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf",
				"/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf",
				"/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf",
				"/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
#elif defined(IXR_MACOS)
				"/System/Library/Fonts/Menlo.ttc",
				"/System/Library/Fonts/Supplemental/Courier New Bold.ttf",
#endif
			};
			for (const char* Path : MediumPaths)
			{
				if (ReadFileBytes(Path, OutBytes))
				{
					return true;
				}
			}
		}
		else
		{
			static constexpr const char* RegularPaths[] = {
				"gamedata/fonts/eng/robotomono.ttf",
				"gamedata/fonts/editors/robotomono.ttf",
				"gamedata/fonts/eng/consola.ttf",
				"gamedata/fonts/eng/robotomono-medium.ttf",
#if defined(IXR_WINDOWS)
				"C:\\Windows\\Fonts\\consola.ttf",
				"C:\\Windows\\Fonts\\lucon.ttf",
				"C:\\Windows\\Fonts\\cour.ttf",
#elif defined(IXR_LINUX)
				"/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
				"/usr/share/fonts/TTF/DejaVuSansMono.ttf",
				"/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
#elif defined(IXR_MACOS)
				"/System/Library/Fonts/Menlo.ttc",
				"/System/Library/Fonts/Supplemental/Courier New.ttf",
#endif
			};
			for (const char* Path : RegularPaths)
			{
				if (ReadFileBytes(Path, OutBytes))
				{
					return true;
				}
			}
		}
		return false;
	}

	bool BakeFont(SDL_Renderer* Renderer, const std::vector<u8>& FontData, float PixelHeight, BakedFont& OutFont)
	{
		OutFont.Destroy();
		if (Renderer == nullptr || FontData.empty())
		{
			return false;
		}

		const int FontOffset = stbtt_GetFontOffsetForIndex(FontData.data(), 0);
		if (FontOffset < 0)
		{
			return false;
		}

		stbtt_fontinfo Info = {};
		if (!stbtt_InitFont(&Info, FontData.data(), FontOffset))
		{
			return false;
		}

		const float Scale = stbtt_ScaleForPixelHeight(&Info, PixelHeight);
		int RawAscent = 0;
		int RawDescent = 0;
		int RawLineGap = 0;
		stbtt_GetFontVMetrics(&Info, &RawAscent, &RawDescent, &RawLineGap);

		OutFont.Ascent = std::round(static_cast<float>(RawAscent) * Scale);
		const float ComputedLineH = std::round(static_cast<float>(RawAscent - RawDescent + RawLineGap) * Scale);
		OutFont.LineHeight = std::max(PixelHeight * 1.25f, ComputedLineH + 2.0f);

		constexpr int AtlasW = 512;
		constexpr int AtlasH = 512;
		std::vector<u8> AlphaBitmap(static_cast<size_t>(AtlasW) * static_cast<size_t>(AtlasH), 0);

		int PenX = 2;
		int PenY = 2;
		int RowHeight = 0;

		for (int Code = BakedFont::FirstChar; Code <= BakedFont::LastChar; ++Code)
		{
			int AdvanceWidth = 0;
			int LeftBearing = 0;
			stbtt_GetCodepointHMetrics(&Info, Code, &AdvanceWidth, &LeftBearing);

			int X0 = 0;
			int Y0 = 0;
			int X1 = 0;
			int Y1 = 0;
			stbtt_GetCodepointBitmapBox(&Info, Code, Scale, Scale, &X0, &Y0, &X1, &Y1);

			const int W = X1 - X0;
			const int H = Y1 - Y0;

			if (PenX + W + 2 >= AtlasW)
			{
				PenX = 2;
				PenY += RowHeight + 2;
				RowHeight = 0;
			}
			if (PenY + H + 2 >= AtlasH)
			{
				break;
			}

			GlyphInfo& Glyph = OutFont.Glyphs[Code - BakedFont::FirstChar];
			Glyph.Advance = std::round(static_cast<float>(AdvanceWidth) * Scale);
			Glyph.OffsetX = static_cast<float>(X0);
			Glyph.OffsetY = static_cast<float>(Y0);
			Glyph.bValid = true;

			if (W > 0 && H > 0)
			{
				stbtt_MakeCodepointBitmap(
					&Info,
					AlphaBitmap.data() + static_cast<size_t>(PenY) * static_cast<size_t>(AtlasW) + static_cast<size_t>(PenX),
					W,
					H,
					AtlasW,
					Scale,
					Scale,
					Code
				);
				Glyph.SrcRect = { static_cast<float>(PenX), static_cast<float>(PenY), static_cast<float>(W), static_cast<float>(H) };
				PenX += W + 2;
				if (H > RowHeight)
				{
					RowHeight = H;
				}
			}
		}

		// Convert alpha mask to white RGBA32 texture for crisp tinted alpha blending.
		std::vector<u32> RgbaPixels(static_cast<size_t>(AtlasW) * static_cast<size_t>(AtlasH), 0);
		for (size_t i = 0; i < AlphaBitmap.size(); ++i)
		{
			const u32 Alpha = AlphaBitmap[i];
			RgbaPixels[i] = (Alpha << 24) | 0x00FFFFFFu;
		}

		OutFont.Texture = SDL_CreateTexture(Renderer, SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STATIC, AtlasW, AtlasH);
		if (OutFont.Texture == nullptr)
		{
			return false;
		}

		SDL_UpdateTexture(OutFont.Texture, nullptr, RgbaPixels.data(), AtlasW * static_cast<int>(sizeof(u32)));
		SDL_SetTextureBlendMode(OutFont.Texture, SDL_BLENDMODE_BLEND);
		SDL_SetTextureScaleMode(OutFont.Texture, SDL_SCALEMODE_NEAREST);

		const GlyphInfo& RefGlyph = OutFont.Glyphs['M' - BakedFont::FirstChar];
		OutFont.MonoAdvance = RefGlyph.Advance > 0.0f ? RefGlyph.Advance : std::round(PixelHeight * 0.6f);
		OutFont.bLoaded = true;
		return true;
	}

	float MeasureTextWidth(const BakedFont& Font, float FallbackScale, const char* Text)
	{
		if (Text == nullptr || Text[0] == '\0')
		{
			return 0.0f;
		}
		if (!Font.bLoaded)
		{
			return static_cast<float>(std::strlen(Text)) * 8.0f * FallbackScale;
		}
		float Width = 0.0f;
		for (const unsigned char* Ptr = reinterpret_cast<const unsigned char*>(Text); *Ptr != '\0'; ++Ptr)
		{
			if (*Ptr == '\t')
			{
				Width += Font.MonoAdvance * 4.0f;
				continue;
			}
			const int Code = (*Ptr >= BakedFont::FirstChar && *Ptr <= BakedFont::LastChar) ? static_cast<int>(*Ptr) : '?';
			Width += Font.Glyphs[Code - BakedFont::FirstChar].Advance;
		}
		return Width;
	}

	void DrawTextLine(SDL_Renderer* Renderer, const BakedFont& Font, float FallbackScale, float X, float Y, u8 R, u8 G, u8 B, const char* Text)
	{
		if (Text == nullptr || Text[0] == '\0')
		{
			return;
		}
		if (!Font.bLoaded)
		{
			SDL_SetRenderDrawColor(Renderer, R, G, B, 255);
			if (FallbackScale != 1.0f)
			{
				SDL_SetRenderScale(Renderer, FallbackScale, FallbackScale);
				SDL_RenderDebugText(Renderer, X / FallbackScale, Y / FallbackScale, Text);
				SDL_SetRenderScale(Renderer, 1.0f, 1.0f);
			}
			else
			{
				SDL_RenderDebugText(Renderer, X, Y, Text);
			}
			return;
		}

		SDL_SetTextureColorMod(Font.Texture, R, G, B);
		float PenX = std::round(X);
		const float BaselineY = std::round(Y + Font.Ascent);

		for (const unsigned char* Ptr = reinterpret_cast<const unsigned char*>(Text); *Ptr != '\0'; ++Ptr)
		{
			if (*Ptr == '\t')
			{
				PenX += Font.MonoAdvance * 4.0f;
				continue;
			}
			const int Code = (*Ptr >= BakedFont::FirstChar && *Ptr <= BakedFont::LastChar) ? static_cast<int>(*Ptr) : '?';
			const GlyphInfo& Glyph = Font.Glyphs[Code - BakedFont::FirstChar];
			if (Glyph.SrcRect.w > 0.0f && Glyph.SrcRect.h > 0.0f)
			{
				const SDL_FRect DstRect = {
					PenX + Glyph.OffsetX,
					BaselineY + Glyph.OffsetY,
					Glyph.SrcRect.w,
					Glyph.SrcRect.h
				};
				SDL_RenderTexture(Renderer, Font.Texture, &Glyph.SrcRect, &DstRect);
			}
			PenX += Glyph.Advance;
		}
	}

	void LoadLines(EditorState& State, const char* Text)
	{
		State.Lines.clear();
		if (Text == nullptr || Text[0] == '\0')
		{
			State.Lines.emplace_back("No callstack information available.");
			return;
		}

		const char* Ptr = Text;
		while (*Ptr != '\0')
		{
			const char* LineStart = Ptr;
			while (*Ptr != '\0' && *Ptr != '\n')
			{
				++Ptr;
			}
			size_t Len = static_cast<size_t>(Ptr - LineStart);
			if (Len > 0 && LineStart[Len - 1] == '\r')
			{
				--Len;
			}
			State.Lines.emplace_back(LineStart, Len);
			if (*Ptr == '\n')
			{
				++Ptr;
			}
		}

		if (State.Lines.empty())
		{
			State.Lines.emplace_back();
		}
	}

	std::string JoinLines(const EditorState& State)
	{
		std::string Result;
		for (size_t i = 0; i < State.Lines.size(); ++i)
		{
			if (i > 0)
			{
				Result.push_back('\n');
			}
			Result.append(State.Lines[i]);
		}
		return Result;
	}

	std::string WrapMarkdownCodeBlock(const std::string& Text)
	{
		std::string Result;
		Result.reserve(Text.size() + 10);
		Result.append("```\n");
		Result.append(Text);
		if (!Text.empty() && Text.back() != '\n')
		{
			Result.push_back('\n');
		}
		Result.append("```");
		return Result;
	}

	int VisibleLineCount(const EditorState& State)
	{
		const float InnerH = BoxH - BoxPad * 2.0f - ScrollbarW - 6.0f;
		return std::max(1, static_cast<int>(InnerH / State.LineHeight));
	}

	int VisibleColCount(const EditorState& State)
	{
		const float InnerW = BoxW - BoxPad * 2.0f - ScrollbarW - 6.0f;
		return std::max(1, static_cast<int>(InnerW / State.CharAdvance));
	}

	int MaxLineLength(const EditorState& State)
	{
		int MaxLen = 0;
		for (const std::string& Line : State.Lines)
		{
			const int Len = static_cast<int>(Line.size());
			if (Len > MaxLen)
			{
				MaxLen = Len;
			}
		}
		return MaxLen;
	}

	int MaxScrollY(const EditorState& State)
	{
		return std::max(0, static_cast<int>(State.Lines.size()) - VisibleLineCount(State));
	}

	int MaxScrollX(const EditorState& State)
	{
		return std::max(0, MaxLineLength(State) - VisibleColCount(State) + 2);
	}

	void ClampScroll(EditorState& State)
	{
		State.ScrollY = std::clamp(State.ScrollY, 0, MaxScrollY(State));
		State.ScrollX = std::clamp(State.ScrollX, 0, MaxScrollX(State));
	}

	void EnsureCursorVisible(EditorState& State)
	{
		const int VisLines = VisibleLineCount(State);
		if (State.CursorLine < State.ScrollY)
		{
			State.ScrollY = State.CursorLine;
		}
		else if (State.CursorLine >= State.ScrollY + VisLines)
		{
			State.ScrollY = State.CursorLine - VisLines + 1;
		}

		const int VisCols = VisibleColCount(State);
		if (State.CursorCol < State.ScrollX)
		{
			State.ScrollX = State.CursorCol;
		}
		else if (State.CursorCol >= State.ScrollX + VisCols)
		{
			State.ScrollX = State.CursorCol - VisCols + 1;
		}
		ClampScroll(State);
	}

	bool HasSelection(const EditorState& State)
	{
		return State.bHasSelection && (State.SelAnchorLine != State.CursorLine || State.SelAnchorCol != State.CursorCol);
	}

	void ClearSelection(EditorState& State)
	{
		State.bHasSelection = false;
		State.SelAnchorLine = State.CursorLine;
		State.SelAnchorCol = State.CursorCol;
	}

	void BeginOrKeepSelection(EditorState& State, bool bExtend)
	{
		if (bExtend)
		{
			if (!State.bHasSelection)
			{
				State.SelAnchorLine = State.CursorLine;
				State.SelAnchorCol = State.CursorCol;
				State.bHasSelection = true;
			}
		}
		else
		{
			State.bHasSelection = false;
		}
	}

	void GetSelectionRange(const EditorState& State, int& StartLine, int& StartCol, int& EndLine, int& EndCol)
	{
		if (State.SelAnchorLine < State.CursorLine || (State.SelAnchorLine == State.CursorLine && State.SelAnchorCol <= State.CursorCol))
		{
			StartLine = State.SelAnchorLine;
			StartCol = State.SelAnchorCol;
			EndLine = State.CursorLine;
			EndCol = State.CursorCol;
		}
		else
		{
			StartLine = State.CursorLine;
			StartCol = State.CursorCol;
			EndLine = State.SelAnchorLine;
			EndCol = State.SelAnchorCol;
		}
	}

	std::string GetSelectedText(const EditorState& State)
	{
		if (!HasSelection(State))
		{
			return {};
		}
		int StartLine = 0;
		int StartCol = 0;
		int EndLine = 0;
		int EndCol = 0;
		GetSelectionRange(State, StartLine, StartCol, EndLine, EndCol);

		if (StartLine == EndLine)
		{
			const std::string& Line = State.Lines[static_cast<size_t>(StartLine)];
			return Line.substr(static_cast<size_t>(StartCol), static_cast<size_t>(EndCol - StartCol));
		}

		std::string Result = State.Lines[static_cast<size_t>(StartLine)].substr(static_cast<size_t>(StartCol));
		for (int i = StartLine + 1; i < EndLine; ++i)
		{
			Result.push_back('\n');
			Result.append(State.Lines[static_cast<size_t>(i)]);
		}
		Result.push_back('\n');
		Result.append(State.Lines[static_cast<size_t>(EndLine)], 0, static_cast<size_t>(EndCol));
		return Result;
	}

	void DeleteSelection(EditorState& State)
	{
		if (!HasSelection(State))
		{
			ClearSelection(State);
			return;
		}
		int StartLine = 0;
		int StartCol = 0;
		int EndLine = 0;
		int EndCol = 0;
		GetSelectionRange(State, StartLine, StartCol, EndLine, EndCol);

		if (StartLine == EndLine)
		{
			State.Lines[static_cast<size_t>(StartLine)].erase(static_cast<size_t>(StartCol), static_cast<size_t>(EndCol - StartCol));
		}
		else
		{
			std::string Tail = State.Lines[static_cast<size_t>(EndLine)].substr(static_cast<size_t>(EndCol));
			State.Lines[static_cast<size_t>(StartLine)].erase(static_cast<size_t>(StartCol));
			State.Lines[static_cast<size_t>(StartLine)].append(Tail);
			State.Lines.erase(State.Lines.begin() + StartLine + 1, State.Lines.begin() + EndLine + 1);
		}

		State.CursorLine = StartLine;
		State.CursorCol = StartCol;
		ClearSelection(State);
		State.CopiedUntilTicks = 0;
		EnsureCursorVisible(State);
	}

	bool IsWordChar(char C)
	{
		return (C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') || (C >= '0' && C <= '9') || C == '_';
	}

	void ScreenPosToCursor(const EditorState& State, float MX, float MY, int& OutLine, int& OutCol)
	{
		const int ClickedLine = State.ScrollY + static_cast<int>(std::floor((MY - (BoxY + BoxPad)) / State.LineHeight));
		OutLine = std::clamp(ClickedLine, 0, static_cast<int>(State.Lines.size()) - 1);
		const int ClickedCol = State.ScrollX + static_cast<int>(std::floor((MX - (BoxX + BoxPad) + State.CharAdvance * 0.5f) / State.CharAdvance));
		OutCol = std::clamp(ClickedCol, 0, static_cast<int>(State.Lines[static_cast<size_t>(OutLine)].size()));
	}

	void InsertTextAtCursor(EditorState& State, const char* Text)
	{
		if (Text == nullptr || Text[0] == '\0')
		{
			return;
		}
		if (HasSelection(State))
		{
			DeleteSelection(State);
		}
		if (State.Lines.empty())
		{
			State.Lines.emplace_back();
		}

		for (const char* Ptr = Text; *Ptr != '\0'; ++Ptr)
		{
			if (*Ptr == '\r')
			{
				continue;
			}
			if (*Ptr == '\n')
			{
				std::string& Cur = State.Lines[static_cast<size_t>(State.CursorLine)];
				std::string Tail = Cur.substr(static_cast<size_t>(State.CursorCol));
				Cur.erase(static_cast<size_t>(State.CursorCol));
				++State.CursorLine;
				State.Lines.insert(State.Lines.begin() + State.CursorLine, std::move(Tail));
				State.CursorCol = 0;
			}
			else
			{
				std::string& Cur = State.Lines[static_cast<size_t>(State.CursorLine)];
				Cur.insert(static_cast<size_t>(State.CursorCol), 1, *Ptr);
				++State.CursorCol;
			}
		}
		ClearSelection(State);
		State.CopiedUntilTicks = 0;
		EnsureCursorVisible(State);
	}

	void BuildLogoGeometry(std::vector<SDL_Vertex>& OutVertices, std::vector<int>& OutIndices)
	{
		OutVertices.clear();
		OutIndices.clear();

		const float ContentH = static_cast<float>(WindowHeight) - WinBarHeight;
		const float LogoScale = (ContentH * 0.76f) / 727.0f;
		const float OffsetX = -500.0f * LogoScale;
		const float OffsetY = WinBarHeight + ContentH * 0.5f - 500.0f * LogoScale;

		auto AddVertex = [&](const PointF& Pt) -> int
		{
			SDL_Vertex V = {};
			V.position.x = Pt.X * LogoScale + OffsetX;
			V.position.y = Pt.Y * LogoScale + OffsetY;
			V.color = { 1.0f, 1.0f, 1.0f, 1.0f };
			const int Index = static_cast<int>(OutVertices.size());
			OutVertices.push_back(V);
			return Index;
		};

		auto AddQuad = [&](const PointF& A, const PointF& B, const PointF& C, const PointF& D)
		{
			const int IdxA = AddVertex(A);
			const int IdxB = AddVertex(B);
			const int IdxC = AddVertex(C);
			const int IdxD = AddVertex(D);
			OutIndices.push_back(IdxA);
			OutIndices.push_back(IdxB);
			OutIndices.push_back(IdxC);
			OutIndices.push_back(IdxA);
			OutIndices.push_back(IdxC);
			OutIndices.push_back(IdxD);
		};

		auto AddRing = [&](const PointF* Outer, const PointF* Inner, int Count)
		{
			for (int i = 0; i < Count; ++i)
			{
				const int Next = (i + 1) % Count;
				AddQuad(Outer[i], Outer[Next], Inner[Next], Inner[i]);
			}
		};

		// 1. Bottom trapezoid bar
		AddQuad(
			{ 250.823f, 863.968f },
			{ 292.933f, 791.416f },
			{ 706.487f, 791.416f },
			{ 748.597f, 863.968f }
		);

		// 2. Inner hexagon ring
		static constexpr PointF InnerRingOuter[6] = {
			{ 450.655f, 249.240f }, { 548.636f, 249.298f }, { 730.322f, 524.112f },
			{ 654.167f, 639.489f }, { 345.088f, 639.489f }, { 269.116f, 524.299f }
		};
		static constexpr PointF InnerRingInner[6] = {
			{ 477.288f, 321.792f }, { 522.022f, 321.792f }, { 655.690f, 524.322f },
			{ 627.562f, 566.937f }, { 371.767f, 566.937f }, { 343.629f, 524.299f }
		};
		AddRing(InnerRingOuter, InnerRingInner, 6);

		// 3. Outer hexagon ring
		static constexpr PointF OuterRingOuter[6] = {
			{ 409.000f, 137.012f }, { 590.500f, 137.023f }, { 845.500f, 524.000f },
			{ 695.350f, 751.728f }, { 304.079f, 751.728f }, { 153.852f, 524.112f }
		};
		static constexpr PointF OuterRingInner[6] = {
			{ 436.261f, 209.552f }, { 563.159f, 209.552f }, { 770.927f, 524.345f },
			{ 668.735f, 679.176f }, { 330.694f, 679.176f }, { 228.502f, 524.345f }
		};
		AddRing(OuterRingOuter, OuterRingInner, 6);

		// 4. Left diagonal wing
		AddQuad(
			{ 370.987f, 137.000f },
			{ 115.494f, 524.112f },
			{ 41.000f, 524.112f },
			{ 296.483f, 137.000f }
		);

		// 5. Right diagonal wing
		AddQuad(
			{ 702.937f, 137.000f },
			{ 958.420f, 524.112f },
			{ 883.925f, 524.112f },
			{ 628.433f, 137.000f }
		);

		// 6. Center triangle
		const int T0 = AddVertex({ 393.188f, 527.249f });
		const int T1 = AddVertex({ 498.829f, 361.479f });
		const int T2 = AddVertex({ 606.232f, 527.249f });
		OutIndices.push_back(T0);
		OutIndices.push_back(T1);
		OutIndices.push_back(T2);
	}

	// Rasterizes the IX-Ray logo triangles once with 4x4 subpixel SSAA into a blended SDL_Texture.
	// Using a 16-bit per-pixel sample mask eliminates seams and double-counting on shared quad edges.
	SDL_Texture* CreateAntiAliasedLogoTexture(SDL_Renderer* Renderer, int Width, int Height)
	{
		if (Renderer == nullptr || Width <= 0 || Height <= 0)
		{
			return nullptr;
		}

		std::vector<SDL_Vertex> Vertices;
		std::vector<int> Indices;
		BuildLogoGeometry(Vertices, Indices);

		std::vector<u16> SampleMasks(static_cast<size_t>(Width) * static_cast<size_t>(Height), 0);

		// 4x4 rotated-grid-friendly stratified offsets in [0, 1)
		static constexpr float SubOffsets[4] = { 0.125f, 0.375f, 0.625f, 0.875f };

		auto EdgeFn = [](float Ax, float Ay, float Bx, float By, float Px, float Py) -> float
		{
			return (Px - Ax) * (By - Ay) - (Py - Ay) * (Bx - Ax);
		};

		for (size_t i = 0; i + 2 < Indices.size(); i += 3)
		{
			float Ax = Vertices[static_cast<size_t>(Indices[i + 0])].position.x;
			float Ay = Vertices[static_cast<size_t>(Indices[i + 0])].position.y;
			float Bx = Vertices[static_cast<size_t>(Indices[i + 1])].position.x;
			float By = Vertices[static_cast<size_t>(Indices[i + 1])].position.y;
			float Cx = Vertices[static_cast<size_t>(Indices[i + 2])].position.x;
			float Cy = Vertices[static_cast<size_t>(Indices[i + 2])].position.y;

			const float Area = EdgeFn(Ax, Ay, Bx, By, Cx, Cy);
			if (std::fabs(Area) < 1e-5f)
			{
				continue;
			}
			if (Area < 0.0f)
			{
				std::swap(Bx, Cx);
				std::swap(By, Cy);
			}

			const int MinX = std::clamp(static_cast<int>(std::floor(std::min({ Ax, Bx, Cx }))), 0, Width - 1);
			const int MaxX = std::clamp(static_cast<int>(std::ceil(std::max({ Ax, Bx, Cx }))), 0, Width - 1);
			const int MinY = std::clamp(static_cast<int>(std::floor(std::min({ Ay, By, Cy }))), 0, Height - 1);
			const int MaxY = std::clamp(static_cast<int>(std::ceil(std::max({ Ay, By, Cy }))), 0, Height - 1);

			for (int Y = MinY; Y <= MaxY; ++Y)
			{
				u16* RowMask = SampleMasks.data() + static_cast<size_t>(Y) * static_cast<size_t>(Width);
				for (int X = MinX; X <= MaxX; ++X)
				{
					u16 Mask = RowMask[X];
					if (Mask == 0xFFFFu)
					{
						continue;
					}
					for (int sy = 0; sy < 4; ++sy)
					{
						const float Py = static_cast<float>(Y) + SubOffsets[sy];
						for (int sx = 0; sx < 4; ++sx)
						{
							const u16 Bit = static_cast<u16>(1u << (sy * 4 + sx));
							if ((Mask & Bit) != 0)
							{
								continue;
							}
							const float Px = static_cast<float>(X) + SubOffsets[sx];
							if (EdgeFn(Ax, Ay, Bx, By, Px, Py) >= 0.0f
								&& EdgeFn(Bx, By, Cx, Cy, Px, Py) >= 0.0f
								&& EdgeFn(Cx, Cy, Ax, Ay, Px, Py) >= 0.0f)
							{
								Mask |= Bit;
							}
						}
					}
					RowMask[X] = Mask;
				}
			}
		}

		std::vector<u32> RgbaPixels(static_cast<size_t>(Width) * static_cast<size_t>(Height), 0);
		for (size_t i = 0; i < SampleMasks.size(); ++i)
		{
			const u32 Covered = static_cast<u32>(std::popcount(SampleMasks[i]));
			if (Covered > 0)
			{
				const u32 Alpha = (Covered * 255u + 8u) / 16u;
				RgbaPixels[i] = (Alpha << 24) | 0x00FFFFFFu;
			}
		}

		SDL_Texture* Texture = SDL_CreateTexture(Renderer, SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STATIC, Width, Height);
		if (Texture == nullptr)
		{
			return nullptr;
		}
		SDL_UpdateTexture(Texture, nullptr, RgbaPixels.data(), Width * static_cast<int>(sizeof(u32)));
		SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND);
		SDL_SetTextureScaleMode(Texture, SDL_SCALEMODE_NEAREST);
		return Texture;
	}

	void DrawWinBar(
		SDL_Renderer* Renderer,
		const BakedFont& Font,
		const SDL_FRect& BarRect,
		const SDL_FRect& MinRect,
		const SDL_FRect& CloseRect,
		bool bHoverMin,
		bool bPressMin,
		bool bHoverClose,
		bool bPressClose
	)
	{
		// Title bar background + subtle bottom divider
		SDL_SetRenderDrawColor(Renderer, 13, 13, 13, 255);
		SDL_RenderFillRect(Renderer, &BarRect);
		SDL_SetRenderDrawColor(Renderer, 34, 34, 34, 255);
		SDL_RenderLine(Renderer, 0.0f, BarRect.h - 1.0f, BarRect.w, BarRect.h - 1.0f);

		// Left title text
		const char* BarTitle = "IX-Ray Crash Reporter";
		const float LabelH = Font.bLoaded ? Font.Ascent : 8.0f;
		const float TextY = (BarRect.h - LabelH) * 0.5f - (Font.bLoaded ? 1.0f : 0.0f);
		DrawTextLine(Renderer, Font, 1.0f, 14.0f, TextY, 170, 170, 170, BarTitle);

		// Minimize button
		if (bPressMin)
		{
			SDL_SetRenderDrawColor(Renderer, 44, 44, 44, 255);
			SDL_RenderFillRect(Renderer, &MinRect);
		}
		else if (bHoverMin)
		{
			SDL_SetRenderDrawColor(Renderer, 30, 30, 30, 255);
			SDL_RenderFillRect(Renderer, &MinRect);
		}
		{
			const u8 IconC = (bHoverMin || bPressMin) ? 255 : 185;
			SDL_SetRenderDrawColor(Renderer, IconC, IconC, IconC, 255);
			const float Cx = MinRect.x + MinRect.w * 0.5f;
			const float Cy = MinRect.y + MinRect.h * 0.5f;
			SDL_RenderLine(Renderer, Cx - 5.0f, Cy, Cx + 5.0f, Cy);
		}

		// Close button
		if (bPressClose)
		{
			SDL_SetRenderDrawColor(Renderer, 162, 34, 22, 255);
			SDL_RenderFillRect(Renderer, &CloseRect);
		}
		else if (bHoverClose)
		{
			SDL_SetRenderDrawColor(Renderer, 196, 43, 28, 255);
			SDL_RenderFillRect(Renderer, &CloseRect);
		}
		{
			const u8 IconC = (bHoverClose || bPressClose) ? 255 : 185;
			SDL_SetRenderDrawColor(Renderer, IconC, IconC, IconC, 255);
			const float Cx = CloseRect.x + CloseRect.w * 0.5f;
			const float Cy = CloseRect.y + CloseRect.h * 0.5f;
			SDL_RenderLine(Renderer, Cx - 4.5f, Cy - 4.5f, Cx + 4.5f, Cy + 4.5f);
			SDL_RenderLine(Renderer, Cx + 4.5f, Cy - 4.5f, Cx - 4.5f, Cy + 4.5f);
		}
	}

	void DrawButton(SDL_Renderer* Renderer, const BakedFont& Font, const SDL_FRect& Rect, const char* Label, bool bHovered, bool bPressed)
	{
		if (bPressed)
		{
			SDL_SetRenderDrawColor(Renderer, 42, 42, 42, 255);
		}
		else if (bHovered)
		{
			SDL_SetRenderDrawColor(Renderer, 29, 29, 29, 255);
		}
		else
		{
			SDL_SetRenderDrawColor(Renderer, 17, 17, 17, 255);
		}
		SDL_RenderFillRect(Renderer, &Rect);

		if (bHovered || bPressed)
		{
			SDL_SetRenderDrawColor(Renderer, 96, 96, 96, 255);
		}
		else
		{
			SDL_SetRenderDrawColor(Renderer, 58, 58, 58, 255);
		}
		SDL_RenderRect(Renderer, &Rect);

		const float LabelW = MeasureTextWidth(Font, 1.0f, Label);
		const float LabelH = Font.bLoaded ? Font.Ascent : 8.0f;
		const float TextX = Rect.x + (Rect.w - LabelW) * 0.5f;
		const float TextY = Rect.y + (Rect.h - LabelH) * 0.5f - (Font.bLoaded ? 1.0f : 0.0f);

		DrawTextLine(Renderer, Font, 1.0f, TextX, TextY, 255, 255, 255, Label);
	}

	SDL_FRect ComputeVThumbRect(const EditorState& State, const SDL_FRect& TrackRect)
	{
		const int Total = std::max(1, static_cast<int>(State.Lines.size()));
		const int Visible = VisibleLineCount(State);
		if (Total <= Visible)
		{
			return { TrackRect.x, TrackRect.y, TrackRect.w, TrackRect.h };
		}
		const float Ratio = static_cast<float>(Visible) / static_cast<float>(Total);
		const float ThumbH = std::max(24.0f, TrackRect.h * Ratio);
		const float ScrollFrac = static_cast<float>(State.ScrollY) / static_cast<float>(Total - Visible);
		const float ThumbY = TrackRect.y + (TrackRect.h - ThumbH) * ScrollFrac;
		return { TrackRect.x, ThumbY, TrackRect.w, ThumbH };
	}

	SDL_FRect ComputeHThumbRect(const EditorState& State, const SDL_FRect& TrackRect)
	{
		const int MaxX = MaxScrollX(State);
		const int Visible = VisibleColCount(State);
		const int Total = MaxX + Visible;
		if (MaxX <= 0 || Total <= Visible)
		{
			return { TrackRect.x, TrackRect.y, TrackRect.w, TrackRect.h };
		}
		const float Ratio = static_cast<float>(Visible) / static_cast<float>(Total);
		const float ThumbW = std::max(28.0f, TrackRect.w * Ratio);
		const float ScrollFrac = static_cast<float>(State.ScrollX) / static_cast<float>(MaxX);
		const float ThumbX = TrackRect.x + (TrackRect.w - ThumbW) * ScrollFrac;
		return { ThumbX, TrackRect.y, ThumbW, TrackRect.h };
	}
} // namespace

void Show(const char* Title, const char* Details)
{
	const char* SafeTitle = (Title != nullptr && Title[0] != '\0') ? Title : "IX-Ray process has crashed";
	const char* SafeDetails = (Details != nullptr && Details[0] != '\0') ? Details : "No callstack information available.";

	if ((SDL_WasInit(SDL_INIT_VIDEO) & SDL_INIT_VIDEO) == 0)
	{
		if (!SDL_InitSubSystem(SDL_INIT_VIDEO))
		{
			char ErrMsg[256] = {};
			std::snprintf(ErrMsg, sizeof(ErrMsg), "! CrashWindow: SDL_InitSubSystem failed: %s", SDL_GetError());
			xrLogger::WriteCrashLine(ErrMsg);
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, SafeTitle, SafeDetails, nullptr);
			return;
		}
	}

	// Create the crash window BEFORE hiding existing game windows so SDL never hits a 0-window state.
	SDL_Window* Window = SDL_CreateWindow(
		SafeTitle,
		WindowWidth,
		WindowHeight,
		SDL_WINDOW_BORDERLESS | SDL_WINDOW_ALWAYS_ON_TOP
	);
	if (Window == nullptr)
	{
		char ErrMsg[256] = {};
		std::snprintf(ErrMsg, sizeof(ErrMsg), "! CrashWindow: SDL_CreateWindow failed: %s", SDL_GetError());
		xrLogger::WriteCrashLine(ErrMsg);
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, SafeTitle, SafeDetails, nullptr);
		return;
	}

	const SDL_WindowID CrashWindowId = SDL_GetWindowID(Window);

	// Release fullscreen/grab and hide all other SDL windows (game + ImGui tool windows) so they cannot cover or steal input.
	if (g_AppInfo.IsPrimaryThread())
	{
		int ExistingCount = 0;
		SDL_Window** ExistingWindows = SDL_GetWindows(&ExistingCount);
		if (ExistingWindows != nullptr)
		{
			for (int i = 0; i < ExistingCount; ++i)
			{
				SDL_Window* Other = ExistingWindows[i];
				if (Other != nullptr && Other != Window)
				{
					SDL_SetWindowRelativeMouseMode(Other, false);
					SDL_SetWindowMouseGrab(Other, false);
					SDL_SetWindowKeyboardGrab(Other, false);
					SDL_SetWindowFullscreen(Other, false);
					SDL_HideWindow(Other);
				}
			}
			SDL_free(ExistingWindows);
		}
	}

	SDL_ShowCursor();
	SDL_SetWindowPosition(Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_SetWindowRelativeMouseMode(Window, false);

	// Prefer the software renderer so the crash UI still renders even if the GPU / D3D12 / Vulkan device crashed.
	SDL_Renderer* Renderer = SDL_CreateRenderer(Window, "software");
	if (Renderer == nullptr)
	{
		Renderer = SDL_CreateRenderer(Window, nullptr);
	}
	if (Renderer == nullptr)
	{
		char ErrMsg[256] = {};
		std::snprintf(ErrMsg, sizeof(ErrMsg), "! CrashWindow: SDL_CreateRenderer failed: %s", SDL_GetError());
		xrLogger::WriteCrashLine(ErrMsg);
		SDL_DestroyWindow(Window);
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, SafeTitle, SafeDetails, nullptr);
		return;
	}

	// Bake anti-aliased TrueType monospace fonts (Medium for header & buttons, Regular for callstack).
	BakedFont TitleFont;
	BakedFont ButtonFont;
	BakedFont BodyFont;
	{
		std::vector<u8> MediumFontData;
		if (LoadMonospaceFontData(MediumFontData, true))
		{
			BakeFont(Renderer, MediumFontData, 28.0f, TitleFont);
			BakeFont(Renderer, MediumFontData, 14.0f, ButtonFont);
		}
		std::vector<u8> RegularFontData;
		if (LoadMonospaceFontData(RegularFontData, false))
		{
			BakeFont(Renderer, RegularFontData, 14.0f, BodyFont);
		}
		else if (!MediumFontData.empty())
		{
			BakeFont(Renderer, MediumFontData, 14.0f, BodyFont);
		}
	}

	const SDL_FRect WinBarRect = { 0.0f, 0.0f, static_cast<float>(WindowWidth), WinBarHeight };
	const SDL_FRect CloseRect = { static_cast<float>(WindowWidth) - WinBarButtonW - 1.0f, 1.0f, WinBarButtonW, WinBarHeight - 2.0f };
	const SDL_FRect MinRect = { CloseRect.x - WinBarButtonW, 1.0f, WinBarButtonW, WinBarHeight - 2.0f };

	const SDL_FRect BoxRect = { BoxX, BoxY, BoxW, BoxH };
	const SDL_FRect CopyRect = { BoxX, ButtonY, ButtonW, ButtonH };
	const SDL_FRect QuitRect = { BoxX + BoxW - ButtonW, ButtonY, ButtonW, ButtonH };
	const SDL_FRect VScrollTrackRect = { BoxX + BoxW - ScrollbarW - 4.0f, BoxY + 4.0f, ScrollbarW, BoxH - ScrollbarW - 12.0f };
	const SDL_FRect HScrollTrackRect = { BoxX + 4.0f, BoxY + BoxH - ScrollbarW - 4.0f, BoxW - ScrollbarW - 12.0f, ScrollbarW };

	WinBarHitTestContext HitCtx = { WinBarRect, MinRect, CloseRect };
	SDL_SetWindowHitTest(Window, &WinBarHitTest, &HitCtx);

	const int LogoTexW = static_cast<int>(LogoClipWidth);
	const int LogoTexH = WindowHeight;
	SDL_Texture* LogoTexture = CreateAntiAliasedLogoTexture(Renderer, LogoTexW, LogoTexH);
	std::vector<SDL_Vertex> LogoVertices;
	std::vector<int> LogoIndices;
	if (LogoTexture == nullptr)
	{
		BuildLogoGeometry(LogoVertices, LogoIndices);
	}

	EditorState State;
	State.CharAdvance = BodyFont.bLoaded ? BodyFont.MonoAdvance : 8.0f;
	State.LineHeight = BodyFont.bLoaded ? BodyFont.LineHeight : 14.0f;
	LoadLines(State, SafeDetails);

	SDL_ShowWindow(Window);
	SDL_RaiseWindow(Window);
	SDL_StartTextInput(Window);

	// Drop stale input/close events queued by the game before the crash (e.g. Enter/Esc from console).
	SDL_PumpEvents();
	SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);

	const Uint64 StartTicks = SDL_GetTicks();
	bool bRunning = true;

	while (bRunning)
	{
		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			switch (Event.type)
			{
			case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
				if (Event.window.windowID == CrashWindowId)
				{
					bRunning = false;
				}
				break;

			case SDL_EVENT_MOUSE_WHEEL:
			{
				const bool bShift = (SDL_GetModState() & SDL_KMOD_SHIFT) != 0;
				if (Event.wheel.x != 0.0f)
				{
					State.ScrollX += static_cast<int>(Event.wheel.x * 6.0f);
				}
				if (Event.wheel.y != 0.0f)
				{
					if (bShift)
					{
						State.ScrollX -= static_cast<int>(Event.wheel.y * 6.0f);
					}
					else
					{
						State.ScrollY -= static_cast<int>(Event.wheel.y * 3.0f);
					}
				}
				ClampScroll(State);
				break;
			}

			case SDL_EVENT_MOUSE_MOTION:
			{
				const float MX = Event.motion.x;
				const float MY = Event.motion.y;
				State.bHoveredCopy = PointInRect(MX, MY, CopyRect);
				State.bHoveredQuit = PointInRect(MX, MY, QuitRect);
				State.bHoveredMin = PointInRect(MX, MY, MinRect);
				State.bHoveredClose = PointInRect(MX, MY, CloseRect);

				if (State.bDraggingVScroll)
				{
					const int MaxY = MaxScrollY(State);
					const SDL_FRect Thumb = ComputeVThumbRect(State, VScrollTrackRect);
					const float Travel = VScrollTrackRect.h - Thumb.h;
					if (MaxY > 0 && Travel > 0.0f)
					{
						const float Frac = std::clamp((MY - VScrollTrackRect.y - State.DragScrollOffset) / Travel, 0.0f, 1.0f);
						State.ScrollY = static_cast<int>(Frac * static_cast<float>(MaxY) + 0.5f);
						ClampScroll(State);
					}
				}
				else if (State.bDraggingHScroll)
				{
					const int MaxX = MaxScrollX(State);
					const SDL_FRect Thumb = ComputeHThumbRect(State, HScrollTrackRect);
					const float Travel = HScrollTrackRect.w - Thumb.w;
					if (MaxX > 0 && Travel > 0.0f)
					{
						const float Frac = std::clamp((MX - HScrollTrackRect.x - State.DragScrollOffset) / Travel, 0.0f, 1.0f);
						State.ScrollX = static_cast<int>(Frac * static_cast<float>(MaxX) + 0.5f);
						ClampScroll(State);
					}
				}
				else if (State.bSelectingMouse)
				{
					ScreenPosToCursor(State, MX, MY, State.CursorLine, State.CursorCol);
					State.bHasSelection = (State.CursorLine != State.SelAnchorLine || State.CursorCol != State.SelAnchorCol);
					EnsureCursorVisible(State);
				}
				break;
			}

			case SDL_EVENT_MOUSE_BUTTON_DOWN:
			{
				if (Event.button.button == SDL_BUTTON_LEFT)
				{
					const float MX = Event.button.x;
					const float MY = Event.button.y;

					if (PointInRect(MX, MY, CloseRect))
					{
						State.bPressedClose = true;
						State.bFocused = false;
					}
					else if (PointInRect(MX, MY, MinRect))
					{
						State.bPressedMin = true;
						State.bFocused = false;
					}
					else if (PointInRect(MX, MY, CopyRect))
					{
						State.bPressedCopy = true;
						State.bFocused = false;
					}
					else if (PointInRect(MX, MY, QuitRect))
					{
						State.bPressedQuit = true;
						State.bFocused = false;
					}
					else if (PointInRect(MX, MY, VScrollTrackRect) && MaxScrollY(State) > 0)
					{
						const SDL_FRect Thumb = ComputeVThumbRect(State, VScrollTrackRect);
						State.bDraggingVScroll = true;
						if (PointInRect(MX, MY, Thumb))
						{
							State.DragScrollOffset = MY - Thumb.y;
						}
						else
						{
							State.DragScrollOffset = Thumb.h * 0.5f;
							const float Travel = VScrollTrackRect.h - Thumb.h;
							if (Travel > 0.0f)
							{
								const float Frac = std::clamp((MY - VScrollTrackRect.y - State.DragScrollOffset) / Travel, 0.0f, 1.0f);
								State.ScrollY = static_cast<int>(Frac * static_cast<float>(MaxScrollY(State)) + 0.5f);
								ClampScroll(State);
							}
						}
					}
					else if (PointInRect(MX, MY, HScrollTrackRect) && MaxScrollX(State) > 0)
					{
						const SDL_FRect Thumb = ComputeHThumbRect(State, HScrollTrackRect);
						State.bDraggingHScroll = true;
						if (PointInRect(MX, MY, Thumb))
						{
							State.DragScrollOffset = MX - Thumb.x;
						}
						else
						{
							State.DragScrollOffset = Thumb.w * 0.5f;
							const float Travel = HScrollTrackRect.w - Thumb.w;
							if (Travel > 0.0f)
							{
								const float Frac = std::clamp((MX - HScrollTrackRect.x - State.DragScrollOffset) / Travel, 0.0f, 1.0f);
								State.ScrollX = static_cast<int>(Frac * static_cast<float>(MaxScrollX(State)) + 0.5f);
								ClampScroll(State);
							}
						}
					}
					else if (PointInRect(MX, MY, BoxRect))
					{
						State.bFocused = true;
						const bool bShift = (SDL_GetModState() & SDL_KMOD_SHIFT) != 0;
						int ClickedLine = 0;
						int ClickedCol = 0;
						ScreenPosToCursor(State, MX, MY, ClickedLine, ClickedCol);

						if (Event.button.clicks >= 3)
						{
							State.SelAnchorLine = ClickedLine;
							State.SelAnchorCol = 0;
							State.CursorLine = ClickedLine;
							State.CursorCol = static_cast<int>(State.Lines[static_cast<size_t>(ClickedLine)].size());
							State.bHasSelection = (State.CursorCol > 0);
							State.bSelectingMouse = false;
						}
						else if (Event.button.clicks == 2)
						{
							const std::string& Line = State.Lines[static_cast<size_t>(ClickedLine)];
							const int Len = static_cast<int>(Line.size());
							int WordStart = std::clamp(ClickedCol, 0, Len);
							int WordEnd = WordStart;
							if (WordStart < Len)
							{
								const bool bWord = IsWordChar(Line[static_cast<size_t>(WordStart)]);
								while (WordStart > 0 && IsWordChar(Line[static_cast<size_t>(WordStart - 1)]) == bWord)
								{
									--WordStart;
								}
								while (WordEnd < Len && IsWordChar(Line[static_cast<size_t>(WordEnd)]) == bWord)
								{
									++WordEnd;
								}
							}
							State.SelAnchorLine = ClickedLine;
							State.SelAnchorCol = WordStart;
							State.CursorLine = ClickedLine;
							State.CursorCol = WordEnd;
							State.bHasSelection = (WordEnd > WordStart);
							State.bSelectingMouse = false;
						}
						else
						{
							BeginOrKeepSelection(State, bShift);
							State.CursorLine = ClickedLine;
							State.CursorCol = ClickedCol;
							if (!bShift)
							{
								State.SelAnchorLine = ClickedLine;
								State.SelAnchorCol = ClickedCol;
							}
							State.bHasSelection = bShift && (State.CursorLine != State.SelAnchorLine || State.CursorCol != State.SelAnchorCol);
							State.bSelectingMouse = true;
						}
					}
					else
					{
						State.bFocused = false;
					}
				}
				break;
			}

			case SDL_EVENT_MOUSE_BUTTON_UP:
			{
				if (Event.button.button == SDL_BUTTON_LEFT)
				{
					const float MX = Event.button.x;
					const float MY = Event.button.y;

					if (State.bPressedClose && PointInRect(MX, MY, CloseRect))
					{
						bRunning = false;
					}
					else if (State.bPressedMin && PointInRect(MX, MY, MinRect))
					{
						SDL_MinimizeWindow(Window);
					}
					else if (State.bPressedCopy && PointInRect(MX, MY, CopyRect))
					{
						const std::string FullText = WrapMarkdownCodeBlock(JoinLines(State));
						SDL_SetClipboardText(FullText.c_str());
						State.CopiedUntilTicks = SDL_GetTicks() + 2000;
					}
					else if (State.bPressedQuit && PointInRect(MX, MY, QuitRect))
					{
						bRunning = false;
					}
					State.bPressedClose = false;
					State.bPressedMin = false;
					State.bPressedCopy = false;
					State.bPressedQuit = false;
					State.bDraggingVScroll = false;
					State.bDraggingHScroll = false;
					State.bSelectingMouse = false;
				}
				break;
			}

			case SDL_EVENT_TEXT_INPUT:
				if (State.bFocused)
				{
					InsertTextAtCursor(State, Event.text.text);
				}
				break;

			case SDL_EVENT_KEY_DOWN:
			{
				const SDL_Keycode Key = Event.key.key;
				const bool bCtrl = (Event.key.mod & SDL_KMOD_CTRL) != 0;
				const bool bShift = (Event.key.mod & SDL_KMOD_SHIFT) != 0;

				if (Key == SDLK_ESCAPE)
				{
					bRunning = false;
					break;
				}

				if (bCtrl && Key == SDLK_A && State.bFocused)
				{
					State.SelAnchorLine = 0;
					State.SelAnchorCol = 0;
					State.CursorLine = static_cast<int>(State.Lines.size()) - 1;
					State.CursorCol = static_cast<int>(State.Lines.back().size());
					State.bHasSelection = (State.CursorLine != 0 || State.CursorCol != 0);
					break;
				}

				if (bCtrl && Key == SDLK_C)
				{
					const std::string TextToCopy = HasSelection(State) ? GetSelectedText(State) : WrapMarkdownCodeBlock(JoinLines(State));
					SDL_SetClipboardText(TextToCopy.c_str());
					State.CopiedUntilTicks = SDL_GetTicks() + 2000;
					break;
				}

				if (bCtrl && Key == SDLK_X && State.bFocused)
				{
					if (HasSelection(State))
					{
						const std::string Selected = GetSelectedText(State);
						SDL_SetClipboardText(Selected.c_str());
						DeleteSelection(State);
					}
					break;
				}

				if (bCtrl && Key == SDLK_V && State.bFocused)
				{
					char* Clip = SDL_GetClipboardText();
					if (Clip != nullptr)
					{
						InsertTextAtCursor(State, Clip);
						SDL_free(Clip);
					}
					break;
				}

				if (!State.bFocused)
				{
					break;
				}

				std::string& CurLine = State.Lines[static_cast<size_t>(State.CursorLine)];
				switch (Key)
				{
				case SDLK_LEFT:
					if (!bShift && HasSelection(State))
					{
						int SL = 0, SC = 0, EL = 0, EC = 0;
						GetSelectionRange(State, SL, SC, EL, EC);
						State.CursorLine = SL;
						State.CursorCol = SC;
						ClearSelection(State);
					}
					else
					{
						BeginOrKeepSelection(State, bShift);
						if (State.CursorCol > 0)
						{
							--State.CursorCol;
						}
						else if (State.CursorLine > 0)
						{
							--State.CursorLine;
							State.CursorCol = static_cast<int>(State.Lines[static_cast<size_t>(State.CursorLine)].size());
						}
					}
					EnsureCursorVisible(State);
					break;

				case SDLK_RIGHT:
					if (!bShift && HasSelection(State))
					{
						int SL = 0, SC = 0, EL = 0, EC = 0;
						GetSelectionRange(State, SL, SC, EL, EC);
						State.CursorLine = EL;
						State.CursorCol = EC;
						ClearSelection(State);
					}
					else
					{
						BeginOrKeepSelection(State, bShift);
						if (State.CursorCol < static_cast<int>(CurLine.size()))
						{
							++State.CursorCol;
						}
						else if (State.CursorLine + 1 < static_cast<int>(State.Lines.size()))
						{
							++State.CursorLine;
							State.CursorCol = 0;
						}
					}
					EnsureCursorVisible(State);
					break;

				case SDLK_UP:
					BeginOrKeepSelection(State, bShift);
					if (State.CursorLine > 0)
					{
						--State.CursorLine;
						State.CursorCol = std::min(State.CursorCol, static_cast<int>(State.Lines[static_cast<size_t>(State.CursorLine)].size()));
					}
					EnsureCursorVisible(State);
					break;

				case SDLK_DOWN:
					BeginOrKeepSelection(State, bShift);
					if (State.CursorLine + 1 < static_cast<int>(State.Lines.size()))
					{
						++State.CursorLine;
						State.CursorCol = std::min(State.CursorCol, static_cast<int>(State.Lines[static_cast<size_t>(State.CursorLine)].size()));
					}
					EnsureCursorVisible(State);
					break;

				case SDLK_HOME:
					BeginOrKeepSelection(State, bShift);
					State.CursorCol = 0;
					EnsureCursorVisible(State);
					break;

				case SDLK_END:
					BeginOrKeepSelection(State, bShift);
					State.CursorCol = static_cast<int>(CurLine.size());
					EnsureCursorVisible(State);
					break;

				case SDLK_PAGEUP:
					BeginOrKeepSelection(State, bShift);
					State.CursorLine = std::max(0, State.CursorLine - VisibleLineCount(State));
					State.CursorCol = std::min(State.CursorCol, static_cast<int>(State.Lines[static_cast<size_t>(State.CursorLine)].size()));
					EnsureCursorVisible(State);
					break;

				case SDLK_PAGEDOWN:
					BeginOrKeepSelection(State, bShift);
					State.CursorLine = std::min(static_cast<int>(State.Lines.size()) - 1, State.CursorLine + VisibleLineCount(State));
					State.CursorCol = std::min(State.CursorCol, static_cast<int>(State.Lines[static_cast<size_t>(State.CursorLine)].size()));
					EnsureCursorVisible(State);
					break;

				case SDLK_RETURN:
				case SDLK_KP_ENTER:
					InsertTextAtCursor(State, "\n");
					break;

				case SDLK_BACKSPACE:
					if (HasSelection(State))
					{
						DeleteSelection(State);
					}
					else if (State.CursorCol > 0)
					{
						CurLine.erase(static_cast<size_t>(State.CursorCol - 1), 1);
						--State.CursorCol;
						ClearSelection(State);
						State.CopiedUntilTicks = 0;
						EnsureCursorVisible(State);
					}
					else if (State.CursorLine > 0)
					{
						std::string& Prev = State.Lines[static_cast<size_t>(State.CursorLine - 1)];
						const int JoinCol = static_cast<int>(Prev.size());
						Prev.append(CurLine);
						State.Lines.erase(State.Lines.begin() + State.CursorLine);
						--State.CursorLine;
						State.CursorCol = JoinCol;
						ClearSelection(State);
						State.CopiedUntilTicks = 0;
						EnsureCursorVisible(State);
					}
					break;

				case SDLK_DELETE:
					if (HasSelection(State))
					{
						DeleteSelection(State);
					}
					else if (State.CursorCol < static_cast<int>(CurLine.size()))
					{
						CurLine.erase(static_cast<size_t>(State.CursorCol), 1);
						ClearSelection(State);
						State.CopiedUntilTicks = 0;
						EnsureCursorVisible(State);
					}
					else if (State.CursorLine + 1 < static_cast<int>(State.Lines.size()))
					{
						CurLine.append(State.Lines[static_cast<size_t>(State.CursorLine + 1)]);
						State.Lines.erase(State.Lines.begin() + State.CursorLine + 1);
						ClearSelection(State);
						State.CopiedUntilTicks = 0;
						EnsureCursorVisible(State);
					}
					break;

				default:
					break;
				}
				break;
			}

			default:
				break;
			}
		}

		// Clear background (#050505)
		SDL_SetRenderDrawColor(Renderer, 5, 5, 5, 255);
		SDL_RenderClear(Renderer);

		// Render anti-aliased IX-Ray logo clipped to the left panel below the winbar
		const SDL_Rect LogoClip = { 1, static_cast<int>(WinBarHeight), static_cast<int>(LogoClipWidth), WindowHeight - static_cast<int>(WinBarHeight) - 1 };
		SDL_SetRenderClipRect(Renderer, &LogoClip);
		if (LogoTexture != nullptr)
		{
			const SDL_FRect LogoDst = { 0.0f, 0.0f, static_cast<float>(LogoTexW), static_cast<float>(LogoTexH) };
			SDL_RenderTexture(Renderer, LogoTexture, nullptr, &LogoDst);
		}
		else
		{
			SDL_RenderGeometry(
				Renderer,
				nullptr,
				LogoVertices.data(),
				static_cast<int>(LogoVertices.size()),
				LogoIndices.data(),
				static_cast<int>(LogoIndices.size())
			);
		}
		SDL_SetRenderClipRect(Renderer, nullptr);

		// Title
		DrawTextLine(Renderer, TitleFont, 2.0f, TitleX, TitleY, 255, 255, 255, SafeTitle);

		// Callstack text edit box
		SDL_SetRenderDrawColor(Renderer, 14, 14, 14, 255);
		SDL_RenderFillRect(Renderer, &BoxRect);
		SDL_SetRenderDrawColor(Renderer, State.bFocused ? 72 : 46, State.bFocused ? 72 : 46, State.bFocused ? 72 : 46, 255);
		SDL_RenderRect(Renderer, &BoxRect);

		// Clipped text content at native 1:1 pixel scale
		const SDL_Rect TextClip = {
			static_cast<int>(BoxX + BoxPad),
			static_cast<int>(BoxY + BoxPad),
			static_cast<int>(BoxW - BoxPad * 2.0f - ScrollbarW - 6.0f),
			static_cast<int>(BoxH - BoxPad * 2.0f - ScrollbarW - 6.0f)
		};
		SDL_SetRenderClipRect(Renderer, &TextClip);

		const bool bHasSel = HasSelection(State);
		int SelStartLine = 0;
		int SelStartCol = 0;
		int SelEndLine = 0;
		int SelEndCol = 0;
		if (bHasSel)
		{
			GetSelectionRange(State, SelStartLine, SelStartCol, SelEndLine, SelEndCol);
		}

		const int VisLines = VisibleLineCount(State);
		const int EndLine = std::min(static_cast<int>(State.Lines.size()), State.ScrollY + VisLines);
		for (int i = State.ScrollY; i < EndLine; ++i)
		{
			const std::string& Line = State.Lines[static_cast<size_t>(i)];
			const float Y = BoxY + BoxPad + static_cast<float>(i - State.ScrollY) * State.LineHeight;

			if (bHasSel && i >= SelStartLine && i <= SelEndLine)
			{
				const int HlStart = (i == SelStartLine) ? SelStartCol : 0;
				const int HlEnd = (i == SelEndLine) ? SelEndCol : (static_cast<int>(Line.size()) + 1);
				if (HlEnd > HlStart)
				{
					const float X0 = BoxX + BoxPad + static_cast<float>(HlStart - State.ScrollX) * State.CharAdvance;
					const float X1 = BoxX + BoxPad + static_cast<float>(HlEnd - State.ScrollX) * State.CharAdvance;
					const SDL_FRect SelRect = { X0, Y, X1 - X0, State.LineHeight };
					if (State.bFocused)
					{
						SDL_SetRenderDrawColor(Renderer, 48, 86, 138, 255);
					}
					else
					{
						SDL_SetRenderDrawColor(Renderer, 52, 52, 56, 255);
					}
					SDL_RenderFillRect(Renderer, &SelRect);
				}
			}

			if (State.ScrollX < static_cast<int>(Line.size()))
			{
				DrawTextLine(Renderer, BodyFont, 1.0f, BoxX + BoxPad, Y, 230, 230, 230, Line.c_str() + State.ScrollX);
			}
		}

		// Blinking caret
		if (State.bFocused && (((SDL_GetTicks() - StartTicks) / 500) % 2 == 0))
		{
			if (State.CursorLine >= State.ScrollY && State.CursorLine < EndLine)
			{
				const float CaretX = BoxX + BoxPad + static_cast<float>(State.CursorCol - State.ScrollX) * State.CharAdvance;
				const float CaretY = BoxY + BoxPad + static_cast<float>(State.CursorLine - State.ScrollY) * State.LineHeight;
				const SDL_FRect CaretRect = { CaretX, CaretY, 1.5f, State.LineHeight - 2.0f };
				SDL_SetRenderDrawColor(Renderer, 255, 255, 255, 255);
				SDL_RenderFillRect(Renderer, &CaretRect);
			}
		}
		SDL_SetRenderClipRect(Renderer, nullptr);

		// Vertical scrollbar
		if (MaxScrollY(State) > 0)
		{
			SDL_SetRenderDrawColor(Renderer, 24, 24, 24, 255);
			SDL_RenderFillRect(Renderer, &VScrollTrackRect);
			const SDL_FRect Thumb = ComputeVThumbRect(State, VScrollTrackRect);
			SDL_SetRenderDrawColor(Renderer, State.bDraggingVScroll ? 120 : 72, State.bDraggingVScroll ? 120 : 72, State.bDraggingVScroll ? 120 : 72, 255);
			SDL_RenderFillRect(Renderer, &Thumb);
		}

		// Horizontal scrollbar
		if (MaxScrollX(State) > 0)
		{
			SDL_SetRenderDrawColor(Renderer, 24, 24, 24, 255);
			SDL_RenderFillRect(Renderer, &HScrollTrackRect);
			const SDL_FRect Thumb = ComputeHThumbRect(State, HScrollTrackRect);
			SDL_SetRenderDrawColor(Renderer, State.bDraggingHScroll ? 120 : 72, State.bDraggingHScroll ? 120 : 72, State.bDraggingHScroll ? 120 : 72, 255);
			SDL_RenderFillRect(Renderer, &Thumb);
		}

		// Buttons
		const bool bShowCopied = State.CopiedUntilTicks != 0 && SDL_GetTicks() < State.CopiedUntilTicks;
		DrawButton(Renderer, ButtonFont, CopyRect, bShowCopied ? "Copied!" : "Copy to Clipboard", State.bHoveredCopy, State.bPressedCopy);
		DrawButton(Renderer, ButtonFont, QuitRect, "Quit", State.bHoveredQuit, State.bPressedQuit);

		// Stylized dark winbar
		DrawWinBar(Renderer, ButtonFont, WinBarRect, MinRect, CloseRect, State.bHoveredMin, State.bPressedMin, State.bHoveredClose, State.bPressedClose);

		// Subtle 1px outer border
		const SDL_FRect OuterBorder = { 0.0f, 0.0f, static_cast<float>(WindowWidth), static_cast<float>(WindowHeight) };
		SDL_SetRenderDrawColor(Renderer, 42, 42, 42, 255);
		SDL_RenderRect(Renderer, &OuterBorder);

		SDL_RenderPresent(Renderer);
		SDL_Delay(16);
	}

	if (LogoTexture != nullptr)
	{
		SDL_DestroyTexture(LogoTexture);
	}
	TitleFont.Destroy();
	ButtonFont.Destroy();
	BodyFont.Destroy();
	SDL_StopTextInput(Window);
	SDL_DestroyRenderer(Renderer);
	SDL_DestroyWindow(Window);
}
} // namespace CrashWindow
