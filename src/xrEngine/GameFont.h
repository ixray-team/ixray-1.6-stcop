#pragma once
#include "../Include/xrRender/FontRender.h"

struct FT_FaceRec_;
using FT_Face = FT_FaceRec_*;

#ifndef IXR_WINDOWS
struct ABC
{
    int abcA;
    u32 abcB;
    int abcC;
};
#endif

class ENGINE_API CGameFont final
{
	friend class dxFontRender;
	friend class FontRender;

	enum EStyle : u64
	{
		eBold = 4196692,
		eStrike = 4196725,
		eUnderline = 4196715,
		eItalic = 4196708
	};
public:
	enum EAligment
	{
		alLeft = 0,
		alRight,
		alCenter
	};
	enum EGradientMode
	{
		gm_vert = 0,
		gm_horz = 1,
		gm_back = 2,
		gm_down = 3,
		gm_count
	};

private:
	struct String
	{
		string2048 string;
		xr_string  string_utf8;
		float x, y;
		float height;
		u32 c;
		EAligment align;
		bool gradient;
		EGradientMode gradientMode;
		u32 gradientColor;
	};

	struct BaseData
	{
		bool OpenType = false;
		u16 Size;
		const char* Name;
		const char* Shader;
		const char* Style;
	};

	BaseData Data;
protected:
	float fCurrentHeight = 0.0f;
	float fCurrentX = 0.0f;
	float fCurrentY = 0.0f;
	bool fGradientEnabled = false;
	EGradientMode fGradientMode = gm_vert;

	u32 uFlags;
	u32 dwCurrentColor;
	u32 dwGradientColor;

	EAligment eCurrentAlignment;
	xrCriticalSection s_cs;
	xr_vector<String> strings;
	IFontRender* pFontRender;

public:
	enum
	{
		fsDeviceIndependent = 1 << 0, //#DELETE_ME deprecated
		fsValid = 1 << 1,
		fsMultibyte = 1 << 2,
		fsForceDWORD = u32(-1)
	};
	
	CGameFont(const char* section, u32 flags = 0);
	~CGameFont();

	void  ReInit();
	void  SetColor(u32 C) { dwCurrentColor = C; }
	void  SetGradientColor(u32 C) { dwGradientColor = C; }
	void  SetHeight(float S);
	float GetHeight() { return fCurrentHeight; }
	void  SetAligment(EAligment aligment) { eCurrentAlignment = aligment; }
	
	/**
	 * Извлекает ширину строки.
	 * @param s строка с текстом.
	 * @return количество пикселей.
	 */
	float SizeOf_(const char* s);

	/**
	 * Извлекает ширину символа.
	 * @param s строка, из которой будет извлечена ширина текста в пикселях.
	 * @return количество пикселей.
	 */
	float SizeOf_(int cChar);


	/**
	 * Извлекает высоту текста в пикселях.
	 * @param s строка, из которой будет извлечена высота текста в пикселях.
	 * @return количество пикселей.
	 */
	float CurrentHeight_();

	void OutSetI(float x, float y);
	void OutSet(float x, float y);

	void MasterOut(
			BOOL bCheckDevice,
			BOOL bUseCoords,
			BOOL bScaleCoords,
			BOOL bUseSkip,
			float _x,
			float _y,
			float _skip,
			const char* fmt,
			va_list p
		);

	BOOL IsMultibyte() { return uFlags & fsMultibyte;}
	u16 SplitByWidth(u16* puBuffer, u16 uBufferSize, float fTargetWidth, const char* pszText);
	u16 GetCutLengthPos(float fTargetWidth, const char* pszText);

	void SetGradient(bool val) { fGradientEnabled = val; }
	void SetGradientMode(EGradientMode mode) { fGradientMode = mode; }

	/**
	 * 
	 * Выводит на экран текст относительно указанных координат.
	 * @param _x - откуда рисуем по x
	 * @param _y - откуда рисуем по y
	 * @param fmt - форматированный текст для вывода.
	 */
	void OutI(float _x, float _y, const char* fmt, ...);
	
	/**
	 * Выводит на экран текст относительно указанных координат.
	 * @param _x откуда рисуем по x
	 * @param _y откуда рисуем по y
	 * @param fmt форматированный текст для вывода.
	 */
	void Out(float _x, float _y, const char* fmt, ...);
	
	/**
	 * Добавляет строку с текстом в поток вывода, относительно установленного SetOut(x, y).
	 * В последующем, относительно предыдущих строк.
	 * @param fmt форматированная строка.
	 */
	void OutNext(const char* fmt, ...);

	/**
	 * Устанавливает смещение вывода относительно левого края экрана.
	 * @param x сколько пикселей нужно отступить.
	 */
	void OutLeft(float x);

	/**
	 * Устанавливает смещение вывода относительно правого края экрана.
	 * @param x сколько пикселей нужно отступить.
	 */
	void OutRight(float y);

	/**
	 * Устанавливает смещение вывода относительно верхнего края экрана.
	 * @param y сколько пикселей нужно отступить.
	 */
	void OutTop(float y);

	/**
	 * Устанавливает смещение вывода относительно нижнего края экрана.
	 * @param y сколько пикселей нужно отступить.
	 */
	void OutBottom(float y);
	
	/**
	 * Пихает пустую строчку в поток вывода.
	 * @param val коэффициент, на который будет умножен размер шрифта в пикселях, определяя размер пустой строки.
	 */
	void OutSkip(float val = 1.f);
	
	/**
	 * Выводит шрифт на экран.
	 */
	void OnRender();

	void Clear() { xrCriticalSectionGuard g(&s_cs); strings.clear(); }

	struct Style
	{
		u32 bold : 1;
		u32 italic : 1;
		u32 underline : 1;
		u32 strike : 1;
	};

	struct Glyph
	{
		RECT TextureCoord;
		ABC Abc;
		int yOffset;
	};

	IC u32 GetSize()
	{
		return Size;
	}

	IC float GetLetterSpacing()
	{
		return LetterSpacing;
	}

	IC void SetLetterSpacing(float spacing)
	{
		LetterSpacing = spacing;
	}

	IC float GetLineSpacing()
	{
		return LineSpacing;
	}

	IC void SetLineSpacing(float spacing)
	{
		LineSpacing = spacing;
	}

	IC Style GetStyle()
	{
		return Style;
	}

	IC const char* GetName()
	{
		return Name;
	}

	const Glyph* GetGlyphInfo(int ch);

	// returns symbol width in pixels
	float WidthOf(int ch);
	float WidthOf(const char* str);

private:
	float LetterSpacing = 0; //that must be in CUIText from new font system
	float LineSpacing = 0; //that must be in CUIText from new font system

	const char* Name; //#TODO change type

	u32 Size;
	Style Style;
	FT_Face OurFont;

	xr_map<int, Glyph> GlyphData;

	void Prepare(const char* name, const char* shader, const char* style, u32 size);
	void Initialize(const char* name, const char* shader, const char* style, u32 size);
	void Initialize2(const char* name, const char* shader, const char* style, u32 size);

	static bool bFreetypeInitialized;

	static void InitializeFreetype();
};
