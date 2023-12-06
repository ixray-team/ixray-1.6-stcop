#pragma once
#include "MbHelpers.h"
#include "../Include/xrRender/FontRender.h"

class ENGINE_API CGameFont
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

private:
	struct String
	{
		string1024 string;
		float x, y;
		float height;
		u32 c;
		EAligment align;
	};

	struct BaseData
	{
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

	u32 uFlags;
	u32 dwCurrentColor;

	EAligment eCurrentAlignment;
	xr_vector<String> strings;
	IFontRender* pFontRender;

public:
	enum
	{
		fsGradient = (1 << 0),
		fsDeviceIndependent = (1 << 1), //#DELETE_ME deprecated
		fsValid = (1 << 2),

		fsMultibyte = (1 << 3),

		fsForceDWORD = u32(-1)
	};


public:
	CGameFont(const char* section, u32 flags = 0);
	//CGameFont(const char* shader, const char* texture, u32 flags = 0);
	~CGameFont();

	void ReInit();
	inline void SetColor(u32 C) { dwCurrentColor = C; };

	//inline void SetHeightI(float S);
	inline void SetHeight(float S);

	inline float GetHeight() { return fCurrentHeight; };
	inline void SetAligment(EAligment aligment) { eCurrentAlignment = aligment; }

	float SizeOf_(const char* s);
	float SizeOf_(const wide_char* wsStr);
	float SizeOf_(const char cChar);  // only ANSII

	float CurrentHeight_();

	void OutSetI(float x, float y);
	void OutSet(float x, float y);

	void MasterOut(BOOL bCheckDevice, BOOL bUseCoords, BOOL bScaleCoords, BOOL bUseSkip, float _x, float _y, float _skip, const char* fmt, va_list p);

	BOOL IsMultibyte() {
		return uFlags & fsMultibyte;
	};
	u16 SplitByWidth(u16* puBuffer, u16 uBufferSize, float fTargetWidth, const char* pszText);
	u16 GetCutLengthPos(float fTargetWidth, const char* pszText);

	void OutI(float _x, float _y, const char* fmt, ...);
	void Out(float _x, float _y, const char* fmt, ...);
	void OutNext(const char* fmt, ...);

	void OutSkip(float val = 1.f);

	void OnRender();

	inline void Clear() { strings.clear(); };

	//shared_str m_font_name;

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

	inline u32 GetSize()
	{
		return Size;
	}

	inline float GetLetterSpacing()
	{
		return LetterSpacing;
	}

	inline void SetLetterSpacing(float spacing)
	{
		LetterSpacing = spacing;
	}

	inline float GetLineSpacing()
	{
		return LineSpacing;
	}

	inline void SetLineSpacing(float spacing)
	{
		LineSpacing = spacing;
	}

	inline Style GetStyle()
	{
		return Style;
	}

	inline const char* GetName()
	{
		return Name;
	}

	const Glyph* GetGlyphInfo(char ch);

	// returns symbol width in pixels
	int WidthOf(const char ch);
	int WidthOf(const char* str);

private:
	static const u32 FirstChar = 0x04;
	static const u32 LastChar = 0xFF;

	float LetterSpacing; //that must be in CUIText from new font system
	float LineSpacing; //that must be in CUIText from new font system

	const char* Name; //#TODO change type

	u32 Size;
	Style Style;

	xr_map<char, Glyph> GlyphData;

	void Prepare(const char* name, const char* shader, const char* style, u32 size);
	void Initialize(const char* name, const char* shader, const char* style, u32 size);
	void Initialize2(const char* name, const char* shader, const char* style, u32 size);

	static bool bFreetypeInitialized;

	static void InitializeFreetype();
};
