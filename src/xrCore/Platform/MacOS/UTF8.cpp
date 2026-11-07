#include "stdafx.h"
#include "TextEncoding.h"
#include <iconv.h>

namespace
{
using ConvertedText = std::unique_ptr<char, decltype(&SDL_free)>;

class TextConverter final
{
public:
	TextConverter(const char* Destination, const char* Source) : Handle(iconv_open(Destination, Source)) {}
	~TextConverter()
	{
		if (Handle != reinterpret_cast<iconv_t>(-1))
		{
			iconv_close(Handle);
		}
	}
	TextConverter(const TextConverter&) = delete;
	TextConverter& operator=(const TextConverter&) = delete;
	iconv_t Handle;
};

xr_string ConvertEncoding(const xr_string& Input, const char* Destination, const char* Source)
{
	TextConverter Converter(Destination, Source);
	xr_string Result;
	Result.resize(Input.size() * 4 + 4);
	char* Read = const_cast<char*>(Input.c_str());
	char* Write = Result.data();
	size_t ReadSize = Input.size();
	size_t WriteSize = Result.size();
	if (Converter.Handle != reinterpret_cast<iconv_t>(-1) && iconv(Converter.Handle, &Read, &ReadSize, &Write, &WriteSize) != size_t(-1))
	{
		Result.resize(Result.size() - WriteSize);
		return Result;
	}
	// Invalid legacy bytes must not be returned as purported UTF-8.
	Result = Input;
	for (char& Byte : Result)
	{
		if (static_cast<unsigned char>(Byte) >= 0x80)
		{
			Byte = '?';
		}
	}
	return Result;
}
} // namespace


XRCORE_API const char* Platform::ANSI_TO_TCHAR(const char* C)
{
	return C;
}

XRCORE_API const char* Platform::ANSI_TO_TCHAR_U8(const char* C)
{
	return C;
}

XRCORE_API xr_string Platform::ANSI_TO_UTF8(const xr_string& ansi)
{
	return ConvertEncoding(ansi, "UTF-8", "WINDOWS-1251");
}

namespace Platform
{
XRCORE_API xr_string TCHAR_TO_ANSI_U8(const char* C)
{
	return C;
}

XRCORE_API xr_string UTF8_to_CP1251(xr_string const& utf8)
{
	return IsUTF8(utf8.c_str()) ? ConvertEncoding(utf8, "WINDOWS-1251", "UTF-8") : utf8;
}
} // namespace Platform

xr_vector<u32> Platform::DecodeText(const char* Text)
{
	xr_vector<u32> Result;
	if (Text == nullptr)
	{
		return Result;
	}
	const xr_string UTF8 = IsUTF8(Text) ? xr_string(Text) : ANSI_TO_UTF8(Text);
	ConvertedText Buffer(SDL_iconv_string("UTF-32LE", "UTF-8", UTF8.c_str(), UTF8.size() + 1), SDL_free);
	if (Buffer)
	{
		const u32* Codepoint = reinterpret_cast<const u32*>(Buffer.get());
		while (*Codepoint)
		{
			Result.push_back(*Codepoint++);
		}
	}
	return Result;
}

