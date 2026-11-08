#pragma once

#include "ComparisonReader.h"
#include <bit>
#include <cmath>

namespace SdkPackages::TextureComparison
{
using ComparisonReader::Add;
using ComparisonReader::Chunks;
using ComparisonReader::Fail;
using ComparisonReader::Hex;
using ComparisonReader::Number;
using ComparisonReader::Range;
using ComparisonReader::Raw;
using ComparisonReader::Reader;
using ComparisonReader::Tail;
using ComparisonReader::Text;
struct Snapshot : ComparisonReader::Snapshot
{
	bool IsTexture = true;
};
inline bool Property(Snapshot& State, const char* Name, const xr_string& Value, xr_string& Error)
{
	return Add(State, "Texture", {}, Name, Value, Error);
}
inline xr_string Choice(u32 Value, std::initializer_list<const char*> Names)
{
	return Value < Names.size() ? xr_string(*(Names.begin() + Value)) : xr_string("Unknown value") + " (" + Number(Value) + ")";
}
inline bool Integer(Reader& Input, Range& Data, Snapshot& State, const char* Name, xr_string& Error, size_t Size = 4, bool IsHex = false)
{
	u32 Value;
	return Input.Integer(Data, Value, Size) && Property(State, Name, IsHex ? Hex(Value) : Number(Value), Error);
}
inline bool Float(Reader& Input, Range& Data, Snapshot& State, const char* Name, xr_string& Error)
{
	u32 Bits;
	if (!Input.Integer(Data, Bits))
	{
		return false;
	}
	const auto Value = std::bit_cast<float>(Bits);
	if (!std::isfinite(Value))
	{
		return Fail(Error, "Texture settings contain a non-finite number.");
	}
	string32 Text;
	xr_sprintf(Text, "%.9g", Value);
	return Property(State, Name, Text, Error);
}
inline bool String(Reader& Input, Range& Data, Snapshot& State, const char* Name, xr_string& Error)
{
	xr_string Value;
	return Text(Input, Data, Value) && Property(State, Name, Value, Error);
}
inline bool Flag(Snapshot& State, const char* Name, bool IsEnabled, xr_string& Error)
{
	return Property(State, Name, (IsEnabled ? "Enabled" : "Disabled"), Error);
}
inline bool ReadThumbnail(Reader& Input, Snapshot& State, xr_string& Error)
{
	Chunks Parts;
	if (!Input.IsValid() || !Input.Chunks(Input.Root, [&](u32 Id, Range Data)
										  {
			const auto Key = Id & 0x7fffffff;
			if ((Id & 0x80000000) && Key >= 0x0810 && Key <= 0x0819 && Key != 0x0811)
			{
				return Fail(Error, "Compressed texture settings are not supported for content comparison.");
			}
			if (Parts.contains(Key) || Parts.contains(Key | 0x80000000))
			{
				return Fail(Error, "Duplicate texture data block.");
			}
			return Parts.emplace(Id, Data).second; }))
	{
		return false;
	}
	u32 Type, Version;
	if (!Parts.contains(0x0813) || !Input.Integer(Parts[0x0813], Type))
	{
		return Fail(Error, "Texture thumbnail type is missing or unreadable.");
	}
	if (Type != 1)
	{
		State.IsTexture = false;
		return true;
	}
	if (!Parts.contains(0x0810) || !Input.Integer(Parts[0x0810], Version, 2) || Version != 0x0012 || !Parts.contains(0x0812))
	{
		return Fail(Error, "Texture thumbnail version or parameters are unsupported.");
	}
	for (auto& [Id, Data] : Parts)
	{
		u32 Value;
		switch (Id)
		{
			case 0x0810:
			case 0x0813: break;
			case 0x0812:
			{
				u32 Flags;
				if (!Input.Integer(Data, Value) || !Property(State, "Format", Choice(Value, {"DXT1", "DXT1 Alpha", "DXT3", "DXT5", "16 bit (4:4:4:4)", "16 bit (1:5:5:5)", "16 bit (5:6:5)", "24 bit RGB", "32 bit RGBA", "NVHS", "NVHU", "8 bit alpha", "8 bit luminance", "16 bit alpha/luminance", "BC7"}), Error) || !Input.Integer(Data, Flags))
				{
					return false;
				}
				struct FlagName
				{
					u32 Mask;
					const char* Name;
				};
				const FlagName Names[] = {
					{1u << 0, "Generate mipmaps"}, {1u << 1, "Binary alpha"}, {1u << 4, "Alpha border"}, {1u << 5, "Color border"}, {1u << 6, "Fade to color"}, {1u << 7, "Fade to alpha"}, {1u << 8, "Dither color"}, {1u << 9, "Dither each mipmap"}, {1u << 23, "Detail as diffuse"}, {1u << 24, "Implicit lighting"}, {1u << 25, "Source alpha channel"}, {1u << 26, "Detail as bump"}, {1u << 27, "Has surface"}
				};
				u32 Known = 0;
				for (const auto& Entry : Names)
				{
					Known |= Entry.Mask;
					if (!Flag(State, Entry.Name, (Flags & Entry.Mask) != 0, Error))
					{
						return false;
					}
				}
				if (!Property(State, "Other flags", Hex(Flags & ~Known), Error) || !Integer(Input, Data, State, "Border color (ARGB)", Error, 4, true) || !Integer(Input, Data, State, "Fade color (ARGB)", Error, 4, true) || !Integer(Input, Data, State, "Fade amount", Error) || !Input.Integer(Data, Value) || !Property(State, "Mipmap filter", Choice(Value, {"Box", "Cubic", "Point", "Triangle", "Quadratic", "Advanced", "Catrom", "Mitchell", "Gaussian", "Sinc", "Bessel", "Hanning", "Hamming", "Blackman", "Kaiser"}), Error) || !Integer(Input, Data, State, "Width", Error) || !Integer(Input, Data, State, "Height", Error))
				{
					return false;
				}
				break;
			}
			case 0x0814:
				if (!Input.Integer(Data, Value) || !Property(State, "Texture type", Choice(Value, {"2D texture", "Cube map", "Bump map", "Normal map", "Terrain"}), Error))
				{
					return false;
				}
				break;
			case 0x0815:
				if (!String(Input, Data, State, "Detail texture", Error) || !Float(Input, Data, State, "Detail scale", Error))
				{
					return false;
				}
				break;
			case 0x0816:
				if (!Input.Integer(Data, Value) || !Property(State, "Material", Choice(Value, {"OrenNayar / Blin", "Blin / Phong", "Phong / Metal", "Metal / OrenNayar", "PBR"}), Error) || !Float(Input, Data, State, "Material weight", Error))
				{
					return false;
				}
				break;
			case 0x0817:
				if (!Float(Input, Data, State, "Bump virtual height", Error) || !Input.Integer(Data, Value) || !Property(State, "Bump mode", Choice(Value, {"Reserved", "None", "Use", "Use parallax"}), Error) || !String(Input, Data, State, "Bump texture", Error))
				{
					return false;
				}
				break;
			case 0x0818:
				if (!String(Input, Data, State, "External normal map", Error))
				{
					return false;
				}
				break;
			case 0x0819:
				if (!Integer(Input, Data, State, "Fade delay (mipmaps)", Error, 1))
				{
					return false;
				}
				break;
			default:
				State.HasOpaqueData = true;
				if (!Raw(State, "Texture", Hex(Id), (Id & 0x7fffffff) == 0x0811 ? "Thumbnail data" : "Other texture data", Data, Error))
				{
					return false;
				}
				continue;
		}
		if (!Tail(State, "Texture", "Parameter suffix", Id, Data, Error))
		{
			return false;
		}
	}
	return true;
}
inline xr_string FourCC(u32 Code)
{
	xr_string Value;
	for (u32 Shift = 0; Shift < 32; Shift += 8)
	{
		const auto Letter = static_cast<u8>(Code >> Shift);
		if (Letter < 32 || Letter > 126)
		{
			return "FourCC " + Number(Code);
		}
		Value.push_back(static_cast<char>(Letter));
	}
	return Value;
}
inline bool ReadDds(Reader& Input, Snapshot& State, xr_string& Error)
{
	auto Data = Input.Root;
	u32 Magic;
	xr_array<u32, 31> Header{};
	if (!Input.IsValid() || !Input.Integer(Data, Magic) || Magic != 0x20534444)
	{
		return Fail(Error, "Invalid DDS signature.");
	}
	for (auto& Value : Header)
	{
		if (!Input.Integer(Data, Value))
		{
			return false;
		}
	}
	if (Header[0] != 124 || Header[18] != 32 || !Header[2] || !Header[3] || (Header[1] & 0x1007) != 0x1007)
	{
		return Fail(Error, "Invalid DDS header.");
	}
	const auto AddNumberLambda = [&](const char* Name, size_t Index, bool IsHex = false)
	{
		return Property(State, Name, IsHex ? Hex(Header[Index]) : Number(Header[Index]), Error);
	};
	if (!AddNumberLambda("Width", 3) || !AddNumberLambda("Height", 2) || !AddNumberLambda("Depth", 5) || !AddNumberLambda("Mipmap count", 6) || !AddNumberLambda("Header flags", 1, true) || !AddNumberLambda("Pitch or linear size", 4) || !AddNumberLambda("Pixel format flags", 19, true) || !AddNumberLambda("Bits per pixel", 21) || !AddNumberLambda("Red mask", 22, true) || !AddNumberLambda("Green mask", 23, true) || !AddNumberLambda("Blue mask", 24, true) || !AddNumberLambda("Alpha mask", 25, true) || !AddNumberLambda("Surface flags", 26, true) || !AddNumberLambda("Cube/volume flags", 27, true) || !Property(State, "Format", (Header[19] & 4) ? FourCC(Header[20]) : "Uncompressed pixels", Error) || !Property(State, "Stored FourCC", Hex(Header[20]), Error))
	{
		return false;
	}
	if ((Header[19] & 4) && Header[20] == 0x30315844)
	{
		for (const auto Name : {"DXGI format", "Resource dimension", "Resource flags", "Array size", "Alpha mode"})
		{
			if (!Integer(Input, Data, State, Name, Error))
			{
				return false;
			}
		}
	}
	if (Data.Begin == Data.End)
	{
		return Fail(Error, "Texture has no stored image data.");
	}
	State.HasOpaqueData = true;
	return Raw(State, "Texture", {}, "Stored image data", Data, Error) && Raw(State, "Texture", {}, "Reserved DDS header data", {32, 76}, Error) && Raw(State, "Texture", {}, "Reserved DDS surface data", {116, 128}, Error);
}
inline bool ReadTga(Reader& Input, Snapshot& State, xr_string& Error)
{
	auto Data = Input.Root;
	u32 IdSize, MapType, Type, MapFirst, MapSize, MapBits, X, Y, Width, Height, Bits, Descriptor;
	if (!Input.IsValid() || !Input.Integer(Data, IdSize, 1) || !Input.Integer(Data, MapType, 1) || !Input.Integer(Data, Type, 1) || !Input.Integer(Data, MapFirst, 2) || !Input.Integer(Data, MapSize, 2) || !Input.Integer(Data, MapBits, 1) || !Input.Integer(Data, X, 2) || !Input.Integer(Data, Y, 2) || !Input.Integer(Data, Width, 2) || !Input.Integer(Data, Height, 2) || !Input.Integer(Data, Bits, 1) || !Input.Integer(Data, Descriptor, 1))
	{
		return false;
	}
	if (!Width || !Height || MapType > 1 || !(Type == 1 || Type == 2 || Type == 3 || Type == 9 || Type == 10 || Type == 11) || !(Bits == 8 || Bits == 16 || Bits == 24 || Bits == 32) || Data.End - Data.Begin <= IdSize + (static_cast<u64>(MapSize) * MapBits + 7) / 8)
	{
		return Fail(Error, "Invalid or unsupported TGA header.");
	}
	State.HasOpaqueData = true;
	return Property(State, "Width", Number(Width), Error) && Property(State, "Height", Number(Height), Error) && Property(State, "Bits per pixel", Number(Bits), Error) && Property(State, "Alpha bits", Number(Descriptor & 15), Error) && Property(State, "Image type", Number(Type), Error) && Flag(State, "RLE compression", Type >= 9, Error) && Flag(State, "Top origin", (Descriptor & 32) != 0, Error) && Flag(State, "Right origin", (Descriptor & 16) != 0, Error) && Property(State, "Image descriptor", Hex(Descriptor), Error) && Property(State, "X origin", Number(X), Error) && Property(State, "Y origin", Number(Y), Error) && Property(State, "Image ID length", Number(IdSize), Error) && Property(State, "Color map type", Number(MapType), Error) && Property(State, "Color map first entry", Number(MapFirst), Error) && Property(State, "Color map entries", Number(MapSize), Error) && Property(State, "Color map bits", Number(MapBits), Error) && Raw(State, "Texture", {}, "Stored image data", Data, Error);
}
inline bool Read(const xr_path& CurrentPath, const xr_path& ImportedPath, AssetComparison& Result, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {})
{
	const auto Extension = SdkPackages::Details::Lower(Utf8(ImportedPath.extension()));
	bool IsTexture = true, HasOpaqueData = false;
	const auto DecodeLambda = [&](Reader& Input, Snapshot& State, xr_string& DecodeError)
	{
		const bool IsSuccess = Extension == ".thm" ? ReadThumbnail(Input, State, DecodeError) : Extension == ".dds" ? ReadDds(Input, State, DecodeError)
																							: Extension == ".tga"	? ReadTga(Input, State, DecodeError)
																													: Fail(DecodeError, "Unsupported texture comparison format.");
		IsTexture &= State.IsTexture;
		HasOpaqueData |= State.HasOpaqueData;
		return IsSuccess;
	};
	if (!ComparisonReader::Read<Snapshot>(CurrentPath, ImportedPath, Result, Error, Stop, Progress, DecodeLambda))
	{
		return false;
	}
	if (!IsTexture)
	{
		Result = {};
		Result.Notices.push_back("This .thm is not a texture thumbnail. Only file information is available.");
		return true;
	}
	if (HasOpaqueData)
	{
		Result.Notices.push_back("Stored image, preview and auxiliary data are compared byte by byte; individual pixels are not displayed.");
	}
	if (Extension != ".thm")
	{
		Result.Notices.push_back("Texture processing settings such as bump and detail are compared in their .thm files. This view shows the selected image file.");
	}
	return true;
}
} // namespace SdkPackages::TextureComparison
