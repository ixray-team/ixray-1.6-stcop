#include "stdafx.h"
#include "TextureDependencyFormats.h"
#include "DependencyReader.h"
#include <cmath>

namespace SdkPackages::TextureDependencyFormats
{
namespace Details
{
static xr_string Key(xr_string Text)
{
	std::replace(Text.begin(), Text.end(), '\\', '/');
	return SdkPackages::Details::Lower(Text);
}
static bool Float(const xr_string& Text)
{
	float Value;
	const auto Parsed = std::from_chars(Text.data(), Text.data() + Text.size(), Value);
	return Parsed.ec == std::errc{} && Parsed.ptr == Text.data() + Text.size() && std::isfinite(Value);
}
static bool SequenceLine(std::istream& Input, xr_string& Line, xr_string& Error)
{
	Line.clear();
	char Letter;
	while (Input.get(Letter))
	{
		if (Letter == '\r' || Letter == '\n')
		{
			if (Letter == '\r' && Input.peek() == '\n')
			{
				Input.get();
			}
			return true;
		}
		// IReader::r_string requires length < 255 for the renderer's string256 buffer.
		if (Line.size() >= 254)
		{
			return SdkPackages::Details::Fail(Error, "Texture sequence lines must not exceed 254 bytes.");
		}
		Line.push_back(Letter);
	}
	if (Input.bad())
	{
		return SdkPackages::Details::Fail(Error, "Cannot read texture sequence data.");
	}
	return !Line.empty();
}
} // namespace Details

bool ReadIndex(const xr_path& Path, xr_hash_map<xr_string, Description>& Result, xr_string& Error, std::stop_token Stop)
{
	Result.clear();
	Error.clear();
	const auto SectionLambda = [&](const xr_string& Name, const DependencyFormats::Details::Fields& Fields)
	{
		if (Name != "association" && Name != "specification")
		{
			return true;
		}
		for (const auto& [Texture, Value] : Fields)
		{
			if (Stop.stop_requested())
			{
				return SdkPackages::Details::Fail(Error, "Dependency check cancelled.");
			}
			if (Result.size() >= 100000 || Texture.size() > 1024 || !Presentation::IsUtf8(Texture))
			{
				return SdkPackages::Details::Fail(Error, "Texture description index exceeds its limits or has invalid names.");
			}
			auto& Description = Result[Details::Key(Texture)];
			if (Name == "association")
			{
				const auto Comma = Value.find(','), Next = Comma == xr_string::npos ? xr_string::npos : Value.find(',', Comma + 1);
				if (Comma == xr_string::npos || !Details::Float(DependencyFormats::Details::Trim(Value.substr(Comma + 1, Next == xr_string::npos ? xr_string::npos : Next - Comma - 1))))
				{
					return SdkPackages::Details::Fail(Error, "Malformed texture detail association.");
				}
				Description.Detail = DependencyFormats::Details::Trim(Value.substr(0, Comma));
				if (Description.Detail.empty() || Description.Detail.size() > 1024 || !Presentation::IsUtf8(Description.Detail))
				{
					return SdkPackages::Details::Fail(Error, "Invalid detail texture name.");
				}
				Description.HasDetail = true;
				Description.DetailUsage = Value.find("usage[diffuse_or_bump]") != xr_string::npos ? 3 : Value.find("usage[bump]") != xr_string::npos  ? 2
																									: Value.find("usage[diffuse]") != xr_string::npos ? 1
																																					  : 0;
			}
			else
			{
				const auto End = Value.find(']'), Material = Value.find("material[");
				const auto MaterialEnd = Material == xr_string::npos ? xr_string::npos : Value.find(']', Material + 9);
				if (!Value.starts_with("bump_mode[") || End == xr_string::npos || End <= 10 || MaterialEnd == xr_string::npos || !Details::Float(DependencyFormats::Details::Trim(Value.substr(Material + 9, MaterialEnd - Material - 9))))
				{
					return SdkPackages::Details::Fail(Error, "Malformed texture bump specification.");
				}
				const auto Mode = DependencyFormats::Details::Trim(Value.substr(10, End - 10));
				Description.HasSpecification = true;
				if (Mode.starts_with("use:"))
				{
					Description.Bump = DependencyFormats::Details::Trim(Mode.substr(4));
					// Legacy textures.ltx uses use: with an empty name to leave bump mapping unset.
					if (Description.Bump.size() > 1024 || !Presentation::IsUtf8(Description.Bump))
					{
						return SdkPackages::Details::Fail(Error, "Invalid bump texture name.");
					}
				}
			}
		}
		return true;
	};
	return DependencyFormats::Details::ReadLtx(Path, SectionLambda, Error, Stop, true);
}

bool ReadThumbnail(const xr_path& Path, Description& Result, xr_string& Error, std::stop_token Stop)
{
	Result = {};
	Error.clear();
	DependencyFormats::Details::Reader Input(Path, Error, Stop);
	DependencyFormats::Details::Range Data;
	u32 Type, Flags, Value;
	if (!Input.IsValid() || !Input.Find(Input.Root, 0x0813, Data) || !Input.Integer(Data, Type) || Type != 1 || !Input.Find(Input.Root, 0x0812, Data) || Data.End - Data.Begin < 32 || !Input.Integer(Data, Value) || !Input.Integer(Data, Flags))
	{
		return SdkPackages::Details::Fail(Error, "Unreadable texture thumbnail parameters.");
	}
	Type = 0;
	if (Input.Find(Input.Root, 0x0814, Data) && !Input.Integer(Data, Type))
	{
		return false;
	}
	if (!Error.empty() || Type > 4)
	{
		return SdkPackages::Details::Fail(Error, "Unsupported texture thumbnail type.");
	}
	const bool IsApplicable = Type == 0 || Type == 3 || Type == 4;
	Result.HasSpecification = IsApplicable;
	if (Input.Find(Input.Root, 0x0815, Data))
	{
		if (!Input.String(Data, Result.Detail) || !Input.Integer(Data, Value))
		{
			return false;
		}
		Result.DetailUsage = ((Flags & (1u << 23)) ? 1 : 0) | ((Flags & (1u << 26)) ? 2 : 0);
		Result.HasDetail = IsApplicable && !Result.Detail.empty() && Result.DetailUsage;
	}
	if (!Error.empty())
	{
		return false;
	}
	if (Input.Find(Input.Root, 0x0817, Data))
	{
		u32 Mode;
		xr_string Bump;
		if (!Input.Integer(Data, Value) || !Input.Integer(Data, Mode) || !Input.String(Data, Bump))
		{
			return false;
		}
		if (Mode > 3)
		{
			return SdkPackages::Details::Fail(Error, "Unsupported bump mode in texture thumbnail.");
		}
		Result.BumpMode = Mode;
		if (IsApplicable && (Mode == 2 || Mode == 3))
		{
			Result.HasEmptyBumpReference = Bump.empty();
			Result.Bump = std::move(Bump);
		}
	}
	return Error.empty();
}

bool ReadSequence(const xr_path& Path, xr_vector<xr_string>& Frames, xr_string& Error, std::stop_token Stop)
{
	Frames.clear();
	Error.clear();
	if (Stop.stop_requested())
	{
		return SdkPackages::Details::Fail(Error, "Dependency check cancelled.");
	}
	std::error_code Code;
	if (std::filesystem::file_size(Path, Code) > 1024 * 1024 || Code)
	{
		return SdkPackages::Details::Fail(Error, "Cannot read texture sequence or it exceeds 1 MiB.");
	}
	std::ifstream Input(Path, std::ios::binary);
	u32 Fps;
	xr_string Line;
	if (!Details::SequenceLine(Input, Line, Error))
	{
		return Error.empty() ? SdkPackages::Details::Fail(Error, "Texture sequence has no frame rate.") : false;
	}
	if (SdkPackages::Details::Lower(Line) == "cycled")
	{
		if (!Details::SequenceLine(Input, Line, Error))
		{
			return Error.empty() ? SdkPackages::Details::Fail(Error, "Texture sequence has no frame rate.") : false;
		}
	}
	// Do not normalize BOM or padded cycled headers: atoi would see zero in the renderer.
	Line = DependencyFormats::Details::Trim(Line);
	if (!DependencyFormats::Details::Number(Line, Fps) || !Fps || Fps > 1000)
	{
		return SdkPackages::Details::Fail(Error, "Texture sequence frame rate must be between 1 and 1000.");
	}
	while (Details::SequenceLine(Input, Line, Error))
	{
		if (Stop.stop_requested())
		{
			return SdkPackages::Details::Fail(Error, "Dependency check cancelled.");
		}
		Line = DependencyFormats::Details::Trim(Line);
		if (Line.empty())
		{
			continue;
		}
		if (Frames.size() >= 10000 || !Presentation::IsUtf8(Line))
		{
			return SdkPackages::Details::Fail(Error, "Texture sequence exceeds its frame/name limits or has invalid names.");
		}
		Frames.push_back(Line);
	}
	if (!Error.empty())
	{
		return false;
	}
	if (!Input.eof() || Frames.empty())
	{
		return SdkPackages::Details::Fail(Error, "Texture sequence is unreadable, empty or contains an oversized line.");
	}
	return true;
}
} // namespace SdkPackages::TextureDependencyFormats
