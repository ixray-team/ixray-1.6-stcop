#include "stdafx.h"
#include "GitLFSConfig.h"

CGitLFSConfig& CGitLFSConfig::Instance()
{
	static CGitLFSConfig Instance;
	return Instance;
}

void CGitLFSConfig::Load()
{
	string_path ConfigPath;
	FS.update_path(ConfigPath, "$app_data_root$", "LevelEditor_GitLFS.ini");

	Patterns.clear();

	if (!FS.exist(ConfigPath))
	{
		LoadDefaultPatterns();
		return;
	}

	CInifile Ini(ConfigPath, true, false, true);

	if (Ini.section_exist("Patterns"))
	{
		for (const auto& [Key, Value] : Ini.r_section("Patterns").Data)
		{
			SLFSPattern Pattern;
			Pattern.Pattern = Key.c_str();

			xr_string ValueStr = Value.c_str();
			size_t CommaPos = ValueStr.find(',');
			if (CommaPos != xr_string::npos)
			{
				xr_string EnabledStr = ValueStr.substr(0, CommaPos);
				Pattern.Enabled = atoi(EnabledStr.c_str()) != 0;
				Pattern.Description = ValueStr.substr(CommaPos + 1);
			}
			else
			{
				Pattern.Enabled = atoi(ValueStr.c_str()) != 0;
			}

			Patterns.push_back(Pattern);
		}
	}

	AutoTrackEnabled = Ini.r_bool("Settings", "AutoTrackEnabled");

	if (Patterns.empty())
	{
		LoadDefaultPatterns();
	}
}

void CGitLFSConfig::Save()
{
	string_path ConfigPath;
	FS.update_path(ConfigPath, "$app_data_root$", "LevelEditor_GitLFS.ini");

	CInifile Ini(ConfigPath, false, false, false);

	Ini.w_bool("Settings", "AutoTrackEnabled", AutoTrackEnabled);

	for (const auto& Pattern : Patterns)
	{
		xr_string Value = Pattern.Enabled ? "1" : "0";
		if (!Pattern.Description.empty())
		{
			Value += "," + Pattern.Description;
		}

		Ini.w_string("Patterns", Pattern.Pattern.c_str(), Value.c_str());
	}

	Ini.save_as();
}

void CGitLFSConfig::AddPattern(const xr_string& Pattern, const xr_string& Description)
{
	for (auto& Existing : Patterns)
	{
		if (Existing.Pattern == Pattern)
		{
			Existing.Description = Description;
			Existing.Enabled = true;
			return;
		}
	}

	SLFSPattern NewPattern;
	NewPattern.Pattern = Pattern;
	NewPattern.Description = Description;
	NewPattern.Enabled = true;

	Patterns.push_back(NewPattern);
}

void CGitLFSConfig::RemovePattern(const xr_string& Pattern)
{
	Patterns.erase
	(
		std::remove_if
		(
			Patterns.begin(), 
			Patterns.end(), 
			[&Pattern](const SLFSPattern& Other)
			{
				return Other.Pattern == Pattern;
			}
		),
		Patterns.end()
	);
}

void CGitLFSConfig::SetPatternEnabled(const xr_string& Pattern, bool Enabled)
{
	for (auto& Existing : Patterns)
	{
		if (Existing.Pattern == Pattern)
		{
			Existing.Enabled = Enabled;
			return;
		}
	}
}

bool CGitLFSConfig::ShouldTrackWithLFS(const xr_string& FilePath) const
{
	if (!AutoTrackEnabled)
	{
		return false;
	}

	for (const auto& Existing : Patterns)
	{
		if (Existing.Enabled && MatchesPattern(FilePath, Existing.Pattern))
		{
			return true;
		}
	}

	return false;
}

void CGitLFSConfig::LoadDefaultPatterns()
{
	Patterns.clear();

	Patterns.push_back({"*.level", true, "Level files"});
	Patterns.push_back({"*.part", true, "Level part files"});
	Patterns.push_back({"*.ogf", true, "Game object files"});
	Patterns.push_back({"*.dds", true, "Texture files"});
	Patterns.push_back({"*.tga", true, "Texture files"});
	Patterns.push_back({"*.thm", true, "Thumbnail files"});
	Patterns.push_back({"*.xr", true, "X-Ray engine files"});
	Patterns.push_back({"*.ltx", true, "Configuration files"});
	Patterns.push_back({"*.bones", true, "Skeleton files"});
	Patterns.push_back({"*.obj", true, "3D object files"});
	Patterns.push_back({"*.skl", true, "Skeleton files"});
	Patterns.push_back({"*.details", true, "Detail files"});
	Patterns.push_back({"*.dm", true, "Detail model files"});
	Patterns.push_back({"*.pe", true, "Particle effect files"});
	Patterns.push_back({"*.wav", true, "Sound files"});
	Patterns.push_back({"*.ogg", true, "Sound files"});
	Patterns.push_back({"*.geom", true, "Geometry files"});
	Patterns.push_back({"*.geomx", true, "Geometry files"});
}

bool CGitLFSConfig::MatchesPattern(const xr_string& FilePath, const xr_string& Pattern) const
{
	if (Pattern.empty())
	{
		return false;
	}

	xr_string LowerPattern = Pattern;
	for (char& Char : LowerPattern)
	{
		Char = static_cast<char>(tolower(static_cast<unsigned char>(Char)));
	}

	xr_string LowerFilePath = FilePath;
	for (char& Char : LowerFilePath)
	{
		Char = static_cast<char>(tolower(static_cast<unsigned char>(Char)));
	}

	if (LowerPattern.length() >= 2 && LowerPattern[0] == '*' && LowerPattern[1] == '.')
	{
		xr_string Extension = LowerPattern.substr(1);
		if (LowerFilePath.length() >= Extension.length())
		{
			return LowerFilePath.substr(LowerFilePath.length() - Extension.length()) == Extension;
		}
	}
	else if (LowerPattern.find('*') != xr_string::npos)
	{
		size_t StarPos = LowerPattern.find('*');
		if (StarPos == 0)
		{
			xr_string Suffix = LowerPattern.substr(1);
			if (LowerFilePath.length() >= Suffix.length())
			{
				return LowerFilePath.substr(LowerFilePath.length() - Suffix.length()) == Suffix;
			}
		}
		else
		{
			xr_string Prefix = LowerPattern.substr(0, StarPos);
			xr_string Suffix = LowerPattern.substr(StarPos + 1);

			if (LowerFilePath.length() >= Prefix.length() + Suffix.length())
			{
				return LowerFilePath.substr(0, Prefix.length()) == Prefix && LowerFilePath.substr(LowerFilePath.length() - Suffix.length()) == Suffix;
			}
		}
	}
	else
	{
		return LowerFilePath == LowerPattern;
	}

	return false;
}
