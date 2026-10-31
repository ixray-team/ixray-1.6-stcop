#pragma once

namespace ShaderCache
{
	inline bool AccumulateShaderIncludesCRC(IReader& Reader, u32& ShaderCRC, xr_hash_set<xr_string>& VisitedIncludes)
	{
		xr_string Line;
		while (!Reader.eof())
		{
			Reader.r_string(Line);
			_Trim(Line);
			const char* Text = Line.c_str();
			if (*Text != '#')
			{
				continue;
			}

			++Text;
			while (*Text == ' ' || *Text == '\t')
			{
				++Text;
			}
			if (strncmp(Text, "include", 7) != 0 || (Text[7] != ' ' && Text[7] != '\t' && Text[7] != '"' && Text[7] != '<'))
			{
				continue;
			}

			Text += 7;
			while (*Text == ' ' || *Text == '\t')
			{
				++Text;
			}
			if (*Text != '"' && *Text != '<')
			{
				return false;
			}

			const char Terminator = *Text++ == '"' ? '"' : '>';
			const char* End = strchr(Text, Terminator);
			if (!End || End == Text)
			{
				return false;
			}
			const xr_string IncludeName(Text, (u32)(End - Text));
			if (!VisitedIncludes.insert(IncludeName).second)
			{
				continue;
			}

			string_path IncludePath;
			xr_strconcat(IncludePath, ::Render->getShaderPath(), IncludeName.c_str());
			IReader* Include = FS.r_open(_game_shaders_, IncludePath);
			if (!Include)
			{
				Include = FS.r_open(_game_shaders_, IncludeName.c_str());
			}
			if (!Include)
			{
				return false;
			}

			ShaderCRC = crc32(Include->pointer(), Include->length(), ShaderCRC);
			const bool HasAllIncludes = AccumulateShaderIncludesCRC(*Include, ShaderCRC, VisitedIncludes);
			FS.r_close(Include);
			if (!HasAllIncludes)
			{
				return false;
			}
		}
		return true;
	}

	inline bool CalculateShaderCRC(const void* SourceData, u32 SourceSize, u32& SourceCRC)
	{
		SourceCRC = crc32(SourceData, SourceSize);
		IReader Source((char*)SourceData, SourceSize);
		xr_hash_set<xr_string> VisitedIncludes;
		return AccumulateShaderIncludesCRC(Source, SourceCRC, VisitedIncludes);
	}
}
