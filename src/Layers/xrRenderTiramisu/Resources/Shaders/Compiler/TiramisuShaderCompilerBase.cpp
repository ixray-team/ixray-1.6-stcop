#include "TiramisuShaderCompilerBase.h"

#include "stdafx.h"
#include "TiramisuShaderCompilerDesktop.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesContainer.h"

#if IXR_ENABLE_SHADER_COMPILER
TiramisuShaderCompilerBase* TiramisuShaderCompilerBase::Create(nri::GraphicsAPI GraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader)
{
	return new TiramisuShaderCompilerDesktop(GraphicsAPI,NeedCreateShaderPDB, DebugShader);
}

TiramisuShaderCompilerBase::TiramisuShaderCompilerBase(bool NeedCreateShaderPDB, bool DebugShader) : 
	bNeedCreateShaderPDB(NeedCreateShaderPDB), bDebugShader(DebugShader), IncludeCrc32(0), IncludeCount(0), IncludeSize(0)
{
}

bool TiramisuShaderCompilerBase::Check(const char* SourceFileName, const TiramisuShaderDefinesContainer& Defines, const char* ResultFileName)
{
	string_path FileName;
	xr_strconcat(FileName, ResultFileName, ".checksum");

	if (!FileExists(FileName))
	{
		return false;
	}
	u32 FileSourceSize = 0;
	u32 FileSourceCrc32 = 0;
	{
		xr_vector<char> Data;
		ReadFile(SourceFileName, Data);
		FileSourceSize = Data.size();
		FileSourceCrc32 = crc32(Data.data(), Data.size());
	}

	xr_vector<char> DataFileResult;
	ReadFile(FileName, DataFileResult);
	IReader FileResult(DataFileResult.data(), DataFileResult.size());

	if (DataFileResult.size() == 32)
	{
		return false;
	}

	bool Result = (FileResult.r_u8()>0) ==		 bDebugShader;
	Result = Result && (FileResult.r_u8()>0) ==	 bNeedCreateShaderPDB;
	Result = Result && FileResult.r_u32() == FileSourceSize;
	Result = Result && FileResult.r_u32() == FileSourceCrc32;
	Result = Result && FileResult.r_u32() == Defines.GetDefines().size();
	Result = Result && FileResult.r_u32() == Defines.GetCRC32();
	Result = Result && FileResult.r_u32() == GetIncludeCrc32();
	Result = Result && FileResult.r_u32() == GetIncludeCount();
	Result = Result && FileResult.r_u32() == GetIncludeSize();
	return Result;
}

bool TiramisuShaderCompilerBase::Check(const char* RootSignature, const char* ResultFileName)
{
	if (!FileExists(ResultFileName))
	{
		return false;
	}

	u32 FileSourceSize = 0;
	u32 FileSourceCrc32 = 0;
	FileSourceSize = xr_strlen(RootSignature);
	FileSourceCrc32 = crc32(RootSignature, FileSourceSize);

	xr_vector<char> DataFileResult;
	ReadFile(ResultFileName, DataFileResult);
	IReader FileResult(DataFileResult.data(), DataFileResult.size());
	if (DataFileResult.size() < 8)
	{
		return false;
	}

	bool Result = FileResult.r_u32() == FileSourceSize;
	Result = Result && FileResult.r_u32() == FileSourceCrc32;

	return Result;
}

void TiramisuShaderCompilerBase::RescanIncludes()
{
	FS_FileSet Files;

	xr_vector<char> Text;
	IncludeCount = Files.size();
	IncludeSize = 0;
	IncludeCrc32 = 0xFFFFFFFF;

	string_path SourcePath;
	FS.update_path(SourcePath, "$game_shaders$", "r5\\common\\");
	FS.file_list(Files, SourcePath, FS_ListFiles, "*.*");

	IncludeCount += Files.size();
	for (const FS_File& File : Files)
	{
		Text.clear();
		bool bUTF8;

		string_path FilePath;
		xr_strconcat(FilePath, SourcePath, File.name.c_str());
		ReadTextFile(FilePath, Text, bUTF8);
		IncludeSize += Text.size();
		IncludeCrc32 = crc32(Text.data(), Text.size(), IncludeCrc32);
	}
}

bool TiramisuShaderCompilerBase::CheckIncludes()
{
	FS_FileSet Files;
	u32 NewIncludeCount = Files.size();
	u32 NewIncludeSize = 0;
	u32 NewIncludeCrc32 = 0xFFFFFFFF;

	string_path SourcePath;
	FS.update_path(SourcePath, "$game_shaders$", "r5\\common\\");
	FS.file_list(Files, SourcePath, FS_ListFiles, "*.*");

	xr_vector<char> Text;
	NewIncludeCount += Files.size();

	for (const FS_File& File : Files)
	{
		Text.clear();
		bool bUTF8;
		{
			string_path FilePath;
			xr_strconcat(FilePath, SourcePath, File.name.c_str());
			ReadTextFile(FilePath, Text, bUTF8);
			NewIncludeSize += Text.size();
			NewIncludeCrc32 = crc32(Text.data(), Text.size(), NewIncludeCrc32);
		}
	}

	if (IncludeCount != NewIncludeCount || IncludeCrc32 != NewIncludeCrc32 || IncludeSize != NewIncludeSize)
	{
		IncludeCount = NewIncludeCount;
		IncludeCrc32 = NewIncludeCrc32;
		IncludeSize = NewIncludeSize;
		return false;
	}

	return true;
}

void TiramisuShaderCompilerBase::ReadBinaryFile(const char* FileName, xr_vector<char>& Data)
{
	ReadFile(FileName, Data);
	Data.erase(Data.begin(), Data.begin());
}

void TiramisuShaderCompilerBase::ReadRootSignatureFile(const char* FileName, xr_vector<char>& Data)
{
	ReadFile(FileName, Data);
	Data.erase(Data.begin(), Data.begin() + 4 * 2);
}

void TiramisuShaderCompilerBase::ReadTextFile(const char* FileName, xr_vector<char>& Data, bool& bIsUTF8)
{
	ReadFile(FileName, Data);

	if (Data.size() > 2)
	{
		bIsUTF8 = Data[0] == 0xEF;
		bIsUTF8 = bIsUTF8 && Data[1] == 0xBB;
		bIsUTF8 = bIsUTF8 && Data[2] == 0xBF;
	}

	if (bIsUTF8)
	{
		Data.erase(Data.begin(), Data.begin() + 3);
	}

	for (char& ch : Data)
	{
		if (ch > 0x80)
		{
			ch = '?';
		}
	}
}

#endif