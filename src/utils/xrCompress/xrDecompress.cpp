#include "StdAfx.h"
#include "xrDecompress.h"

#include <iostream>

const char xrDecompressor::s_Separators[4] = "\\/";

xrDecompressor::xrDecompressor(const char* outDir)
	: m_OutDir(outDir)
{
	if (!std::filesystem::exists(Platform::ValidPath(outDir)))
	{
		std::filesystem::create_directory(Platform::ValidPath(outDir));
	}
}

int xrDecompressor::ExtractFile(const char* filename)
{
	// FX: Skip shader objects cache
	if (strstr(filename, ".ps\\"))
		return 0;
	
	if (strstr(filename, ".vs\\"))
		return 0;
	
	if (strstr(filename, ".cs\\"))
		return 0;
	
	if (strstr(filename, ".gs\\"))
		return 0;
	
	if (strstr(filename, ".ds\\"))
		return 0;
	
	if (strstr(filename, ".hs\\"))
		return 0;

	const char* outPath;
	string_path dir = {};
	xr_strcpy(dir, filename);

	char* rightmostSlash = strrchr(dir, '\\');
	if (rightmostSlash)
	{
		*rightmostSlash = 0;
		if (!(outPath = CreatePath(dir)))
		{
			Msg("[ERROR] Failed to find or creare path %s", dir);
			return -1;
		}
	}

	IReader* pReader = FS.r_open(_game_data_, filename);
	if (pReader)
	{
		size_t size = pReader->length();
		Msg("[extract] %s (%d byte(s))", filename, (int)size);
		void* buffer = malloc(size);
		pReader->r(buffer, size);
		FS.r_close(pReader);

		char fullPath[MAX_PATH + 1];
		sprintf(fullPath, "%s%c%s", m_OutDir, s_Separators[0], filename);
		FILE* FF = fopen(fullPath, "wb");

		if (!FF)
		{
			free(buffer);
			return -3;
		}

		if (size)
		{
			if (fwrite(buffer, size, 1, FF) != 1)
			{
				free(buffer);
				fclose(FF);
				return -4;
			}
		}

		free(buffer);
		fclose(FF);
	}
	else
	{
		Msg("[ERROR] Failed to extract %s", filename);
		return -2;
	}
	return 0;
}

const char* xrDecompressor::CreatePath(const char* path)
{
	if (m_PathCache.find(path)==m_PathCache.end())
	{
		char relPath[MAX_PATH + 1];
		xr_strcpy(relPath, path);

		char fullPath[MAX_PATH + 1];
		sprintf(fullPath, "%s%c%s", m_OutDir, s_Separators[0], path);

		if (CreateDir(m_OutDir, path) < 0)
			return nullptr;
		else
			m_PathCache[path] = fullPath;
	}

	return m_PathCache[path].c_str();
}

int xrDecompressor::CreateDir(const char* base, const char* path)
{
	string_path newBase = {};
	string_path newPath = {};
	string_path relPath = {};

	xr_strcpy(newBase, base);
	xr_strcpy(relPath, path);

	char* token = strtok(relPath, s_Separators);
	sprintf(newBase, "%s%c%s", newBase, s_Separators[0], token);
	while (token)
	{
		if (token != relPath && strlen(newPath))
			sprintf(newPath, "%s%c%s", newPath, s_Separators[0], token);
		else if (token != relPath)
			xr_strcpy(newPath, token);

		token = strtok(nullptr, s_Separators);
	}

	Msg("[mkdir] %s", newBase);
	if (_mkdir(newBase) < 0 && errno == ENOENT)
		return -1;

	if (xr_strlen(newPath))
		return CreateDir(newBase, newPath);

	return 0;
}

void xrDecompressor::Decompress()
{
	FS.load_all_unloaded_archives();
	
	auto LogCallback = [](const char* Msg)
	{
		std::cout << Msg << std::endl;
	};

	xrLogger::AddLogCallback(LogCallback);

	CTimer timer = {};
	Msg("Start Decompress\n");
	timer.Start();

	auto files = FS.file_list_open(_game_data_);
	VERIFY(files);

	Msg("%d file(s) to decompress in %d archive(s)\n", (int)files->size(), (int)FS.m_archives.size());

	xr_parallel_foreach(files->begin(), files->end(), 
	[this](const char* File)
	{
		if (int err = ExtractFile(File))
		{
			Msg("[ERROR] Failed to extract %s: %d", File, err);
		}
	});

	Msg("End Decompress: %d", timer.GetElapsed_ms());

	FS.file_list_close(files);

	for (auto& Arch : FS.m_archives)
		FS.unload_archive(Arch);

	xrLogger::RemoveLogCallback(LogCallback);
}