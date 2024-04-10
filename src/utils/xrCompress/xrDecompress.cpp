#include "StdAfx.h"
#include "xrDecompress.h"

const char xrDecompressor::s_Separators[3] = "\\/";

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
    const char* outPath;
    string_path dir = {};
    xr_strcpy(dir, filename);

    char* rightmostSlash = strrchr(dir, '\\');
    if (rightmostSlash)
    {
        *rightmostSlash = 0;
        if (!(outPath = CreatePath(dir)))
        {
            printf("[ERROR] Failed to find or creare path %s\n", dir);
            return -1;
        }
    }

    IReader* pReader = FS.r_open("$game_data$", filename);
    if (pReader)
    {
        size_t size = pReader->length();
        printf("[extract] %s (%d byte(s))\n", filename, (int)size);
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
        printf("[ERROR] Failed to extract %s", filename);
        return -2;
    }
    return 0;
}

const char* xrDecompressor::CreatePath(const char* path)
{
    if (!m_PathCache.contains(path))
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

    printf("[mkdir] %s\n", newBase);
    if (_mkdir(newBase) < 0 && errno == ENOENT)
        return -1;

    if (xr_strlen(newPath))
        return CreateDir(newBase, newPath);

    return 0;
}

void xrDecompressor::Decompress()
{
    FS.load_all_unloaded_archives();

    auto files = FS.file_list_open("$game_data$");
    VERIFY(files);

    printf("%d file(s) to decompress in %d archive(s)\n\n", (int)files->size(), (int)FS.m_archives.size());

    for (auto File : *files)
    {
        if (int err = ExtractFile(File))
        {
            printf("[ERROR] Failed to extract %s: %d", File, err);
            FS.file_list_close(files);
            return;
        }
    }

    FS.file_list_close(files);

    for (auto& Arch : FS.m_archives)
        FS.unload_archive(Arch);
}