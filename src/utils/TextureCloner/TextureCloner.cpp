#include "stdafx.h"

//#define COPY_ALL_TEXTURE_LIST
//#define COPY_NORMAL_MAPS
void* hConsole = nullptr;

void MyLog(const char* text)
{
#ifdef IXR_WINDOWS
    int color_idx = 15;

    switch(text[0]) {
        case '*':
        color_idx = 2;
        break;
        case '!':
        color_idx = 12;
        break;
    }

    SetConsoleTextAttribute(hConsole, color_idx);
#endif

    std::cout << text << std::endl;
}

void CopyTexture(const char* texture)
{
    if(texture && texture[0]) {
        string_path file_unpacked = {};
        string_path file_from = {};
        string_path file_to = {};

        bool dds_unpacked_ex = !!FS.exist(file_unpacked, "$path_unpacked$", texture, ".dds");
        bool dds_from_ex = !!FS.exist(file_from, "$path_from$", texture, ".dds");
        bool dds_to_ex = !!FS.exist(file_to, "$path_to$", texture, ".dds");

        if(dds_to_ex || dds_unpacked_ex) {
            Msg("* Skiped [%s]", texture);
            return;
        }

        if(!dds_from_ex) {
            Msg("! Can`t find texture [%s]", texture);
            return;
        }

        FS.file_copy(file_from, file_to);
        Msg("* Copied [%s]", texture);

        bool thm_unpacked_ex = !!FS.exist(file_unpacked, "$path_unpacked$", texture, ".thm");
        bool thm_from_ex = !!FS.exist(file_from, "$path_from$", texture, ".thm");
        bool thm_to_ex = !!FS.exist(file_to, "$path_to$", texture, ".thm");

        if(!thm_from_ex) {
            Msg("! Can`t find thm [%s]", texture);
            return;
        }

        FS.file_copy(file_from, file_to);

        STextureParams tp;
        IReader* F = FS.r_open(file_from);

        if(F) {
            R_ASSERT2(F->find_chunk(THM_CHUNK_TYPE), texture);

            F->r_u32();
            tp.Clear();
            tp.Load(*F);
            FS.r_close(F);

            xr_string temp_name;

            if(tp.bump_name.size() > 0) {
                temp_name = xr_string(tp.bump_name.c_str()) + "#";
                CopyTexture(tp.bump_name.c_str());
                CopyTexture(temp_name.c_str());
            }

            if(tp.detail_name.size() > 0) {
                CopyTexture(tp.detail_name.c_str());
            }

#ifdef COPY_NORMAL_MAPS
            if(tp.ext_normal_map_name.size() > 0) {
                CopyTexture(tp.ext_normal_map_name.c_str());
            }
#endif

            if(tp.type == tp.ttTerrain) {
                temp_name = xr_string(texture) + "_mask";
                CopyTexture(temp_name.c_str());

                temp_name = xr_string(texture) + "_det";
                CopyTexture(temp_name.c_str());
            }
        }
    }
}

void CopyTextureList(const char* S)
{
    string_path textureItem = {};

#ifdef COPY_ALL_TEXTURE_LIST
    int count = _GetItemCount(S);
    for(int it = 0; it < count; ++it) {
        _GetItem(S, it, textureItem);

        if(textureItem && textureItem[0]) {
            CopyTexture(textureItem);
        }
    }
#else
    _GetItem(S, 0, textureItem);

    if(textureItem && textureItem[0]) {
        CopyTexture(textureItem);
    }
#endif
}

void LoadVisual(IReader* visual)
{
    if(visual) {
        if(visual->find_chunk(OGF_TEXTURE)) {
            string256 fnT = "", fnS = "";

            visual->r_stringZ(fnT, sizeof(fnT));
            visual->r_stringZ(fnS, sizeof(fnS));

            CopyTextureList(fnT);
        }

        if(visual->find_chunk(OGF_CHILDREN)) {
            IReader* OBJ = visual->open_chunk(OGF_CHILDREN);
            if(OBJ) {
                for(u32 count = 0; auto O = OBJ->open_chunk(count); ++count) {
                    LoadVisual(O);
                    O->close();
                }
            }
        }
    }
}

int main()
{
#ifdef IXR_WINDOWS
    system("TextureCloner");
    hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
#endif

    Debug._initialize(false);
    Core._initialize("TextureCloner", 0, FALSE);
    xrLogger::AddLogCallback(MyLog);

    FS.append_path("$working_folder$", "", 0, false);
    FS.append_path("$logs$", "", 0, false);

    FS._initialize(CLocatorAPI::flTargetFolderOnly, Core.WorkingPath);

    Log("\nStart application");

    string_path filelist = {};
    R_ASSERT2(FS.exist(filelist, "$working_folder$", "filelist.ltx"), filelist);

    auto textures = CInifile::Create(filelist);

    R_ASSERT2(textures, filelist);
    R_ASSERT2(textures->section_exist("path_list"), filelist);

    const char* from = textures->r_string_wb("path_list", "from").c_str();
    const char* to = textures->r_string_wb("path_list", "to").c_str();

    R_ASSERT2(std::filesystem::exists(from), from);

    const char* unpacked = !!textures->line_exist("path_list", "unpacked") ? textures->r_string_wb("path_list", "unpacked").c_str() : to;

    FS.append_path("$path_unpacked$", unpacked, 0, true);
    FS.append_path("$path_from$", from, 0, true);
    FS.append_path("$path_to$", to, 0, true);

    const char* line = nullptr;
    const char* value = nullptr;

    Log("Begin texture clonong");

    if(textures->section_exist("textures")) {
        for(size_t i = 0, lines = textures->line_count("textures"); i < lines; ++i) {
            if(textures->r_line("textures", i, &line, &value)) {
                CopyTexture(line);
            }
        }
    }
    else {
        if(textures->line_exist("path_list", "level")) {
            const char* level = textures->r_string_wb("path_list", "level").c_str();
            FS.append_path("$level$", level, 0, false);

            string_path level_path = {};
            R_ASSERT2(FS.exist(level_path, "$level$", "level"), level_path);

            auto fsLevel = FS.r_open(level_path);
            auto chunk = fsLevel->open_chunk(fsL_SHADERS);
            R_ASSERT2(chunk, "Level doesn't builded correctly.");

            u32 count = chunk->r_u32();

            for(u32 i = 0; i < count; ++i) {
                string_path textureItem = {};
                const char* S = (const char*)chunk->pointer();
                chunk->skip_stringZ();

                if(S && S[0]) {
                    _GetItem(S, _GetItemCount(S, '/') - 1, textureItem, '/');
                    CopyTextureList(textureItem);
                }
            }

            chunk->close();
            chunk = fsLevel->open_chunk(fsL_VISUALS);
            R_ASSERT2(chunk, "Level doesn't builded correctly.");

            for(u32 index = 0; IReader * visual = chunk->open_chunk(index); ++index) {
                LoadVisual(visual);
                visual->close();
            }

            chunk->close();
            FS.r_close(fsLevel);
        }
        else {
            FS_FileSet flist;
            FS.file_list(flist, "$path_from$", FS_ListFiles | FS_ClampExt, "*.dds");
            for(auto& file : flist) {
                CopyTexture(file.name.c_str());
            }
        }
    }

    Log("End texture clonong");
    CInifile::Destroy(textures);

    xrLogger::CloseLog();
    xrLogger::RemoveLogCallback(MyLog);

    Core._destroy();
    return 0;
}

