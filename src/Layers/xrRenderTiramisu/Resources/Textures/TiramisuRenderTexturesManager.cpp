#include "TiramisuRenderTexturesManager.h"
#include "TiramisuRenderTextureSequence.h"

TiramisuRenderTexturesManager::TiramisuRenderTexturesManager()
{
    CheckIsGameThread();
}

TiramisuRenderTexturesManager::~TiramisuRenderTexturesManager()
{
    CheckIsGameThread();
    FlushNextFrame();
    VERIFY(Textures.empty());
}

TiramisuRenderTexture* TiramisuRenderTexturesManager::GetTexture(const shared_str& InName, bool bSrgb)
{
    CheckIsGameThread();
    if (InName == "ui\\ui_actor_MP_screen")
    {
        __nop();
    }

    if (ErrorTextures.contains(InName))
    {
        return GRenderResourcesManager->BlackTexture;
    }
    {
        auto Iterator = FreeTexturesNextFrame.find(InName);
        if (Iterator != FreeTexturesNextFrame.end())
        {
            TiramisuRenderTexture* Result = Iterator->second;
            FreeTexturesNextFrame.erase(Iterator);
            if (Result->IsDynamic())
            {
                TexturesDynamic.insert({InName,Result});
            }
            Textures.insert({InName,Result});
            Result->Counter = 1;
            return Result; 
        }
    }
    
    auto Iterator = Textures.find(InName);
    if (Iterator == Textures.end())
    {
        TiramisuRenderTexture* NewTexture = nullptr;
        {
            auto LambdaFixTextureName = [](LPSTR fn)
            {
                auto _ext = strext(fn);
                if (_ext && (0 == _stricmp(_ext, ".tga") ||
                    0 == _stricmp(_ext, ".dds") ||
                    0 == _stricmp(_ext, ".bmp") ||
                    0 == _stricmp(_ext, ".ogm")))
                {
                    *_ext = 0;
                }
            };
            
            string_path FilePathName = {};
            string_path FileName = {};
            xr_strcpy(FileName, InName.c_str());
            LambdaFixTextureName(FileName);
           
            if (FS.exist(FilePathName, "$level$", FileName, ".dds") || FS.exist(FilePathName, "$game_saves$", FileName, ".dds") || FS.exist(FilePathName, _game_textures_, FileName, ".dds"))
            {
                TiramisuRenderTexture2D* NewTexture2D = new TiramisuRenderTexture2D(InName);
                NewTexture2D->Owner = this;
                NewTexture = NewTexture2D;
                Msg("* Loaded Texture: %s", FilePathName);
                if (!NewTexture2D->LoadFromFile(FilePathName,bSrgb))
                {
                    Msg("! Can't loaded texture: %s", FileName);
                    xr_delete(NewTexture2D);
                    ErrorTextures.insert(InName);
                    return GRenderResourcesManager->BlackTexture;
                }
            }
            // else if (FS.exist(FilePathName, _game_textures_, FileName, ".seq"))
            // {
            //     TiramisuRenderTextureSequence* NewTextureSeq = new TiramisuRenderTextureSequence(InName);
            //     NewTextureSeq->Owner = this;
            //     NewTexture = NewTextureSeq;
            //
            //     if (!NewTextureSeq->LoadFromSeqFile(FilePathName))
            //     {
            //         Msg("! Can't loaded texture: %s", FileName);
            //         xr_delete(NewTextureSeq);
            //         ErrorTextures.insert(InName);
            //         return GRenderResourcesManager->BlackTexture;
            //     }
            //     TexturesDynamic[InName] = NewTextureSeq;
            // }
            else
            {
                Msg("! Can't loaded texture: %s", FileName);
                ErrorTextures.insert(InName);
                return GRenderResourcesManager->BlackTexture;
            }
        }
        
        Iterator = Textures.insert({InName,NewTexture}).first;
    }
    else
    {
        Iterator->second->Counter++;
    }
    return Iterator->second;
}

void TiramisuRenderTexturesManager::Free(TiramisuRenderTexture* InTexture)
{
    CheckIsGameThread();
    if (!InTexture || InTexture->Owner != this)
    {
        return;
    }
    if (InTexture->Name == "ui\\ui_actor_MP_screen")
    {
        __nop();
    }
    if (--InTexture->Counter == 0)
    {
        Textures.erase(InTexture->Name);

        if (TexturesDynamic.contains(InTexture->Name))
        {
            TexturesDynamic.erase(InTexture->Name);
        }

        FreeTexturesNextFrame.insert({InTexture->Name,InTexture});
    }
}

void TiramisuRenderTexturesManager::FlushNextFrame()
{
    CheckIsGameThread();
    for (auto & [Name,Texture] : FreeTexturesNextFrame)
    {
        xr_delete(Texture);
    }
    FreeTexturesNextFrame.clear();

    for (auto& [Name, Texture] : TexturesDynamic)
    {
        Texture->Update();
    }
}

void TiramisuRenderTexturesManager::Copy(TiramisuRenderTexture* InTexture)
{
    CheckIsGameThread();
    if (InTexture->Owner != this)
    {
        return;
    }
    if (InTexture->Name == "ui\\ui_actor_MP_screen")
    {
        __nop();
    }
    InTexture->Counter++;
}
