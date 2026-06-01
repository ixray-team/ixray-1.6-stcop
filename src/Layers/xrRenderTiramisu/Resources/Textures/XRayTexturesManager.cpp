#include "XRayTexturesManager.h"
#include "XRayTextureSeq.h"

XRayTexturesManager::XRayTexturesManager()
{
}

XRayTexturesManager::~XRayTexturesManager()
{
    FlushNextFrame();
    VERIFY(Textures.empty());
}

XRayTexture* XRayTexturesManager::GetTexture(const shared_str& InName, bool bSrgb)
{
    if (ErrorTextures.contains(InName))
    {
        return GRenderResourcesManager->BlackTexture;
    }
    {
        auto Iterator = FreeTexturesNextFrame.find(InName);
        if (Iterator != FreeTexturesNextFrame.end())
        {
            XRayTexture* Result = Iterator->second;
            FreeTexturesNextFrame.erase(Iterator);
            Textures.insert({InName,Result});
            Result->Counter = 1;
            return Result; 
        }
    }
    
    auto Iterator = Textures.find(InName);
    if (Iterator == Textures.end())
    {
        XRayTexture2D* NewTexture2D = new XRayTexture2D(InName);
        NewTexture2D->Owner = this;
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
                Msg("* Loaded Texture: %s", FilePathName);
                if (!NewTexture2D->LoadFromFile(FilePathName,bSrgb))
                {
                    Msg("! Can't loaded texture: %s", FileName);
                    xr_delete(NewTexture2D);
                    ErrorTextures.insert(InName);
                    return GRenderResourcesManager->BlackTexture;
                }
            }
            else if (FS.exist(FilePathName, _game_textures_, FileName, ".seq"))
            {
                xr_delete(NewTexture2D);
                XRayTextureSeq* NewTextureSeq = new XRayTextureSeq(InName);
                NewTexture2D = NewTextureSeq;
                NewTexture2D->Owner = this;

                if (!NewTextureSeq->LoadFromSeqFile(FilePathName))
                {
                    Msg("! Can't loaded texture: %s", FileName);
                    xr_delete(NewTextureSeq);
                    NewTexture2D = nullptr;
                    ErrorTextures.insert(InName);
                    return GRenderResourcesManager->BlackTexture;
                }
                TexturesSeq[InName] = NewTextureSeq;
            }
            else
            {
                Msg("! Can't loaded texture: %s", FileName);
                ErrorTextures.insert(InName);
                return GRenderResourcesManager->BlackTexture;
            }
        }
        Iterator = Textures.insert({InName,NewTexture2D}).first;
    }
    else
    {
        Iterator->second->Counter++;
    }
    return Iterator->second;
}

void XRayTexturesManager::Free(XRayTexture* InTexture)
{
    if (InTexture->Owner != this)
    {
        return;
    }

    if (--InTexture->Counter == 0)
    {
        Textures.erase(InTexture->Name);

        if (TexturesSeq.contains(InTexture->Name))
        {
            TexturesSeq.erase(InTexture->Name);
        }

        FreeTexturesNextFrame.insert({InTexture->Name,InTexture});
    }
}

void XRayTexturesManager::FlushNextFrame()
{
    for (auto & [Name,Texture] : FreeTexturesNextFrame)
    {
        xr_delete(Texture);
    }
    FreeTexturesNextFrame.clear();

    for (auto& [_, Seq] : TexturesSeq)
    {
        Seq->Update();
    }
}

void XRayTexturesManager::Copy(XRayTexture* InTexture)
{
    if (InTexture->Owner != this)
    {
        return;
    }
    InTexture->Counter++;
}
