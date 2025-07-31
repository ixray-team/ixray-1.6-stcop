#include "stdafx.h"

int GAMESAVE_SIZE = 128;
int SM_FOR_SEND_WIDTH = 640;
int SM_FOR_SEND_HEIGHT = 480;

void CRender::Screenshot(ScreenshotMode mode, LPCSTR name) {
    ScreenshotImpl(mode, name, nullptr);

    if (mode == SM_FOR_GAMESAVE && name && name[0])
    {
        xr_string fixName = xr_string(name).Split('\\').back();

        ref_texture pTexture{};
        pTexture.create(fixName.c_str());

        pTexture->Unload();
        pTexture->Load();
    }
}

void CRender::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) {
    if (mode != SM_FOR_MPSENDING)
    {
        Log("~ Not implemented screenshot mode...");
        return;
    }
    ScreenshotImpl(mode, nullptr, &memory_writer);
}

void CRender::ScreenshotAsyncBegin() {
    VERIFY(!m_bMakeAsyncSS);
    m_bMakeAsyncSS = true;
}

void DoAsyncScreenshot() {
    RImplementation.Target->DoAsyncScreenshot();
}
