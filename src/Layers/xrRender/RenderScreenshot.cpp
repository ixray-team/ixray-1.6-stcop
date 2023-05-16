#include "stdafx.h"

int GAMESAVE_SIZE = 128;
int SM_FOR_SEND_WIDTH = 640;
int SM_FOR_SEND_HEIGHT = 480;

void CRender::Screenshot(ScreenshotMode mode, LPCSTR name) {
    ScreenshotImpl(mode, name, NULL);
}

void CRender::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) {
    if (mode != SM_FOR_MPSENDING)
    {
        Log("~ Not implemented screenshot mode...");
        return;
    }
    ScreenshotImpl(mode, NULL, &memory_writer);
}

void CRender::ScreenshotAsyncBegin() {
    VERIFY(!m_bMakeAsyncSS);
    m_bMakeAsyncSS = true;
}

void DoAsyncScreenshot() {
    RImplementation.Target->DoAsyncScreenshot();
}
