#include "../xrEngine/stdafx.h"

#include <SDL3/SDL.h>
#include <SDL3/SDL_video.h>
#include <SDL_Ext/SDL_image.h>

#include <cstdlib>
#include <ctime>

#include "splash.h"
#include "splash_eff_ng.h"  //NOVA GODA PRIKOL
#include "splash_eff_crt.h"
#include <Windows.h>
#include<string>
#include "resource.h"
#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

#define HAVE_SLD3TTF 0


//CHANGE THIS TO 1 TO DISABLE SPLASH EVENTS (NEW YEAR AND ETC)
#define DISABLE_SPLASH_EVENTS 0


//#if HAVE_SLD3TTF
//#include <SDL3_ttf/SDL_ttf.h>
//#endif

EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#pragma warning(disable: 4047)
HINSTANCE hInstanceG = (HINSTANCE)&__ImageBase;
#pragma warning(default: 4047)

namespace splash
{
    // == WINDOW
    int WINDOW_WIDTH = 98;
    int WINDOW_HEIGHT = 78;

    static SDL_Window* window = NULL;
    static SDL_Renderer* renderer = NULL;

    const char* SPLASH_STATUS = "";
    int progress_percent = 0;
    // WINDOW ==

    // == LOADING AMIN SHNYAGA
    constexpr int LD_COLS = 8;
    constexpr int LD_ROWS = 2;
    constexpr int LD_FRAME_COUNT = LD_COLS * LD_ROWS;
    constexpr float LD_FRAME_TIME = 0.05f;
    SDL_Texture* LD_atlas = nullptr;
    // LOADING AMIN SHNYAGA ==

    // == FONT
#if HAVESDL3TTF
#else
    SDL_Texture* fontTexture = nullptr;

    const int CHARS_PER_ROW = 14;
    const int CHARS_PER_COL = 7;
    //bool outText = false;

    int CHAR_WIDTH = 0;
    int CHAR_HEIGHT = 0;
#endif

    enum E_SPLASH_RENDER_PRIKOL
    {
        NORMIS = 0,
        CRT,
        NOVA_GODA,
        SPOOKY,
    } splash_render_prikol;

    void splash::SetProgressStatus(int prog, const char* status)
    {
        progress_percent = prog;
        SPLASH_STATUS = status;
    }

    SDL_Surface* LoadPNGSurfaceFromResource(unsigned char* imageData, LPCTSTR lpName, LPCTSTR lpType) {
        HMODULE hMODULE = hInstanceG;

        HRSRC hRes = FindResource(hMODULE, lpName, lpType);
        if (!hRes) {
            //ErrorMsg("Failed to find resource (ID %d)", lpName);
            return nullptr;
        }

        HGLOBAL hMem = LoadResource(hInstanceG, hRes);
        if (!hMem) {
            //ErrorMsg("Failed to load resource (ID %d)", lpName);
            return nullptr;
        }

        void* pResData = LockResource(hMem);
        if (!pResData) {
            //ErrorMsg("Failed to lock resource (ID %d)", lpName);
            return nullptr;
        }

        DWORD resSize = SizeofResource(hInstanceG, hRes);

        int width, height, channels;
        imageData = stbi_load_from_memory((unsigned char*)pResData, resSize, &width, &height, &channels, STBI_rgb_alpha);
        if (!imageData) {
            //ErrorMsg("Failed to decode PNG (ID %d)", lpName);
            return nullptr;
        }

        SDL_Surface* surface = SDL_CreateSurfaceFrom(width, height, SDL_PIXELFORMAT_RGBA32, imageData, width * 4);
        if (!surface) {
            stbi_image_free(imageData);
            //ErrorMsg("Failed to create pixel format (ID %d). %s", lpName, SDL_GetError());
            return nullptr;
        }

        return surface;
    }

    void renderPrikolHub(SDL_Surface* overlaySurf, SDL_Texture* overlayTex)
    {

        switch (splash_render_prikol)
        {
        case NORMIS: { break; }
        case SPOOKY: { break; }
        case NOVA_GODA: { splash::nova_goda::update_and_draw_snow(renderer, WINDOW_WIDTH, WINDOW_HEIGHT); break; }
        case CRT: { splash::crt::update_and_draw(renderer, overlaySurf, overlayTex, WINDOW_WIDTH, WINDOW_HEIGHT); break; }
        default:
            break;
        }
    }

    void RenderText(const char* text, int x, int y) {
        if (!fontTexture) return;

        size_t len = strlen(text);
        for (size_t i = 0; i < len; ++i) {
            unsigned char c = static_cast<unsigned char>(text[i]);
            int charIndex = c - 32;
            int srcX = (charIndex % CHARS_PER_ROW) * CHAR_WIDTH;
            int srcY = (charIndex / CHARS_PER_ROW) * CHAR_HEIGHT;

            SDL_FRect srcRect = { (float)srcX, (float)srcY, CHAR_WIDTH, CHAR_HEIGHT };
            SDL_FRect dstRect = { (float)x + i * CHAR_WIDTH, (float)y, (float)CHAR_WIDTH, (float)CHAR_HEIGHT };

            SDL_RenderTexture(renderer, fontTexture, &srcRect, &dstRect);
        }
    }
    void UpdatepProgressBar(int progress = 0, const char* status = "")
    {
        int pgHeight = 10;

        SDL_FRect progressBarBackground = { 0, (float)WINDOW_HEIGHT - (float)pgHeight, (float)WINDOW_WIDTH, (float)pgHeight };
        SDL_FRect progressBarFill =
        {
            progressBarBackground.x,
            progressBarBackground.y,
            (progress * progressBarBackground.w) / 100,
            progressBarBackground.h
        };

        SDL_SetRenderDrawColor(renderer, 30, 30, 30, 255);
        SDL_RenderFillRect(renderer, &progressBarBackground);

        SDL_SetRenderDrawColor(renderer, 58, 134, 255, 255);
        SDL_RenderFillRect(renderer, &progressBarFill);

        //RenderText(status, (WINDOW_WIDTH - (strlen(status) * CHAR_WIDTH)) / 2, progressBarBackground.y - CHAR_HEIGHT);
        RenderText(status, 32.f, progressBarBackground.y - CHAR_HEIGHT - (CHAR_HEIGHT / 3));
    }

#if! DISABLE_SPLASH_EVENTS
    bool IsBetweenDec25AndJan5()
    {
        std::time_t t = std::time(nullptr);
        std::tm tm;
        localtime_s(&tm, &t);

        int month = tm.tm_mon + 1;
        int day = tm.tm_mday;

        if (month == 12 && day >= 25 && day <= 31)  return true;

        if (month == 1 && day >= 1 && day <= 5)  return true;

        return false;
    }

    bool IsBetweenOct30AndNov5()
    {
        std::time_t t = std::time(nullptr);
        std::tm tm;
        localtime_s(&tm, &t);

        int month = tm.tm_mon + 1;
        int day = tm.tm_mday;

        if (month == 10 && day >= 30 && day <= 31)  return true;

        if (month == 11 && day >= 1 && day <= 5)  return true;

        return false;
    }
#endif
    bool IsWindowFocused(SDL_Window* window)
    {
        if (!window) return false;

        Uint32 flags = SDL_GetWindowFlags(window);
        return (flags & SDL_WINDOW_INPUT_FOCUS) != 0;
    }
    void RaiseWindowNoFocus(SDL_Window* window, HWND hwnd)
    {
        if (!window) return;


        if (!hwnd) return;

        SetWindowPos(
            hwnd,
            HWND_TOP,
            0, 0, 0, 0,
            SWP_NOMOVE |
            SWP_NOSIZE |
            SWP_NOACTIVATE |
            SWP_SHOWWINDOW
        );
    }
    bool running = true;
    int Show()
    {
        srand((unsigned)time(nullptr));

        SDL_SetAppMetadata("Chezze splash", "1.3.3.7-01a", "com.chezze.ix_splash");

        if (!SDL_Init(SDL_INIT_VIDEO))
            return 1;

#if DISABLE_SPLASH_EVENTS
        sphash_render_prikol = NORMIS;
#else
        //idk where the splash shound to enable crt effect, so let's just disable it for now ! :-)

        if (IsBetweenDec25AndJan5()) splash_render_prikol = NOVA_GODA;
        else if (IsBetweenOct30AndNov5()) splash_render_prikol = SPOOKY;
        //else if (crt) sphash_render_prikol = CRT;
        else splash_render_prikol = NORMIS;
#endif
        
        unsigned char* imageData = nullptr;

        SDL_Surface* surface = NULL;

        //check if exist splash.png in exe dir
        {
            bool extern_splash = false;

// \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
//Mr Forserx, dobav' SDL3_image. Bez nego ne work load png from bin dir
// /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ .
#if 0

            //wchar_t exePath[MAX_PATH]{};
            std::wstring exePath;
            exePath.resize(MAX_PATH, 0);
            //DWORD len = GetModuleFileNameW(nullptr, exePath, MAX_PATH);
            auto path_size(GetModuleFileNameW(nullptr, &exePath.front(), MAX_PATH));
            exePath.resize(path_size);

            //if (len != 0 && len < MAX_PATH)
            if (path_size != 0 && path_size < MAX_PATH)
            {
                //wchar_t* lastSlash = wcsrchr(exePath, L'\\');
                //if (!lastSlash) return false;

               // wcscpy_s(lastSlash + 1, MAX_PATH - (lastSlash - exePath), L"splash.png");

                size_t lastSlash = exePath.find_last_of('\\');

                if (lastSlash != -1)
                    exePath.replace(lastSlash + 1, exePath.size() - (lastSlash + 1), L"");

                //if (exePath[exePath.size()] != '\\')
                //	exePath += L'\\';

                exePath += L"splash.png";

                if (GetFileAttributesW(exePath.c_str()) != INVALID_FILE_ATTRIBUTES)
                {
                    //blin blyat, i hope this works with unicode paths
                    int size = WideCharToMultiByte(
                        CP_UTF8,
                        0,
                        exePath.c_str(),
                        -1,
                        nullptr,
                        0,
                        nullptr,
                        nullptr
                    );
                    std::string result(size - 1, '\0');

                    WideCharToMultiByte(
                        CP_UTF8,
                        0,
                        exePath.c_str(),
                        -1,
                        &result[0],
                        size,
                        nullptr,
                        nullptr
                    );

                    surface = IMG_Load(result.c_str());
                    extern_splash = (surface != nullptr);
                }
            }

#endif

            if (!extern_splash)
            {
                int res_id = 0;
                switch (splash_render_prikol)
                {
                case NOVA_GODA:
                    res_id = IDB_SPLASH_BG_NG;
                    break;
                case SPOOKY:
                    res_id = IDB_SPLASH_BG_HW;
                    break;
                default:
                    res_id = IDB_SPLASH_BG;
                    break;
                }

                surface = LoadPNGSurfaceFromResource(imageData, MAKEINTRESOURCE(res_id), TEXT("PNG"));

                if (!surface) {
                    SDL_Log("Couldn't load bitmap: %s", SDL_GetError());
                    return SDL_APP_FAILURE;
                }
            }
        }
        WINDOW_WIDTH = surface->w;
        WINDOW_HEIGHT = surface->h;

        if (!SDL_CreateWindowAndRenderer("chezze/renderer/ixray_splash", WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_BORDERLESS/* | SDL_WINDOW_TRANSPARENT*/, &window, &renderer)) {
            SDL_Log("Couldn't create window/renderer: %s", SDL_GetError());
            return SDL_APP_FAILURE;
        }
        SDL_SetRenderLogicalPresentation(renderer, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_LOGICAL_PRESENTATION_LETTERBOX);

        //////////////////////////////
        SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
        if (!texture) {
            SDL_Log("Couldn't create static texture: %s", SDL_GetError());
            return SDL_APP_FAILURE;
        }
        SDL_DestroySurface(surface);

        // overlay surface (noise + scanlines)
        SDL_Surface* overlaySurf = SDL_CreateSurface(WINDOW_WIDTH, WINDOW_HEIGHT, SDL_PIXELFORMAT_RGBA8888
        );
        //
        SDL_Texture* overlayTex = SDL_CreateTexture(
            renderer,
            SDL_PIXELFORMAT_RGBA8888,
            SDL_TEXTUREACCESS_STREAMING,
            WINDOW_WIDTH, WINDOW_HEIGHT
        );
        //
        SDL_SetTextureBlendMode(overlayTex, SDL_BLENDMODE_BLEND);
        //////////////////////////////
        surface = LoadPNGSurfaceFromResource(imageData, MAKEINTRESOURCE(IDB_LOAD_ICON), TEXT("PNG"));
        stbi_image_free(imageData);

        LD_atlas = SDL_CreateTextureFromSurface(renderer, surface);
        if (!LD_atlas)
            return 1;
        SDL_DestroySurface(surface);
        stbi_image_free(imageData);

        //font
        //surface = IMG_Load("font.png");
        surface = LoadPNGSurfaceFromResource(imageData, MAKEINTRESOURCE(IDB_FONT), TEXT("PNG"));
        stbi_image_free(imageData);

        fontTexture = SDL_CreateTextureFromSurface(renderer, surface);

        CHAR_WIDTH = surface->w / CHARS_PER_ROW;
        CHAR_HEIGHT = surface->h / CHARS_PER_COL;
        //


        float texW, texH;
        SDL_GetTextureSize(LD_atlas, &texW, &texH);

        int frameW = texW / LD_COLS;
        int frameH = texH / LD_ROWS;

        int currentFrame = 0;
        float timer = 0.0f;

        Uint64 prevTicks = SDL_GetTicks();
        
        SDL_Event e;

        if (splash_render_prikol == NOVA_GODA)
            splash::nova_goda::init_snow(WINDOW_WIDTH, WINDOW_HEIGHT);

        SDL_PropertiesID props = SDL_GetWindowProperties(window);
        HWND hwnd = (HWND)SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WIN32_HWND_POINTER, NULL);

        if (!hwnd) {
            SDL_Log("Failed to get HWND: %s", SDL_GetError());
        }
        else {
            //HZ, MAYBE THIS HELP WITH FOCUSING WINDOW ON LAUNCH
            SetForegroundWindow(hwnd);
            //SDL_Log("HWND: %p", hwnd);
        }



        while (running)
        {

            Uint64 now = SDL_GetTicks();
            float delta = (now - prevTicks) / 1000.0f;
            prevTicks = now;

            timer += delta;
            if (timer >= LD_FRAME_TIME)
            {
                timer -= LD_FRAME_TIME;
                currentFrame = (currentFrame + 1) % LD_FRAME_COUNT;
            }

            //while (SDL_PollEvent(&e))
            //{
            //    if (e.type == SDL_EVENT_QUIT)
            //        running = false;
            //}
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderClear(renderer);

            SDL_RenderTexture(renderer, texture, nullptr, nullptr);

            {
                int col = currentFrame % LD_COLS;
                int row = currentFrame / LD_COLS;

                SDL_FRect src{
                    float(col * frameW),
                    float(row * frameH),
                    float(frameW),
                    float(frameH)
                };
                //POSITIONING OF LOADING ANIMATION SCHNYAGA
                SDL_FRect dst{
                     1.f, WINDOW_HEIGHT - 10.f - frameH,
                     //WINDOW_WIDTH - 5.f - frameW, WINDOW_HEIGHT - 5.f - frameH,
                     //WINDOW_WIDTH - 5.f - frameW, 5.f,
                     float(frameW),
                     float(frameH)
                };
                SDL_RenderTexture(renderer, LD_atlas, &src, &dst);
            }
            UpdatepProgressBar(progress_percent, SPLASH_STATUS);

            renderPrikolHub(overlaySurf, overlayTex);

#ifdef DEBUG_DRAW
    #ifdef NDEBUG
                RenderText("DEV BUILD", 0, 0);
    #else
                RenderText("DEBUG BUILD", 0, 0);
    #endif
#endif // !_NDEBUG

            SDL_RenderPresent(renderer);

            SDL_Delay(16); // ~60 FPS
        }

        SDL_DestroyTexture(texture);
        SDL_DestroyTexture(overlayTex);
        SDL_DestroySurface(overlaySurf);
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(window);

        //IMG_Quit();
        //SDL_Quit();
        return 1;
    }

    void Close()
    {
        //SDL_Event e{};
        //e.type = SDL_EVENT_QUIT;
        //SDL_PushEvent(&e);
        running = false;
    }
}

