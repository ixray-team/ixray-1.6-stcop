#include "../xrEngine/stdafx.h"

#define STB_IMAGE_IMPLEMENTATION
#include <SDL_Ext/SDL_image.h>
#include <SDL3/SDL.h>
#include "resource.h"
#include "Splash.h"

#define ErrorMsg(fmt,...) Msg("[Game Splash]<%s ~ line %d>" fmt, __FUNCTION__, __LINE__, __VA_ARGS__)

#pragma warning(disable: 4047)
HINSTANCE hInstanceG = (HINSTANCE)&__ImageBase;
#pragma warning(default: 4047)

SDL_Window* splashWindow = nullptr;
SDL_Renderer* splashRenderer = nullptr;
SDL_Texture* texture = nullptr;
bool isInit = false;

SDL_Surface* LoadPNGSurfaceFromResource(unsigned char* imageData, LPCTSTR lpName, LPCTSTR lpType) {
	HMODULE hMODULE = hInstanceG;

	HRSRC hRes = FindResource(hMODULE, lpName, lpType);
	if (!hRes) {
		ErrorMsg("Failed to find resource (ID %d)", lpName);
		return nullptr;
	}

	HGLOBAL hMem = LoadResource(hInstanceG, hRes);
	if (!hMem) {
		ErrorMsg("Failed to load resource (ID %d)", lpName);
		return nullptr;
	}

	void* pResData = LockResource(hMem);
	if (!pResData) {
		ErrorMsg("Failed to lock resource (ID %d)", lpName);
		return nullptr;
	}

	DWORD resSize = SizeofResource(hInstanceG, hRes);

	int width, height, channels;
	imageData = stbi_load_from_memory((unsigned char*)pResData, resSize, &width, &height, &channels, STBI_rgb_alpha);
	if (!imageData) {
		ErrorMsg("Failed to decode PNG (ID %d)", lpName);
		return nullptr;
	}

	SDL_Surface* surface = SDL_CreateSurfaceFrom(
		imageData,
		width,
		height,
		width * 4,
		SDL_PIXELFORMAT_RGBA32
	);

	if (!surface) {
		stbi_image_free(imageData);
		ErrorMsg("Failed to create pixel format (ID %d). %s", lpName, SDL_GetError());
		return nullptr;
	}

	return surface;
}

SDL_Surface* LoadSplashSurface(unsigned char* imageData, LPCTSTR lpName, LPCTSTR lpType)
{
	char path[MAX_PATH];

	GetModuleFileNameA(NULL, path, MAX_PATH);
	xr_string splash_path = path;

	splash_path = splash_path.erase(splash_path.find_last_of('\\'), splash_path.size() - 1);
	splash_path += "\\splash.png";

	int width, height, channels;
	imageData = stbi_load(splash_path.c_str(), &width, &height, &channels, STBI_rgb_alpha);

	return !imageData ?
		LoadPNGSurfaceFromResource(imageData, lpName, lpType)
		:
		SDL_CreateSurfaceFrom(
			imageData,
			width,
			height,
			width * 4,
			SDL_PIXELFORMAT_RGBA32
		);
}

void Destroy()
{
	SDL_DestroyRenderer(splashRenderer);
	SDL_DestroyWindow(splashWindow);
	SDL_DestroyTexture(texture);

	splashRenderer = nullptr;
	splashWindow = nullptr;
	texture = nullptr;
}


namespace splash
{
	void show()
	{
		if (isInit) return;

		if (SDL_Init(SDL_INIT_VIDEO) != 0) {
			ErrorMsg("SDL_Init Error: %s", SDL_GetError());
			return;
		}

		unsigned char* imageData = nullptr;

		SDL_Surface* surface = LoadSplashSurface(imageData, MAKEINTRESOURCE(IDB_PNG1), _T("PNG"));

		if (!surface)
		{
			ErrorMsg("Failed to create surface (ID %d)", IDB_PNG1);
			return;
		}

		int WinH = surface->h;
		int WinW = surface->w;

		splashWindow = SDL_CreateWindow(
			"Loading...",
			WinW,
			WinH,
			SDL_WINDOW_BORDERLESS | SDL_WINDOW_ALWAYS_ON_TOP |
			SDL_WINDOW_NOT_FOCUSABLE | SDL_WINDOW_TRANSPARENT
		);

		if (!splashWindow) {
			Destroy();
			ErrorMsg("SDL_CreateWindow Error: %s", SDL_GetError());
			return;
		}

		splashRenderer = SDL_CreateRenderer(splashWindow, 0, SDL_RENDERER_SOFTWARE);
		if (!splashRenderer) {
			Destroy();
			ErrorMsg("SDL_CreateRenderer Error: %s", SDL_GetError());
			return;
		}

		texture = SDL_CreateTextureFromSurface(splashRenderer, surface);
		SDL_DestroySurface(surface);
		stbi_image_free(imageData);

		if (!texture) {
			Destroy();
			ErrorMsg("Failed to create texture (ID %d)", IDB_PNG1);
			return;
		}

		{
			SDL_QueryTexture(texture, nullptr, nullptr, &WinW, &WinH);

			SDL_FRect dstRect = { 0, 0, (float)WinW, (float)WinH };
			SDL_RenderTexture(splashRenderer, texture, nullptr, &dstRect);
			SDL_RenderPresent(splashRenderer);
		}

		isInit = true;
		return;
	}


	void hide()
	{
		Destroy();
	}
}