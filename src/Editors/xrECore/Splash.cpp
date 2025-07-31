#include "stdafx.h"

#define STB_IMAGE_IMPLEMENTATION
#include <SDL_Ext/SDL_image.h>
#include <SDL3/SDL.h>
#include "resource.h"
#include "Splash.h"
#define ErrorMsg(fmt,...) Msg("[Editor Splash]<%s ~ line %d>" fmt, __FUNCTION__, __LINE__, __VA_ARGS__)

EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#pragma warning(disable: 4047)
HINSTANCE hInstanceG = (HINSTANCE)&__ImageBase;
#pragma warning(default: 4047)

SDL_Window* splashWindow = nullptr;
SDL_Renderer* splashRenderer = nullptr;
SDL_Texture* texture = nullptr;
bool isInit = false;
int WinW = 0, WinH = 0;

//
SDL_Texture* fontTexture = nullptr;
const int CHAR_WIDTH = 9;
const int CHAR_HEIGHT = 17;
const int CHARS_PER_ROW = 14;
bool outText = false;

void RenderText(const char* text, int x, int y) {
	if (!outText) return;

	for (size_t i = 0; i < strlen(text); ++i) {
		char c = text[i];
		int charIndex = c - 32;
		int srcX = (charIndex % CHARS_PER_ROW) * CHAR_WIDTH;
		int srcY = (charIndex / CHARS_PER_ROW) * CHAR_HEIGHT;

		SDL_FRect srcRect = { srcX, srcY, CHAR_WIDTH, CHAR_HEIGHT };
		SDL_FRect dstRect = { x + i * CHAR_WIDTH, y, CHAR_WIDTH, CHAR_HEIGHT };

		SDL_RenderTexture(splashRenderer, fontTexture, &srcRect, &dstRect);
	}
}

void Update(int progress = 0, const char*status = "")
{
	int pgHeight = 10;

	SDL_FRect progressBarBackground = { 0, WinH - pgHeight, WinW, pgHeight };
	SDL_FRect progressBarFill = 
	{ 
		progressBarBackground.x, 
		progressBarBackground.y,
		(progress * progressBarBackground.w) / 100,
		progressBarBackground.h
	};

	SDL_RenderClear(splashRenderer);

	SDL_QueryTexture(texture, nullptr, nullptr, &WinW, &WinH);

	SDL_FRect dstRect = { 0, 0, WinW, WinH };
	SDL_RenderTexture(splashRenderer, texture, nullptr, &dstRect);

	SDL_SetRenderDrawColor(splashRenderer, 150, 150, 150, 255);
	SDL_RenderFillRect(splashRenderer, &progressBarBackground);

	SDL_SetRenderDrawColor(splashRenderer, 3, 181, 3, 255);
	SDL_RenderFillRect(splashRenderer, &progressBarFill);

	RenderText(status, (WinW - (strlen(status) * CHAR_WIDTH) )/2, progressBarBackground.y - CHAR_HEIGHT);

	SDL_RenderPresent(splashRenderer);
}

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

void Destroy()
{
	SDL_DestroyRenderer(splashRenderer);
	SDL_DestroyWindow(splashWindow);
	SDL_DestroyTexture(texture);
	SDL_DestroyTexture(fontTexture);

	splashRenderer = nullptr;
	splashWindow = nullptr;
	texture = nullptr;
	fontTexture = nullptr;
}

namespace splash
{
	ECORE_API void show(int idb)
	{
		if (isInit) return;
		
		outText = true;
		
		if (SDL_Init(0) != 0) {
			ErrorMsg("SDL_Init Error: %s", SDL_GetError());
			return;
		}
		
		unsigned char* imageData = nullptr;

		SDL_Surface* surface = LoadPNGSurfaceFromResource(imageData, MAKEINTRESOURCE(idb), TEXT("PNG"));
		
		if (!surface)
		{
			ErrorMsg("Failed to create surface (ID %d)", idb);
			return;
		}

		WinH = surface->h;
		WinW = surface->w;

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

		splashRenderer = SDL_CreateRenderer(splashWindow, NULL, NULL
		/*"splashRenderer", SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC*/);
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
			ErrorMsg("Failed to create texture (ID %d)", idb);
			return;
		}

		SDL_Surface* fontSurface = LoadPNGSurfaceFromResource(imageData, MAKEINTRESOURCE(IDB_FONT), TEXT("PNG"));

		if (!fontSurface)
		{
			ErrorMsg("Failed to create font surface (ID %d)", idb);
			outText = false;
		}

		if (outText){
			fontTexture = SDL_CreateTextureFromSurface(splashRenderer, fontSurface);
			SDL_DestroySurface(fontSurface);

			if (!fontTexture) {
				ErrorMsg("Failed to create font texture (ID %d)", idb);
				outText = false;
			}
		}

		Update();

		isInit = true;
		return;
	}

	ECORE_API void splash::update(int progress, const char*status)
	{
		if (!isInit) return;
		
		Update(progress, status);

		return;
	}

	ECORE_API void hide()
	{
		Destroy();
	}
}