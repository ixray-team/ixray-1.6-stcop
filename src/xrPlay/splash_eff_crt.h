#pragma once

namespace splash
{
    namespace crt
    {
        //Uint32 rand_gray()
        //{
        //    Uint8 v = rand() % 256;
        //    return (v << 24) | (v << 16) | (v << 8) | 255;
        //}

        void update_and_draw(SDL_Renderer* r, SDL_Surface* overlaySurf, SDL_Texture* overlayTex, int& win_width, int& win_height)
        {
            static int scanOffset = 0;
            // --- build CRT overlay ---
            SDL_LockSurface(overlaySurf);
            Uint32* pixels = (Uint32*)overlaySurf->pixels;

            for (int y = 0; y < win_height; y++)
            {
                bool scanline = ((y + scanOffset) % 4 == 0);

                for (int x = 0; x < win_width; x++)
                {
                    Uint8 noise = rand() % 50;// 40; // noise strength
                    Uint8 alpha = scanline ? 85 : 50;//90 : 30;

                    Uint32 pixel =
                        (noise << 24) |     // R
                        (noise << 16) |     // G
                        (noise << 8) |     // B
                        alpha;              // A

                    pixels[y * win_width + x] = pixel;
                }
            }

            SDL_UnlockSurface(overlaySurf);
            SDL_UpdateTexture(overlayTex, nullptr,
                overlaySurf->pixels, overlaySurf->pitch);

            scanOffset++;

            // --- render ---
            //SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
            //SDL_RenderClear(renderer);
            //
            //SDL_RenderTexture(renderer, texture, nullptr, nullptr);

            // flicker
            Uint8 flicker = 180 + rand() % 50;
            SDL_SetTextureAlphaMod(overlayTex, flicker);

            SDL_RenderTexture(r, overlayTex, nullptr, nullptr);
        }
    }
}