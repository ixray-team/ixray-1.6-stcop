#pragma once

namespace splash
{
    namespace nova_goda
    {
        struct Snowflake
        {
            float x;
            float y;
            float speed;
            float radius;
            float phase;
        };
        constexpr int SNOW_COUNT = 178/*300*/;
        Snowflake snow[SNOW_COUNT];

        void init_snow(int win_width, int win_height)
        {
            
            for (int i = 0; i < SNOW_COUNT; i++)
            {
                snow[i].x = float(rand() % win_width);
                snow[i].y = float(rand() % win_height);
                //snow[i].speed = 0.5f + (rand() % 100) / 100.0f;
                snow[i].speed = (rand() % 100) / 100.0f;
                snow[i].radius = 1.0f + (rand() % 3);
                snow[i].phase = (rand() % 360) * 0.017f;
            }
        }

        void draw_circle(SDL_Renderer* r, int cx, int cy, int radius)
        {
            for (int w = -radius; w <= radius; w++)
                for (int h = -radius; h <= radius; h++)
                    if (w * w + h * h <= radius * radius)
                        SDL_RenderPoint(r, float(cx + w), float(cy + h));
        }

        void update_and_draw_snow(SDL_Renderer* r, int &win_width, int &win_height)
        {
            //SDL_SetRenderDrawColor(r, 255, 255, 255, 178);
            SDL_SetRenderDrawColor(r, 255, 255, 255, 78);
            SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);

            for (int i = 0; i < SNOW_COUNT; i++)
            {
                Snowflake& s = snow[i];

                s.y += s.speed;
                s.phase += 0.01f;
                s.x += sinf(s.phase) * 0.3f;

                if (s.y > win_height)
                {
                    s.y = -5;
                    s.x = float(rand() % win_width);
                }

                draw_circle(r, (int)s.x, (int)s.y, (int)s.radius);
            }
        }

    }
}