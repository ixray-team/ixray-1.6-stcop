#pragma once
#include <imgui.h>
#include <vector>
#include <random>
#include "EmbeddedTexture.h"
#include <chrono>
#include "Common.h"
namespace v_obj
{
	class Scene 
	{
    public:
        //тут написано, что это червяки, но мне лень делать под них текстуру, по этому это просто поинты
        static constexpr float WorldSize = 700.0f;
        static constexpr int TargetWorms = 250;
        //static constexpr int TargetWorms = 10;
        static constexpr float GameTime = 10.0f * 60.0f;
        //static constexpr float GameTime = 10.0f;

        Scene();
        ~Scene() = default;

        void Update(float dt);
        IntroAction Draw();

        void Reset();

        bool diagonalMovement = true;

        Texture frogTexture;
        void Update();
    private:
        bool tut_complete = false;
        void drawTut(const ImVec2& origin);

        IntroAction status_game;

        Texture m_Background;
        Texture m_Tut;
        Texture m_GgCirlc;

        Texture m_But00;
        Texture m_ButTexts;

        bool button_eshkere(int index, ImVec2);

        Texture m_Final;
        bool final_eshkere(int index, ImVec2);


        struct Frog
        {
            ImVec2 position{ 350.0f, 320.0f };

            float radius = 18.0f;
            float speed = 180.0f;

            float angle = 0.0f;

            bool jumping = false;
            float jumpTime = 0.0f;

            static constexpr float JumpDuration = 0.45f;
        };

        struct Worm
        {
            ImVec2 position{};
            float radius = 7.0f;

            float lifetime = 6.0f;
        };

        struct Obstacle
        {
            enum class Type
            {
                Rectangle,
                Circle
            };

            Type type;

            ImVec2 min;
            ImVec2 max;

            ImVec2 center;
            float radius;
        };

    private:
        std::chrono::steady_clock::time_point m_lastFrame;

        void UpdateFrog(float dt);
        void UpdateWorms(float dt);
        void SpawnWorm();

        bool IsInsideWorld(ImVec2 position, float radius) const;
        bool CollidesWithObstacle(ImVec2 position, float radius) const;
        bool CollidesWithCircle(
            ImVec2 position,
            float radius,
            const Obstacle& obstacle) const;
        bool IsCircleInsideRect(
            ImVec2 circle,
            float radius,
            const Obstacle& obstacle) const;

        void DrawFrog(const ImVec2& origin);
        void DrawWorm(const ImVec2& origin, const Worm& worm);
        void DrawObstacle(const ImVec2& origin, const Obstacle& obstacle);

        void ImageRotated(
            ImTextureID texture,
            ImVec2 center,
            ImVec2 size,
            float angle);

    private:
        Frog frog;
        std::vector<Worm> worms;
        std::vector<Obstacle> obstacles;

        int score = 0;

        float remainingTime = GameTime;

        float wormSpawnTimer = 0.0f;
        float wormSpawnInterval = 1.5f;

        bool gameOver = false;

        std::mt19937 randomEngine;

	};
}