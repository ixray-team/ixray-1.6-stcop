#include "StdAfx.h"
#include "Scene.h"
#include <imgui_internal.h>

#include <algorithm>
#include <cmath>

#include "res/s_gm_embedded.h"
#include "res/s_gg_embedded.h"

#include "res/s_gg_circle_embedded.h"
#include "res/s_tut_embedded.h"

#include "res/I_but_embedded.h"
//#include "res/I_nextbut01_embedded.h"
#include "res/I_buttexts_embedded.h"
#include "res/final_embedded.h"

namespace v_obj
{
    bool Scene::button_eshkere(int index, ImVec2 pos)
    {

        // текстура текста растянута по X, мне лень фиксить
        //      upd: поправил
        static int iddb = 0;

        if (m_But00.IsValid() /*&& m_But01.IsValid()*/)
        {
            const auto appsize = ImGui::GetContentRegionMax();
            float title_bar_height = ImGui::GetFrameHeight();

            const ImVec2 size{
                static_cast<float>(m_But00.width),
                static_cast<float>(m_But00.height / 2.f)
            };

            //const ImVec2 pos{
            //    (appsize.x - size.x)/2,
            //    appsize.y - size.y //+ title_bar_height
            //};

            ImGui::SetCursorPos(pos);

            ImGui::InvisibleButton(std::format("intro_button##{}", ++iddb).c_str(), size);

            const bool hovered = ImGui::IsItemHovered();
            const bool clicked = ImGui::IsItemClicked();

            ImGui::SetCursorPos(pos);

            const ImVec2 uv0 = hovered
                ? ImVec2(0.0f, 0.5f)
                : ImVec2(0.0f, 0.0f);

            const ImVec2 uv1 = hovered
                ? ImVec2(1.0f, 1.0f)
                : ImVec2(1.0f, 0.5f);

            ImGui::Image(
				m_But00.srv->GetRawSRV(),
                size,
                uv0,
                uv1
            );


            // =========================
            // TEXT
            // =========================

            const int t_i = index;
            constexpr float textHeight = 40.0f;

            const float uvY0 =
                (t_i * textHeight) /
                static_cast<float>(m_ButTexts.height);

            const float uvY1 =
                ((t_i + 1) * textHeight) /
                static_cast<float>(m_ButTexts.height);

            const ImVec2 textSize{
                size.x / 1.5f,
                textHeight / 2
            };

            ImGui::SetCursorPos({
                pos.x + m_ButTexts.height / 2 ,
                pos.y + textSize.y
                });

            ImGui::Image(
				m_ButTexts.srv->GetRawSRV(),
                textSize,
                { 0.0f, uvY0 },
                { 1.0f, uvY1 }
            );

            return clicked;
        }
        return false;
    }

    bool Scene::final_eshkere(int index, ImVec2 pos)
    {
        if (m_Final.IsValid() /*&& m_But01.IsValid()*/)
        {
            const auto appsize = ImGui::GetContentRegionMax();

            const ImVec2 size{
                static_cast<float>(m_Final.width),
                static_cast<float>(m_Final.height)
            };

            //const ImVec2 pos{
            //    (appsize.x - size.x)/2,
            //    appsize.y - size.y //+ title_bar_height

            ImGui::SetCursorPos(pos);

            // =========================
            // bg
            // =========================

            const int t_i = index;
            constexpr float textHeight = 234.0f;

            const float uvY0 =
                (t_i * textHeight) /
                static_cast<float>(m_Final.height);

            const float uvY1 =
                ((t_i + 1) * textHeight) /
                static_cast<float>(m_Final.height);

            const ImVec2 textSize{
                size.x,
                textHeight
            };

            //ImGui::SetCursorPos({
            //    pos.x + m_ButTexts.height / 2 ,
            //    pos.y + textSize.y
            //    });

            ImGui::Image(
				m_Final.srv->GetRawSRV(),
                textSize,
                { 0.0f, uvY0 },
                { 1.0f, uvY1 }
            );

            return true;
        }
        return false;
    }


    namespace
    {
        constexpr float PI = 3.14159265358979323846f;

        float Length(ImVec2 v)
        {
            return std::sqrt(v.x * v.x + v.y * v.y);
        }

        ImVec2 Normalize(ImVec2 v)
        {
            const float length = Length(v);

            if (length <= 0.0001f)
                return {};

            return {
                v.x / length,
                v.y / length
            };
        }

        ImVec2 Rotate(ImVec2 v, float angle)
        {
            const float c = std::cos(angle);
            const float s = std::sin(angle);

            return {
                v.x * c - v.y * s,
                v.x * s + v.y * c
            };
        }
    }

    Scene::Scene()
        : randomEngine(std::random_device{}()), status_game{IntroAction::eNone}
    {
        m_lastFrame = std::chrono::steady_clock::now();

        m_Background = LoadTexture(
            Assets::s_gm,
            Assets::s_gm_size);
        frogTexture = LoadTexture(
            Assets::s_gg,
            Assets::s_gg_size);

        m_Tut = LoadTexture(
            Assets::s_tut,
            Assets::s_tut_size);
        m_GgCirlc = LoadTexture(
            Assets::s_gg_circle,
            Assets::s_gg_circle_size);

        m_But00 = LoadTexture(Assets::I_but, Assets::I_but_size);
        m_But00.height /= 1.5;
        m_But00.width /= 1.5;

        m_ButTexts = LoadTexture(Assets::I_buttexts, Assets::I_buttexts_size);
        m_Final = LoadTexture(Assets::final, Assets::final_size);
        //m_But01.height /= 1.5;
        //m_But01.width /= 1.5;

        Reset();
    }

    void Scene::Reset()
    {
        frog.position = {
            WorldSize * 0.5f,
            WorldSize * 0.4f
        };

        frog.angle = 0.0f;

        frog.jumping = false;
        frog.jumpTime = 0.0f;

        score = 0;

        remainingTime = GameTime;

        wormSpawnTimer = 0.0f;

        worms.clear();

        obstacles =
        {
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 424.0f, 579.0f },
                38.4f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 494.0f, 413.0f },
                30.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 532.0f, 418.0f },
                32.6f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 289.0f, 180.0f },
                43.3f
            },
            {
                Obstacle::Type::Rectangle,
                { 301.0f, 163.0f },
                { 344.0f, 187.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 323.0f, 165.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 183.0f, 330.0f },
                40.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 316.0f, 360.0f },
                20.6f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 392.0f, 638.0f },
                47.5f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 457.0f, 632.0f },
                46.4f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 583.0f, 510.0f },
                23.3f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 491.9f, 225.0f },
                30.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 221.0f, 50.0f },
                50.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 281.0f, 45.1f },
                45.1f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 393.0f, 61.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 424.0f, 68.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 484.0f, 81.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 454.0f, 75.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 75.0f, 241.0f },
                28.8f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 89.0f, 197.0f },
                45.5f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 119.0f, 197.0f },
                25.6f
            },
            {
                Obstacle::Type::Rectangle,
                { 0.0f, 1.0f },
                { 100.0f, 251.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 96.0f, 0.0f },
                { 309.0f, 50.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 0.0f, 243.0f },
                { 30.0f, 402.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 34.0f, 357.0f },
                27.2f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 39.0f, 267.0f },
                20.5f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 62.0f, 637.0f },
                43.3f
            },
            {
                Obstacle::Type::Rectangle,
                { 247.0f, 670.0f },
                { 700.0f, 700.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 168.0f, 474.0f },
                21.3f
            },
            {
                Obstacle::Type::Rectangle,
                { 261.0f, 426.0f },
                { 276.0f, 473.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 256.0f, 439.0f },
                13.5f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 255.0f, 465.0f },
                10.2f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 241.0f, 455.0f },
                19.8f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 215.0f, 460.0f },
                20.2f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 189.0f, 464.0f },
                20.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 637.0f, 293.0f },
                63.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 638.0f, 231.0f },
                { 700.0f, 348.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 671.3f, 471.0f },
                28.7f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 688.0f, 432.0f },
                12.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 680.0f, 433.0f },
                { 700.0f, 534.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 608.0f, 501.0f },
                19.8f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 636.0f, 488.0f },
                19.4f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 621.6f, 657.6f },
                42.4f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 671.0f, 625.0f },
                29.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 638.0f, 624.0f },
                { 700.0f, 688.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 494.0f, 670.0f },
                25.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 226.0f, 672.0f },
                28.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 0.0f, 653.0f },
                { 209.0f, 700.0f },
                {},
                0.0f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 105.0f, 657.0f },
                23.9f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 19.1f, 405.0f },
                19.1f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 36.9f, 587.0f },
                36.9f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 19.7f, 518.0f },
                19.7f
            },
            {
                Obstacle::Type::Circle,
                {},
                {},
                { 23.0f, 548.0f },
                23.0f
            },
            {
                Obstacle::Type::Rectangle,
                { 0.0f, 500.0f },
                { 26.0f, 700.0f },
                {},
                0.0f
            },
        };

        gameOver = false;

        for (int i = 0; i < 5; ++i)
            SpawnWorm();
    }

    void Scene::Update()
    {
        if (!tut_complete)
            return;

        const auto now = std::chrono::steady_clock::now();

        float deltaTime =
            std::chrono::duration<float>(
                now - m_lastFrame
            ).count();

        m_lastFrame = now;
#undef min
        deltaTime = std::min(deltaTime, 0.1f);

        if (gameOver)
            return;

        remainingTime -= deltaTime;

        if (remainingTime <= 0.0f)
        {
            remainingTime = 0.0f;
            gameOver = true;
            return;
        }

        if (score >= TargetWorms)
        {
            gameOver = true;
            return;
        }

        UpdateFrog(deltaTime);
        UpdateWorms(deltaTime);

    }

    void Scene::drawTut(const ImVec2& origin)
    {
        if (!m_Tut.IsValid() || !m_GgCirlc.IsValid()) 
        {
            tut_complete = true;
            return;
        }
        const auto appsize = ImGui::GetContentRegionMax();
        float title_bar_height = ImGui::GetFrameHeight();

        ImGui::SetCursorPos({ 0,title_bar_height });
        ImGui::Image(
			m_Tut.srv->GetRawSRV(),
            {
                static_cast<float>(m_Tut.width),
                static_cast<float>(m_Tut.height),
            }
            );

        //ImGui::SetCursorPos({ frog.position.x - 48, frog.position.y - 48.f});
        //ImGui::Image(
        //    m_GgCirlc.srv->GetRawSRV(),
        //    {
        //        static_cast<float>(m_GgCirlc.width),
        //        static_cast<float>(m_GgCirlc.height),
        //    }
        //    );
        const ImVec2 center =
        {
            origin.x + frog.position.x,
            origin.y + frog.position.y
        };
        ImageRotated(
            (ImTextureID)m_GgCirlc.srv,
            {
                center.x,
                center.y
            },
            {
                48.0f,
                48.0f
            },
            frog.angle);

        const ImVec2 size{
                static_cast<float>(m_But00.width),
                static_cast<float>(m_But00.height / 2.f)
        };
        if (button_eshkere(0,{
                (appsize.x - size.x)/2,
                appsize.y - size.y //+ title_bar_height
            }))
        {
            tut_complete = true;
        }
    }

    void Scene::Update(float dt)
    {
        if (gameOver)
            return;

        remainingTime -= dt;

        if (remainingTime <= 0.0f)
        {
            remainingTime = 0.0f;
            gameOver = true;
            return;
        }

        if (score >= TargetWorms)
        {
            gameOver = true;
            return;
        }

        UpdateFrog(dt);
        UpdateWorms(dt);
    }

    void Scene::UpdateFrog(float dt)
    {
        ImVec2 direction{};

        if (ImGui::IsKeyDown(ImGuiKey_W))
            direction.y -= 1.0f;

        if (ImGui::IsKeyDown(ImGuiKey_S))
            direction.y += 1.0f;

        if (ImGui::IsKeyDown(ImGuiKey_A))
            direction.x -= 1.0f;

        if (ImGui::IsKeyDown(ImGuiKey_D))
            direction.x += 1.0f;

        if (!diagonalMovement)
        {
            if (direction.x != 0.0f)
                direction.y = 0.0f;
        }

        direction = Normalize(direction);

        if (Length(direction) > 0.0f)
        {
            frog.angle = std::atan2(direction.y, direction.x);

            ImVec2 newPosition =
            {
                frog.position.x + direction.x * frog.speed * dt,
                frog.position.y + direction.y * frog.speed * dt
            };

            const bool canPassObstacles = frog.jumping;

            if (!canPassObstacles &&
                CollidesWithObstacle(newPosition, frog.radius))
            {

                ImVec2 xPosition =
                {
                    newPosition.x,
                    frog.position.y
                };

                if (!CollidesWithObstacle(xPosition, frog.radius))
                    frog.position.x = xPosition.x;

                ImVec2 yPosition =
                {
                    frog.position.x,
                    newPosition.y
                };

                if (!CollidesWithObstacle(yPosition, frog.radius))
                    frog.position.y = yPosition.y;
            }
            else
            {
                frog.position = newPosition;
            }
        }

        frog.position.x = std::clamp(
            frog.position.x,
            frog.radius,
            WorldSize - frog.radius);

        frog.position.y = std::clamp(
            frog.position.y,
            frog.radius,
            WorldSize - frog.radius);

        if (!frog.jumping &&
            ImGui::IsKeyPressed(ImGuiKey_Space))
        {
            frog.jumping = true;
            frog.jumpTime = 0.0f;
        }

        if (frog.jumping)
        {
            frog.jumpTime += dt;

            if (frog.jumpTime >= Frog::JumpDuration)
            {
                frog.jumping = false;
                frog.jumpTime = 0.0f;
            }
        }
    }

    void Scene::UpdateWorms(float dt)
    {
        wormSpawnTimer += dt;

        if (wormSpawnTimer >= wormSpawnInterval)
        {
            wormSpawnTimer = 0.0f;

            if (worms.size() < 20)
                SpawnWorm();
        }


        for (auto& worm : worms)
            worm.lifetime -= dt;

        worms.erase(
            std::remove_if(
                worms.begin(),
                worms.end(),
                [&](const Worm& worm)
                {

                    if (!frog.jumping)
                    {
                        const ImVec2 delta =
                        {
                            frog.position.x - worm.position.x,
                            frog.position.y - worm.position.y
                        };

                        const float distance = Length(delta);

                        if (distance <= frog.radius + worm.radius)
                        {
                            ++score;
                            return true;
                        }
                    }

                    return worm.lifetime <= 0.0f;
                }),
            worms.end());
    }

    void Scene::SpawnWorm()
    {
        std::uniform_real_distribution<float> distribution(
            20.0f,
            WorldSize - 20.0f);

        for (int attempt = 0; attempt < 100; ++attempt)
        {
            ImVec2 position =
            {
                distribution(randomEngine),
                distribution(randomEngine)
            };

            if (CollidesWithObstacle(position, 10.0f))
                continue;

            const ImVec2 delta =
            {
                position.x - frog.position.x,
                position.y - frog.position.y
            };

            if (Length(delta) < 80.0f)
                continue;

            worms.push_back({
                position,
                7.0f,
                5.0f
                });

            return;
        }
    }

    bool Scene::IsInsideWorld(
        ImVec2 position,
        float radius) const
    {
        return
            position.x >= radius &&
            position.y >= radius &&
            position.x <= WorldSize - radius &&
            position.y <= WorldSize - radius;
    }

    bool Scene::IsCircleInsideRect(
        ImVec2 circle,
        float radius,
        const Obstacle& obstacle) const
    {
        const float closestX = std::clamp(
            circle.x,
            obstacle.min.x,
            obstacle.max.x);

        const float closestY = std::clamp(
            circle.y,
            obstacle.min.y,
            obstacle.max.y);

        const float dx = circle.x - closestX;
        const float dy = circle.y - closestY;

        return
            dx * dx + dy * dy <
            radius * radius;
    }

    bool Scene::CollidesWithObstacle(
        ImVec2 position,
        float radius) const
    {
        if (!IsInsideWorld(position, radius))
            return true;

        for (const auto& obstacle : obstacles)
        {
            switch (obstacle.type)
            {
            case Obstacle::Type::Rectangle:
                if (IsCircleInsideRect(
                    position,
                    radius,
                    obstacle))
                {
                    return true;
                }
                break;

            case Obstacle::Type::Circle:
                if (CollidesWithCircle(
                    position,
                    radius,
                    obstacle))
                {
                    return true;
                }
                break;
            }
        }

        return false;
    }

    bool Scene::CollidesWithCircle(
        ImVec2 position,
        float radius,
        const Obstacle& obstacle) const
    {
        const float dx =
            position.x - obstacle.center.x;

        const float dy =
            position.y - obstacle.center.y;

        const float combinedRadius =
            radius + obstacle.radius;

        return
            dx * dx + dy * dy <
            combinedRadius * combinedRadius;
    }

    IntroAction Scene::Draw()
    {
        /*ImGui::BeginChild(
            "GameWorld",
            ImVec2(WorldSize, WorldSize),
            true,
            ImGuiWindowFlags_NoScrollbar |
            ImGuiWindowFlags_NoScrollWithMouse);*/

        const ImVec2 origin = ImGui::GetCursorScreenPos();

        ImDrawList* drawList = ImGui::GetWindowDrawList();


        if (m_Background.IsValid())
        {
            ImGui::Image(
				m_Background.srv->GetRawSRV(),
                {
                    static_cast<float>(m_Background.width),
                    static_cast<float>(m_Background.height),
                }
                );
        }


        //for (const auto& obstacle : obstacles)
        //    DrawObstacle(origin, obstacle);
        if (tut_complete)
            for (const auto& worm : worms)
                DrawWorm(origin, worm);


        DrawFrog(origin);

        if (!tut_complete)
            drawTut(origin);

        drawList->AddRect(
            origin,
            {
                origin.x + WorldSize,
                origin.y + WorldSize
            },
            IM_COL32(20, 20, 20, 255),
            0.0f,
            0,
            3.0f);

        //ImGui::EndChild();

        ImGui::SetCursorPos({ 0,30 });

        ImGui::Text(
            "Points: %d / %d",
            score,
            TargetWorms);

        const int totalSeconds =
            static_cast<int>(remainingTime);

        const int minutes = totalSeconds / 60;
        const int seconds = totalSeconds % 60;

        ImGui::Text(
            "Time: %02d:%02d",
            minutes,
            seconds);

        //if (frog.jumping)
        //    ImGui::Text("JUMP!");

        if (gameOver)
        {
            int krasava = (score >= TargetWorms ? 1 : 2);
            //int krasava = 1;


            const float title_bar_height = ImGui::GetFrameHeight();
            const auto appsize = ImGui::GetContentRegionMax();
            final_eshkere(0, {0,
                (appsize.y - 700/3)/2
                });

            final_eshkere(krasava, { 0,
                    (appsize.y - 700 / 3) / 2
                });
            const auto bspos = ImGui::GetCursorPos();
            float secondButPosY = (appsize.x - m_But00.width) / 2;
            if (krasava == 2)
            {
                if (button_eshkere(1, { appsize.x / 2 - m_But00.width, bspos.y }))
                    Reset();
                secondButPosY = appsize.x / 2;
            }

            if (button_eshkere(0, { secondButPosY, bspos.y }))
                status_game = IntroAction::eComplete;
        }

        return status_game;
    }

    void Scene::DrawObstacle(
        const ImVec2& origin,
        const Obstacle& obstacle)
    {
        ImDrawList* drawList = ImGui::GetWindowDrawList();

        if (obstacle.type == Obstacle::Type::Rectangle)
        {
            drawList->AddRectFilled(
                {
                    origin.x + obstacle.min.x,
                    origin.y + obstacle.min.y
                },
            {
                origin.x + obstacle.max.x,
                origin.y + obstacle.max.y
            },
                IM_COL32(90, 65, 40, 2));

            drawList->AddRect(
                {
                    origin.x + obstacle.min.x,
                    origin.y + obstacle.min.y
                },
            {
                origin.x + obstacle.max.x,
                origin.y + obstacle.max.y
            },
                IM_COL32(50, 3, 3, 255),
                0.0f,
                0,
                2.0f);
        }
        else if (obstacle.type == Obstacle::Type::Circle)
        {
            const ImVec2 center =
            {
                origin.x + obstacle.center.x,
                origin.y + obstacle.center.y
            };

            drawList->AddCircleFilled(
                center,
                obstacle.radius,
                IM_COL32(90, 3, 3, 5));

            drawList->AddCircle(
                center,
                obstacle.radius,
                IM_COL32(50, 4, 4, 255),
                32,
                2.0f);
        }
    }

    void Scene::DrawWorm(
        const ImVec2& origin,
        const Worm& worm)
    {
        ImDrawList* drawList = ImGui::GetWindowDrawList();

        const ImVec2 position =
        {
            origin.x + worm.position.x,
            origin.y + worm.position.y
        };

        /*
             вместо текстуры.
        */
        drawList->AddCircleFilled(
            position,
            worm.radius,
            IM_COL32(210, 90, 70, 255));

        drawList->AddCircle(
            position,
            worm.radius,
            IM_COL32(80, 40, 30, 255),
            12,
            2.0f);
    }

    void Scene::DrawFrog(
        const ImVec2& origin)
    {
        const ImVec2 center =
        {
            origin.x + frog.position.x,
            origin.y + frog.position.y
        };

        float jumpHeight = 0.0f;

        if (frog.jumping)
        {
            const float t =
                frog.jumpTime / Frog::JumpDuration;

            jumpHeight =
                std::sin(t * PI) * 12.0f;
        }

        if (frogTexture.IsValid())
        {
            ImageRotated(
                (ImTextureID)frogTexture.srv,
                {
                    center.x,
                    center.y - jumpHeight
                },
            {
                48.0f,
                48.0f
            },
                frog.angle);

            return;
        }
    }

    void Scene::ImageRotated(
        ImTextureID texture,
        ImVec2 center,
        ImVec2 size,
        float angle)
    {
        ImDrawList* drawList = ImGui::GetWindowDrawList();

        const ImVec2 halfSize =
        {
            size.x * 0.5f,
            size.y * 0.5f
        };

        /*
            Четыре угла исходного изображения.
        */
        ImVec2 points[4] =
        {
            { -halfSize.x, -halfSize.y },
            {  halfSize.x, -halfSize.y },
            {  halfSize.x,  halfSize.y },
            { -halfSize.x,  halfSize.y }
        };

        /*
            Поворачиваем каждый угол.
        */
        for (auto& point : points)
        {
            point = Rotate(point, angle);

            point.x += center.x;
            point.y += center.y;
        }

        /*
            UV стандартной текстуры.
        */
        const ImVec2 uv0{ 0.0f, 0.0f };
        const ImVec2 uv1{ 1.0f, 0.0f };
        const ImVec2 uv2{ 1.0f, 1.0f };
        const ImVec2 uv3{ 0.0f, 1.0f };

        drawList->AddImageQuad(
            texture,
            points[0],
            points[1],
            points[2],
            points[3],
            uv0,
            uv1,
            uv2,
            uv3,
            IM_COL32_WHITE);
    }
}