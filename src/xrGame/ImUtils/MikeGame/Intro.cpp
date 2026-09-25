#include "StdAfx.h"
#include "Intro.h"

#include <cmath>

#include "res/I_but_embedded.h"

#include "res/i_bg_embedded.h"
#include "res/i_characters_embedded.h"
#include "res/i_dialog_container_embedded.h"
#include "res/i_magic_embedded.h"

#include "res/i_dialog_nicks_embedded.h"
#include "res/i_dialog_text_embedded.h"
#include "res/i_as_embedded.h"
#include "res/I_buttexts_embedded.h"
#include "res/i_lyagushka_embedded.h"

namespace v_obj
{
	Intro::Intro() : scene_it(0)
	{
		m_Characters = LoadTexture(Assets::i_characters,Assets::i_characters_size);

		m_But00 = LoadTexture(Assets::I_but, Assets::I_but_size);
		m_But00.height /= 1.5;
		m_But00.width /= 1.5;

		m_ButTexts = LoadTexture(Assets::I_buttexts, Assets::I_buttexts_size);
		//m_ButTexts.height /= 1.5;
		//m_ButTexts.width /= 1.5;


		m_Background = LoadTexture(Assets::i_bg, Assets::i_bg_size);

		m_Dialog = LoadTexture(Assets::i_dialog_container, Assets::i_dialog_container_size);
		m_DialogText = LoadTexture(Assets::i_dialog_text, Assets::i_dialog_text_size);
		m_Magic = LoadTexture(Assets::i_magic, Assets::i_magic_size);
	
		m_Dialog_nicks = LoadTexture(Assets::i_dialog_nicks, Assets::i_dialog_nicks_size);
		m_ac = LoadTexture(Assets::i_as, Assets::i_as_size);
		m_Character_lyaguha = LoadTexture(Assets::i_lyagushka, Assets::i_lyagushka_size);
	}

	IntroAction Intro::Draw()
	{
		const auto appsize = ImGui::GetContentRegionAvail();
		float title_bar_height = ImGui::GetFrameHeight();
		//backgrounds
		if (scene_it >= 0 && scene_it <=5)
		{
			drawBackgrond(PC);
		}
		else if (
			(scene_it >= 6 && scene_it <= 10) || 
			scene_it == 12 || scene_it == 13 || scene_it == 15
			)
		{
			drawBackgrond(Window);
			if (scene_it < 15)
				drawCharacter(appsize, title_bar_height, Character::v2v3v4, Right);

			if (m_Magic.IsValid() && scene_it == 13)
			{
				ImGui::SetCursorPos({ appsize.x - (m_Characters.width/4) - (20), title_bar_height * 2 });
				ImGui::Image(
					m_Magic.srv->GetRawSRV(),
					{ static_cast<float>(m_Magic.width), static_cast<float>(m_Magic.height)
					});
			}
		}
		else if (scene_it == 11 || scene_it == 14 || scene_it == 16)
		{
			drawBackgrond(Door);
			if (scene_it < 14)
				drawCharacter(appsize, title_bar_height, Character::Forser, Left);

			if (scene_it >= 14)
			{
				if (m_Character_lyaguha.IsValid())
				{
					ImGui::SetCursorPos({ 10, (appsize.y) / 2 });
					ImGui::Image(
						m_Character_lyaguha.srv->GetRawSRV(),
						{ static_cast<float>(m_Character_lyaguha.width), 
						  static_cast<float>(m_Character_lyaguha.height)
						}

					);
				}
			}
		}
		
		if (scene_it >= 0 && scene_it <= 8)
		{
			drawCharacter(appsize, title_bar_height, Character::Dmitry, Left);
		}
		
		
		if (m_Dialog.IsValid()) 
		{
			ImGui::SetCursorPos({ 0, appsize.y - m_Dialog.height - title_bar_height -15 });
			ImGui::Image(
				m_Dialog.srv->GetRawSRV(),
				{ static_cast<float>(m_Dialog.width), static_cast<float>(m_Dialog.height) });
		}

		if (m_But00.IsValid() /*&& m_But01.IsValid()*/)
		{
			const ImVec2 size{
				static_cast<float>(m_But00.width),
				static_cast<float>(m_But00.height/2.f)
			};

			const ImVec2 pos{
				(appsize.x - size.x),
				appsize.y - size.y + title_bar_height
			};

			ImGui::SetCursorPos(pos);

			ImGui::InvisibleButton("intro_button", size);

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

			const int t_i = 0;
			constexpr float textHeight = 40.0f;

			const float uvY0 =
				(t_i * textHeight) /
				static_cast<float>(m_ButTexts.height);

			const float uvY1 =
				((t_i + 1) * textHeight) /
				static_cast<float>(m_ButTexts.height);

			const ImVec2 textSize{
				size.x/1.5f,
				textHeight/2
			};

			ImGui::SetCursorPos({
				pos.x + m_ButTexts.height /2 ,
				pos.y + textSize.y
				});

			ImGui::Image(
				m_ButTexts.srv->GetRawSRV(),
				textSize,
				{ 0.0f, uvY0 },
				{ 1.0f, uvY1 }
			);

			if (clicked)
			{
				scene_it++;
				//scene_it = 17;
			}
		}

		{
			ImGui::SetCursorPos({ 0,title_bar_height });
#if 0
			if (m_ac.IsValid())
			{
				ImGui::Image(
					m_ac.srv->GetRawSRV(),
					{
						100.f,
						80.f,
					});
			}
#else
			//ImGui::SetCursorPos({ (appsize.x - m_Logo.width) / 2.f,190 });

			const float time = static_cast<float>(ImGui::GetTime());

			//constexpr float amplitude = 0.04f;
			constexpr float amplitude = 100.0f;
			constexpr float speed = 0.001f;

			const float angle =
				std::sin(time * speed) * amplitude;

			const ImVec2 size(100.0f, 80.f);

			const ImVec2 pos = ImGui::GetCursorScreenPos();

			ImVec2 center{
				pos.x + size.x * 0.5f,
				pos.y + size.y * 0.5f
			};

			ImageRotated(
				m_ac.srv->GetRawSRV(),
				center,
				size,
				angle);
#endif
		}

		ImGui::SetCursorPos({ 30, appsize.y - m_Dialog.height });

		DrawDialogLine(scene_it);

		switch (scene_it)
		{
		case 0:
			break;
		case 1:
		case 2:
		case 5:
			drawDialogNick(appsize, title_bar_height, Character::Dmitry);
			break;
		case 6:
		case 7:
		case 8:
		case 12:
		case 13:
			drawDialogNick(appsize, title_bar_height, Character::v2v3v4);
			break;
		case 11:
		case 16:
			drawDialogNick(appsize, title_bar_height, Character::Forser);
			break;
		}

		if (scene_it == 17)
		{
			return IntroAction::eComplete;
		}

		return IntroAction::eNone;
	}
	void Intro::DrawDialogLine(int n)
	{
		if (!m_DialogText.IsValid())
			return;

		constexpr float lineHeight = 24.0f;

		const float ymin = (n * lineHeight) / m_DialogText.height;
		const float ymax = ((n + 1) * lineHeight) / m_DialogText.height;

		ImGui::Image(
			m_DialogText.srv->GetRawSRV(),
			{
				static_cast<float>(m_DialogText.width),
				lineHeight
			},
			{ 0.0f, ymin },
			{ 1.0f, ymax }
		);
	}
	void Intro::drawCharacter(const ImVec2& appsize, float& title_bar_height, Character n, CharacterPos p)
	{
		if (m_Characters.IsValid())
		{
			ImGui::SetCursorPos({ (p == Left ? 0 : appsize.x- m_Characters.width / Character::CharacterCount), title_bar_height});

			const float frameWidth = 1.0f / static_cast<float>(Character::CharacterCount);

			const float xmin = frameWidth * static_cast<float>(n);
			const float xmax = xmin + frameWidth;

			ImGui::Image(
				m_Characters.srv->GetRawSRV(),
				{
					static_cast<float>(m_Characters.width / Character::CharacterCount),
					static_cast<float>(m_Characters.height)
				},
				{ xmin, 0.0f },
				{ xmax, 1.0f }
			);
		}
	}

	void Intro::drawDialogNick(const ImVec2&appsize, float& title_bar_height, Character n)
	{
		if (m_Dialog_nicks.IsValid())
		{
			const float frameHeight = 1.0f / static_cast<float>(Character::CharacterCount);

			const float ymin = frameHeight * static_cast<float>(n);
			const float ymax = ymin + frameHeight;

			ImGui::SetCursorPos({
				15.0f,
				appsize.y - m_Dialog.height - title_bar_height
				});

			ImGui::Image(
				m_Dialog_nicks.srv->GetRawSRV(),
				{
					static_cast<float>(m_Dialog_nicks.width),
					static_cast<float>(m_Dialog_nicks.height / Character::CharacterCount)
				},
				{ 0.0f, ymin },
				{ 1.0f, ymax }
			);
		}
		ImGui::SetCursorPos({ 30, appsize.y - m_Dialog.height+10 });
	}

	void Intro::drawBackgrond(Background n)
	{
		if (m_Background.IsValid())
		{
			constexpr int columns = 2;
			constexpr int rows = 2;

			const int x = n % columns;
			const int y = n / columns;

			const float xmin = static_cast<float>(x) / columns;
			const float xmax = static_cast<float>(x + 1) / columns;

			const float ymin = static_cast<float>(y) / rows;
			const float ymax = static_cast<float>(y + 1) / rows;

			ImGui::Image(
				m_Background.srv->GetRawSRV(),
				{
					static_cast<float>(m_Background.width / columns),
					static_cast<float>(m_Background.height / rows)
				},
				{ xmin, ymin },
				{ xmax, ymax }
			);
		}
	}

}