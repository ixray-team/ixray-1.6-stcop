#include "StdAfx.h"
#include "Game.h"


namespace v_obj
{
	void Game::Draw()
	{
		switch (m_state)
		{
		case GameState::eMenu:
		{
			const auto action = m_menu->Draw();

			switch (action)
			{
			case MenuAction::eStartGame:
			{
				m_state = GameState::eIntro;
				xr_delete(m_menu);
				if (!m_intro)
				{
					m_intro = new Intro();
				}
				break;
			}
			case MenuAction::eExit:
			{
				m_state = GameState::eVSE_konec;
				xr_delete(m_menu);
				break;
			}
			case MenuAction::eNone:
				break;
			}

			break;
		}
		case GameState::eIntro:
		{
			const auto action = m_intro->Draw();

			switch (action)
			{
			case IntroAction::eComplete:
				m_state = GameState::eScene;
				xr_delete(m_intro);
				if (!m_scene)
				{
					m_scene = new Scene();
				}
				break;

			case IntroAction::eNone:
				
				break;
			}

			break;
		}
		case GameState::eScene:
		{
			m_scene->Update();
			const auto action = m_scene->Draw();
			switch (action)
			{
			case IntroAction::eComplete:
				m_state = GameState::eVSE_konec;
				xr_delete(m_scene);
				break;

			case IntroAction::eNone:
				break;
			}

			break;
		}
		case GameState::eVSE_konec:
			ImGui::Text("The game is not loaded");
			if (ImGui::Button("Load the game"))
			{
				if (!m_menu)
				{
					m_menu = new Menu();
				}
				m_state = GameState::eMenu;
			}
			break;
		default:
			ImGui::Text("TY SLOMAL IGRY!!!");
			break;
		}
	}

	Game::Game() : m_state(GameState::eMenu)
	{
		m_menu = new Menu();
	}
}