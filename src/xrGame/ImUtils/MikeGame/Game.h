#pragma once
#include "Menu.h"
#include "Intro.h"
#include "Scene.h"
#include <memory>
namespace v_obj
{
	class Game
	{
	public:
		Game();
		~Game() = default;

		void Draw();

		//bool isAlive()
		//{
		//	return m_state != eVSE_konec;
		//}
	private:

		GameState m_state;
		Menu* m_menu = nullptr;
		Intro* m_intro = nullptr;
		Scene* m_scene = nullptr;
	};



}