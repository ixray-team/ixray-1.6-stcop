#pragma once

#include "Common.h"
#include "EmbeddedTexture.h"

#define tablo 1

namespace v_obj
{

	class Menu
	{
	public:
		Menu();
		~Menu() = default;

		MenuAction Draw();

	private:
		Texture m_Background;
		Texture m_Logo;
				
		Texture m_mmBut00;
		Texture m_mmBut01;
#if tablo
		Texture m_mmTablo;
#endif
	};

}