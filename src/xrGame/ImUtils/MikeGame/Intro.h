#pragma once

#include "Common.h"
#include "EmbeddedTexture.h"

namespace v_obj
{

	class Intro
	{
	private:
		enum Background
		{
			Window,
			Door,
			PC,
			Null,

			BackgroundCount,
		};
		enum Character
		{
			Dmitry,
			Forser,
			v2v3v4,

			CharacterCount
		};
		enum CharacterPos
		{
			Left,
			Right
		};
	public:
		Intro();
		~Intro() = default;

		IntroAction Draw();

	private:
		void drawCharacter(const ImVec2&, float&, Character, CharacterPos);
		void DrawDialogLine(int n);
		void drawDialogNick(const ImVec2&,float&, Character);
		void drawBackgrond(Background);
		int scene_it;


		Texture m_Background;
		Texture m_Characters;
		Texture m_Character_lyaguha;
		Texture m_Dialog_nicks;
		Texture m_DialogText;
		Texture m_Dialog;//container

		Texture m_But00;
		Texture m_ButTexts;

		Texture m_Magic;
		Texture m_ac;

		//characters

	};

}