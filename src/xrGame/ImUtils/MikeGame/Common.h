#pragma once
#include "imgui.h"

namespace v_obj
{
	enum GameState
	{
		eMenu,
		eIntro,
		eScene,
		eVSE_konec,
	};
	enum class MenuAction
	{
		eNone,
		eStartGame,
		eExit,
	};
	enum class IntroAction
	{
		eNone,
		eComplete
	};
	void ImageRotated(
		ImTextureID texture,
		ImVec2 center,
		ImVec2 size,
		float angle);
}