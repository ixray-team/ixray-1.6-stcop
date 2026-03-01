#pragma once


inline ECORE_API bool IconButton(
	const char* id,
	ImTextureRef texture,
	ImDrawFlags rounding_flags = ImDrawFlags_RoundCornersAll,
	float rounding = 6.0f, ImVec2 button_size = { 26.f,26.f }, ImVec2 image_size = { 20.0f, 20.0f });

inline ECORE_API bool TextToggleButton(
	const char* id,
	const char* text,
	bool& value,
	ImVec2 size = ImVec2(-1, 26.0f),
	ImDrawFlags rounding_flags = ImDrawFlags_RoundCornersAll,
	float rounding = 6.0f);

ECORE_API bool IXBeginMainMenuBar();

ECORE_API void IXEndMainMenuBar();