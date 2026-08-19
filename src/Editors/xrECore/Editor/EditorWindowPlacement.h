#pragma once

// Прямоугольник окна или рабочей области без зависимости от SDL/Win32 типов.
struct FEditorWindowPlacementRect
{
	int X = 0;
	int Y = 0;
	int Width = 0;
	int Height = 0;
};

// Проверяет, что верхняя левая область окна, за которую его можно вернуть на
// экран, полностью попадает в рабочую область хотя бы одного дисплея.
[[nodiscard]] constexpr bool IsEditorWindowTitleAreaVisible(
	const FEditorWindowPlacementRect& Window,
	const FEditorWindowPlacementRect& Display
) noexcept
{
	if (Window.Width <= 0 || Window.Height <= 0 ||
		Display.Width <= 0 || Display.Height <= 0)
	{
		return false;
	}

	const int RequiredWidth = Window.Width < 64 ? Window.Width : 64;
	const int RequiredHeight = Window.Height < 32 ? Window.Height : 32;
	const int WindowRight = Window.X + RequiredWidth;
	const int WindowBottom = Window.Y + RequiredHeight;
	const int DisplayRight = Display.X + Display.Width;
	const int DisplayBottom = Display.Y + Display.Height;

	return Window.X >= Display.X && Window.Y >= Display.Y &&
		WindowRight <= DisplayRight && WindowBottom <= DisplayBottom;
}
