#pragma once

// Viewport lighting of the editors: lit or unlit view, weather, hemi, AO, placed lights, rain.
class UILightingForm: public IEditorWnd
{
public:
	UILightingForm() = default;
	~UILightingForm() override = default;
	void Draw() override;

	static void Update();
	static void Show();

private:
	void DrawViewMode();
	void DrawEnvironment();
	void DrawAmbient();
	void DrawLights();
	void DrawRain();

	static UILightingForm* Form;
};
