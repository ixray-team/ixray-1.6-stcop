#pragma once

#include "ViewerEditor.h"

namespace ImUtils::Viewer
{
	class CViewerModeScene : public CViewerEditor
	{
	public:
		const char* Name() const override { return "Scene"; }
	};

	class CViewerModeGameplay : public CViewerEditor
	{
	public:
		const char* Name() const override { return "Gameplay"; }
		void Tick(CViewerSession& session) override;
		void RenderUI(CViewerSession& session) override;

		char SpawnSection[64] = "m_dog_e";
		bool WanderNpcs = false;

	private:
		u32 NextWanderTime = 0;
	};

	class CViewerModeRenderTest : public CViewerEditor
	{
	public:
		const char* Name() const override { return "RenderTest"; }
		void OnEnter(CViewerSession& session) override;
		void OnExit(CViewerSession& session) override;
		void RenderUI(CViewerSession& session) override;

	private:
		void ApplyToggles(CViewerSession& session) const;
	};

	xr_unique_ptr<IViewerMode> CreateViewerLegacyMode(int index);
	int GetViewerModeCount();
	int GetViewerPresetCount();
	const char* GetViewerPresetName(int index);
	void ApplyViewerPreset(int index, SViewerPreset& preset, int& modeIndex);
}
