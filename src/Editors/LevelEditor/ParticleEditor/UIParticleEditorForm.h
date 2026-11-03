#pragma once
#include "../Viewports/IViewport.h"

class CParticleTool;
class CContentView;

// Particle Editor workspace: a top bar tab with its own dock space
class UIParticleEditorForm :
	public IViewport
{
public:
	UIParticleEditorForm();
	virtual ~UIParticleEditorForm();

	virtual void Draw() override;
	virtual void Render() override;

	static void Create(int Tab);
	static void Open(const char* ItemName = nullptr);
	static void Destroy();
	static bool IsActive();
	static bool SaveCurrent();
	// Particle tool while the Particles tab is active, otherwise nullptr
	static CParticleTool* GetActiveTool();
	static CContentView* GetContentView() { return ContentBrowser; }
	static void Update();

private:
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);
	void DrawItemList();
	void DrawProperties();

	static UIParticleEditorForm* Form;
	static CContentView* ContentBrowser;

	CParticleTool* Tool = nullptr;
	bool WasActive = false;
	int PrevViewID = 0;
	string256 SearchFilter = {};
};
