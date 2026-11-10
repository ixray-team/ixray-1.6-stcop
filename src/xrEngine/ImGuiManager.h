#pragma once

struct ImFont;

namespace ImGuiPriority
{
	constexpr u32 High = 1;
	constexpr u32 Medium = 24;
	constexpr u32 Low = 48;
}

// Windows that engine and game code draw each frame. It does not own an ImGui context
class ENGINE_API CImGuiWindowRegistry
{
public:
	using DrawFunction = std::function<void()>;

	void SetMainViewport(DrawFunction&& Function);
	void Subscribe(shared_str Name, u32 RenderPriority, DrawFunction&& Function);
	void Unsubscribe(shared_str Name);
	void UnsubscribeAll();

	void DrawMainViewport() const;
	void DrawAll() const;

private:
	struct DrawCommand
	{
		shared_str Name;
		DrawFunction Function;
	};

	xr_map<u32, DrawCommand> Commands;
};

// Owner of the ImGui context and its platform and RHI backends. One per process:
// the engine sets CImGuiManager, an editor replaces it with its own implementation
class ENGINE_API IImGuiManager
{
public:
	virtual ~IImGuiManager() = default;

	// RHI backend lifetime, driven by the renderer device
	virtual void InitDevice() = 0;
	virtual void ResetDevice() = 0;
	virtual void DestroyDevice() = 0;

	// Builds and draws one frame into the bound render target
	virtual void RenderFrame() = 0;

	virtual bool IsCapturingInputs() const = 0;
	virtual bool IsGUIRendering() const = 0;

	CImGuiWindowRegistry& Windows() { return WindowRegistry; }

private:
	CImGuiWindowRegistry WindowRegistry;
};

class ENGINE_API CImGuiManager final : public IImGuiManager
{
public:
	static CImGuiManager& Engine();

	void InitPlatform();
	void DestroyPlatform();
	void NewPlatformFrame();

	void InitDevice() override;
	void ResetDevice() override;
	void DestroyDevice() override;
	void RenderFrame() override;

	bool IsCapturingInputs() const override;
	bool IsGUIRendering() const override;

private:
	void LoadFont(ImFont*& FontHandle, const char* Font, float Scale);
	void UpdateCapture();

	xr_vector<IReader*> FontReaders;
	bool DrawUIRender = true;
	bool CaptureInputs = false;
	bool CaptureKeyProcessed = false;
};

// Never null: points to CImGuiManager::Engine() unless an editor installed its own manager
extern ENGINE_API IImGuiManager* GImGUIManager;
