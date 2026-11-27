#pragma once

#include "ViewerEditorHistory.h"

namespace ImUtils::Viewer { class CViewerSession; }

namespace ImUtils::Viewer
{
	struct SViewerPreset;

	enum class EViewerEditorKind : u8
	{
		LevelSandbox,
		OgfPreview,
		ParticleEditor,
		AiEditor,
		MaterialEditor,
	};

	enum class EViewerEditorCommand : u8
	{
		Load,
		SetParameter,
		Undo,
		Redo,
		ApplyMemory,
		TestHistory,
		Play,
		Stop,
		Save,
		Rotate,
		Reset,
	};

	struct SViewerMemoryConfig
	{
		u32 GpuBytes = 4u * 1024u * 1024u;
		u32 CpuBytes = 64u * 1024u;
		bool AllowGpuResize = false;
		bool AllowCpuResize = false;
	};

	class CViewerCpuPool
	{
	public:
		void Init(u32 bytes);
		void Reset() { Used = 0; }
		void Shutdown();
		bool Resize(u32 bytes, bool allow);
		void* Allocate(u32 bytes, u32 alignment = 16);
		u32 Capacity() const { return Size; }
		u32 UsedBytes() const { return Used; }
		u32 PeakBytes() const { return Peak; }

	private:
		xr_unique_ptr<u8[]> Storage;
		u32 Size = 0;
		u32 Used = 0;
		u32 Peak = 0;
	};

	class IViewerMode
	{
	public:
		virtual ~IViewerMode() = default;
		virtual const char* Name() const = 0;
		virtual void Configure(SViewerPreset& preset) {}
		virtual void OnEnter(CViewerSession& session) {}
		virtual void OnExit(CViewerSession& session) {}
		virtual void OnSessionReady(CViewerSession& session) {}
		virtual void Tick(CViewerSession& session) {}
		virtual void RenderUI(CViewerSession& session) {}
		virtual bool HandleRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) { return false; }
	};

	class CViewerEditor : public IViewerMode
	{
	public:
		~CViewerEditor() override = default;
		void Configure(SViewerPreset& preset) override;
		void OnEnter(CViewerSession& session) override;
		void OnExit(CViewerSession& session) override;
		void Tick(CViewerSession& session) override;
		void RenderUI(CViewerSession& session) override;
		bool HandleRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) override;
		SViewerMemoryConfig MemoryConfig;
		CViewerCpuPool CpuPool;
		CViewerEditorHistory History;

	protected:
		CViewerSession* EditorSession = nullptr;
		virtual const char* HistoryFolder() const { return "viewer"; }
		virtual bool HasPersistentState() const { return false; }
		virtual bool WriteState(IWriter& state) { return true; }
		virtual bool ReadState(IReader& state) { return true; }
		virtual void Restored(CViewerSession& session) {}
		virtual void PrepareHistoryTest(CViewerSession& session, u32 index) {}
		virtual bool VerifyHistoryTestState(CViewerSession& session, u32 index) { return true; }
		virtual void DrawEditor(CViewerSession& session) {}
		virtual void TickEditor(CViewerSession& session) {}
		virtual bool EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) { return false; }
		bool CommitHistory(const char* label);
		bool RestoreLatest(CViewerSession& session);
		bool Undo(CViewerSession& session);
		bool Redo(CViewerSession& session);
		bool ApplyMemory(CViewerSession& session);
		bool TestHistory(CViewerSession& session);
		void CenterCamera(CViewerSession& session, const Fvector& focus, float distance, float pitch = -0.2f);
	};
}
