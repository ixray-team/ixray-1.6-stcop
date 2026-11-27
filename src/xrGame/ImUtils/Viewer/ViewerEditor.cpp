#include "StdAfx.h"
#include "ViewerEditor.h"
#include "ViewerSession.h"

#include <filesystem>

namespace ImUtils::Viewer
{
	void CViewerCpuPool::Init(u32 bytes)
	{
		Shutdown();
		if (bytes == 0)
			return;
		Storage = xr_unique_ptr<u8[]>(static_cast<u8*>(Memory.mem_alloc(bytes)));
		Size = bytes;
	}

	void CViewerCpuPool::Shutdown()
	{
		Storage.reset();
		Size = 0;
		Used = 0;
		Peak = 0;
	}

	bool CViewerCpuPool::Resize(u32 bytes, bool allow)
	{
		if (bytes == Size)
			return true;
		if (Used != 0 || !allow)
			return false;
		Init(bytes);
		return Size == bytes;
	}

	void* CViewerCpuPool::Allocate(u32 bytes, u32 alignment)
	{
		if (bytes == 0 || Size == 0 || Used >= Size || alignment == 0 || (alignment & (alignment - 1)) != 0)
			return nullptr;
		const u32 padding = (alignment - Used % alignment) % alignment;
		if (padding > Size - Used || bytes > Size - Used - padding)
			return nullptr;
		const u32 aligned = Used + padding;
		if (aligned > Size || bytes > Size - aligned)
			return nullptr;
		Used = aligned + bytes;
		Peak = std::max(Peak, Used);
		return Storage.get() + aligned;
	}

	void CViewerEditor::Configure(SViewerPreset& preset)
	{
		preset.SpawnActor = false;
		preset.ArmActor = false;
		preset.Resources.set(u32(ELevelResource::Hud), false);
		preset.Resources.set(u32(ELevelResource::LevelScripts), false);
		MemoryConfig = preset.Memory;
	}

	void CViewerEditor::OnEnter(CViewerSession& session)
	{
		EditorSession = &session;
		CpuPool.Init(MemoryConfig.CpuBytes);
		if (HasPersistentState())
		{
			string_path path;
			xr_string file = xr_string(HistoryFolder()) + "\\history.bin";
			FS.update_path(path, _app_data_root_, file.c_str());
			History.Open(path);
		}
	}

	void CViewerEditor::OnExit(CViewerSession& session)
	{
		CpuPool.Shutdown();
		EditorSession = nullptr;
	}

	void CViewerEditor::Tick(CViewerSession& session)
	{
		CpuPool.Reset();
		TickEditor(session);
	}

	void CViewerEditor::CenterCamera(CViewerSession& session, const Fvector& focus, float distance, float pitch)
	{
		session.SetCameraFocus(focus, distance, pitch);
	}

	bool CViewerEditor::CommitHistory(const char* label)
	{
		if (!HasPersistentState())
			return true;
		CMemoryWriter state;
		if (!WriteState(state))
			return false;
		if (!History.Append(label, state))
		{
			Msg("! %s: history append failed", Name());
			return false;
		}
		return true;
	}

	bool CViewerEditor::RestoreLatest(CViewerSession& session)
	{
		if (!History.HasInitialState())
			return false;
		CMemoryWriter state;
		if (!History.Restore(History.CurrentId() - 1, state))
			return false;
		IReader reader(state.pointer(), state.size());
		if (!ReadState(reader) || !reader.eof())
			return false;
		Restored(session);
		return true;
	}

	bool CViewerEditor::Undo(CViewerSession& session)
	{
		if (!History.CanUndo())
			return false;
		CMemoryWriter state;
		if (!History.Undo(state))
		{
			Msg("! %s: history restore failed at cursor %llu", Name(), (unsigned long long)History.CurrentId());
			return false;
		}
		IReader reader(state.pointer(), state.size());
		if (!ReadState(reader))
		{
			Msg("! %s: state decode failed at cursor %llu", Name(), (unsigned long long)History.CurrentId());
			return false;
		}
		if (!reader.eof())
		{
			Msg("! %s: trailing state bytes at cursor %llu", Name(), (unsigned long long)History.CurrentId());
			return false;
		}
		Restored(session);
		if (!History.SetCursor(History.CurrentId() - 1))
		{
			Msg("! %s: cursor update failed", Name());
			return false;
		}
		return true;
	}

	bool CViewerEditor::Redo(CViewerSession& session)
	{
		if (!History.CanRedo())
			return false;
		CMemoryWriter state;
		if (!History.Redo(state))
			return false;
		IReader reader(state.pointer(), state.size());
		if (!ReadState(reader) || !reader.eof())
			return false;
		Restored(session);
		return History.SetCursor(History.CurrentId() + 1);
	}

	bool CViewerEditor::ApplyMemory(CViewerSession& session)
	{
		CpuPool.Reset();
		if (!CpuPool.Resize(MemoryConfig.CpuBytes, MemoryConfig.AllowCpuResize))
		{
			Msg("! %s: CPU resize rejected while allocations are outstanding or resizing is disabled", Name());
			return false;
		}
		const u32 previous = session.GpuPool.BudgetBytes();
		if (previous != MemoryConfig.GpuBytes)
		{
			if (!MemoryConfig.AllowGpuResize)
			{
				Msg("! %s: GPU resize is disabled", Name());
				return false;
			}
			session.ReleaseRuntimeTerrainScene();
			session.GpuPool.Shutdown();
			if (!session.GpuPool.Init(MemoryConfig.GpuBytes))
			{
				session.GpuPool.Init(previous);
				session.UploadTerrainToGpuPool();
				return false;
			}
			session.UploadTerrainToGpuPool();
		}
		session.Preset.Memory = MemoryConfig;
		session.GetWindowState().Preset.Memory = MemoryConfig;
		Msg("* %s: GPU pool %u bytes; CPU scratch %u bytes", Name(), session.GpuPool.BudgetBytes(), CpuPool.Capacity());
		return true;
	}

	bool CViewerEditor::HandleRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value)
	{
		switch (command)
		{
		case EViewerEditorCommand::Undo:
			return Undo(session);
		case EViewerEditorCommand::Redo:
			return Redo(session);
		case EViewerEditorCommand::ApplyMemory:
		{
			u32 gpuBytes = 0;
			u32 cpuBytes = 0;
			int allow = 0;
			const int fields = argument != nullptr ? sscanf(argument, "%u %u %d", &gpuBytes, &cpuBytes, &allow) : 0;
			if (fields >= 1)
				MemoryConfig.GpuBytes = clampr(gpuBytes, 64u * 1024u, 512u * 1024u * 1024u);
			if (fields >= 2)
				MemoryConfig.CpuBytes = clampr(cpuBytes, 1024u, 16u * 1024u * 1024u);
			if (fields >= 3)
			{
				MemoryConfig.AllowGpuResize = allow != 0;
				MemoryConfig.AllowCpuResize = allow != 0;
			}
			return ApplyMemory(session);
		}
		case EViewerEditorCommand::TestHistory:
			return TestHistory(session);
		default:
			return EditorRequest(session, command, argument, value);
		}
	}

	void CViewerEditor::RenderUI(CViewerSession& session)
	{
		DrawEditor(session);
		if (ImGui::CollapsingHeader("Memory"))
		{
			ImGui::InputScalar("GPU bytes", ImGuiDataType_U32, &MemoryConfig.GpuBytes);
			ImGui::InputScalar("CPU bytes", ImGuiDataType_U32, &MemoryConfig.CpuBytes);
			ImGui::Checkbox("Allow GPU reallocation", &MemoryConfig.AllowGpuResize);
			ImGui::Checkbox("Allow CPU reallocation", &MemoryConfig.AllowCpuResize);
			ImGui::Text("CPU scratch: %u / %u bytes (peak %u)", CpuPool.UsedBytes(), CpuPool.Capacity(), CpuPool.PeakBytes());
			if (ImGui::Button("Apply memory limits"))
				session.RequestEditorCommand(EViewerEditorCommand::ApplyMemory);
		}
		if (HasPersistentState() && ImGui::CollapsingHeader("History", ImGuiTreeNodeFlags_DefaultOpen))
		{
			ImGui::BeginDisabled(!History.CanUndo());
			const bool undo = ImGui::Button("Undo");
			session.UiProbeObserve("Undo", undo);
			if (undo)
				session.RequestEditorCommand(EViewerEditorCommand::Undo);
			ImGui::EndDisabled();
			ImGui::SameLine();
			ImGui::BeginDisabled(!History.CanRedo());
			const bool redo = ImGui::Button("Redo");
			session.UiProbeObserve("Redo", redo);
			if (redo)
				session.RequestEditorCommand(EViewerEditorCommand::Redo);
			ImGui::EndDisabled();
			ImGui::Text("Current state: %llu; only %u window entries are kept in memory", (unsigned long long)History.CurrentId(), History.EntryCount());
			for (u32 index = 0; index < History.EntryCount(); ++index)
			{
				const SViewerHistoryEntry* entry = History.Entry(index);
				if (entry != nullptr)
					ImGui::Text("%llu %s", (unsigned long long)entry->Id, entry->Label);
			}
			ImGui::TextDisabled("%s", History.Path());
		}
	}

	bool CViewerEditor::TestHistory(CViewerSession& session)
	{
		if (!HasPersistentState() || !session.IsActive())
			return false;
		string_path originalPath;
		xr_strcpy(originalPath, History.Path());
		const xr_string relative = xr_string(HistoryFolder()) + "\\history.selftest.bin";
		string_path testPath;
		FS.update_path(testPath, _app_data_root_, relative.c_str());
		IWriter* fixture = FS.w_open(testPath);
		if (fixture == nullptr || !fixture->valid())
		{
			FS.w_close(fixture);
			return false;
		}
		FS.w_close(fixture);
		if (!History.Open(testPath))
			return false;
		bool passed = true;
		for (u32 index = 0; index < 25; ++index)
		{
			char label[32];
			xr_sprintf(label, "test %02u", index);
			PrepareHistoryTest(session, index);
			passed &= VerifyHistoryTestState(session, index);
			passed &= CommitHistory(label);
		}
		passed &= History.EntryCount() == CViewerEditorHistory::WindowCapacity;
		u32 undone = 0;
		while (History.CanUndo())
		{
			if (!Undo(session))
			{
				Msg("! %s: undo failed at %u, cursor %llu", Name(), undone, (unsigned long long)History.CurrentId());
				passed = false;
				break;
			}
			++undone;
		}
		passed &= undone == 24 && VerifyHistoryTestState(session, 0);
		u32 redone = 0;
		while (History.CanRedo())
		{
			if (!Redo(session))
			{
				Msg("! %s: redo failed at %u, cursor %llu", Name(), redone, (unsigned long long)History.CurrentId());
				passed = false;
				break;
			}
			++redone;
		}
		passed &= redone == 24 && VerifyHistoryTestState(session, 24);
		const u64 cursor = History.CurrentId();
		History.Open(History.Path());
		passed &= History.CurrentId() == cursor && History.CanUndo();
		passed &= RestoreLatest(session) && VerifyHistoryTestState(session, 24);
		History.Open(originalPath);
		Msg("* %s: 25 records, undo %u, redo %u, reopen cursor %llu -> %s", Name(), undone, redone, (unsigned long long)cursor, passed ? "PASS" : "FAIL");
		return passed;
	}
}
