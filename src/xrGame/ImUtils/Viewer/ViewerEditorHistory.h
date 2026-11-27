#pragma once

namespace ImUtils::Viewer
{
	struct SViewerHistoryEntry
	{
		u64 Id = 0;
		u64 Offset = 0;
		char Label[32] = {};
	};

	class CViewerEditorHistory
	{
	public:
		static constexpr u32 WindowCapacity = 10;
		static constexpr u32 MaximumRecordBytes = 16u * 1024u * 1024u;

		bool Open(const char* path);
		bool Append(const char* label, const CMemoryWriter& state);
		bool Restore(u64 id, CMemoryWriter& state) const;
		bool SetCursor(u64 id);
		bool CanUndo() const { return Cursor > 1; }
		bool CanRedo() const { return NextAvailable; }
		bool Undo(CMemoryWriter& state);
		bool Redo(CMemoryWriter& state);
		u64 CurrentId() const { return Cursor; }
		bool HasInitialState() const { return HasInitial; }
		const SViewerHistoryEntry* Entry(u32 index) const
		{
			if (index >= WindowCount)
				return nullptr;
			return &Window[(WindowNext + WindowCapacity - WindowCount + index) % WindowCapacity];
		}
		u32 EntryCount() const { return WindowCount; }
		const char* Path() const { return FilePath; }

	private:
		void PushWindow(const SViewerHistoryEntry& entry);
		bool ReadRecord(u64 id, SViewerHistoryEntry& entry, CMemoryWriter* state) const;

		string_path FilePath = {};
		SViewerHistoryEntry Window[WindowCapacity] = {};
		u32 WindowCount = 0;
		u32 WindowNext = 0;
		u64 NextId = 0;
		u64 Cursor = 0;
		bool HasInitial = false;
		bool NextAvailable = false;
		bool NeedsReplace = false;
	};
}
