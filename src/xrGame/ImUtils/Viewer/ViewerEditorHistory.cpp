#include "StdAfx.h"
#include "ViewerEditorHistory.h"

#include <filesystem>

namespace ImUtils::Viewer
{
	namespace ViewerEditorHistoryDetail
	{
		constexpr u32 HeaderBytes = sizeof(u32) + 32 + sizeof(u64) + sizeof(u32);

		bool ReadHeader(IReader& reader, SViewerHistoryEntry& entry, u32& payloadBytes)
		{
			const u32 recordBytes = reader.r_u32();
			if (recordBytes < HeaderBytes || recordBytes > CViewerEditorHistory::MaximumRecordBytes || reader.elapsed() < HeaderBytes)
				return false;
			reader.r(entry.Label, sizeof(entry.Label));
			entry.Label[sizeof(entry.Label) - 1] = 0;
			entry.Id = reader.r_u64();
			payloadBytes = reader.r_u32();
			return payloadBytes <= CViewerEditorHistory::MaximumRecordBytes - HeaderBytes && recordBytes == HeaderBytes + payloadBytes;
		}
	}

	bool CViewerEditorHistory::Open(const char* path)
	{
		if (path == nullptr || path[0] == 0)
			return false;
		xr_strcpy(FilePath, path);
		WindowCount = 0;
		WindowNext = 0;
		NextId = 0;
		Cursor = 0;
		HasInitial = false;
		NextAvailable = false;
		NeedsReplace = false;
		std::error_code error;
		std::filesystem::create_directories(std::filesystem::path(FilePath).parent_path(), error);
		if (error)
		{
			Msg("! viewer history: cannot create '%s'", FilePath);
			return false;
		}
		if (!FS.exist(FilePath))
			return true;

		IReader* reader = FS.r_open(FilePath);
		if (reader == nullptr)
			return false;
		bool valid = true;
		while (!reader->eof())
		{
			SViewerHistoryEntry entry;
			entry.Offset = reader->tell();
			if (reader->elapsed() < 4)
			{
				valid = false;
				break;
			}
			u32 payloadBytes = 0;
			if (!ViewerEditorHistoryDetail::ReadHeader(*reader, entry, payloadBytes) || reader->elapsed() < payloadBytes || entry.Id != NextId || entry.Id == u64(-1))
			{
				valid = false;
				break;
			}
			reader->advance(payloadBytes);
			NextId = entry.Id + 1;
			Cursor = NextId;
			if (!reader->eof())
				PushWindow(entry);
		}
		FS.r_close(reader);
		HasInitial = valid && NextId != 0;
		if (!valid)
		{
			Msg("! viewer history: malformed stream '%s'; it will be replaced by the next recorded state", FilePath);
			NextId = 0;
			Cursor = 0;
			WindowCount = 0;
			WindowNext = 0;
			NeedsReplace = true;
			return true;
		}
		NextAvailable = false;
		return true;
	}

	void CViewerEditorHistory::PushWindow(const SViewerHistoryEntry& entry)
	{
		Window[WindowNext] = entry;
		WindowNext = (WindowNext + 1) % WindowCapacity;
		WindowCount = std::min(WindowCount + 1, WindowCapacity);
	}

	bool CViewerEditorHistory::Append(const char* label, const CMemoryWriter& state)
	{
		if (label == nullptr || state.size() > MaximumRecordBytes - ViewerEditorHistoryDetail::HeaderBytes)
			return false;
		if (NeedsReplace)
		{
			IWriter* replacement = FS.w_open(FilePath);
			if (replacement == nullptr)
				return false;
			FS.w_close(replacement);
			NeedsReplace = false;
		}
		IWriter* writer = FS.w_open_append(FilePath);
		if (writer == nullptr || !writer->valid())
		{
			FS.w_close(writer);
			Msg("! viewer history: cannot append '%s'", FilePath);
			return false;
		}
		SViewerHistoryEntry entry;
		entry.Offset = writer->tell();
		entry.Id = NextId;
		xr_strcpy(entry.Label, sizeof(entry.Label), label);
		writer->w_u32(ViewerEditorHistoryDetail::HeaderBytes + state.size());
		writer->w(entry.Label, sizeof(entry.Label));
		writer->w_u64(entry.Id);
		writer->w_u32(state.size());
		if (state.size() != 0)
			writer->w(const_cast<CMemoryWriter&>(state).pointer(), state.size());
		FS.w_close(writer);
		NextId = entry.Id + 1;
		Cursor = NextId;
		NextAvailable = false;
		PushWindow(entry);
		HasInitial = true;
		return true;
	}

	bool CViewerEditorHistory::ReadRecord(u64 id, SViewerHistoryEntry& entry, CMemoryWriter* state) const
	{
		IReader* reader = FS.r_open(FilePath);
		if (reader == nullptr)
		{
			Msg("! viewer history: cannot open '%s' to restore id %llu", FilePath, (unsigned long long)id);
			return false;
		}
		u64 requestedOffset = u64(-1);
		for (u32 index = 0; index < WindowCount; ++index)
		{
			const SViewerHistoryEntry* cached = Entry(index);
			if (cached != nullptr && cached->Id == id)
			{
				requestedOffset = cached->Offset;
				reader->seek(cached->Offset);
				break;
			}
		}
		bool success = false;
		while (!reader->eof())
		{
			SViewerHistoryEntry current;
			current.Offset = reader->tell();
			if (reader->elapsed() < 4)
				break;
			u32 payloadBytes = 0;
			if (!ViewerEditorHistoryDetail::ReadHeader(*reader, current, payloadBytes) || reader->elapsed() < payloadBytes)
				break;
			if (current.Id == id)
			{
				entry = current;
				if (state != nullptr)
				{
					state->clear();
					state->w(reader->pointer(), payloadBytes);
				}
				reader->advance(payloadBytes);
				success = true;
				break;
			}
			reader->advance(payloadBytes);
		}
		const intptr_t fileBytes = reader->length();
		FS.r_close(reader);
		if (!success)
			Msg("! viewer history: cannot restore id %llu from '%s' (offset %lld, file bytes %lld)", (unsigned long long)id, FilePath, (long long)requestedOffset, (long long)fileBytes);
		return success;
	}

	bool CViewerEditorHistory::Restore(u64 id, CMemoryWriter& state) const
	{
		SViewerHistoryEntry entry;
		return ReadRecord(id, entry, &state);
	}

	bool CViewerEditorHistory::SetCursor(u64 id)
	{
		if (NextId == 0 || id == 0 || id > NextId)
			return false;
		bool cached = false;
		for (u32 index = 0; index < WindowCount; ++index)
			cached = cached || Window[index].Id == id;
		if (id < NextId && !cached)
		{
			SViewerHistoryEntry entry;
			if (!ReadRecord(id, entry, nullptr))
				return false;
		}
		Cursor = id;
		NextAvailable = id < NextId;
		return true;
	}

	bool CViewerEditorHistory::Undo(CMemoryWriter& state)
	{
		return CanUndo() && Restore(Cursor - 2, state);
	}

	bool CViewerEditorHistory::Redo(CMemoryWriter& state)
	{
		return CanRedo() && Restore(Cursor, state);
	}
}
