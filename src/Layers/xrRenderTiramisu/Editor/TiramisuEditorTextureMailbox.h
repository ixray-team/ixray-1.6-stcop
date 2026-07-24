#pragma once

#include "../../../xrCore/xrCore.h"

#include "../../../Include/xrRender/EditorRenderer.h"

#include <limits>
#include <mutex>
#include <optional>
#include <string>
#include <utility>
#include <vector>

// Владеющий CPU-пакет одной texture, готовый к передаче в render thread.
struct FEditorOwnedTextureUpload
{
	FEditorTextureHandle Handle;
	u32 Width = 0;
	u32 Height = 0;
	u32 RowPitch = 0;
	EEditorTextureFormat Format = EEditorTextureFormat::Rgba8Unorm;
	xr_vector<std::byte> Pixels;
	u64 Revision = 0;
	xr_string DebugName;
};

// Снимок очереди texture uploads между editor и renderer.
struct TiramisuEditorTextureMailboxPacket
{
	xr_vector<FEditorOwnedTextureUpload> Updates;
	xr_vector<FEditorTextureHandle> Releases;
};

// Потокобезопасный mailbox texture uploads для editor viewport.
class TiramisuEditorTextureMailbox final
{
public:
	// Create/Update копируют payload, поэтому caller может сразу освободить исходную память.
	[[nodiscard]] FEditorTextureHandle Create(const FEditorTextureUpload& Upload, xr_string* OutDiagnostic = nullptr)
	{
		FEditorOwnedTextureUpload Copy;
		if (!CopyUpload(Upload, Copy, OutDiagnostic))
		{
			return {};
		}

		std::scoped_lock Lock(Mutex);
		u32 Index = FEditorTextureHandle::InvalidIndex;
		if (!FreeSlots.empty())
		{
			Index = FreeSlots.back();
			FreeSlots.pop_back();
		}
		else
		{
			if (Slots.size() >= FEditorTextureHandle::InvalidIndex)
			{
				SetDiagnostic(OutDiagnostic, "Editor texture handle table is full");
				return {};
			}
			Index = static_cast<u32>(Slots.size());
			Slots.emplace_back();
		}

		FSlot& Slot = Slots[Index];
		Slot.Generation = NextGeneration(Slot.Generation);
		Slot.Alive = true;
		Slot.AcceptedRevision = Upload.Revision;
		Copy.Handle = {Index, Slot.Generation};
		Slot.Pending = std::move(Copy);
		return {Index, Slot.Generation};
	}

	bool Update(const FEditorTextureHandle Handle, const FEditorTextureUpload& Upload, xr_string* OutDiagnostic = nullptr)
	{
		FEditorOwnedTextureUpload Copy;
		if (!CopyUpload(Upload, Copy, OutDiagnostic))
		{
			return false;
		}

		std::scoped_lock Lock(Mutex);
		FSlot* Slot = FindAliveSlot(Handle);
		if (!Slot)
		{
			SetDiagnostic(OutDiagnostic, "Editor texture handle is stale or invalid");
			return false;
		}
		if (Upload.Revision < Slot->AcceptedRevision)
		{
			SetDiagnostic(OutDiagnostic, "Editor texture revision moved backwards");
			return false;
		}
		if (Upload.Revision == Slot->AcceptedRevision)
		{
			return true;
		}

		Copy.Handle = Handle;
		Slot->AcceptedRevision = Upload.Revision;
		Slot->Pending = std::move(Copy);
		return true;
	}

	// Инвалидирует generation handle и публикует release для render thread.
	bool Destroy(const FEditorTextureHandle Handle)
	{
		std::scoped_lock Lock(Mutex);
		FSlot* Slot = FindAliveSlot(Handle);
		if (!Slot)
		{
			return false;
		}

		Slot->Alive = false;
		Slot->AcceptedRevision = 0;
		Slot->Pending.reset();
		PendingReleases.push_back(Handle);
		FreeSlots.push_back(Handle.Index);
		return true;
	}

	[[nodiscard]] bool IsAlive(const FEditorTextureHandle Handle) const
	{
		std::scoped_lock Lock(Mutex);
		return FindAliveSlot(Handle) != nullptr;
	}

	// Атомарно забирает накопленные uploads/releases в render-thread пакет.
	bool Consume(TiramisuEditorTextureMailboxPacket& OutPacket)
	{
		std::scoped_lock Lock(Mutex);
		OutPacket.Updates.clear();
		OutPacket.Releases.clear();
		OutPacket.Releases.swap(PendingReleases);
		for (FSlot& Slot : Slots)
		{
			if (!Slot.Pending)
			{
				continue;
			}
			OutPacket.Updates.push_back(std::move(*Slot.Pending));
			Slot.Pending.reset();
		}
		return !OutPacket.Updates.empty() || !OutPacket.Releases.empty();
	}

private:
	// Внутренняя запись ресурса с поколением и состоянием публикации.
	struct FSlot
	{
		u32 Generation = 0;
		u64 AcceptedRevision = 0;
		bool Alive = false;
		xr_optional<FEditorOwnedTextureUpload> Pending;
	};

	[[nodiscard]] static u32 NextGeneration(const u32 Current)
	{
		const u32 Next = Current + 1;
		return Next == 0 ? 1 : Next;
	}

	static void SetDiagnostic(xr_string* OutDiagnostic, const char* Message)
	{
		if (OutDiagnostic)
		{
			*OutDiagnostic = Message;
		}
	}

	[[nodiscard]] static bool CopyUpload(const FEditorTextureUpload& Upload, FEditorOwnedTextureUpload& Out, xr_string* OutDiagnostic)
	{
		if (OutDiagnostic)
		{
			OutDiagnostic->clear();
		}
		if (Upload.Width == 0 || Upload.Height == 0 || Upload.Revision == 0)
		{
			SetDiagnostic(OutDiagnostic, "Editor texture dimensions and revision must be non-zero");
			return false;
		}
		constexpr u64 BytesPerPixel = 4;
		const u64 MinimumPitch =
			static_cast<u64>(Upload.Width) * BytesPerPixel;
		if (Upload.RowPitch < MinimumPitch)
		{
			SetDiagnostic(OutDiagnostic, "Editor texture row pitch is too small");
			return false;
		}
		const u64 RequiredSize =
			static_cast<u64>(Upload.RowPitch) * Upload.Height;
		if (RequiredSize > std::numeric_limits<size_t>::max() ||
			Upload.Pixels.size() < RequiredSize)
		{
			SetDiagnostic(OutDiagnostic, "Editor texture pixel payload is truncated");
			return false;
		}

		Out.Width = Upload.Width;
		Out.Height = Upload.Height;
		Out.RowPitch = Upload.RowPitch;
		Out.Format = Upload.Format;
		Out.Pixels.assign(Upload.Pixels.begin(), Upload.Pixels.begin() + static_cast<size_t>(RequiredSize));
		Out.Revision = Upload.Revision;
		Out.DebugName.assign(Upload.DebugName);
		return true;
	}

	[[nodiscard]] FSlot* FindAliveSlot(const FEditorTextureHandle Handle)
	{
		if (!Handle.IsValid() || Handle.Index >= Slots.size())
		{
			return nullptr;
		}
		FSlot& Slot = Slots[Handle.Index];
		return Slot.Alive && Slot.Generation == Handle.Generation ? &Slot : nullptr;
	}

	[[nodiscard]] const FSlot* FindAliveSlot(const FEditorTextureHandle Handle) const
	{
		if (!Handle.IsValid() || Handle.Index >= Slots.size())
		{
			return nullptr;
		}
		const FSlot& Slot = Slots[Handle.Index];
		return Slot.Alive && Slot.Generation == Handle.Generation ? &Slot : nullptr;
	}

	mutable std::mutex Mutex;
	xr_vector<FSlot> Slots;
	xr_vector<u32> FreeSlots;
	xr_vector<FEditorTextureHandle> PendingReleases;
};
