#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorTextureMailbox.h"

#include <array>
#include <cstddef>
#include <iostream>
#include <string>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}

FEditorTextureUpload MakeUpload(const xr_span<const std::byte> Pixels,
	const u64 Revision, const u32 Width = 2,
	const u32 Height = 2, const u32 RowPitch = 8)
{
	FEditorTextureUpload Upload;
	Upload.Width = Width;
	Upload.Height = Height;
	Upload.RowPitch = RowPitch;
	Upload.Pixels = Pixels;
	Upload.Revision = Revision;
	Upload.DebugName = "mailbox-test";
	return Upload;
}
} // namespace

int main()
{
	const xr_array<std::byte, 16> Pixels = {};
	TiramisuEditorTextureMailbox Mailbox;
	xr_string Diagnostic;
	const FEditorTextureHandle First = Mailbox.Create(MakeUpload(Pixels, 1), &Diagnostic);
	if (!First.IsValid() || !Mailbox.IsAlive(First))
		return Fail("A valid editor texture upload did not create a live handle");

	TiramisuEditorTextureMailboxPacket Packet;
	if (!Mailbox.Consume(Packet) || Packet.Updates.size() != 1 ||
		Packet.Updates[0].Handle != First || Packet.Updates[0].Pixels.size() != 16)
		return Fail("The mailbox did not own and publish a texture upload");
	if (Mailbox.Consume(Packet))
		return Fail("A texture upload was consumed more than once");

	if (!Mailbox.Update(First, MakeUpload(Pixels, 1), &Diagnostic) ||
		Mailbox.Consume(Packet))
		return Fail("An equal texture revision was not coalesced");
	if (!Mailbox.Update(First, MakeUpload(Pixels, 3), &Diagnostic) ||
		!Mailbox.Update(First, MakeUpload(Pixels, 4), &Diagnostic) ||
		!Mailbox.Consume(Packet) || Packet.Updates.size() != 1 ||
		Packet.Updates[0].Revision != 4)
		return Fail("Texture updates were not coalesced to the newest revision");
	if (Mailbox.Update(First, MakeUpload(Pixels, 2), &Diagnostic))
		return Fail("A backwards texture revision was accepted");

	if (!Mailbox.Destroy(First) || Mailbox.IsAlive(First) ||
		!Mailbox.Consume(Packet) || Packet.Releases.size() != 1 ||
		Packet.Releases[0] != First)
		return Fail("Texture destruction was not published");
	if (Mailbox.Update(First, MakeUpload(Pixels, 5), &Diagnostic))
		return Fail("A stale texture handle accepted an update");

	const FEditorTextureHandle Second = Mailbox.Create(MakeUpload(Pixels, 1));
	if (!Second.IsValid() || Second.Index != First.Index ||
		Second.Generation == First.Generation || Mailbox.IsAlive(First))
		return Fail("A recycled texture slot did not advance its generation");

	FEditorTextureUpload Invalid = MakeUpload(Pixels, 1);
	Invalid.RowPitch = 7;
	if (Mailbox.Create(Invalid, &Diagnostic).IsValid())
		return Fail("An undersized row pitch was accepted");
	Invalid = MakeUpload(xr_span(Pixels).first(8), 1);
	if (Mailbox.Create(Invalid, &Diagnostic).IsValid())
		return Fail("A truncated texture payload was accepted");
	Invalid = MakeUpload(Pixels, 0);
	if (Mailbox.Create(Invalid, &Diagnostic).IsValid())
		return Fail("A zero texture revision was accepted");

	return 0;
}
