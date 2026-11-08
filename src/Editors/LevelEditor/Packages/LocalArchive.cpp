#include "stdafx.h"
#include "LocalArchive.h"
#include <archive.h>
#include <archive_entry.h>
#include <cerrno>

namespace SdkPackages::LocalSource
{
static bool SafeName(const xr_string& Name)
{
	if (Name.empty() || !Presentation::IsUtf8(Name) || Name.size() > 4096 || Name.starts_with('/') || Name.find_first_of("\\:<>\"|?*") != xr_string::npos)
	{
		return false;
	}
	for (unsigned char Letter : Name)
	{
		if (Letter < 32 || Letter == 127)
		{
			return false;
		}
	}
	size_t Start = 0;
	while (Start < Name.size())
	{
		const size_t End = Name.find('/', Start);
		const xr_string Part = Name.substr(Start, End == xr_string::npos ? End : End - Start);
		if (Part.empty() || Part == "." || Part == ".." || Part.back() == '.' || Part.back() == ' ')
		{
			return false;
		}
		const auto Stem = Details::Lower(Part.substr(0, Part.find('.')));
		if (Stem == "con" || Stem == "prn" || Stem == "aux" || Stem == "nul" ||
			(Stem.size() == 4 && (Stem.starts_with("com") || Stem.starts_with("lpt")) && Stem[3] >= '1' && Stem[3] <= '9'))
		{
			return false;
		}
		if (End == xr_string::npos)
		{
			break;
		}
		Start = End + 1;
	}
	return true;
}


class ArchiveReader final
{
public:
	ArchiveReader(const xr_path& Path, u64 Size, std::stop_token Stop)
		: Stream(Path, std::ios::binary), Size(Size), Stop(Stop), Handle(archive_read_new()), IsSevenZip(Details::Lower(Utf8(Path.extension())) == ".7z") {}
	~ArchiveReader()
	{
		if (Handle)
		{
			archive_read_free(Handle);
		}
	}
	bool Open()
	{
		if (!Handle || !Stream.is_open())
		{
			return false;
		}
		if (Stop.stop_requested())
		{
			return false;
		}
		if (IsSevenZip ? !ValidateSevenZip() : !ValidateZip())
		{
			return false;
		}
		Stream.clear();
		Stream.seekg(0);
		const auto Format = IsSevenZip ? archive_read_support_format_7zip(Handle) : IsEmptyZip ? archive_read_support_format_zip_streamable(Handle)
																							   : archive_read_support_format_zip_seekable(Handle);
		return Stream.good() && Format == ARCHIVE_OK && archive_read_support_filter_none(Handle) == ARCHIVE_OK &&
			   archive_read_set_seek_callback(Handle, Seek) == ARCHIVE_OK && archive_read_open2(Handle, this, nullptr, Read, Skip, nullptr) == ARCHIVE_OK;
	}
	bool Fail(xr_string& Error) const
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Operation cancelled.");
		}
		Error = "Cannot extract package archive: ";
		const char* Detail = Handle ? archive_error_string(Handle) : nullptr;
		if (Detail)
		{
			for (size_t Index = 0; Index < 512 && Detail[Index]; ++Index)
			{
				Error.push_back(Detail[Index]);
			}
		}
		else
		{
			Error += "invalid data, unsupported compression or output error.";
		}
		return false;
	}
	archive* Get() const { return Handle; }

private:
	static u64 Number(const u8* Bytes, size_t Count)
	{
		u64 Value = 0;
		for (size_t Index = 0; Index < Count; ++Index)
		{
			Value |= static_cast<u64>(Bytes[Index]) << (Index * 8);
		}
		return Value;
	}
	bool ReadAt(u64 Offset, void* Data, size_t Count)
	{
		if (Offset > Size || Count > Size - Offset)
		{
			return false;
		}
		Stream.clear();
		Stream.seekg(static_cast<std::streamoff>(Offset));
		Stream.read(static_cast<char*>(Data), Count);
		return Stream.good();
	}
	bool ValidateSevenZip()
	{
		xr_array<u8, 32> Header{};
		if (!ReadAt(0, Header.data(), Header.size()))
		{
			archive_set_error(Handle, EINVAL, "Truncated 7z start header.");
			return false;
		}
		const u64 Available = Size - Header.size();
		const u64 NextOffset = Number(Header.data() + 12, 8);
		const u64 NextSize = Number(Header.data() + 20, 8);
		// Reject impossible headers before the decoder allocates their entry tables.
		if (NextOffset > Available || NextSize > Available - NextOffset)
		{
			archive_set_error(Handle, EINVAL, "7z next header exceeds archive bounds.");
			return false;
		}
		return true;
	}
	bool ValidateZipDirectory(u64 Offset, u64 Length, u64 Entries)
	{
		u64 Count = 0;
		while (Length)
		{
			if (Stop.stop_requested())
			{
				return false;
			}
			xr_array<u8, 46> Header{};
			if (Length >= 6 && ReadAt(Offset, Header.data(), 6) && Number(Header.data(), 4) == 0x05054b50)
			{
				return Count == Entries && Number(Header.data() + 4, 2) == Length - 6;
			}
			if (Length < Header.size() || !ReadAt(Offset, Header.data(), Header.size()) || Number(Header.data(), 4) != 0x02014b50 || ++Count > Entries || Count > MaxEntries)
			{
				return false;
			}
			const auto NameSize = Number(Header.data() + 28, 2);
			const auto RecordSize = Header.size() + NameSize + Number(Header.data() + 30, 2) + Number(Header.data() + 32, 2);
			if (!NameSize || NameSize > 4096 || RecordSize > Length || Number(Header.data() + 34, 2))
			{
				return false;
			}
			Offset += RecordSize;
			Length -= RecordSize;
		}
		return Count == Entries;
	}
	bool ValidateZip()
	{
		if (Size < 22)
		{
			archive_set_error(Handle, EINVAL, "Truncated ZIP end record.");
			return false;
		}
		xr_vector<u8> Tail(static_cast<size_t>(std::min<u64>(Size, 22 + 65535)));
		if (!ReadAt(Size - Tail.size(), Tail.data(), Tail.size()))
		{
			return false;
		}
		for (size_t Offset = Tail.size() - 22 + 1; Offset-- > 0;)
		{
			const auto* End = Tail.data() + Offset;
			if (Number(End, 4) != 0x06054b50 || Number(End + 20, 2) != Tail.size() - Offset - 22)
			{
				continue;
			}
			const u64 EndOffset = Size - Tail.size() + Offset;
			u64 DirectoryEnd = EndOffset;
			u64 Entries = Number(End + 10, 2);
			u64 DirectorySize = Number(End + 12, 4);
			u64 DirectoryOffset = Number(End + 16, 4);
			if (Number(End + 4, 2) || Number(End + 6, 2) || Number(End + 8, 2) != Entries)
			{
				break;
			}
			xr_array<u8, 20> Locator{};
			const bool HasZip64 = DirectoryEnd >= Locator.size() && ReadAt(DirectoryEnd - Locator.size(), Locator.data(), Locator.size()) && Number(Locator.data(), 4) == 0x07064b50;
			if (HasZip64 || DirectorySize == 0xffffffff || DirectoryOffset == 0xffffffff)
			{
				xr_array<u8, 56> Zip64{};
				if (!HasZip64 || DirectoryEnd < Locator.size() + Zip64.size() || Number(Locator.data() + 4, 4) || Number(Locator.data() + 16, 4) != 1)
				{
					break;
				}
				const auto Zip64Offset = Number(Locator.data() + 8, 8);
				if (!ReadAt(Zip64Offset, Zip64.data(), Zip64.size()) || Number(Zip64.data(), 4) != 0x06064b50 || Number(Zip64.data() + 4, 8) < 44 ||
					Zip64Offset > DirectoryEnd - Locator.size() - Zip64.size() || Number(Zip64.data() + 4, 8) > DirectoryEnd - Locator.size() - Zip64Offset - 12 ||
					Number(Zip64.data() + 16, 4) || Number(Zip64.data() + 20, 4) || Number(Zip64.data() + 24, 8) != Number(Zip64.data() + 32, 8))
				{
					break;
				}
				Entries = Number(Zip64.data() + 32, 8);
				DirectorySize = Number(Zip64.data() + 40, 8);
				DirectoryOffset = Number(Zip64.data() + 48, 8);
				DirectoryEnd = Zip64Offset;
			}
			// Check actual records before libarchive allocates its ZIP directory index.
			if (Entries <= MaxEntries && DirectorySize <= MaxDirectorySize && DirectoryOffset <= DirectoryEnd && DirectorySize <= DirectoryEnd - DirectoryOffset &&
				(!HasZip64 || DirectoryOffset == DirectoryEnd - DirectorySize) && ValidateZipDirectory(DirectoryEnd - DirectorySize, DirectorySize, Entries))
			{
				IsEmptyZip = Entries == 0 && DirectorySize == 0 && DirectoryOffset == 0 && DirectoryEnd == 0;
				// Hide the comment so the decoder uses the same validated end record.
				Size = EndOffset + 22;
				return true;
			}
			break;
		}
		archive_set_error(Handle, EINVAL, "Invalid, multi-volume or oversized ZIP directory.");
		return false;
	}
	static la_ssize_t Read(archive* Handle, void* Data, const void** Buffer)
	{
		auto& Self = *static_cast<ArchiveReader*>(Data);
		if (Self.Stop.stop_requested())
		{
			archive_set_error(Handle, ECANCELED, "Operation cancelled.");
			return -1;
		}
		const auto Available = std::min(static_cast<u64>(Self.Buffer.size()), Self.Size - Self.Position);
		Self.Stream.read(Self.Buffer.data(), static_cast<std::streamsize>(Available));
		const auto Count = Self.Stream.gcount();
		if (Self.Stream.bad())
		{
			archive_set_error(Handle, EIO, "Cannot read archive data.");
			return -1;
		}
		Self.Position += static_cast<u64>(Count);
		*Buffer = Self.Buffer.data();
		return static_cast<la_ssize_t>(Count);
	}
	static la_int64_t Seek(archive* Handle, void* Data, la_int64_t Offset, int Origin)
	{
		auto& Self = *static_cast<ArchiveReader*>(Data);
		if (Self.Stop.stop_requested())
		{
			archive_set_error(Handle, ECANCELED, "Operation cancelled.");
			return -1;
		}
		if (Origin != SEEK_SET && Origin != SEEK_CUR && Origin != SEEK_END)
		{
			archive_set_error(Handle, EINVAL, "Invalid archive seek origin.");
			return -1;
		}
		const u64 Base = Origin == SEEK_CUR ? Self.Position : Origin == SEEK_END ? Self.Size
																				 : 0;
		u64 Target;
		if (Offset < 0)
		{
			const u64 Back = static_cast<u64>(-(Offset + 1)) + 1;
			if (Back > Base)
			{
				archive_set_error(Handle, EINVAL, "Archive seek exceeds its bounds.");
				return -1;
			}
			Target = Base - Back;
		}
		else
		{
			if (Base > Self.Size || static_cast<u64>(Offset) > Self.Size - Base)
			{
				archive_set_error(Handle, EINVAL, "Archive seek exceeds its bounds.");
				return -1;
			}
			Target = Base + static_cast<u64>(Offset);
		}
		Self.Stream.clear();
		Self.Stream.seekg(static_cast<std::streamoff>(Target));
		if (!Self.Stream)
		{
			archive_set_error(Handle, EIO, "Cannot seek in archive data.");
			return -1;
		}
		Self.Position = Target;
		return static_cast<la_int64_t>(Target);
	}
	static la_int64_t Skip(archive* Handle, void* Data, la_int64_t Count)
	{
		auto& Self = *static_cast<ArchiveReader*>(Data);
		if (Count <= 0)
		{
			return 0;
		}
		const auto Size = std::min(static_cast<u64>(Count), Self.Size - Self.Position);
		return Seek(Handle, Data, static_cast<la_int64_t>(Size), SEEK_CUR) < 0 ? -1 : static_cast<la_int64_t>(Size);
	}
	std::ifstream Stream;
	u64 Size;
	u64 Position = 0;
	std::stop_token Stop;
	archive* Handle = nullptr;
	bool IsSevenZip;
	bool IsEmptyZip = false;
	xr_array<char, 65536> Buffer{};
};

static bool ReadArchiveSize(const xr_path& Path, u64 Size, PackageProgress& State, xr_string& Error, std::stop_token Stop)
{
	ArchiveReader Reader(Path, Size, Stop);
	if (!Reader.Open())
	{
		return Reader.Fail(Error);
	}
	u32 Count = 0;
	// Listing headers skips solid streams without decoding the payload.
	for (;;)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Operation cancelled.");
		}
		archive_entry* Entry = nullptr;
		const auto Status = archive_read_next_header(Reader.Get(), &Entry);
		if (Status == ARCHIVE_EOF)
		{
			return archive_read_close(Reader.Get()) == ARCHIVE_OK || Reader.Fail(Error);
		}
		if (Status != ARCHIVE_OK || !Entry)
		{
			return Reader.Fail(Error);
		}
		if (++Count > MaxEntries)
		{
			return Details::Fail(Error, "Package archive contains more than 100000 entries.");
		}
		if (!archive_entry_size_is_set(Entry) || archive_entry_size(Entry) < 0)
		{
			return Details::Fail(Error, "Archive entry has an invalid size.");
		}
		const auto FileSize = static_cast<u64>(archive_entry_size(Entry));
		if (FileSize > MaxFileSize || FileSize > MaxExpandedSize - State.BytesTotal)
		{
			return Details::Fail(Error, "Archive exceeds the 2 GiB per file or 16 GiB expanded size limit.");
		}
		State.BytesTotal += FileSize;
		State.FilesTotal += archive_entry_filetype(Entry) == AE_IFREG;
	}
}

bool ExtractArchive(const xr_path& Path, const xr_path& Target, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress)
{
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Operation cancelled.");
	}
	std::error_code Code;
	const auto Size = std::filesystem::file_size(Path, Code);
	if (Code || Size > MaxExpandedSize)
	{
		return Details::Fail(Error, "Package archive is inaccessible or exceeds the archive size limit.");
	}
	PackageProgress State{"Extracting package archive"};
	if (!ReadArchiveSize(Path, Size, State, Error, Stop))
	{
		return false;
	}
	if (Progress)
	{
		Progress(State);
	}
	ArchiveReader Reader(Path, Size, Stop);
	if (!Reader.Open())
	{
		return Reader.Fail(Error);
	}
	if (std::filesystem::exists(Target, Code) || Code)
	{
		return Details::Fail(Error, "Archive extraction requires a fresh staging directory.");
	}
	std::filesystem::create_directories(Target, Code);
	if (Code)
	{
		return Details::Fail(Error, "Cannot create archive staging directory.");
	}
	u64 Expanded = 0;
	u32 Count = 0;
	xr_hash_set<xr_string> Names;
	xr_array<char, 65536> Buffer;
	for (;;)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Operation cancelled.");
		}
		archive_entry* Entry = nullptr;
		const auto Status = archive_read_next_header(Reader.Get(), &Entry);
		if (Status == ARCHIVE_EOF)
		{
			if (State.BytesCompleted != State.BytesTotal || State.FilesCompleted != State.FilesTotal)
			{
				return Details::Fail(Error, "Archive contents changed while extracting. Retry with an unchanged archive.");
			}
			return archive_read_close(Reader.Get()) == ARCHIVE_OK || Reader.Fail(Error);
		}
		if (Status != ARCHIVE_OK || !Entry)
		{
			return Reader.Fail(Error);
		}
		if (++Count > MaxEntries)
		{
			return Details::Fail(Error, "Package archive contains more than 100000 entries.");
		}
		if (archive_entry_is_encrypted(Entry) > 0)
		{
			return Details::Fail(Error, "Encrypted archive packages are not supported.");
		}
		const auto Type = archive_entry_filetype(Entry);
		if ((Type != AE_IFREG && Type != AE_IFDIR) || archive_entry_symlink(Entry) || archive_entry_hardlink(Entry))
		{
			return Details::Fail(Error, "Archive links and special file types are not supported.");
		}
		const char* Text = archive_entry_pathname_utf8(Entry);
		size_t Length = 0;
		while (Text && Length <= 4096 && Text[Length])
		{
			++Length;
		}
		if (!Text || Length == 0 || Length > 4096)
		{
			return Details::Fail(Error, "Archive entry name is invalid, empty or too long.");
		}
		xr_string Name;
		Name.assign(Text, Length);
		std::replace(Name.begin(), Name.end(), '\\', '/');
		if (Name.ends_with('/'))
		{
			Name.pop_back();
		}
		if (!SafeName(Name) || !Names.insert(Details::Lower(Name)).second)
		{
			return Details::Fail(Error, "Package archive contains unsafe or duplicate paths.");
		}
		if (!archive_entry_size_is_set(Entry) || archive_entry_size(Entry) < 0)
		{
			return Details::Fail(Error, "Archive entry has an invalid size.");
		}
		const auto FileSize = static_cast<u64>(archive_entry_size(Entry));
		if (FileSize > MaxFileSize || FileSize > MaxExpandedSize - Expanded || (Type == AE_IFDIR && FileSize != 0))
		{
			return Details::Fail(Error, "Archive exceeds the 2 GiB per file or 16 GiB expanded size limit.");
		}
		Expanded += FileSize;
		const auto Destination = Join(Target, Name);
		if (!Details::IsWithin(Destination, Target))
		{
			return Details::Fail(Error, "Archive path escapes the staging directory.");
		}
		if (Type == AE_IFDIR)
		{
			std::filesystem::create_directories(Destination, Code);
			if (Code)
			{
				return Details::Fail(Error, "Cannot create archive output directory.");
			}
			continue;
		}
		std::filesystem::create_directories(Destination.parent_path(), Code);
		if (Code)
		{
			return Details::Fail(Error, "Archive entries overlap or output directory is inaccessible.");
		}
		std::ofstream Output(Destination, std::ios::binary | std::ios::out | std::ios::noreplace);
		if (!Output)
		{
			return Details::Fail(Error, "Archive entries overlap or alias an existing path.");
		}
		State.Detail = Name;
		if (Progress)
		{
			Progress(State);
		}
		u64 Written = 0;
		for (;;)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Operation cancelled.");
			}
			const auto Bytes = archive_read_data(Reader.Get(), Buffer.data(), Buffer.size());
			if (Bytes < 0)
			{
				return Reader.Fail(Error);
			}
			if (!Bytes)
			{
				break;
			}
			if (static_cast<u64>(Bytes) > FileSize - Written)
			{
				return Details::Fail(Error, "Archive entry exceeds its declared size.");
			}
			if (static_cast<u64>(Bytes) > State.BytesTotal - State.BytesCompleted)
			{
				return Details::Fail(Error, "Archive contents changed while extracting. Retry with an unchanged archive.");
			}
			Output.write(Buffer.data(), Bytes);
			if (!Output)
			{
				return Details::Fail(Error, "Cannot write extracted archive data.");
			}
			Written += static_cast<u64>(Bytes);
			State.BytesCompleted += static_cast<u64>(Bytes);
			if (Progress)
			{
				Progress(State);
			}
		}
		Output.close();
		if (!Output || Written != FileSize)
		{
			return Details::Fail(Error, "Archive extraction failed: truncated data or output write error.");
		}
		++State.FilesCompleted;
		if (Progress)
		{
			Progress(State);
		}
	}
}
} // namespace SdkPackages::LocalSource
