#include "stdafx.h"
#include "LocalSource.h"
#include <miniz.h>
#include <limits>

namespace SdkPackages::LocalSource
{
static constexpr u64 MaxFileSize = 2ull * 1024 * 1024 * 1024;
static constexpr u64 MaxExpandedSize = 16ull * 1024 * 1024 * 1024;
static constexpr u32 MaxEntries = 100000;
static constexpr size_t MaxAllocation = 64 * 1024 * 1024;

class Reader final
{
public:
	explicit Reader(const xr_path& Path)
		: Stream(Path, std::ios::binary) {}
	~Reader()
	{
		if (IsOpen)
		{
			mz_zip_reader_end(&Handle);
		}
	}
	bool Open(u64 Size)
	{
		Handle.m_pIO_opaque = this;
		Handle.m_pRead = Read;
		Handle.m_pAlloc = Allocate;
		Handle.m_pRealloc = Reallocate;
		Handle.m_pFree = Free;
		IsOpen = Stream.is_open() && mz_zip_reader_init(&Handle, Size, 0);
		return IsOpen;
	}
	mz_zip_archive Handle{};

private:
	static void* Allocate(void*, size_t Count, size_t Size)
	{
		return Size && Count > MaxAllocation / Size ? nullptr : miniz_def_alloc_func(nullptr, Count, Size);
	}
	static void* Reallocate(void*, void* Address, size_t Count, size_t Size)
	{
		return Size && Count > MaxAllocation / Size ? nullptr : miniz_def_realloc_func(nullptr, Address, Count, Size);
	}
	static void Free(void*, void* Address) { mz_free(Address); }
	static size_t Read(void* Data, mz_uint64 Offset, void* Buffer, size_t Size)
	{
		auto& Self = *static_cast<Reader*>(Data);
		if (Offset > static_cast<u64>(std::numeric_limits<std::streamoff>::max()) || Size > static_cast<size_t>(std::numeric_limits<std::streamsize>::max()))
		{
			return 0;
		}
		Self.Stream.clear();
		Self.Stream.seekg(static_cast<std::streamoff>(Offset));
		Self.Stream.read(static_cast<char*>(Buffer), static_cast<std::streamsize>(Size));
		return static_cast<size_t>(Self.Stream.gcount());
	}
	std::ifstream Stream;
	bool IsOpen = false;
};

class Output final
{
public:
	Output(const xr_path& Path, u64 Size, std::stop_token Stop)
		: Stream(Path, std::ios::binary), Size(Size), Stop(Stop) {}
	~Output() = default;
	static size_t Write(void* Data, mz_uint64 Offset, const void* Buffer, size_t Count)
	{
		auto& Self = *static_cast<Output*>(Data);
		if (Self.Stop.stop_requested() || Offset != Self.Written || Count > Self.Size - Self.Written)
		{
			return 0;
		}
		Self.Stream.write(static_cast<const char*>(Buffer), Count);
		if (!Self.Stream.good())
		{
			return 0;
		}
		Self.Written += Count;
		return Count;
	}
	bool Finish()
	{
		Stream.close();
		return Stream.good() && Written == Size;
	}

private:
	std::ofstream Stream;
	u64 Size;
	u64 Written = 0;
	std::stop_token Stop;
};

struct Entry
{
	xr_path Path;
	u32 Index;
	u64 Size;
	bool IsDirectory;
};

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

static bool Entries(Reader& Zip, xr_vector<Entry>& Result, xr_string& Error, std::stop_token Stop)
{
	const u32 Count = mz_zip_reader_get_num_files(&Zip.Handle);
	if (Count > MaxEntries)
	{
		return Details::Fail(Error, "ZIP contains more than 100000 entries.");
	}
	xr_hash_set<xr_string> Names;
	u64 TotalSize = 0;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Operation cancelled.");
		}
		mz_zip_archive_file_stat Stat{};
		if (!mz_zip_reader_file_stat(&Zip.Handle, Index, &Stat) || !Stat.m_is_supported || Stat.m_is_encrypted)
		{
			return Details::Fail(Error, "ZIP contains an unsupported or encrypted entry.");
		}
		const u32 Mode = (Stat.m_external_attr >> 16) & 0170000;
		if ((Mode && Mode != 0100000 && Mode != 0040000) || (Stat.m_external_attr & 0x400))
		{
			return Details::Fail(Error, "ZIP links and special file types are not supported.");
		}
		const u32 Length = mz_zip_reader_get_filename(&Zip.Handle, Index, nullptr, 0);
		if (Length < 2 || Length > 4097)
		{
			return Details::Fail(Error, "ZIP entry name is empty or too long.");
		}
		xr_vector<char> Buffer(Length);
		if (mz_zip_reader_get_filename(&Zip.Handle, Index, Buffer.data(), Length) != Length || Buffer.back() != '\0' || xr_strlen(Buffer.data()) != Length - 1)
		{
			return Details::Fail(Error, "Invalid ZIP entry name.");
		}
		xr_string Name = Buffer.data();
		const bool IsDirectory = Stat.m_is_directory || Mode == 0040000;
		if (Name.ends_with('/'))
		{
			Name.pop_back();
		}
		if (!SafeName(Name) || !Names.insert(Details::Lower(Name)).second)
		{
			return Details::Fail(Error, "ZIP contains unsafe or duplicate paths.");
		}
		if (Stat.m_uncomp_size > MaxFileSize || Stat.m_uncomp_size > MaxExpandedSize - TotalSize)
		{
			return Details::Fail(Error, "ZIP exceeds the 2 GiB per file or 16 GiB expanded size limit.");
		}
		TotalSize += Stat.m_uncomp_size;
		Result.push_back({FromUtf8(Name), Index, Stat.m_uncomp_size, IsDirectory});
	}
	return true;
}

static bool Extract(const xr_path& Path, const xr_path& Target, xr_string& Error, std::stop_token Stop)
{
	std::error_code Code;
	const u64 Size = std::filesystem::file_size(Path, Code);
	if (Code || Size > MaxExpandedSize)
	{
		return Details::Fail(Error, "ZIP is inaccessible or exceeds the archive size limit.");
	}
	Reader Zip(Path);
	if (!Zip.Open(Size))
	{
		return Details::Fail(Error, "Cannot read ZIP archive. It may be invalid or too large.");
	}
	xr_vector<Entry> Files;
	if (!Entries(Zip, Files, Error, Stop))
	{
		return false;
	}
	if (std::filesystem::exists(Target, Code) || Code)
	{
		return Details::Fail(Error, "ZIP extraction requires a fresh staging directory.");
	}
	std::filesystem::create_directories(Target, Code);
	for (const auto& File : Files)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Operation cancelled.");
		}
		const auto Destination = Join(Target, File.Path);
		if (!Details::IsWithin(Destination, Target))
		{
			return Details::Fail(Error, "ZIP path escapes the staging directory.");
		}
		if (File.IsDirectory)
		{
			std::filesystem::create_directories(Destination, Code);
		}
		else
		{
			if (std::filesystem::exists(Destination, Code) || Code)
			{
				return Details::Fail(Error, "ZIP entries overlap or alias an existing path.");
			}
			std::filesystem::create_directories(Destination.parent_path(), Code);
			if (Code)
			{
				return Details::Fail(Error, "Cannot create ZIP output directory.");
			}
			Output FileOutput(Destination, File.Size, Stop);
			if (!mz_zip_reader_extract_to_callback(&Zip.Handle, File.Index, Output::Write, &FileOutput, 0) || !FileOutput.Finish())
			{
				return Details::Fail(Error, Stop.stop_requested() ? "Operation cancelled." : "ZIP extraction failed: corrupt data or output write error.");
			}
		}
		if (Code)
		{
			return Details::Fail(Error, "Cannot extract ZIP package.");
		}
	}
	return true;
}

static bool FindPackageRoot(xr_path& Checkout, xr_string& Error)
{
	std::error_code Code;
	if (std::filesystem::is_regular_file(Join(Checkout, "package.json"), Code))
	{
		return true;
	}
	xr_path Wrapper;
	xr_dir_iter Iter(Checkout, Code), End;
	for (; !Code && Iter != End; Iter.increment(Code))
	{
		const auto Name = Iter->path().filename();
		if (Name == "__MACOSX" || Name == ".DS_Store")
		{
			continue;
		}
		if (!Wrapper.empty() || !Iter->is_directory(Code))
		{
			return Details::Fail(Error, "ZIP must contain package.json at its root or inside one outer folder.");
		}
		Wrapper = Iter->path();
	}
	if (Code || Wrapper.empty() || !std::filesystem::is_regular_file(Join(Wrapper, "package.json"), Code) || Code)
	{
		return Details::Fail(Error, "ZIP package manifest package.json was not found.");
	}
	Checkout = Wrapper;
	return true;
}

bool Prepare(PackageInfo& Info, const xr_path& Stage, xr_path& Checkout, xr_string& Error, std::stop_token Stop)
{
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Operation cancelled.");
	}
	if (!Details::NormalizeLocalSource(Info, Error))
	{
		return false;
	}
	if (Info.Source == PackageSource::Folder)
	{
		Checkout = FromUtf8(Info.LocalPath);
		std::error_code Code;
		return std::filesystem::is_regular_file(Join(Checkout, "package.json"), Code) || Details::Fail(Error, "Select a folder containing package.json.");
	}
	Checkout = Join(Stage, "extracted");
	if (!Extract(FromUtf8(Info.LocalPath), Checkout, Error, Stop))
	{
		return false;
	}
	return FindPackageRoot(Checkout, Error);
}
} // namespace SdkPackages::LocalSource
