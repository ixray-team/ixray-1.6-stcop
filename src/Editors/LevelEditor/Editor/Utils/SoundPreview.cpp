#include "stdafx.h"
#include "SoundPreview.h"

#include "../xrCore/FS_internal.h"

#include <vorbis/vorbisfile.h>

namespace
{
	constexpr u32 SoundCacheMagic = 0x31484353; // "SCH1"
	constexpr u32 SoundCacheVersion = 1;

	size_t ReaderRead(void* Ptr, size_t ElementSize, size_t Count, void* Source)
	{
		IReader* Reader = static_cast<IReader*>(Source);
		const size_t Bytes = std::min<size_t>(ElementSize * Count, (size_t)Reader->elapsed());

		Reader->r(Ptr, (intptr_t)Bytes);
		return ElementSize ? Bytes / ElementSize : 0;
	}

	int ReaderSeek(void* Source, ogg_int64_t Offset, int Whence)
	{
		IReader* Reader = static_cast<IReader*>(Source);
		intptr_t Base = 0;

		switch (Whence)
		{
		case SEEK_SET: Base = 0; break;
		case SEEK_CUR: Base = Reader->tell(); break;
		case SEEK_END: Base = Reader->length(); break;
		default: return -1;
		}

		const intptr_t NewPosition = Base + (intptr_t)Offset;
		if (NewPosition < 0 || NewPosition > Reader->length())
		{
			return -1;
		}

		Reader->seek(NewPosition);
		return 0;
	}

	long ReaderTell(void* Source)
	{
		return (long)static_cast<IReader*>(Source)->tell();
	}

	bool FileExists(const xr_path& File)
	{
		std::error_code Error;
		return std::filesystem::is_regular_file(File, Error);
	}
}

CSoundPreviewCache::~CSoundPreviewCache()
{
	Tasks.wait();
}

void CSoundPreviewCache::Initialize()
{
	string_path Path = {};

	FS.update_path(Path, "$game_data$", "");
	GameDataRoot = NormalizePath(Path);

	FS.update_path(Path, "$server_data_root$", "");
	RawDataRoot = NormalizePath(Path);

	FS.update_path(Path, _game_sounds_, "");
	GameSoundsRoot = NormalizePath(Path);
}

void CSoundPreviewCache::Clear()
{
	Tasks.wait();

	xrCriticalSectionGuard Guard(Lock);
	Entries.clear();
}

xr_string CSoundPreviewCache::NormalizePath(const xr_path& Path)
{
	xr_string Result = xr_path(Path.lexically_normal().make_preferred()).xstring();
	xr_strlwr(Result);
	return Result;
}

bool CSoundPreviewCache::IsPreviewable(const xr_path& File)
{
	xr_string Extension = xr_path(File.extension()).xstring();
	xr_strlwr(Extension);
	return Extension == ".ogg";
}

xr_path CSoundPreviewCache::GetCachePath(const xr_path& SoundFile) const
{
	const xr_string Source = NormalizePath(SoundFile);
	if (GameDataRoot.empty() || RawDataRoot.empty() || !Source.starts_with(GameDataRoot))
	{
		return {};
	}

	xr_path Result = (RawDataRoot + Source.substr(GameDataRoot.length())).c_str();
	Result.replace_extension(".sch");
	return Result;
}

bool CSoundPreviewCache::GetGameSoundName(const xr_path& SoundFile, xr_string& OutName) const
{
	const xr_string Source = NormalizePath(SoundFile);
	if (GameSoundsRoot.empty() || !Source.starts_with(GameSoundsRoot))
	{
		return false;
	}

	xr_path Name = Source.substr(GameSoundsRoot.length()).c_str();
	Name.replace_extension();
	OutName = Name.xstring();
	return !OutName.empty();
}

bool CSoundPreviewCache::GetSourceStamp(const xr_path& SoundFile, u64& OutSize, s64& OutTime)
{
	std::error_code Error;

	OutSize = (u64)std::filesystem::file_size(SoundFile, Error);
	if (Error)
	{
		return false;
	}

	OutTime = (s64)std::filesystem::last_write_time(SoundFile, Error).time_since_epoch().count();
	return !Error;
}

const CSoundPreviewCache::Preview* CSoundPreviewCache::Get(const xr_path& SoundFile)
{
	const xr_string Key = NormalizePath(SoundFile);
	Entry* Item = nullptr;

	{
		xrCriticalSectionGuard Guard(Lock);

		auto Found = Entries.find(Key);
		if (Found != Entries.end())
		{
			Item = Found->second.get();
			return Item->State == EState::Ready ? &Item->Data : nullptr;
		}

		Item = Entries.emplace(Key, xr_make_unique<Entry>()).first->second.get();
	}

	const xr_path CacheFile = GetCachePath(SoundFile);
	const xr_path SourceFile = SoundFile;

	Tasks.run([Item, CacheFile, SourceFile]()
	{
		u64 SourceSize = 0;
		s64 SourceTime = 0;

		if (!GetSourceStamp(SourceFile, SourceSize, SourceTime))
		{
			Item->State = EState::Failed;
			return;
		}

		Item->SourceSize = SourceSize;
		Item->SourceTime = SourceTime;

		if (!CacheFile.empty() && LoadCache(CacheFile, SourceSize, SourceTime, Item->Data))
		{
			Item->State = EState::Ready;
			return;
		}

		if (!Bake(SourceFile, Item->Data))
		{
			Msg("! Can't bake sound preview: %s", SourceFile.xstring().c_str());
			Item->State = EState::Failed;
			return;
		}

		if (!CacheFile.empty())
		{
			SaveCache(CacheFile, SourceSize, SourceTime, Item->Data);
		}

		Item->State = EState::Ready;
	});

	return nullptr;
}

void CSoundPreviewCache::Refresh(const xr_path& SoundFile)
{
	const xr_string Key = NormalizePath(SoundFile);

	xrCriticalSectionGuard Guard(Lock);

	auto Found = Entries.find(Key);
	if (Found == Entries.end() || Found->second->State == EState::Pending)
	{
		return;
	}

	u64 SourceSize = 0;
	s64 SourceTime = 0;
	if (!GetSourceStamp(SoundFile, SourceSize, SourceTime) || SourceSize != Found->second->SourceSize || SourceTime != Found->second->SourceTime)
	{
		Entries.erase(Found);
	}
}

bool CSoundPreviewCache::LoadCache(const xr_path& CacheFile, u64 SourceSize, s64 SourceTime, Preview& Out)
{
	if (!FileExists(CacheFile))
	{
		return false;
	}

	CFileReader Reader(CacheFile.xstring().c_str());

	constexpr intptr_t HeaderSize = sizeof(u32) * 2 + sizeof(u64) + sizeof(s64) + sizeof(float) + sizeof(u32) + sizeof(u16) * 2 + sizeof(u32);
	if (Reader.length() < HeaderSize)
	{
		return false;
	}

	if (Reader.r_u32() != SoundCacheMagic || Reader.r_u32() != SoundCacheVersion || Reader.r_u64() != SourceSize || Reader.r_s64() != SourceTime)
	{
		return false;
	}

	Out.Duration = Reader.r_float();
	Out.SampleRate = Reader.r_u32();
	Out.Channels = Reader.r_u16();
	Out.Lanes = Reader.r_u16();

	if (Reader.r_u32() != PeakCount || Out.Lanes == 0 || Out.Lanes > MaxLanes)
	{
		return false;
	}

	Out.Peaks.resize((size_t)Out.Lanes * PeakCount);

	const intptr_t PeaksSize = intptr_t(Out.Peaks.size() * sizeof(PeakRange));
	if (Reader.elapsed() < PeaksSize)
	{
		return false;
	}

	Reader.r(Out.Peaks.data(), PeaksSize);
	return true;
}

void CSoundPreviewCache::SaveCache(const xr_path& CacheFile, u64 SourceSize, s64 SourceTime, const Preview& Data)
{
	IWriter* Writer = FS.w_open(CacheFile.xstring().c_str());
	if (Writer == nullptr || !Writer->valid())
	{
		Msg("! Can't write sound preview cache: %s", CacheFile.xstring().c_str());
		FS.w_close(Writer);
		return;
	}

	Writer->w_u32(SoundCacheMagic);
	Writer->w_u32(SoundCacheVersion);
	Writer->w_u64(SourceSize);
	Writer->w_s64(SourceTime);
	Writer->w_float(Data.Duration);
	Writer->w_u32(Data.SampleRate);
	Writer->w_u16(Data.Channels);
	Writer->w_u16(Data.Lanes);
	Writer->w_u32(PeakCount);
	Writer->w(Data.Peaks.data(), u32(Data.Peaks.size() * sizeof(PeakRange)));

	FS.w_close(Writer);
}

bool CSoundPreviewCache::Bake(const xr_path& SoundFile, Preview& Out)
{
	if (!FileExists(SoundFile))
	{
		return false;
	}

	CFileReader Reader(SoundFile.xstring().c_str());
	if (Reader.length() <= 0)
	{
		return false;
	}

	ov_callbacks Callbacks = { ReaderRead, ReaderSeek, nullptr, ReaderTell };

	OggVorbis_File Vorbis = {};
	if (ov_open_callbacks(&Reader, &Vorbis, nullptr, 0, Callbacks) < 0)
	{
		return false;
	}

	const vorbis_info* Info = ov_info(&Vorbis, -1);
	const ogg_int64_t TotalFrames = ov_pcm_total(&Vorbis, -1);
	if (Info == nullptr || Info->channels <= 0 || TotalFrames <= 0)
	{
		ov_clear(&Vorbis);
		return false;
	}

	const u32 Channels = (u32)Info->channels;
	const u32 Lanes = Channels == 2 ? 2 : 1;

	Out.Duration = float((double)TotalFrames / (double)Info->rate);
	Out.SampleRate = (u32)Info->rate;
	Out.Channels = (u16)Channels;
	Out.Lanes = (u16)Lanes;
	Out.Peaks.assign((size_t)Lanes * PeakCount, PeakRange{});

	ogg_int64_t Frame = 0;
	int Section = 0;

	while (true)
	{
		float** Pcm = nullptr;
		const int Read = (int)ov_read_float(&Vorbis, &Pcm, 4096, &Section);

		if (Read == OV_HOLE)
		{
			continue;
		}

		if (Read <= 0)
		{
			break;
		}

		for (int Index = 0; Index < Read; Index++, Frame++)
		{
			const u32 Peak = (u32)std::min<ogg_int64_t>(Frame * PeakCount / TotalFrames, PeakCount - 1);

			for (u32 Lane = 0; Lane < Lanes; Lane++)
			{
				float Sample = 0.0f;
				if (Lanes == Channels)
				{
					Sample = Pcm[Lane][Index];
				}
				else
				{
					for (u32 Channel = 0; Channel < Channels; Channel++)
					{
						Sample += Pcm[Channel][Index];
					}
					Sample /= float(Channels);
				}

				PeakRange& Range = Out.Peaks[(size_t)Lane * PeakCount + Peak];
				Range.Min = std::min(Range.Min, Sample);
				Range.Max = std::max(Range.Max, Sample);
			}
		}
	}

	ov_clear(&Vorbis);

	for (PeakRange& Range : Out.Peaks)
	{
		Range.Min = std::clamp(Range.Min, -1.0f, 0.0f);
		Range.Max = std::clamp(Range.Max, 0.0f, 1.0f);
	}

	return true;
}
