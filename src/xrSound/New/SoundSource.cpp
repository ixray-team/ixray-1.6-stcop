/**************************************************************************************
 * Copyright (C) 2026 Anton Kovalev (vertver)
 * New Sound Engine
 ***************************************************************************************
 * Source code is licensed under the following terms:
 *
 * 1. IX-Ray Team License
 *    Non-exclusive, royalty-free, perpetual license is hereby granted to:
 *      - ForserX   (https://github.com/ForserX)
 *      - Drombeys  (https://github.com/Drombeys)
 *      - v2v3v4    (https://github.com/v2v3v4)
 *
 *    Permitted rights:
 *      - Copy, modify, merge, publish and distribute this Software
 *        and its documentation.
 *
 * 2. Public Access License
 *    Non-exclusive, "access-view-study" rights granted to everyone else.
 *
 *    Permitted rights:
 *      - Private copying is allowed, provided that no distribution occurs.
 *      - Public cloning (i.e. "forking") is allowed, but any source code
 *        modification or binary redistribution is prohibited.
 *
 * Usage of this Software beyond the rights granted above is strictly prohibited.
 *
 * The above copyright notice and this license text must be included in all
 * copies or substantial portions of the Software.
 **************************************************************************************/
#include "SoundSource.h"
#include "ogg_utils.h"
#include "SoundBus.h"
#include "SoundConfig.h"
#include "../xrCore/xrAddons.h"

#define SND_CACHE_CHUNK_LINES (32)
#define SND_CACHE_LINE_WIDTH (12)
#define SND_CACHE_LINE_CAPACITY ((SND_BLOCKSIZE + 1) * SND_CACHE_LINE_WIDTH)
// A decoded line is guaranteed to cover at least this many frames after the requested position
#define SND_CACHE_LINE_MIN_AHEAD (SND_CACHE_LINE_CAPACITY / 2)
// How far ahead of the play position the decode thread is asked to stay
#define SND_CACHE_PREFETCH_FRAMES (SND_BLOCKSIZE * 8)
#define SND_CACHE_LINE_MAX_TIME_NS ((u64)SND_CACHE_PREFETCH_FRAMES * 2 * 1000000000 / SND_SAMPLERATE)
#define SND_DEFAULT_MAX_DISTANCE (300.0f)

struct SoundDecodeCommand
{
	xr_string Name;
	u32 Position = 0;
};

struct SoundCacheLine
{
	u32 Start;
	u32 End;
	u64 Timestamp;
	SoundSourceState* Owner;
	float Data[SND_CHANNEL_COUNT][SND_CACHE_LINE_CAPACITY];
};

struct SoundSourcePoolState
{
	xrSRWLock CacheLock;
	xrCriticalSection DecodeLock;
	xr_vector<SoundDecodeCommand> DecodeQueue;
	XRayWorkerThread* DecodeThread = nullptr;
	xr_vector<u32> FreeCacheLines;
	xr_vector<SoundCacheLine*> CacheChunks;
	xr_hash_map<xr_string, SoundSourceState> Sources;
};

struct OggCommentDecl
{
	u32 Version;
	u32 MinSize;
	bool HasVolume;
	bool HasAiDistance;
};

xrSRWLock g_SoundSourceLock;
sound_stats g_SoundStats = {};
static SoundSourcePoolState GSourcePool = {};

static const OggCommentDecl OggCommentDecls[] =
{
	{0x0001, 12, false, false},
	{0x0002, 16, true, false},
	{OGG_COMMENT_VERSION, 20, true, true}
};

static bool Snd_IsCacheLineValid(u32 CacheIdx)
{
	return CacheIdx != 0 && CacheIdx <= (u32)GSourcePool.CacheChunks.size() * SND_CACHE_CHUNK_LINES;
}

static SoundCacheLine* Snd_GetCacheLine(u32 CacheIdx)
{
	return &GSourcePool.CacheChunks[(CacheIdx - 1) / SND_CACHE_CHUNK_LINES][(CacheIdx - 1) % SND_CACHE_CHUNK_LINES];
}

static void Snd_GrowCacheLines()
{
	SoundCacheLine* Chunk = xr_alloc<SoundCacheLine>(SND_CACHE_CHUNK_LINES);
	u32 BaseIdx = (u32)GSourcePool.CacheChunks.size() * SND_CACHE_CHUNK_LINES;
	for (u32 LineIdx = 0; LineIdx < SND_CACHE_CHUNK_LINES; LineIdx++)
	{
		Chunk[LineIdx].Start = 0;
		Chunk[LineIdx].End = 0;
		Chunk[LineIdx].Timestamp = 0;
		Chunk[LineIdx].Owner = nullptr;
	}

	GSourcePool.CacheChunks.push_back(Chunk);
	GSourcePool.FreeCacheLines.reserve(GSourcePool.CacheChunks.size() * SND_CACHE_CHUNK_LINES);
	for (u32 LineIdx = 0; LineIdx < SND_CACHE_CHUNK_LINES; LineIdx++)
	{
		GSourcePool.FreeCacheLines.push_back(BaseIdx + LineIdx + 1);
	}

	SND_STAT_SET(g_SoundStats.cache_lines_total, (u32)GSourcePool.CacheChunks.size() * SND_CACHE_CHUNK_LINES);
	SND_STAT_SET(g_SoundStats.cache_lines_free, (u32)GSourcePool.FreeCacheLines.size());
}

static void Snd_PurgeCacheLine(u32 CacheIdx, bool IsPurgeFromEntry)
{
	// Callers hold CacheLock exclusively and the source lock when unlinking an owner.
	// Always acquire g_SoundSourceLock before GSourcePool.CacheLock.
	if (!Snd_IsCacheLineValid(CacheIdx))
	{
		return;
	}

	SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);
	if (IsPurgeFromEntry && Line->Owner != nullptr)
	{
		u32* Entries = Line->Owner->CacheLines;
		for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
		{
			if (Entries[EntryIdx] == CacheIdx)
			{
				Entries[EntryIdx] = 0;
				break;
			}
		}
	}

	Line->Owner = nullptr;
	Line->Start = 0;
	Line->End = 0;
	Line->Timestamp = 0;
	GSourcePool.FreeCacheLines.push_back(CacheIdx);
	SND_STAT_SET(g_SoundStats.cache_lines_free, (u32)GSourcePool.FreeCacheLines.size());
}

static u32 Snd_NewCacheLine()
{
	if (GSourcePool.FreeCacheLines.empty())
	{
		u32 OldestIdx = 0;
		u64 OldestTimestamp = (u64)-1;
		u32 LineCount = (u32)GSourcePool.CacheChunks.size() * SND_CACHE_CHUNK_LINES;
		for (u32 LineIdx = 1; LineIdx <= LineCount; LineIdx++)
		{
			u64 Timestamp = Snd_GetCacheLine(LineIdx)->Timestamp;
			if (Timestamp != 0 && Timestamp < OldestTimestamp)
			{
				OldestTimestamp = Timestamp;
				OldestIdx = LineIdx;
			}
		}

		bool IsFull = (u64)(LineCount + SND_CACHE_CHUNK_LINES) * sizeof(SoundCacheLine) > (u64)psSoundCacheSizeMB * 1024 * 1024;
		if (OldestIdx != 0 && (IsFull || Snd_GetTimestamp() - OldestTimestamp >= SND_CACHE_LINE_MAX_TIME_NS))
		{
			Snd_PurgeCacheLine(OldestIdx, true);
		}
		else if (!IsFull)
		{
			Snd_GrowCacheLines();
		}
	}

	if (GSourcePool.FreeCacheLines.empty())
	{
		return 0;
	}

	u32 CacheIdx = GSourcePool.FreeCacheLines.back();
	GSourcePool.FreeCacheLines.pop_back();
	// Timestamp stays 0 while the line is being filled outside the lock, so eviction never picks it
	Snd_GetCacheLine(CacheIdx)->Timestamp = 0;
	SND_STAT_SET(g_SoundStats.cache_lines_free, (u32)GSourcePool.FreeCacheLines.size());
	return CacheIdx;
}

// MinFrames: how many frames starting at Position the line must hold (clamped to the end of the source)
static u32 Snd_FindCacheLine(const SoundSourceState* Source, u32 Position, u32 MinFrames)
{
	PROF_EVENT("Sound: FindCacheLine");
	if (Position >= Source->Desc.frames_total)
	{
		return 0;
	}

	u32 NeededFrames = std::clamp(MinFrames, 1u, Source->Desc.frames_total - Position);
	for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
	{
		u32 CacheIdx = Source->CacheLines[EntryIdx];
		if (!Snd_IsCacheLineValid(CacheIdx))
		{
			continue;
		}

		const SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);
		if (Line->End > Line->Start && Position >= Line->Start && Position + NeededFrames <= Line->End)
		{
			SND_STAT_ADD(g_SoundStats.cache_hit_count, 1u);
			return CacheIdx;
		}
	}

	return 0;
}

static u32 Snd_SeekSource(SoundSourceState* Source, u32 Position, bool IsPrecise)
{
	if (ov_pcm_tell(&Source->File) != Position)
	{
		if (IsPrecise)
		{
			ov_pcm_seek(&Source->File, Position);
		}
		else
		{
			ov_pcm_seek_page(&Source->File, Position);
		}
	}

	return (u32)ov_pcm_tell(&Source->File);
}

static u32 Snd_ReadFromSource(SoundSourceState* Source, float** OutBuffer, u32 Frames)
{
	PROF_EVENT("Sound: Decode Vorbis");
	if (Source->File.datasource == nullptr)
	{
		return 0;
	}

	float** Pcm = nullptr;
	int Section = 0;
	u32 Remaining = Frames;
	u32 Offset = 0;
	u32 ChannelCount = std::min((u32)SND_CHANNEL_COUNT, (u32)Source->Desc.channels_count);

	while (Remaining)
	{
		int Status = ov_read_float(&Source->File, &Pcm, Remaining, &Section);
		if (Status == OV_HOLE)
		{
			// Recoverable gap in the stream, libvorbis resyncs on the next call
			continue;
		}

		if (Status <= 0)
		{
			break;
		}
		Remaining -= Status;

		for (u32 Channel = 0; Channel < ChannelCount; Channel++)
		{
			for (int Frame = 0; Frame < Status; Frame++)
			{
				OutBuffer[Channel][Offset + Frame] = std::clamp(Pcm[Channel][Frame], -1.0f, 1.0f);
			}
		}

		Offset += Status;
	}

	if (Source->Desc.channels_count == 1)
	{
		memcpy(OutBuffer[1], OutBuffer[0], Frames * sizeof(float));
	}

	return Frames - Remaining;
}

// Returns true when a line covering Position is in the cache after the call.
// Can be called from the decode thread and from the render thread (on-the-fly decode).
static bool Snd_UpdateCache(SoundSourceState* Source, u32 Position)
{
	PROF_EVENT("Sound: Update Slot Cache");

	// Cache eviction modifies other owners; pin their map entries for the entire update.
	// Lock order: g_SoundSourceLock -> Source->DecodeLock -> GSourcePool.CacheLock
	xrSRWLockGuard SourceGuard(g_SoundSourceLock, true);
	if (Position >= Source->Desc.frames_total)
	{
		return false;
	}

	// Only one thread decodes a given source at a time
	xrCriticalSectionGuard DecodeGuard(Source->DecodeLock);

	u32 NewIdx = 0;
	SoundCacheLine* Line = nullptr;
	{
		xrSRWLockGuard Guard(GSourcePool.CacheLock, false);

		// The other thread may have decoded this line while we were waiting on DecodeLock
		if (Snd_FindCacheLine(Source, Position, SND_BLOCKSIZE) != 0)
		{
			return true;
		}

		if (Source->File.datasource == nullptr)
		{
			return false;
		}

		SND_STAT_ADD(g_SoundStats.cache_miss_count, 1u);
		NewIdx = Snd_NewCacheLine();
		if (!Snd_IsCacheLineValid(NewIdx))
		{
			return false;
		}

		Line = Snd_GetCacheLine(NewIdx);
	}

	// Page seek is cheap but can land far before Position. Fall back to precise seek
	// if the line wouldn't cover enough frames after Position.
	u32 BeginPos = Snd_SeekSource(Source, Position, false);
	if (BeginPos > Position || Position - BeginPos > SND_CACHE_LINE_CAPACITY - SND_CACHE_LINE_MIN_AHEAD)
	{
		BeginPos = Snd_SeekSource(Source, Position, true);
	}

	memset(Line->Data, 0, sizeof(Line->Data));

	float* ChannelData[SND_CHANNEL_COUNT];
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		ChannelData[Channel] = Line->Data[Channel];
	}

	u32 EndPos = BeginPos + Snd_ReadFromSource(Source, ChannelData, SND_CACHE_LINE_CAPACITY);

	xrSRWLockGuard Guard(GSourcePool.CacheLock, false);
	if (BeginPos > Position || EndPos <= Position)
	{
		// Nothing usable decoded (broken stream or frames_total overestimates), give the line back
		Snd_PurgeCacheLine(NewIdx, false);
		return false;
	}

	Line->Owner = Source;
	Line->Start = BeginPos;
	Line->End = EndPos;
	Line->Timestamp = Snd_GetTimestamp();

	u32* OldestEntry = nullptr;
	u64 OldestTimestamp = (u64)-1;
	for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
	{
		u32* Entry = &Source->CacheLines[EntryIdx];
		if (*Entry == 0 || *Entry == NewIdx)
		{
			*Entry = NewIdx;
			return true;
		}

		if (Snd_IsCacheLineValid(*Entry) && Snd_GetCacheLine(*Entry)->Timestamp < OldestTimestamp)
		{
			OldestTimestamp = Snd_GetCacheLine(*Entry)->Timestamp;
			OldestEntry = Entry;
		}
	}

	if (OldestEntry != nullptr)
	{
		Snd_PurgeCacheLine(*OldestEntry, false);
		*OldestEntry = NewIdx;
		return true;
	}

	// No entry to attach the line to
	Snd_PurgeCacheLine(NewIdx, false);
	return false;
}

static bool Snd_ParseOggComment(OggVorbis_File* File, sound_source_desc* Desc)
{
	bool IsParsed = false;
	vorbis_comment* Comment = File != nullptr ? ov_comment(File, -1) : nullptr;
	for (int CommentIdx = 0; Comment != nullptr && !IsParsed && CommentIdx < Comment->comments; CommentIdx++)
	{
		int CommentLength = Comment->comment_lengths[CommentIdx];
		if (Comment->user_comments[CommentIdx] == nullptr || CommentLength < 16)
		{
			continue;
		}

		IReader Reader(Comment->user_comments[CommentIdx], CommentLength);
		u32 Version = Reader.r_u32();
		for (u32 DeclIdx = 0; DeclIdx < sizeof(OggCommentDecls) / sizeof(OggCommentDecls[0]); DeclIdx++)
		{
			const OggCommentDecl* Decl = &OggCommentDecls[DeclIdx];
			if (Version != Decl->Version || Reader.elapsed() < (int)Decl->MinSize)
			{
				continue;
			}

			Desc->min_distance = Reader.r_float();
			Desc->max_distance = Reader.r_float();
			Desc->volume = Decl->HasVolume ? Reader.r_float() : 1.0f;
			Desc->game_type = (u16)Reader.r_u32();
			Desc->max_ai_distance = Decl->HasAiDistance ? Reader.r_float() : SND_DEFAULT_MAX_DISTANCE;
			IsParsed = true;
			break;
		}
	}

	return IsParsed;
}

static void Snd_ApplySoundConfig(const SoundConfigSection* Section, sound_source_desc* Desc)
{
	for (const CInifile::Item& Item : Section->Items)
	{
		const char* Key = Item.first.c_str();
		const char* Value = Item.second.size() ? Item.second.c_str() : "";
		if (xr_strcmp(Key, "volume") == 0)
		{
			Desc->volume = (float)atof(Value);
		}
		else if (xr_strcmp(Key, "min_distance") == 0)
		{
			Desc->min_distance = (float)atof(Value);
		}
		else if (xr_strcmp(Key, "max_distance") == 0)
		{
			Desc->max_distance = (float)atof(Value);
		}
		else if (xr_strcmp(Key, "ai_distance") == 0)
		{
			Desc->max_ai_distance = (float)atof(Value);
		}
		else if (xr_strcmp(Key, "ai_type") == 0)
		{
			Desc->game_type = (u16)strtoul(Value, nullptr, 0);
		}
		else if (xr_strcmp(Key, "bus") == 0)
		{
			Desc->bus = Snd_FindBus(Value);
		}
	}
}

static void Snd_ReadSoundDesc(OggVorbis_File* File, const char* Name, const SoundConfigSection* Section, sound_source_desc* Desc)
{
	Desc->volume = 1.0f;
	Desc->min_distance = 1.0f;
	Desc->max_distance = SND_DEFAULT_MAX_DISTANCE;
	Desc->max_ai_distance = SND_DEFAULT_MAX_DISTANCE;
	Desc->game_type = 0;
	Desc->bus = 0;

	if (Section != nullptr)
	{
		Snd_ApplySoundConfig(Section, Desc);
	}
	else if (!Snd_ParseOggComment(File, Desc))
	{
		Msg("~ Missing or invalid ogg-comment, file: %s", Name);
	}

	// Above 1 is headroom against distance attenuation; the final gain is clamped after it
	Desc->volume = Desc->volume >= 0.0f ? std::min(Desc->volume, 4.0f) : 0.0f;
	if (Desc->min_distance < EPS_S)
	{
		Desc->min_distance = 1.0f;
	}

	if (Desc->max_distance < Desc->min_distance)
	{
		Desc->max_distance = Desc->min_distance + 1.0f;
	}

	if (Desc->max_ai_distance < EPS_S)
	{
		Desc->max_ai_distance = Desc->max_distance;
	}
}

static const CLocatorAPI::file* Snd_ResolveSoundPath(const char* Name, string_path& Path)
{
	const CLocatorAPI::file* File = FS.path_exist("$level$") ? FS.exist(Path, "$level$", Name, ".ogg") : nullptr;
	return File != nullptr ? File : FS.exist(Path, _game_sounds_, Name, ".ogg");
}

static void Snd_LoadSourceFile(SoundSourceState* Source, const char* Name)
{
	string_path Path, BaseName;
	xr_strcpy(BaseName, Name);
	xr_strlwr(BaseName);

	char* Ext = strext(BaseName);
	if (Ext != nullptr)
	{
		*Ext = 0;
	}

	Source->Desc.name = BaseName;

	if (!Snd_ResolveSoundPath(BaseName, Path))
	{
		FS.update_path(Path, _game_sounds_, "$no_sound.ogg");
		Msg("! Can't find sound '%s'", Source->Desc.name.c_str());
	}

	Source->Desc.path = Path;

	IReader* WaveFile = FS.r_open(Path);
	R_ASSERT3(WaveFile && WaveFile->length(), "Can't open wave file:", Path);

	Source->Desc.data_size = WaveFile->length();
	Source->Data = xr_alloc<u8>(Source->Desc.data_size);
	WaveFile->r(Source->Data, WaveFile->length());
	WaveFile->close();
	Source->Reader = new IReader(Source->Data, Source->Desc.data_size);

	ov_callbacks Callbacks = {ov_read_func, ov_seek_func, ov_close_func, ov_tell_func};
	ov_open_callbacks(Source->Reader, &Source->File, nullptr, 0, Callbacks);

	vorbis_info* Info = ov_info(&Source->File, -1);
	R_ASSERT3(Info, "Invalid source info:", Source->Desc.name.c_str());
	R_ASSERT(Info->rate == SND_SAMPLERATE, "Invalid sample rate. Please, convert to 44100 Hz using converters like FFmpeg or foobar2000", Name);

	Source->Desc.channels_count = Info->channels;
	Source->Desc.frames_total = (u32)ov_pcm_total(&Source->File, -1);
	Snd_ReadSoundDesc(&Source->File, BaseName, Snd_FindConfig(BaseName), &Source->Desc);
}

static void Snd_UnloadSourceFile(SoundSourceState* Source)
{
	ov_clear(&Source->File);
	xr_delete(Source->Reader);
	xr_free(Source->Data);
}

SoundSourceState* Snd_LookupSource(const xr_string* Name)
{
	auto Found = GSourcePool.Sources.find(*Name);
	return (Found == GSourcePool.Sources.end() || !Found->second.IsReady) ? nullptr : &Found->second;
}

SoundSourceState* Snd_FindSource(const xr_string* Name)
{
	xrSRWLockGuard Guard(g_SoundSourceLock, true);
	SoundSourceState* Source = Snd_LookupSource(Name);
	if (Source != nullptr)
	{
		Source->Desc.ref_count++;
	}

	return Source;
}

SoundSourceState* Snd_AcquireSource(const xr_string* Name)
{
	SoundSourceState* Source = Snd_FindSource(Name);
	if (Source != nullptr || Name->empty())
	{
		return Source;
	}

	PROF_EVENT("Sound: Load ogg");
	bool IsLoader = false;
	while (!IsLoader)
	{
		{
			xrSRWLockGuard Guard(g_SoundSourceLock);
			Source = &GSourcePool.Sources[*Name];
			if (Source->IsReady)
			{
				Source->Desc.ref_count++;
				return Source;
			}

			if (!Source->IsLoading)
			{
				Source->IsLoading = true;
				IsLoader = true;
			}
		}

		if (!IsLoader)
		{
			std::this_thread::yield();
		}
	}

	Snd_LoadSourceFile(Source, Name->c_str());

	xrSRWLockGuard Guard(g_SoundSourceLock);
	Source->IsReady = true;
	Source->IsLoading = false;
	Source->Desc.ref_count++;
	return Source;
}

void Snd_ReleaseSource(const xr_string* Name)
{
	if (Name->empty())
	{
		return;
	}

	{
		xrSRWLockGuard Guard(g_SoundSourceLock, true);
		SoundSourceState* Source = Snd_LookupSource(Name);
		if (Source == nullptr)
		{
			return;
		}

		R_ASSERT(Source->Desc.ref_count);
		if (--Source->Desc.ref_count != 0)
		{
			return;
		}
	}

	xrSRWLockGuard Guard(g_SoundSourceLock);
	auto Found = GSourcePool.Sources.find(*Name);
	if (Found == GSourcePool.Sources.end() || Found->second.Desc.ref_count != 0)
	{
		return;
	}

	SoundSourceState* Source = &Found->second;
	Snd_UnloadSourceFile(Source);

	xrSRWLockGuard CacheGuard(GSourcePool.CacheLock, false);
	for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
	{
		u32 CacheIdx = Source->CacheLines[EntryIdx];
		if (Snd_IsCacheLineValid(CacheIdx) && Snd_GetCacheLine(CacheIdx)->Owner == Source)
		{
			Snd_PurgeCacheLine(CacheIdx, false);
		}

		Source->CacheLines[EntryIdx] = 0;
	}

	GSourcePool.Sources.erase(Found);
}

void Snd_QueueDecode(const xr_string* Name, u32 Position)
{
	if (Name->empty())
	{
		return;
	}

	{
		xrCriticalSectionGuard Guard(GSourcePool.DecodeLock);
		for (size_t RequestIdx = 0; RequestIdx < GSourcePool.DecodeQueue.size(); RequestIdx++)
		{
			// A queued request already covers this position (its line spans at least SND_CACHE_LINE_MIN_AHEAD frames)
			const SoundDecodeCommand* Queued = &GSourcePool.DecodeQueue[RequestIdx];
			if (Position >= Queued->Position && Position - Queued->Position + SND_BLOCKSIZE <= SND_CACHE_LINE_MIN_AHEAD && Queued->Name == *Name)
			{
				return;
			}
		}

		GSourcePool.DecodeQueue.push_back({*Name, Position});
	}

	// One Run() per queued command: the worker pops exactly one command per wakeup
	GSourcePool.DecodeThread->Run();
}

void Snd_PrefetchSource(const SoundSourceState* Source, const xr_string* Name, u32 Position, bool IsLooped)
{
	const u32 Total = Source->Desc.frames_total;
	if (Total == 0 || Name->empty())
	{
		return;
	}

	// Walk the chain of cached lines from the play position and queue the first frame that isn't
	// covered within the read-ahead window. Lines then come back to back without gaps, and the render
	// thread finds the next one ready by the time it crosses the boundary.
	u32 Missing = (u32)-1;
	{
		xrSRWLockGuard Guard(GSourcePool.CacheLock, true);

		u32 Cursor = Position;
		u32 Budget = SND_CACHE_PREFETCH_FRAMES;
		bool IsWrapped = false;
		while (true)
		{
			if (Cursor >= Total)
			{
				if (!IsLooped || IsWrapped)
				{
					break;
				}

				Cursor = 0;
				IsWrapped = true;
			}

			u32 CacheIdx = Snd_FindCacheLine(Source, Cursor, 1);
			if (!Snd_IsCacheLineValid(CacheIdx))
			{
				Missing = Cursor;
				break;
			}

			const SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);
			u32 Covered = Line->End - Cursor;
			if (Covered >= Budget)
			{
				break;
			}

			Budget -= Covered;
			Cursor = Line->End;
		}
	}

	if (Missing != (u32)-1)
	{
		Snd_QueueDecode(Name, Missing);
	}
}

bool Snd_DecodeNow(SoundSourceState* Source, u32 Position)
{
	PROF_EVENT("Sound: Decode On The Fly");
	return Snd_UpdateCache(Source, Position);
}

bool Snd_HasCacheLine(const SoundSourceState* Source, u32 Position)
{
	xrSRWLockGuard Guard(GSourcePool.CacheLock, true);
	return Snd_FindCacheLine(Source, Position, SND_BLOCKSIZE) != 0;
}

u32 Snd_CopyCached(const SoundSourceState* Source, u32 Position, float** OutData, u32 Frames)
{
	xrSRWLockGuard Guard(GSourcePool.CacheLock, true);

	// Any line holding Position will do: a partial tail is copied and the caller continues from the next line
	u32 CacheIdx = Snd_FindCacheLine(Source, Position, 1);
	if (!Snd_IsCacheLineValid(CacheIdx))
	{
		return 0;
	}

	SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);

	// LRU: lines in active use must not be the eviction candidates. Several readers may hold the shared lock
	xr_atomic_ref<u64>(Line->Timestamp).store(Snd_GetTimestamp(), std::memory_order_relaxed);
	if (Position < Line->Start || Line->End <= Line->Start || Position >= Line->End || Position - Line->Start >= SND_CACHE_LINE_CAPACITY)
	{
		return 0;
	}

	u32 BeginOffset = Position - Line->Start;
	u32 FramesCount = std::min(std::min(Frames, Line->End - Position), (u32)SND_CACHE_LINE_CAPACITY - BeginOffset);
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memcpy(OutData[Channel], &Line->Data[Channel][BeginOffset], FramesCount * sizeof(float));
	}

	return FramesCount;
}

static void Snd_DecodeNext()
{
	SoundDecodeCommand Request;
	{
		xrCriticalSectionGuard Guard(GSourcePool.DecodeLock);
		if (GSourcePool.DecodeQueue.empty())
		{
			return;
		}

		Request = GSourcePool.DecodeQueue.front();
		GSourcePool.DecodeQueue.erase(GSourcePool.DecodeQueue.begin());
	}

	PROF_EVENT("Decode OGG");
	SoundSourceState* Source = Snd_FindSource(&Request.Name);
	if (Source != nullptr)
	{
		Snd_UpdateCache(Source, Request.Position);
		Snd_ReleaseSource(&Request.Name);
	}
}

static void Snd_FreeCacheChunks()
{
	for (size_t ChunkIdx = 0; ChunkIdx < GSourcePool.CacheChunks.size(); ChunkIdx++)
	{
		xr_free(GSourcePool.CacheChunks[ChunkIdx]);
	}

	GSourcePool.CacheChunks.clear();
	GSourcePool.FreeCacheLines.clear();
}

void Snd_InitSources()
{
	Snd_FreeCacheChunks();
	Snd_GrowCacheLines();

	GSourcePool.DecodeThread = new XRayWorkerThread(Snd_DecodeNext, "Sound Decode Thread");
}

void Snd_ShutdownSources()
{
	xr_delete(GSourcePool.DecodeThread);

	GSourcePool.DecodeQueue.clear();
	for (auto& Source : GSourcePool.Sources)
	{
		memset(Source.second.CacheLines, 0, sizeof(Source.second.CacheLines));
	}

	Snd_FreeCacheChunks();
}

u32 XRay::Sound::Mixer::GetSourceCount()
{
	xrSRWLockGuard Guard(g_SoundSourceLock, true);
	return (u32)GSourcePool.Sources.size();
}

const sound_source_desc* XRay::Sound::Mixer::GetSource(u32 Index)
{
	xrSRWLockGuard Guard(g_SoundSourceLock, true);
	u32 ReadyIdx = 0;
	for (auto& Source : GSourcePool.Sources)
	{
		if (Source.second.IsReady && ReadyIdx++ == Index)
		{
			return &Source.second.Desc;
		}
	}

	return nullptr;
}

static shared_str Snd_GetSoundOrigin(const char* SoundPath)
{
	const CLocatorAPI::file* File = FS.exist(SoundPath);
	const CAddonManager::AddonInfo* Addon = (File != nullptr && GAddonsManager != nullptr) ? GAddonsManager->FindAddon(*File) : nullptr;

	string_path Name;
	xr_strconcat(Name, "sounds\\", Addon != nullptr ? Addon->AddonName.c_str() : "vanilla", ".ltx");
	return Name;
}

static CInifile* Snd_GetExportIni(xr_hash_map<shared_str, CInifile*>& Inis, const char* SoundPath)
{
	shared_str Origin = Snd_GetSoundOrigin(SoundPath);
	CInifile*& Ini = Inis[Origin];
	if (Ini == nullptr)
	{
		string_path Path;
		FS.update_path(Path, _game_config_, Origin.c_str());
		Ini = new CInifile(Path, false, true, false);
	}

	return Ini;
}

static void Snd_ReadSoundFile(const char* SoundPath, const char* Name, const SoundConfigSection* Section, sound_source_desc* Desc)
{
	IReader* Reader = FS.r_open(SoundPath);
	OggVorbis_File Ogg = {};
	ov_callbacks Callbacks = {ov_read_func, ov_seek_func, ov_close_func, ov_tell_func};
	bool IsOpened = Reader != nullptr && ov_open_callbacks(Reader, &Ogg, nullptr, 0, Callbacks) == 0;
	Snd_ReadSoundDesc(IsOpened ? &Ogg : nullptr, Name, Section, Desc);
	if (IsOpened)
	{
		ov_clear(&Ogg);
	}

	if (Reader != nullptr)
	{
		FS.r_close(Reader);
	}
}

static SoundSourceState* Snd_LookupSourceByName(const char* Name)
{
	for (auto& [Key, Source] : GSourcePool.Sources)
	{
		if (Source.IsReady && xr_strcmp(Source.Desc.name.c_str(), Name) == 0)
		{
			return &Source;
		}
	}

	return nullptr;
}

static void Snd_CopySoundConfig(const sound_source_desc* Desc, sound_config* Config)
{
	Config->volume = Desc->volume;
	Config->min_distance = Desc->min_distance;
	Config->max_distance = Desc->max_distance;
	Config->max_ai_distance = Desc->max_ai_distance;
	Config->game_type = Desc->game_type;
	Config->bus = Desc->bus;
}

bool Snd_GetSoundConfig(const char* Name, sound_config* Config)
{
	const SoundConfigSection* Section = Snd_FindConfig(Name);
	Config->file = Section != nullptr ? Section->File : shared_str();

	{
		xrSRWLockGuard Guard(g_SoundSourceLock, true);
		if (SoundSourceState* Source = Snd_LookupSourceByName(Name))
		{
			Snd_CopySoundConfig(&Source->Desc, Config);
			return true;
		}
	}

	string_path Path;
	if (!Snd_ResolveSoundPath(Name, Path))
	{
		return false;
	}

	sound_source_desc Desc = {};
	Snd_ReadSoundFile(Path, Name, Section, &Desc);
	Snd_CopySoundConfig(&Desc, Config);
	return true;
}

SoundSourceState* Snd_SetSoundConfig(const char* Name, sound_config* Config)
{
	SoundConfigSection* Section = Snd_EditConfig(Name);
	if (Section->File.size() == 0)
	{
		string_path Path;
		Snd_ResolveSoundPath(Name, Path);
		Section->File = Snd_GetSoundOrigin(Path);
	}

	const struct
	{
		const char* Key;
		float Value;
	} Floats[] = {{"volume", Config->volume}, {"min_distance", Config->min_distance}, {"max_distance", Config->max_distance}, {"ai_distance", Config->max_ai_distance}};

	string64 Value;
	for (const auto& Float : Floats)
	{
		xr_sprintf(Value, "%.3f", Float.Value);
		Snd_SetConfigValue(Section, Float.Key, Value);
	}

	xr_sprintf(Value, "%u", Config->game_type);
	Snd_SetConfigValue(Section, "ai_type", Value);

	SoundBus* Bus = Snd_GetBus(Config->bus);
	Snd_SetConfigValue(Section, "bus", Bus != nullptr ? Bus->Name : shared_str());
	Config->file = Section->File;

	SoundSourceState* Source = Snd_LookupSourceByName(Name);
	if (Source != nullptr)
	{
		Snd_ReadSoundDesc(&Source->File, Name, Section, &Source->Desc);
		Snd_CopySoundConfig(&Source->Desc, Config);
	}

	return Source;
}

void Snd_GetLoadedSources(xr_vector<shared_str>& Names)
{
	xrSRWLockGuard Guard(g_SoundSourceLock, true);
	Names.clear();
	for (auto& [Key, Source] : GSourcePool.Sources)
	{
		if (Source.IsReady)
		{
			Names.push_back(Source.Desc.name);
		}
	}
}

void Snd_ExportConfig()
{
	xr_hash_map<shared_str, CInifile*> Inis;
	FS_FileSet Files;
	FS.file_list(Files, _game_sounds_, FS_ListFiles, "*.ogg");

	u32 Count = 0;
	for (const FS_File& File : Files)
	{
		string_path Section;
		xr_strcpy(Section, File.name.c_str());
		xr_strlwr(Section);
		if (char* Ext = strext(Section))
		{
			*Ext = 0;
		}

		string_path SoundPath;
		FS.update_path(SoundPath, _game_sounds_, File.name.c_str());
		CInifile* Ini = Snd_GetExportIni(Inis, SoundPath);
		if (Ini->section_exist(Section))
		{
			continue;
		}

		sound_source_desc Desc = {};
		Snd_ReadSoundFile(SoundPath, Section, nullptr, &Desc);

		Ini->w_float(Section, "volume", Desc.volume);
		Ini->w_float(Section, "min_distance", Desc.min_distance);
		Ini->w_float(Section, "max_distance", Desc.max_distance);
		Ini->w_float(Section, "ai_distance", Desc.max_ai_distance);
		Ini->w_u32(Section, "ai_type", Desc.game_type);
		Count++;
	}

	for (auto& [Origin, Ini] : Inis)
	{
		Ini->save_as();
		Msg("* [Sound] Exported sounds to '%s'", Ini->fname());
		xr_delete(Ini);
	}

	Msg("* [Sound] Exported %u sounds", Count);
}

void Snd_LoadImpulseResponse(const char* Name, xr_vector<xr_vector<float>>& ChannelAudio, u32& SampleRate, u16& NumChannels)
{
	SoundSourceState Source = {};
	Snd_LoadSourceFile(&Source, Name);
	if (Source.File.datasource == nullptr || strstr(Source.Desc.path.c_str(), "$no_sound") != nullptr)
	{
		Snd_UnloadSourceFile(&Source);
		return;
	}

	NumChannels = (u16)Source.Desc.channels_count;
	SampleRate = SND_SAMPLERATE;

	float* ReadBuffer[SND_CHANNEL_COUNT] = {};
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		ReadBuffer[Channel] = xr_alloc<float>(SND_BLOCKSIZE);
	}

	ChannelAudio.resize(SND_CHANNEL_COUNT);
	u32 Position = 0;
	while (Position < Source.Desc.frames_total)
	{
		u32 Frames = Snd_ReadFromSource(&Source, ReadBuffer, std::min((u32)SND_BLOCKSIZE, Source.Desc.frames_total - Position));
		if (Frames == 0)
		{
			break;
		}

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			ChannelAudio[Channel].resize(Position + Frames);
			memcpy(ChannelAudio[Channel].data() + Position, ReadBuffer[Channel], Frames * sizeof(float));
		}

		Position += Frames;
	}

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		xr_free(ReadBuffer[Channel]);
	}

	Snd_UnloadSourceFile(&Source);
}
