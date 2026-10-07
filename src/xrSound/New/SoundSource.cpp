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

#define SND_CACHE_CHUNK_LINES (32)
#define SND_CACHE_LINE_WIDTH (12)
#define SND_CACHE_LINE_CAPACITY ((SND_BLOCKSIZE + 1) * SND_CACHE_LINE_WIDTH)
#define SND_CACHE_LINE_MAX_TIME_NS (1000000000)
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

		if (OldestIdx == 0 || (Snd_GetTimestamp() - OldestTimestamp) < SND_CACHE_LINE_MAX_TIME_NS)
		{
			Snd_GrowCacheLines();
		}
		else
		{
			Snd_PurgeCacheLine(OldestIdx, true);
		}
	}

	if (GSourcePool.FreeCacheLines.empty())
	{
		return 0;
	}

	u32 CacheIdx = GSourcePool.FreeCacheLines.back();
	GSourcePool.FreeCacheLines.pop_back();
	Snd_GetCacheLine(CacheIdx)->Timestamp = Snd_GetTimestamp();
	SND_STAT_SET(g_SoundStats.cache_lines_free, (u32)GSourcePool.FreeCacheLines.size());
	return CacheIdx;
}

static u32 Snd_FindCacheLine(const SoundSourceState* Source, u32 Position)
{
	PROF_EVENT("Sound: FindCacheLine");
	if (Position >= Source->Desc.frames_total)
	{
		return 0;
	}

	u32 NeededFrames = std::min((u32)SND_BLOCKSIZE, Source->Desc.frames_total - Position);
	for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
	{
		u32 CacheIdx = Source->CacheLines[EntryIdx];
		if (!Snd_IsCacheLineValid(CacheIdx))
		{
			continue;
		}

		const SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);
		if (Line->End >= Line->Start && Position >= Line->Start && Position + NeededFrames <= Line->End)
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
		if (Status == 0)
		{
			break;
		}

		R_ASSERT2(Status >= 0, "Decoding error");
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

static void Snd_UpdateCache(SoundSourceState* Source, u32 Position)
{
	PROF_EVENT("Sound: Update Slot Cache");

	// Cache eviction modifies other owners; pin their map entries for the entire update.
	// Match the source -> cache lock order used by Snd_ReleaseSource.
	xrSRWLockGuard SourceGuard(g_SoundSourceLock, true);
	u32 NewIdx = 0;
	{
		xrSRWLockGuard Guard(GSourcePool.CacheLock, false);
		if (Snd_FindCacheLine(Source, Position) != 0 || Source->File.datasource == nullptr)
		{
			return;
		}

		SND_STAT_ADD(g_SoundStats.cache_miss_count, 1u);
		NewIdx = Snd_NewCacheLine();
		if (!Snd_IsCacheLineValid(NewIdx))
		{
			return;
		}
	}

	SoundCacheLine* Line = Snd_GetCacheLine(NewIdx);
	u32 BeginPos = Snd_SeekSource(Source, Position, false);
	if (BeginPos + SND_CACHE_LINE_CAPACITY < Position + SND_BLOCKSIZE)
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
	Line->Owner = Source;
	Line->Start = BeginPos;
	Line->End = EndPos;

	u32* OldestEntry = nullptr;
	u64 OldestTimestamp = (u64)-1;
	for (u32 EntryIdx = 0; EntryIdx < SND_CACHE_ENTRY_COUNT; EntryIdx++)
	{
		u32* Entry = &Source->CacheLines[EntryIdx];
		if (*Entry == 0 || *Entry == NewIdx)
		{
			*Entry = NewIdx;
			return;
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
	}
}

static void Snd_ParseOggComment(SoundSourceState* Source)
{
	bool IsParsed = false;
	vorbis_comment* Comment = ov_comment(&Source->File, -1);
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

			Source->Desc.min_distance = Reader.r_float();
			Source->Desc.max_distance = Reader.r_float();
			Source->Desc.volume = Decl->HasVolume ? Reader.r_float() : 1.0f;
			Source->Desc.game_type = Reader.r_u32();
			Source->Desc.max_ai_distance = Decl->HasAiDistance ? Reader.r_float() : SND_DEFAULT_MAX_DISTANCE;
			IsParsed = true;
			break;
		}
	}

	if (!IsParsed)
	{
		Msg("~ Missing or invalid ogg-comment, file: %s", Source->Desc.name.c_str());
	}
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

	xr_strconcat(Path, BaseName, ".ogg");
	if (!FS.exist("$level$", Path))
	{
		FS.update_path(Path, _game_sounds_, Path);
	}

	if (!FS.exist(Path))
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
	Source->Desc.volume = 1.0f;
	Source->Desc.min_distance = 1.0f;
	Source->Desc.max_distance = SND_DEFAULT_MAX_DISTANCE;
	Source->Desc.max_ai_distance = SND_DEFAULT_MAX_DISTANCE;

	Snd_ParseOggComment(Source);

	Source->Desc.volume = std::min(Source->Desc.volume, 1.0f);
	if (Source->Desc.min_distance < EPS_S)
	{
		Source->Desc.min_distance = 1.0f;
	}

	if (Source->Desc.max_distance < Source->Desc.min_distance)
	{
		Source->Desc.max_distance = Source->Desc.min_distance + 1.0f;
	}

	if (Source->Desc.max_ai_distance < EPS_S)
	{
		Source->Desc.max_ai_distance = Source->Desc.max_distance;
	}
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
			const SoundDecodeCommand* Queued = &GSourcePool.DecodeQueue[RequestIdx];
			if (Queued->Position == Position && Queued->Name == *Name)
			{
				return;
			}
		}

		GSourcePool.DecodeQueue.push_back({*Name, Position});
	}

	// One Run() per queued command: the worker pops exactly one command per wakeup
	GSourcePool.DecodeThread->Run();
}

bool Snd_HasCacheLine(const SoundSourceState* Source, u32 Position)
{
	xrSRWLockGuard Guard(GSourcePool.CacheLock, true);
	return Snd_FindCacheLine(Source, Position) != 0;
}

u32 Snd_CopyCached(const SoundSourceState* Source, u32 Position, float** OutData, u32 Frames)
{
	xrSRWLockGuard Guard(GSourcePool.CacheLock, true);
	u32 CacheIdx = Snd_FindCacheLine(Source, Position);
	if (!Snd_IsCacheLineValid(CacheIdx))
	{
		return 0;
	}

	const SoundCacheLine* Line = Snd_GetCacheLine(CacheIdx);
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

void XRay::Sound::Mixer::LoadImpulseResponse(const char* Name, xr_vector<xr_vector<float>>& ChannelAudio, u32& SampleRate, u16& NumChannels)
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
