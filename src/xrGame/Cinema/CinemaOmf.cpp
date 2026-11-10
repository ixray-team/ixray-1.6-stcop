#include "StdAfx.h"
#include "CinemaOmf.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/motion.h"

namespace Cinema::Omf
{
bool Locate(const char* Path, string_path& Resolved, bool IsMeshFirst)
{
	if (!Path || !Path[0] || xr_strlen(Path) >= 256)
	{
		return false;
	}
	return FS.exist(Resolved, IsMeshFirst ? _game_meshes_ : "$level$", Path) || FS.exist(Resolved, IsMeshFirst ? "$level$" : _game_meshes_, Path) || FS.exist(Resolved, "$app_data_root$", Path);
}

bool LocateVisual(const char* Visual, string_path& Resolved)
{
	if (!Visual || xr_strlen(Visual) >= 256)
	{
		return false;
	}
	string_path Name = {};
	xr_strcpy(Name, Visual);
	if (!strext(Name))
	{
		xr_strcat(Name, ".ogf");
	}
	return Locate(Name, Resolved, true);
}

bool ReadString(IReader& Reader, xr_string& Value)
{
	if (Reader.elapsed() <= 0 || !memchr(Reader.pointer(), 0, static_cast<size_t>(Reader.elapsed())))
	{
		return false;
	}
	Reader.r_stringZ(Value);
	return true;
}

bool ChunksFit(IReader& Reader)
{
	for (Reader.rewind(); Reader.elapsed() >= 8;)
	{
		Reader.advance(4);
		const u32 Size = Reader.r_u32();
		if (Size > static_cast<u64>(Reader.elapsed()))
		{
			return false;
		}
		Reader.advance(Size);
	}
	const bool IsFit = !Reader.elapsed();
	Reader.rewind();
	return IsFit;
}

bool SkipMotionMarks(IReader& Reader, u32 Marks)
{
	for (u32 Mark = 0; Mark < Marks; ++Mark)
	{
		const char* Text = static_cast<const char*>(Reader.pointer());
		const size_t Remaining = static_cast<size_t>(Reader.elapsed());
		size_t Length = 0;
		while (Length < Remaining && Text[Length] != '\r' && Text[Length] != '\n')
		{
			++Length;
		}
		if (Length == Remaining)
		{
			return false;
		}
		while (Length < Remaining && (Text[Length] == '\r' || Text[Length] == '\n'))
		{
			++Length;
		}
		Reader.advance(static_cast<intptr_t>(Length));
		if (Reader.elapsed() < 4)
		{
			return false;
		}
		const u32 Intervals = Reader.r_u32();
		if (Intervals > static_cast<u32>(Reader.elapsed()) / (2 * sizeof(float)))
		{
			return false;
		}
		Reader.advance(static_cast<intptr_t>(Intervals * 2 * sizeof(float)));
	}
	return true;
}

bool ReadMotionChannel(IReader& Reader, u32 Frames, const u8*& Data, u32& Size)
{
	if (Reader.elapsed() < 1)
	{
		return false;
	}
	Data = static_cast<const u8*>(Reader.pointer());
	const u8 Flags = Reader.r_u8();
	const bool IsRaw = (Flags & flTKeyFFT_Bit) != 0;
	const u64 RotationSize = IsRaw ? sizeof(CKeyQR32) : sizeof(CKeyQR);
	const u64 PositionSize = IsRaw ? sizeof(CKeyQT32) : (Flags & flTKey16IsBit) ? sizeof(CKeyQT16) : sizeof(CKeyQT8);
	u64 Bytes = (Flags & flRKeyAbsent) ? RotationSize : 4 + RotationSize * Frames;
	Bytes += (Flags & flTKeyPresent) ? 4 + PositionSize * Frames + (IsRaw ? 0 : 2 * sizeof(Fvector)) : sizeof(Fvector);
	if (Bytes > static_cast<u64>(Reader.elapsed()) || Bytes >= u64(u32(-1)))
	{
		return false;
	}
	Reader.advance(static_cast<intptr_t>(Bytes));
	Size = static_cast<u32>(Bytes) + 1;
	return true;
}

bool CheckMotionData(IReader& File, const char* Path, const xr_vector<xr_string>& Bones, xr_string& Error)
{
	CReaderGuarded Params(File.open_chunk(OGF_S_SMPARAMS));
	if (!Params || Params->elapsed() < 4)
	{
		Error = xr_string("Invalid animation parameters: ") + Path;
		return false;
	}
	const u16 Version = Params->r_u16();
	const u16 Parts = Params->r_u16();
	if (Version > xrOGF_SMParamsVersion || Parts > MAX_PARTS)
	{
		Error = xr_string("Unsupported animation parameters: ") + Path;
		return false;
	}
	u32 Count = 0;
	xr_vector<bool> IsSeen(Bones.size(), false);
	xr_set<xr_string> SeenNames;
	xr_string Name;
	for (u16 Part = 0; Part < Parts; ++Part)
	{
		if (!ReadString(*Params, Name) || Params->elapsed() < 2)
		{
			Error = xr_string("Invalid animation partition: ") + Path;
			return false;
		}
		for (u16 Bone = 0, PartBones = Params->r_u16(); Bone < PartBones; ++Bone)
		{
			if (!ReadString(*Params, Name) || Params->elapsed() < 4)
			{
				Error = xr_string("Invalid animation bone data: ") + Path;
				return false;
			}
			const u32 Index = Params->r_u32();
			if (std::find(Bones.begin(), Bones.end(), Name) == Bones.end() || Index >= Bones.size() || IsSeen[Index] || !SeenNames.insert(Name).second)
			{
				Error = xr_string("Animation ") + Path + " is incompatible with the model: bone " + Name;
				return false;
			}
			IsSeen[Index] = true;
			++Count;
		}
	}
	if (Count != Bones.size())
	{
		Error = xr_string("Animation bone count differs from the model: ") + Path;
		return false;
	}
	return true;
}

bool CheckMotion(const char* Path, const xr_vector<xr_string>& Bones, xr_string& Error)
{
	string_path Resolved = {};
	if (!Locate(Path, Resolved, false))
	{
		Error = xr_string("Animation file not found: ") + Path;
		return false;
	}
	CReaderGuarded File(FS.r_open(Resolved));
	if (!File)
	{
		Error = xr_string("Could not read animation file: ") + Path;
		return false;
	}
	return CheckMotionData(*File, Path, Bones, Error);
}

bool CheckMotionPath(const char* Path, const xr_vector<xr_string>& Bones, xr_string& Error)
{
	if (xr_strlen(Path) >= 256)
	{
		Error = "Animation path is too long.";
		return false;
	}
	if (!strstr(Path, "\\*.omf"))
	{
		return CheckMotion((xr_string(Path) + ".omf").c_str(), Bones, Error);
	}
	FS_FileSet Files;
	FS.file_list(Files, _game_meshes_, FS_ListFiles, Path);
	FS.file_list(Files, "$level$", FS_ListFiles, Path);
	if (Files.empty())
	{
		Error = xr_string("No animation files found: ") + Path;
		return false;
	}
	return std::all_of(Files.begin(), Files.end(), [&](const FS_File& Item) { return CheckMotion(Item.name.c_str(), Bones, Error); });
}

void WriteMotion(const char* Name, const xr_vector<shared_str>& Bones, const xr_vector<Fquaternion>& Turns, const xr_vector<Fvector>& Shifts, CMemoryWriter& Output)
{
	const u32 Count = static_cast<u32>(Bones.size()), Frames = Count ? static_cast<u32>(Turns.size() / Count) : 0;
	Output.open_chunk(OGF_S_SMPARAMS);
	Output.w_u16(xrOGF_SMParamsVersion);
	Output.w_u16(1);
	Output.w_stringZ("base");
	Output.w_u16(static_cast<u16>(Count));
	for (u32 Bone = 0; Bone < Count; ++Bone)
	{
		Output.w_stringZ(Bones[Bone]);
		Output.w_u32(Bone);
	}
	Output.w_u16(1);
	Output.w_stringZ(Name);
	Output.w_u32(0);
	Output.w_u16(BI_NONE);
	Output.w_u16(0);
	Output.w_float(1.0f);
	Output.w_float(1.0f);
	Output.w_float(2.0f);
	Output.w_float(2.0f);
	Output.w_u32(0);
	Output.close_chunk();
	Output.open_chunk(OGF_S_MOTIONS);
	Output.open_chunk(0);
	Output.w_u32(1);
	Output.close_chunk();
	Output.open_chunk(1);
	Output.w_stringZ(Name);
	Output.w_u32(Frames);
	for (u32 Bone = 0; Bone < Count; ++Bone)
	{
		Output.w_u8(flTKeyFFT_Bit | flTKeyPresent);
		Output.w_u32(0);
		for (u32 Frame = 0; Frame < Frames; ++Frame)
		{
			const Fquaternion& Turn = Turns[Frame * Count + Bone];
			const CKeyQR32 Rotation { Turn.x, Turn.y, Turn.z, Turn.w };
			Output.w(&Rotation, sizeof(Rotation));
		}
		Output.w_u32(0);
		for (u32 Frame = 0; Frame < Frames; ++Frame)
		{
			Output.w_fvector3(Shifts[Frame * Count + Bone]);
		}
	}
	Output.close_chunk();
	Output.close_chunk();
}

bool IsOmfPath(const shared_str& Path)
{
	return Path.size() > 4 && !_stricmp(Path.c_str() + Path.size() - 4, ".omf");
}

bool VisualExists(const char* Visual)
{
	string_path Resolved = {};
	return LocateVisual(Visual, Resolved);
}

bool CheckVisual(const char* Visual, xr_string& Error)
{
	string_path Resolved = {};
	if (!LocateVisual(Visual, Resolved))
	{
		Error = xr_string("Model not found: ") + (Visual ? Visual : "");
		return false;
	}
	CReaderGuarded File(FS.r_open(Resolved));
	if (!File)
	{
		Error = xr_string("Could not read model: ") + Visual;
		return false;
	}
	CReaderGuarded BoneChunk(File->open_chunk(OGF_S_BONE_NAMES));
	if (!BoneChunk)
	{
		return true;
	}
	const u32 Count = BoneChunk->elapsed() < 4 ? u32(-1) : BoneChunk->r_u32();
	if (Count > 65535)
	{
		Error = xr_string("Invalid model bone count: ") + Visual;
		return false;
	}
	xr_vector<xr_string> Bones;
	xr_string Name, Parent;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		if (!ReadString(*BoneChunk, Name) || !ReadString(*BoneChunk, Parent) || BoneChunk->elapsed() < static_cast<intptr_t>(sizeof(Fobb)))
		{
			Error = xr_string("Invalid model bones: ") + Visual;
			return false;
		}
		xr_strlwr(Name.data());
		Bones.push_back(Name);
		BoneChunk->advance(sizeof(Fobb));
	}
	const xr_string BadRefs = xr_string("Invalid model animation references: ") + Visual;
	if (CReaderGuarded Refs(File->open_chunk(OGF_S_MOTION_REFS)); Refs)
	{
		xr_string List, Item;
		const u32 RefCount = ReadString(*Refs, List) && List.size() < sizeof(string_path) ? static_cast<u32>(_GetItemCount(List.c_str())) : u32(-1);
		if (RefCount >= MAX_ANIM_SLOT)
		{
			Error = BadRefs;
			return false;
		}
		for (u32 Index = 0; Index < RefCount; ++Index)
		{
			_GetItem(List.c_str(), static_cast<int>(Index), Item);
			if (!CheckMotionPath(Item.c_str(), Bones, Error))
			{
				return false;
			}
		}
		return true;
	}
	if (CReaderGuarded Refs(File->open_chunk(OGF_S_MOTION_REFS2)); Refs)
	{
		if (Refs->elapsed() < 4)
		{
			Error = BadRefs;
			return false;
		}
		xr_string Path;
		for (u32 Index = 0, RefCount = Refs->r_u32(); Index < RefCount; ++Index)
		{
			if (!ReadString(*Refs, Path))
			{
				Error = BadRefs;
				return false;
			}
			if (!CheckMotionPath(Path.c_str(), Bones, Error))
			{
				return false;
			}
		}
		return true;
	}
	return !File->find_chunk(OGF_S_SMPARAMS) || CheckMotionData(*File, Visual, Bones, Error);
}

bool IndexAnimationFile(const char* Path, AnimationSource& Source)
{
	string_path Resolved = {};
	if (!Locate(Path, Resolved, false))
	{
		return false;
	}
	CReaderGuarded File(FS.r_open(Resolved));
	if (!File || !ChunksFit(*File))
	{
		return false;
	}
	CReaderGuarded Params(File->open_chunk(OGF_S_SMPARAMS));
	if (!Params || !File->find_chunk(OGF_S_MOTIONS) || Params->elapsed() < 4)
	{
		return false;
	}
	const u16 Version = Params->r_u16();
	const u16 Parts = Params->r_u16();
	if (Version > xrOGF_SMParamsVersion || Parts > MAX_PARTS)
	{
		return false;
	}
	xr_set<xr_string> Names;
	xr_set<u32> Indices;
	xr_string Name;
	for (u16 Part = 0; Part < Parts; ++Part)
	{
		if (!ReadString(*Params, Name) || Params->elapsed() < 2)
		{
			return false;
		}
		for (u16 Bone = 0, Count = Params->r_u16(); Bone < Count; ++Bone)
		{
			if (!ReadString(*Params, Name) || Params->elapsed() < 4 || Name.empty())
			{
				return false;
			}
			xr_strlwr(Name.data());
			if (!Names.insert(Name).second || !Indices.insert(Params->r_u32()).second)
			{
				return false;
			}
		}
	}
	if (Names.empty() || Params->elapsed() < 2 || *Indices.rbegin() >= Names.size())
	{
		return false;
	}
	Source.Clips.clear();
	for (u16 Index = 0, Count = Params->r_u16(); Index < Count; ++Index)
	{
		if (!ReadString(*Params, Name) || Name.empty() || Params->elapsed() < 24)
		{
			return false;
		}
		xr_strlwr(Name.data());
		const u32 Flags = Params->r_u32();
		Params->advance(20);
		if (Version >= 4 && (Params->elapsed() < 4 || !SkipMotionMarks(*Params, Params->r_u32())))
		{
			return false;
		}
		Source.Clips.push_back({ Name.c_str(), (Flags & esmFX) != 0 });
	}
	Source.Id = Path;
	return !Source.Clips.empty();
}

bool BuildAdaptedMotion(IKinematics* Skeleton, IKinematicsAnimated* Animated, const char* SourcePath, CMemoryWriter& Output, xr_string& Error)
{
	const auto Fail = [&Error](const char* Text)
	{
		Error = Text;
		return false;
	};
	string_path Resolved = {};
	if (!Skeleton || !Animated || !Skeleton->LL_Bones() || !Locate(SourcePath, Resolved, false))
	{
		return Fail("Animation or target skeleton is unavailable.");
	}
	CReaderGuarded File(FS.r_open(Resolved));
	if (!File || File->length() > 256 * 1024 * 1024 || !ChunksFit(*File))
	{
		return Fail("Unable to read the animation, or the file exceeds 256 MiB.");
	}
	CReaderGuarded Params(File->open_chunk(OGF_S_SMPARAMS));
	CReaderGuarded Motions(File->open_chunk(OGF_S_MOTIONS));
	if (!Params || !Motions || !ChunksFit(*Motions) || Params->elapsed() < 4)
	{
		return Fail("Invalid animation chunks.");
	}
	const u16 Version = Params->r_u16();
	const u16 PartCount = Params->r_u16();
	if (Version > xrOGF_SMParamsVersion || PartCount > MAX_PARTS)
	{
		return Fail("Unsupported animation format.");
	}
	xr_vector<xr_string> SourceNames;
	xr_vector<xr_string> PartNames;
	xr_string Name;
	for (u16 Part = 0; Part < PartCount; ++Part)
	{
		if (!ReadString(*Params, Name) || Name.size() >= 128 || Params->elapsed() < 2)
		{
			return Fail("Invalid animation partition.");
		}
		PartNames.push_back(Name);
		for (u16 Bone = 0, Count = Params->r_u16(); Bone < Count; ++Bone)
		{
			if (!ReadString(*Params, Name) || Name.empty() || Name.size() >= 128 || Params->elapsed() < 4)
			{
				return Fail("Invalid animation bone.");
			}
			xr_strlwr(Name.data());
			const u32 Index = Params->r_u32();
			if (Index >= 65535)
			{
				return Fail("Invalid animation bone index.");
			}
			if (SourceNames.size() <= Index)
			{
				SourceNames.resize(Index + 1);
			}
			if (!SourceNames[Index].empty())
			{
				return Fail("Duplicate animation bone index.");
			}
			SourceNames[Index] = Name;
		}
	}
	if (SourceNames.empty() || Params->elapsed() < 2)
	{
		return Fail("Animation has no bones.");
	}
	if (std::any_of(SourceNames.begin(), SourceNames.end(), [](const xr_string& Item) { return Item.empty(); }))
	{
		return Fail("Incomplete animation bone indices.");
	}
	const u16 TargetCount = Skeleton->LL_BoneCount();
	if (!TargetCount)
	{
		return Fail("Target model has no bones.");
	}
	xr_vector<shared_str> TargetNames(TargetCount);
	for (const auto& Bone : *Skeleton->LL_Bones())
	{
		if (Bone.second >= TargetCount || !Bone.first.size() || Bone.first.size() >= 128)
		{
			return Fail("Invalid target skeleton.");
		}
		TargetNames[Bone.second] = Bone.first;
	}
	xr_vector<s32> TargetToSource(TargetCount, -1);
	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		if (!TargetNames[Target].size())
		{
			return Fail("Incomplete target skeleton.");
		}
		for (u32 Index = 0; Index < SourceNames.size(); ++Index)
		{
			if (!_stricmp(TargetNames[Target].c_str(), SourceNames[Index].c_str()))
			{
				TargetToSource[Target] = static_cast<s32>(Index);
				break;
			}
		}
	}
	const u16 Definitions = Params->r_u16();
	CReaderGuarded CountChunk(Motions->open_chunk(0));
	if (!CountChunk || CountChunk->elapsed() < 4)
	{
		return Fail("Invalid animation count.");
	}
	const u32 MotionCount = CountChunk->r_u32();
	if (MotionCount != Definitions || MotionCount >= 0x3fff)
	{
		return Fail("Animation definitions do not match the motion data.");
	}
	Output.open_chunk(OGF_S_SMPARAMS);
	Output.w_u16(Version);
	Output.w_u16(1);
	Output.w_stringZ("base");
	Output.w_u16(TargetCount);
	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		Output.w_stringZ(TargetNames[Target]);
		Output.w_u32(Target);
	}
	Output.w_u16(Definitions);
	const CPartition& Partitions = Animated->partitions();
	for (u32 Definition = 0; Definition < Definitions; ++Definition)
	{
		if (!ReadString(*Params, Name) || Name.empty() || Name.size() >= 128 || Params->elapsed() < 24)
		{
			return Fail("Invalid animation definition.");
		}
		const u32 Flags = Params->r_u32();
		const u16 SourcePart = Params->r_u16();
		u16 TargetPart = SourcePart == BI_NONE ? BI_NONE : 0;
		if (Flags & esmFX)
		{
			TargetPart = SourcePart < SourceNames.size() ? Skeleton->LL_BoneID(SourceNames[SourcePart].c_str()) : BI_NONE;
			TargetPart = TargetPart == BI_NONE || TargetPart >= TargetCount ? Skeleton->LL_GetBoneRoot() : TargetPart;
		}
		else if (SourcePart < PartNames.size())
		{
			for (u16 Part = 0; Part < MAX_PARTS; ++Part)
			{
				if (Partitions.part(Part).Name.size() && !_stricmp(Partitions.part(Part).Name.c_str(), PartNames[SourcePart].c_str()))
				{
					TargetPart = Part;
					break;
				}
			}
		}
		Output.w_stringZ(Name);
		Output.w_u32(Flags);
		Output.w_u16(TargetPart);
		Output.w(Params->pointer(), 18);
		Params->advance(18);
		if (Version >= 4)
		{
			if (Params->elapsed() < 4)
			{
				return Fail("Invalid animation marks.");
			}
			const u32 Marks = Params->r_u32();
			const u8* Start = static_cast<const u8*>(Params->pointer());
			if (!SkipMotionMarks(*Params, Marks))
			{
				return Fail("Invalid animation marks.");
			}
			Output.w_u32(Marks);
			Output.w(Start, static_cast<u32>(static_cast<const u8*>(Params->pointer()) - Start));
		}
	}
	Output.close_chunk();
	Output.open_chunk(OGF_S_MOTIONS);
	Output.open_chunk(0);
	Output.w_u32(MotionCount);
	Output.close_chunk();
	xr_vector<const u8*> Channels(SourceNames.size(), nullptr);
	xr_vector<u32> Sizes(SourceNames.size(), 0);
	for (u32 Motion = 0; Motion < MotionCount; ++Motion)
	{
		CReaderGuarded Input(Motions->open_chunk(Motion + 1));
		if (!Input || !ReadString(*Input, Name) || Name.empty() || Name.size() >= 128 || Input->elapsed() < 4)
		{
			return Fail("Invalid animation frames.");
		}
		const u32 Frames = Input->r_u32();
		if (!Frames)
		{
			return Fail("Animation has no frames.");
		}
		for (size_t Bone = 0; Bone < SourceNames.size(); ++Bone)
		{
			if (!ReadMotionChannel(*Input, Frames, Channels[Bone], Sizes[Bone]))
			{
				return Fail("Invalid animation bone frames.");
			}
		}
		Output.open_chunk(Motion + 1);
		Output.w_stringZ(Name);
		Output.w_u32(Frames);
		for (u32 Bone = 0; Bone < TargetCount; ++Bone)
		{
			const s32 Index = TargetToSource[Bone];
			if (Index >= 0)
			{
				Output.w(Channels[Index], Sizes[Index]);
				continue;
			}
			const Fmatrix& Bind = Skeleton->GetBoneData(static_cast<u16>(Bone)).get_bind_transform();
			const Fquaternion Q = Fquaternion().set(Bind).normalize();
			const CKeyQR32 Rotation { Q.x, Q.y, Q.z, Q.w };
			Output.w_u8(flTKeyFFT_Bit | flRKeyAbsent);
			Output.w(&Rotation, sizeof(Rotation));
			Output.w_fvector3(Bind.c);
		}
		Output.close_chunk();
	}
	for (u32 Motion = 0; Motion < MotionCount; ++Motion)
	{
		CReaderGuarded Input(Motions->open_chunk(MotionCount + 2 + Motion));
		if (!Input)
		{
			continue;
		}
		if (Input->elapsed() < 4)
		{
			return Fail("Invalid animation notifications.");
		}
		const u32 BoneCount = Input->r_u32();
		CMemoryWriter Notifications;
		u32 MappedBones = 0;
		for (u32 Bone = 0; Bone < BoneCount; ++Bone)
		{
			if (Input->elapsed() < 6)
			{
				return Fail("Invalid animation notifications.");
			}
			const u16 SourceBone = Input->r_u16();
			const u32 EventCount = Input->r_u32();
			const u8* Start = static_cast<const u8*>(Input->pointer());
			for (u32 Event = 0; Event < EventCount; ++Event)
			{
				if (Input->elapsed() < 8)
				{
					return Fail("Invalid animation notifications.");
				}
				Input->advance(4);
				for (u32 Index = 0, Names = Input->r_u32(); Index < Names; ++Index)
				{
					if (!ReadString(*Input, Name))
					{
						return Fail("Invalid animation notifications.");
					}
				}
			}
			const auto Target = std::find(TargetToSource.begin(), TargetToSource.end(), static_cast<s32>(SourceBone));
			if (Target != TargetToSource.end())
			{
				Notifications.w_u16(static_cast<u16>(Target - TargetToSource.begin()));
				Notifications.w_u32(EventCount);
				Notifications.w(Start, static_cast<u32>(static_cast<const u8*>(Input->pointer()) - Start));
				++MappedBones;
			}
		}
		Output.open_chunk(MotionCount + 2 + Motion);
		Output.w_u32(MappedBones);
		if (Notifications.size())
		{
			Output.w(Notifications.pointer(), Notifications.size());
		}
		Output.close_chunk();
	}
	Output.close_chunk();
	return true;
}
}
