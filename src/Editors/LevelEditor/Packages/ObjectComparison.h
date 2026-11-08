#pragma once

#include "ComparisonReader.h"
#include <bit>
#include <cmath>

namespace SdkPackages::ObjectComparison
{
using ComparisonReader::Add;
using ComparisonReader::Chunks;
using ComparisonReader::Fail;
using ComparisonReader::Hex;
using ComparisonReader::Number;
using ComparisonReader::Range;
using ComparisonReader::Raw;
using ComparisonReader::Reader;
using ComparisonReader::Tail;
using ComparisonReader::Text;
struct Snapshot : ComparisonReader::Snapshot
{
	u64 Vertices = 0;
	u64 Faces = 0;
	bool HasSharedMaterials = false;
};

inline bool Collect(Reader& Input, Range Area, Chunks& Result, xr_string& Error)
{
	return Input.Chunks(Area, [&](u32 Id, Range Data)
						{
		if (Id & 0x80000000)
		{
			return Fail(Error, "Compressed object blocks are not supported for content comparison.");
		}
		return Result.emplace(Id, Data).second || Fail(Error, "Duplicate object data block."); });
}
inline bool Options(Reader& Input, Range& Data, xr_string& Result, xr_string& Error)
{
	const auto Size = Data.End - Data.Begin;
	if (!Size || Size > 65537)
	{
		return Fail(Error, "Object game options exceed 64 KiB or are malformed.");
	}
	Result.resize(static_cast<size_t>(Size));
	if (!Input.Read(Data, Result.data(), Result.size()))
	{
		return false;
	}
	if (Result.back() != '\0' || Result.find('\0') != Result.size() - 1)
	{
		return Fail(Error, "Object game options exceed 64 KiB or are malformed.");
	}
	Result.pop_back();
	if (!Presentation::IsUtf8(Result))
	{
		Result = Platform::ANSI_TO_UTF8(Result.c_str()).c_str();
	}
	return true;
}
inline bool Vector(Reader& Input, Range& Data, xr_string& Result, xr_string& Error)
{
	xr_array<float, 3> Values{};
	for (auto& Value : Values)
	{
		u32 Bits;
		if (!Input.Integer(Data, Bits))
		{
			return false;
		}
		Value = std::bit_cast<float>(Bits);
		if (!std::isfinite(Value))
		{
			return Fail(Error, "Object bounds or transform contains a non-finite number.");
		}
	}
	string128 Text;
	xr_sprintf(Text, "%.9g, %.9g, %.9g", Values[0], Values[1], Values[2]);
	Result = Text;
	return true;
}
inline bool ReadSurfaces(Reader& Input, Range Data, u32 Format, Snapshot& State, xr_string& Error, Range* CompileOverrides = nullptr)
{
	u32 Count;
	if (!Input.Integer(Data, Count) || Count > 65536 || !Add(State, "Object", {}, "Materials", Number(Count), Error) || !Add(State, "Object", {}, "Material format", Hex(Format), Error))
	{
		return Fail(Error, "Invalid object material list.");
	}
	for (u32 Index = 0; Index < Count; ++Index)
	{
		xr_string Name, Shader, Compile, Material, Texture, VMap;
		u32 Shared = 0, Flags, Fvf, Coordinates;
		if ((Format == 0x0908 && !Input.Integer(Data, Shared, 1)) || !Text(Input, Data, Name) || Name.empty())
		{
			return Fail(Error, "Invalid object material name.");
		}
		if (Shared)
		{
			State.HasSharedMaterials = true;
			if (!Text(Input, Data, VMap))
			{
				return false;
			}
		}
		else if (Format == 0x0905)
		{
			if (!Text(Input, Data, Shader) || !Input.Integer(Data, Flags, 1) || !Input.Integer(Data, Fvf) || !Input.Integer(Data, Coordinates) || !Text(Input, Data, Texture) || !Text(Input, Data, VMap))
			{
				return false;
			}
			Compile = "default";
			if (CompileOverrides && !Text(Input, *CompileOverrides, Compile))
			{
				return false;
			}
		}
		else
		{
			if (!Text(Input, Data, Shader) || !Text(Input, Data, Compile) || (Format != 0x0906 && !Text(Input, Data, Material)) || !Text(Input, Data, Texture) || !Text(Input, Data, VMap))
			{
				return false;
			}
		}
		if (Format != 0x0905 && (!Input.Integer(Data, Flags) || !Input.Integer(Data, Fvf) || !Input.Integer(Data, Coordinates)))
		{
			return false;
		}
		if (!Add(State, "Material", Name, "Shared material", Shared ? Name : xr_string{}, Error) ||
			!Add(State, "Material", Name, "UV map", VMap, Error) || !Add(State, "Material", Name, "Two-sided", (Flags & 1 ? "Enabled" : "Disabled"), Error) ||
			!Add(State, "Material", Name, "Vertex format", Hex(Fvf), Error) || !Add(State, "Material", Name, "Texture coordinate sets", Number(Coordinates), Error) ||
			!Add(State, "Material", Name, "Other material flags", Hex(Flags & ~1u), Error))
		{
			return false;
		}
		if (!Shared && (!Add(State, "Material", Name, "Texture", Texture, Error) || !Add(State, "Material", Name, "Shader", Shader, Error) || !Add(State, "Material", Name, "Compile shader", Compile, Error) || !Add(State, "Material", Name, "Game material", Material, Error)))
		{
			return false;
		}
	}
	return Data.Begin == Data.End || Fail(Error, "Unrecognized trailing object material data.");
}
inline bool ReadMesh(Reader& Input, Range Area, Snapshot& State, xr_string& Error)
{
	Chunks Parts;
	if (!Collect(Input, Area, Parts, Error))
	{
		return false;
	}
	u32 Version;
	xr_string Name;
	if (!Parts.contains(0x1000) || !Input.Integer(Parts[0x1000], Version, 2) || Version != 0x11 || !Parts.contains(0x1001) || !Text(Input, Parts[0x1001], Name) || Name.empty())
	{
		return Fail(Error, "Unreadable or unsupported object mesh format.");
	}
	if (!Add(State, "Mesh", Name, "Mesh format", Hex(Version), Error))
	{
		return false;
	}
	for (const auto Id : {0x1005u, 0x1006u})
	{
		u32 Count;
		if (!Parts.contains(Id) || !Input.Integer(Parts[Id], Count) || Parts[Id].End - Parts[Id].Begin != static_cast<u64>(Count) * (Id == 0x1005 ? 12 : 24))
		{
			return Fail(Error, "Invalid object vertex or face data size.");
		}
		(Id == 0x1005 ? State.Vertices : State.Faces) += Count;
		if (!Add(State, "Mesh", Name, Id == 0x1005 ? "Vertices" : "Faces", Number(Count), Error) || !Raw(State, "Mesh", Name, Id == 0x1005 ? "Vertex positions" : "Face indices", Parts[Id], Error))
		{
			return false;
		}
	}
	if (Parts.contains(0x1002))
	{
		u32 Flags;
		if (!Input.Integer(Parts[0x1002], Flags, 1))
		{
			return false;
		}
		const char* Names[] = {"Visible", "Locked", "Smoothing mask", "Optimized"};
		for (u32 Index = 0; Index < 4; ++Index)
		{
			if (!Add(State, "Mesh", Name, Names[Index], (Flags & (1u << Index) ? "Enabled" : "Disabled"), Error))
			{
				return false;
			}
		}
		if (!Add(State, "Mesh", Name, "Other mesh flags", Hex(Flags & ~15u), Error))
		{
			return false;
		}
	}
	if (Parts.contains(0x1004))
	{
		xr_string Minimum, Maximum;
		if (!Vector(Input, Parts[0x1004], Minimum, Error) || !Vector(Input, Parts[0x1004], Maximum, Error) || !Add(State, "Mesh", Name, "Bounds minimum", Minimum, Error) || !Add(State, "Mesh", Name, "Bounds maximum", Maximum, Error))
		{
			return false;
		}
	}
	const u32 UvFormat = Parts.contains(0x1012) ? 0x1012 : Parts.contains(0x1011) ? 0x1011
																				  : 0x1007;
	for (const auto& [Id, Data] : Parts)
	{
		if (Id == 0x1000 || Id == 0x1001 || Id == 0x1002 || Id == 0x1004)
		{
			if (!Tail(State, "Mesh", Name, Id, Data, Error))
			{
				return false;
			}
			continue;
		}
		if (Id == 0x1005 || Id == 0x1006)
		{
			continue;
		}
		const char* Field = Id == UvFormat ? "UV maps and weights" : Id == 0x1008 ? "UV and weight bindings"
																 : Id == 0x1009	  ? "Surface assignments"
																 : Id == 0x1013	  ? "Smoothing groups"
																 : Id == 0x1014	  ? "Normals"
																 : Id == 0x1010	  ? "Mesh build options"
																				  : nullptr;
		if (!Raw(State, "Mesh", Name, Field ? xr_string(Field) : Hex(Id), Data, Error))
		{
			return false;
		}
		State.HasOpaqueData = true;
	}
	return true;
}
inline bool ReadSnapshot(Reader& Input, Snapshot& State, xr_string& Error)
{
	Chunks Roots, Parts;
	if (!Input.IsValid() || !Collect(Input, Input.Root, Roots, Error) || !Roots.contains(0x7777) || !Collect(Input, Roots[0x7777], Parts, Error))
	{
		return Fail(Error, "Cannot read the editable object body.");
	}
	for (const auto& [Id, Data] : Roots)
	{
		if (Id != 0x7777)
		{
			State.HasOpaqueData = true;
			if (!Raw(State, "Object", Hex(Id), "Other object data", Data, Error))
			{
				return false;
			}
		}
	}
	u32 Version;
	if (!Parts.contains(0x0900) || !Input.Integer(Parts[0x0900], Version, 2) || Version != 0x10)
	{
		return Fail(Error, "Unsupported editable object format for content comparison.");
	}
	if (!Add(State, "Object", {}, "Object format", Hex(Version), Error))
	{
		return false;
	}
	if (Parts.contains(0x0903))
	{
		u32 Flags;
		if (!Input.Integer(Parts[0x0903], Flags))
		{
			return false;
		}
		const char* Names[] = {"Dynamic", "Progressive", "Use LOD", "HOM", "Multiple usage", "Sound occluder", "High-quality export", "Skip optimization"};
		for (u32 Index = 0; Index < 8; ++Index)
		{
			if (!Add(State, "Object", {}, Names[Index], (Flags & (1u << Index) ? "Enabled" : "Disabled"), Error))
			{
				return false;
			}
		}
		if (!Add(State, "Object", {}, "Other object flags", Hex(Flags & ~255u), Error))
		{
			return false;
		}
	}
	for (const auto Id : {0x0912u, 0x0925u})
	{
		if (Parts.contains(Id))
		{
			xr_string TextValue;
			if (!(Id == 0x0912 ? Options(Input, Parts[Id], TextValue, Error) : Text(Input, Parts[Id], TextValue)) || !Add(State, "Object", {}, Id == 0x0912 ? "Game options" : "LOD reference", TextValue, Error))
			{
				return false;
			}
		}
	}
	u32 SurfaceFormat = 0;
	for (const auto Id : {0x0908u, 0x0907u, 0x0906u, 0x0905u})
	{
		if (Parts.contains(Id))
		{
			SurfaceFormat = Id;
			break;
		}
	}
	if (!SurfaceFormat)
	{
		return Fail(Error, "Object material list is missing.");
	}
	if (!ReadSurfaces(Input, Parts[SurfaceFormat], SurfaceFormat, State, Error, SurfaceFormat == 0x0905 && Parts.contains(0x0918) ? &Parts[0x0918] : nullptr))
	{
		return false;
	}
	if (!Parts.contains(0x0910))
	{
		return Fail(Error, "Object mesh list is missing.");
	}
	Chunks Meshes;
	if (!Collect(Input, Parts[0x0910], Meshes, Error) || !Add(State, "Object", {}, "Meshes", Number(Meshes.size()), Error))
	{
		return false;
	}
	u32 ExpectedMeshId = 0;
	for (const auto& [Id, Data] : Meshes)
	{
		if (Id != ExpectedMeshId++)
		{
			return Fail(Error, "Object mesh numbering is not contiguous from zero.");
		}
		if (!ReadMesh(Input, Data, State, Error))
		{
			return false;
		}
	}
	if (!Add(State, "Object", {}, "Total vertices", Number(State.Vertices), Error) || !Add(State, "Object", {}, "Total faces", Number(State.Faces), Error))
	{
		return false;
	}
	for (const auto& [Id, Data] : Parts)
	{
		if (Id == 0x0900 || Id == 0x0903 || Id == 0x0912 || Id == 0x0925 || (Id == 0x0918 && SurfaceFormat == 0x0905))
		{
			if (!Tail(State, "Object", {}, Id, Data, Error))
			{
				return false;
			}
			continue;
		}
		if (Id == SurfaceFormat || Id == 0x0910)
		{
			continue;
		}
		if (Id == 0x0920)
		{
			xr_string Position, Rotation;
			auto Cursor = Data;
			if (!Vector(Input, Cursor, Position, Error) || !Vector(Input, Cursor, Rotation, Error) || !Add(State, "Object", {}, "Actor position", Position, Error) || !Add(State, "Object", {}, "Actor rotation", Rotation, Error))
			{
				return false;
			}
			if (!Tail(State, "Object", {}, Id, Cursor, Error))
			{
				return false;
			}
			continue;
		}
		State.HasOpaqueData = true;
		const char* Field = Id == 0x0913 || Id == 0x0921 ? "Skeleton data" : Id == 0x0916 || Id == 0x0924 || Id == 0x0926 ? "Animation data"
																		 : Id == 0x0919 || Id == 0x0923					  ? "Bone groups"
																		 : Id == 0x0922									  ? "Object history"
																														  : "Other object data";
		if (!Raw(State, "Object", Hex(Id), Field, Data, Error))
		{
			return false;
		}
	}
	return true;
}
inline bool Read(const xr_path& CurrentPath, const xr_path& ImportedPath, AssetComparison& Result, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {})
{
	bool HasOpaqueData = false, HasSharedMaterials = false;
	const auto DecodeLambda = [&](Reader& Input, Snapshot& State, xr_string& DecodeError)
	{
		if (!ReadSnapshot(Input, State, DecodeError))
		{
			return false;
		}
		HasOpaqueData |= State.HasOpaqueData;
		HasSharedMaterials |= State.HasSharedMaterials;
		return true;
	};
	if (!ComparisonReader::Read<Snapshot>(CurrentPath, ImportedPath, Result, Error, Stop, Progress, DecodeLambda))
	{
		return false;
	}
	if (HasOpaqueData)
	{
		Result.Notices.push_back("Geometry, UV data, skeletons and other stored blocks are compared byte by byte; their individual elements are not listed.");
	}
	if (HasSharedMaterials)
	{
		Result.Notices.push_back("Shared materials are compared by their stored references. External shared material settings are not resolved here.");
	}
	return true;
}
} // namespace SdkPackages::ObjectComparison
