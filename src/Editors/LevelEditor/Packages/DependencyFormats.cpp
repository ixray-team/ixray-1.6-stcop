#include "stdafx.h"
#include "DependencyFormats.h"
#include "DependencyReader.h"

namespace SdkPackages::DependencyFormats
{
namespace Details
{
static bool Add(xr_vector<Reference>& References, Kind Type, const xr_string& Name, xr_string& Error, const xr_string& Shader = {})
{
	if (Name.empty())
	{
		return true;
	}
	if (References.size() >= 100000 || Name.size() > 1024 || !Presentation::IsUtf8(Name))
	{
		return Fail(Error, "Reference count, length or encoding is not supported; the dependency check is incomplete.");
	}
	References.push_back({Type, Name, Shader});
	return true;
}

static bool ReadList(Reader& Input, Range Area, u32 Id, Scene& Result, xr_string& Error, size_t Depth);

static bool ReadEntity(Reader& Input, Range Area, Scene& Result, xr_string& Error, size_t Depth)
{
	if (Depth > 16)
	{
		return Fail(Error, "Nested scene objects exceed the dependency scan depth limit.");
	}
	Range Class, Body;
	u32 Type;
	if (!Input.Find(Area, 0x7703, Class) || !Input.Integer(Class, Type) || !Input.Find(Area, 0x7777, Body))
	{
		return Fail(Error, "Scene object class/body is missing or unreadable.");
	}
	Range Data;
	xr_string Name;
	if (Type == 2)
	{
		Result.HasObjectData = true;
		u32 Version;
		if (!Input.Find(Body, 0x0900, Data) || !Input.Integer(Data, Version, 2) || Version < 0x10 || Version > 0x12)
		{
			return Fail(Error, "Unsupported scene object version.");
		}
		if (!Input.Find(Body, 0x0902, Data))
		{
			return Fail(Error, "Scene object reference is missing.");
		}
		if (Version <= 0x11)
		{
			Data.Begin += 8;
		}
		if (!Input.String(Data, Name) || !Add(Result.References, Kind::Object, Name, Error))
		{
			return false;
		}
		u32 Flags = 0;
		if (Input.Find(Body, 0x0905, Data) && !Input.Integer(Data, Flags))
		{
			return false;
		}
		if ((Flags & 1) && Input.Find(Body, 0x0906, Data))
		{
			u32 Count;
			if (!Input.Integer(Data, Count) || Count > 65536)
			{
				return Fail(Error, "Invalid surface override count.");
			}
			for (u32 Index = 0; Index < Count; ++Index)
			{
				xr_string Shader;
				for (u32 Field = 0; Field < 6; ++Field)
				{
					if (!Input.String(Data, Name) || (Field == 4 && !Add(Result.References, Kind::Texture, Name, Error, Shader)))
					{
						return false;
					}
					if (Field == 1)
					{
						Shader = Name;
					}
				}
			}
		}
	}
	else if (Type == 0)
	{
		if (Input.Find(Body, 0x0005, Data))
		{
			return Fail(Error, "Old open-group references cannot be checked completely.");
		}
		return ReadList(Input, Body, 0x0001, Result, Error, Depth + 1);
	}
	else if (Type == 1 || Type == 3)
	{
		if (Input.Find(Body, Type == 1 ? 0xc415 : 0xb439, Data))
		{
			return Input.String(Data, Name) && Add(Result.References, Kind::Texture, Name, Error);
		}
	}
	return Error.empty();
}

static bool ReadList(Reader& Input, Range Area, u32 Id, Scene& Result, xr_string& Error, size_t Depth)
{
	Range List;
	if (!Input.Find(Area, Id, List))
	{
		return Error.empty();
	}
	Result.HasObjectData = true;
	const auto EntityLambda = [&](u32 Class, Range Entity)
	{
		if (Class & 0x80000000)
		{
			return Fail(Error, "Compressed scene entries cannot be checked completely.");
		}
		return ReadEntity(Input, Entity, Result, Error, Depth);
	};
	return Input.Chunks(List, EntityLambda);
}

static bool ReadBinaryScene(Reader& Input, Scene& Result, xr_string& Error)
{
	bool HasTool = false;
	if (!ReadList(Input, Input.Root, 0x7708, Result, Error, 0))
	{
		return false;
	}
	for (u32 Type : {0u, 1u, 2u, 3u, 12u})
	{
		Range Tool;
		if (!Input.Find(Input.Root, 0x8000 + Type, Tool))
		{
			if (!Error.empty())
			{
				return false;
			}
			continue;
		}
		HasTool = true;
		if (Type != 12)
		{
			if (!ReadList(Input, Tool, 0x0003, Result, Error, 0))
			{
				return false;
			}
		}
		else
		{
			Range Models;
			if (Input.Find(Tool, 0x0001, Models))
			{
				Result.HasObjectData = true;
				const auto DetailLambda = [&](u32 Id, Range Model)
				{
					if (Id & 0x80000000)
					{
						return Fail(Error, "Compressed detail entries cannot be checked completely.");
					}
					Range Reference;
					xr_string Name;
					return Input.Find(Model, 0x0101, Reference) && Input.String(Reference, Name) && Add(Result.References, Kind::Object, Name, Error);
				};
				if (!Input.Chunks(Models, DetailLambda))
				{
					return false;
				}
			}
		}
	}
	Range Version;
	if (!HasTool && !Result.HasObjectData && !Input.Find(Input.Root, 0x9df3, Version))
	{
		return Fail(Error, "Unrecognized binary scene format.");
	}
	return Error.empty();
}
} // namespace Details

bool ReadScene(const xr_path& Path, Scene& Result, xr_string& Error, std::stop_token Stop)
{
	Result = {};
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Dependency check cancelled.");
	}
	std::ifstream Header(Path, std::ios::binary);
	char First = 0;
	bool HasMarker = false;
	for (size_t Index = 0; Index < 4096; ++Index)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Dependency check cancelled.");
		}
		if (!Header.get(First))
		{
			break;
		}
		if (First != ' ' && First != '\t' && First != '\r' && First != '\n')
		{
			HasMarker = true;
			break;
		}
	}
	if (!HasMarker)
	{
		return Details::Fail(Error, "Cannot identify the scene format within the 4096-byte header limit.");
	}
	if (First != '[' && static_cast<u8>(First) != 0xef)
	{
		Details::Reader Input(Path, Error, Stop);
		return Input.IsValid() && Details::ReadBinaryScene(Input, Result, Error);
	}
	xr_hash_set<xr_string> SeenSections;
	xr_vector<std::pair<xr_string, u32>> DeclaredLists;
	size_t DeclaredEntries = 0;
	const auto DeclareLambda = [&](const xr_string& Prefix, const xr_string& Value)
	{
		u32 Count;
		if (!Details::Number(Value, Count) || Count > 100000 || DeclaredEntries + Count > 100000)
		{
			return Details::Fail(Error, "Invalid or oversized declared scene object list.");
		}
		DeclaredEntries += Count;
		DeclaredLists.emplace_back(Prefix, Count);
		Result.HasObjectData = true;
		return true;
	};
	const auto SectionLambda = [&](const xr_string& Section, const Details::Fields& Values)
	{
		if (SeenSections.size() >= 100000 || !SeenSections.insert(Section).second)
		{
			return Details::Fail(Error, "Duplicate scene section or section limit exceeded.");
		}
		const auto GetLambda = [&](const char* Key) -> xr_string
		{
			const auto Iter = Values.find(Key);
			return Iter != Values.end() ? Iter->second : xr_string{};
		};
		if (Section == "main" && Values.contains("objects_count"))
		{
			if (!DeclareLambda("object", GetLambda("objects_count")))
			{
				return false;
			}
		}
		if (Section == "main" && Values.contains("detail_objects_count") && !DeclareLambda("detail_object", GetLambda("detail_objects_count")))
		{
			return false;
		}
		for (const char* Prefix : {"ingroup", "attached"})
		{
			const xr_string Key = xr_string(Prefix) + "_count";
			const xr_string Name = Section + "_" + Prefix;
			if (Section.starts_with("object_") && Values.contains(Key) && !DeclareLambda(Name, GetLambda(Key.c_str())))
			{
				return false;
			}
		}
		if (Section.starts_with("object_") && GetLambda("clsid") == "2")
		{
			Result.HasObjectData = true;
			if (GetLambda("reference_name").empty())
			{
				return Details::Fail(Error, "A scene object has no reference_name.");
			}
			if (!Details::Add(Result.References, Kind::Object, GetLambda("reference_name"), Error))
			{
				return false;
			}
			u32 Flags = 0;
			Details::Number(GetLambda("flags"), Flags);
			if (Flags & 1)
			{
				u32 Count;
				if (!Details::Number(GetLambda("000001"), Count) || Count > 65536)
				{
					return Details::Fail(Error, "Invalid scene surface overrides.");
				}
				for (u32 Index = 0; Index < Count; ++Index)
				{
					char Key[16];
					xr_sprintf(Key, "%06u", 6 + Index * 6);
					char ShaderKey[16];
					xr_sprintf(ShaderKey, "%06u", 3 + Index * 6);
					if (!Values.contains(Key) || !Details::Add(Result.References, Kind::Texture, GetLambda(Key), Error, GetLambda(ShaderKey)))
					{
						return Details::Fail(Error, "A surface override texture is missing or unreadable.");
					}
				}
			}
		}
		if (Section.starts_with("object_") && (GetLambda("clsid") == "1" || GetLambda("clsid") == "3"))
		{
			return Details::Add(Result.References, Kind::Texture, GetLambda(GetLambda("clsid") == "1" ? "texture_name" : "fallof_texture"), Error);
		}
		if (Section.starts_with("detail_object_") && Values.contains("reference"))
		{
			Result.HasObjectData = true;
			return Details::Add(Result.References, Kind::Object, GetLambda("reference"), Error);
		}
		return true;
	};
	if (!Details::ReadLtx(Path, SectionLambda, Error, Stop))
	{
		return false;
	}
	for (const auto& [Prefix, Count] : DeclaredLists)
	{
		for (u32 Index = 0; Index < Count; ++Index)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			const xr_string Entry = Prefix + "_" + std::to_string(Index).c_str();
			if (!SeenSections.contains(Entry))
			{
				return Details::Fail(Error, ("Declared scene entry is missing: " + Entry).c_str());
			}
		}
	}
	return true;
}

bool ReadObject(const xr_path& Path, xr_vector<Reference>& Result, xr_string& Error, std::stop_token Stop)
{
	Result.clear();
	Error.clear();
	Details::Reader Input(Path, Error, Stop);
	Details::Range Body, Data, Version;
	u32 Number;
	if (!Input.IsValid() || !Input.Find(Input.Root, 0x7777, Body) || !Input.Find(Body, 0x0900, Version) || !Input.Integer(Version, Number, 2) || Number != 0x10)
	{
		return Details::Fail(Error, "Unreadable or unsupported editable object version.");
	}
	u32 Format = 0;
	for (u32 Candidate : {0x0908u, 0x0907u, 0x0906u, 0x0905u})
	{
		if (Input.Find(Body, Candidate, Data))
		{
			Format = Candidate;
			break;
		}
		if (!Error.empty())
		{
			return false;
		}
	}
	u32 Count;
	if (!Format || !Input.Integer(Data, Count) || Count > 65536)
	{
		return Details::Fail(Error, "Missing or invalid object surface data.");
	}
	for (u32 Index = 0; Index < Count; ++Index)
	{
		xr_string Name;
		xr_string Shader;
		u32 IsShared = 0;
		if (Format == 0x0908 && !Input.Integer(Data, IsShared, 1))
		{
			return false;
		}
		if (IsShared)
		{
			if (!Input.String(Data, Name) || !Details::Add(Result, Kind::SharedMaterial, Name, Error) || !Input.String(Data, Name))
			{
				return false;
			}
		}
		else if (Format != 0x0905)
		{
			const u32 Fields = Format == 0x0906 ? 5 : 6;
			for (u32 Field = 0; Field < Fields; ++Field)
			{
				if (!Input.String(Data, Name) || (Field == Fields - 2 && !Details::Add(Result, Kind::Texture, Name, Error, Shader)))
				{
					return false;
				}
				if (Field == 1)
				{
					Shader = Name;
				}
			}
		}
		else
		{
			if (!Input.String(Data, Name) || !Input.String(Data, Shader) || !Input.Integer(Data, Number, 1) || !Input.Integer(Data, Number) || !Input.Integer(Data, Number) || Number != 1 || !Input.String(Data, Name) || !Details::Add(Result, Kind::Texture, Name, Error, Shader) || !Input.String(Data, Name))
			{
				return Details::Fail(Error, "Unsupported legacy surface data.");
			}
			continue;
		}
		for (u32 Field = 0; Field < 3; ++Field)
		{
			if (!Input.Integer(Data, Number))
			{
				return false;
			}
		}
		if (Number != 1)
		{
			return Details::Fail(Error, "Unsupported surface texture coordinate count.");
		}
	}
	return true;
}

bool ReadMaterials(const xr_path& Path, xr_hash_map<xr_string, Reference>& Result, xr_string& Error, std::stop_token Stop)
{
	Result.clear();
	Error.clear();
	const auto SectionLambda = [&](const xr_string&, const Details::Fields& Fields)
	{
		const auto Name = Fields.find("nameraw"), Texture = Fields.find("texture");
		if (Name != Fields.end() && Texture != Fields.end())
		{
			const auto Shader = Fields.find("shader");
			Result[Name->second] = {Kind::Texture, Texture->second, Shader != Fields.end() ? Shader->second : xr_string{}};
		}
		return true;
	};
	return Details::ReadLtx(Path, SectionLambda, Error, Stop);
}

bool ReadTerrainShaders(const xr_path& Path, xr_hash_set<xr_string>& Result, xr_string& Error, std::stop_token Stop)
{
	Result.clear();
	Error.clear();
	Details::Reader Input(Path, Error, Stop);
	Details::Range Blenders;
	if (!Input.IsValid() || !Input.Find(Input.Root, 2, Blenders))
	{
		return Details::Fail(Error, "Cannot read shader template descriptions.");
	}
	const auto ShaderLambda = [&](u32 Id, Details::Range Data)
	{
		if (Id & 0x80000000)
		{
			return Details::Fail(Error, "Compressed shader descriptions cannot be checked.");
		}
		xr_array<char, 8> Class{};
		xr_array<char, 128> Name{};
		// CBlender_DESC begins with a little-endian CLASS_ID and a fixed 128-byte name.
		if (!Input.Read(Data, Class.data(), Class.size()) || !Input.Read(Data, Name.data(), Name.size()))
		{
			return false;
		}
		const auto End = std::find(Name.begin(), Name.end(), '\0');
		if (End == Name.end())
		{
			return Details::Fail(Error, "Unterminated shader template name.");
		}
		if (std::equal(Class.begin(), Class.end(), " dloDmmB"))
		{
			xr_string Text(Name.data(), static_cast<u32>(End - Name.begin()));
			std::replace(Text.begin(), Text.end(), '\\', '/');
			Result.insert(SdkPackages::Details::Lower(Text));
		}
		return true;
	};
	return Input.Chunks(Blenders, ShaderLambda);
}
} // namespace SdkPackages::DependencyFormats
