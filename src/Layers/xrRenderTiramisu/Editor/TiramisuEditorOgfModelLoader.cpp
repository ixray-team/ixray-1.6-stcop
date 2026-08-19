#include "TiramisuEditorOgfModelLoader.h"

#include "../../../xrEngine/Fmesh.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <functional>
#include <limits>

namespace
{
constexpr u32 FvfPositionMask = 0x400eu;
constexpr u32 FvfXyz = 0x0002u;
constexpr u32 FvfNormal = 0x0010u;
constexpr u32 FvfPointSize = 0x0020u;
constexpr u32 FvfDiffuse = 0x0040u;
constexpr u32 FvfSpecular = 0x0080u;
constexpr u32 FvfTextureCountMask = 0x0f00u;
constexpr u32 FvfTextureCountShift = 8u;
constexpr u32 FvfTextureFormatShift = 16u;
constexpr u32 MaxOgfVertexCount = 1u << 24u;
constexpr u32 MaxOgfIndexCount = 1u << 26u;
constexpr u32 MaxOgfHierarchyDepth = 32u;
constexpr u32 MaxOgfBoneCount = 1024u;
constexpr size_t MaxOgfBoneNameLength = 1024u;
constexpr size_t SerializedBoneShapeSize =
	4u + sizeof(Fobb) + sizeof(Fsphere) + sizeof(Fcylinder);
constexpr size_t SerializedJointLimitSize =
	sizeof(Fvector2) + 2u * sizeof(float);
constexpr size_t SerializedIkDataVersion0Size =
	sizeof(u32) + 3u * SerializedJointLimitSize +
	5u * sizeof(float);
constexpr size_t SerializedIkDataVersion1Size =
	SerializedIkDataVersion0Size + sizeof(float);
constexpr u16 InvalidBoneIndex = std::numeric_limits<u16>::max();

static_assert(sizeof(Fobb) == 60u);
static_assert(sizeof(Fsphere) == 16u);
static_assert(sizeof(Fcylinder) == 32u);
static_assert(SerializedBoneShapeSize == 112u);
static_assert(SerializedIkDataVersion0Size == 72u);
static_assert(SerializedIkDataVersion1Size == 76u);

#pragma pack(push, 2)
struct FSkeletalVertex1W
{
	Fvector Position;
	Fvector Normal;
	Fvector Tangent;
	Fvector Binormal;
	float U = 0.0f;
	float V = 0.0f;
	u32 Bone = 0;
};

struct FSkeletalVertex2W
{
	u16 Bones[2] = {};
	Fvector Position;
	Fvector Normal;
	Fvector Tangent;
	Fvector Binormal;
	float Weight = 0.0f;
	float U = 0.0f;
	float V = 0.0f;
};

struct FSkeletalVertex3W
{
	u16 Bones[3] = {};
	Fvector Position;
	Fvector Normal;
	Fvector Tangent;
	Fvector Binormal;
	float Weights[2] = {};
	float U = 0.0f;
	float V = 0.0f;
};

struct FSkeletalVertex4W
{
	u16 Bones[4] = {};
	Fvector Position;
	Fvector Normal;
	Fvector Tangent;
	Fvector Binormal;
	float Weights[3] = {};
	float U = 0.0f;
	float V = 0.0f;
};
#pragma pack(pop)

static_assert(sizeof(FSkeletalVertex1W) == 60);
static_assert(sizeof(FSkeletalVertex2W) == 64);
static_assert(sizeof(FSkeletalVertex3W) == 70);
static_assert(sizeof(FSkeletalVertex4W) == 76);

struct FScopedReader
{
	IReader* Reader = nullptr;

	explicit FScopedReader(IReader* InReader) : Reader(InReader) {}
	~FScopedReader()
	{
		if (Reader)
		{
			Reader->close();
		}
	}

	IReader* operator->() const { return Reader; }
	IReader& operator*() const { return *Reader; }
	explicit operator bool() const { return Reader != nullptr; }
};

void SetFailure(
	FTiramisuEditorOgfModelSource& Model,
	const ETiramisuEditorOgfLoadStatus Status,
	const char* Diagnostic
)
{
	Model.Status = Status;
	Model.Diagnostic = Diagnostic;
}

[[nodiscard]] bool IsFinite(const Fmatrix& Matrix)
{
	for (const float Value : Matrix.mm)
	{
		if (!std::isfinite(Value))
		{
			return false;
		}
	}
	return true;
}

[[nodiscard]] bool ReadBoundedString(
	IReader& Reader,
	xr_string& OutValue,
	const size_t MaxLength
)
{
	const size_t Available = Reader.elapsed();
	const size_t SearchLength = std::min(Available, MaxLength + 1u);
	const auto* Begin = static_cast<const char*>(Reader.pointer());
	const auto* End = static_cast<const char*>(
		std::memchr(Begin, 0, SearchLength)
	);
	if (!End)
	{
		return false;
	}
	const size_t Length = static_cast<size_t>(End - Begin);
	OutValue.assign(Begin, Length);
	Reader.advance(static_cast<intptr_t>(Length + 1u));
	return true;
}

[[nodiscard]] bool LoadSkeletonBones(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model
)
{
	FScopedReader BoneNames(Visual.open_chunk(OGF_S_BONE_NAMES));
	if (!BoneNames || BoneNames->elapsed() < sizeof(u32))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF has no valid bone names");
		return false;
	}
	const u32 BoneCount = BoneNames->r_u32();
	if (BoneCount == 0 || BoneCount > MaxOgfBoneCount ||
		BoneCount > InvalidBoneIndex)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF bone count is invalid");
		return false;
	}
	Model.Bones.resize(BoneCount);
	xr_vector<xr_string> ParentNames(BoneCount);
	xr_hash_map<xr_string, u16> BoneIndicesByName;
	BoneIndicesByName.reserve(BoneCount);
	for (u32 Index = 0; Index < BoneCount; ++Index)
	{
		FTiramisuEditorOgfBoneSource& Bone = Model.Bones[Index];
		if (!ReadBoundedString(
				*BoneNames, Bone.Name, MaxOgfBoneNameLength
			) || Bone.Name.empty() ||
			!ReadBoundedString(
				*BoneNames, ParentNames[Index], MaxOgfBoneNameLength
			) || BoneNames->elapsed() < sizeof(Fobb))
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF bone record is truncated");
			return false;
		}
		xr_strlwr(Bone.Name);
		xr_strlwr(ParentNames[Index]);
		Bone.BindLocal.identity();
		Bone.BindModel.identity();
		Bone.ModelToBone.identity();
		BoneNames->advance(sizeof(Fobb));
		if (!BoneIndicesByName.try_emplace(
				Bone.Name, static_cast<u16>(Index)
			).second)
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF has duplicate bone names");
			return false;
		}
	}
	u32 RootCount = 0;
	for (u32 Index = 0; Index < BoneCount; ++Index)
	{
		if (ParentNames[Index].empty())
		{
			Model.RootBoneIndex = static_cast<u16>(Index);
			++RootCount;
			continue;
		}
		const auto Parent = BoneIndicesByName.find(ParentNames[Index]);
		if (Parent == BoneIndicesByName.end() || Parent->second == Index)
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF bone parent is invalid");
			return false;
		}
		Model.Bones[Index].ParentIndex = Parent->second;
	}
	if (RootCount != 1)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF must have exactly one root bone");
		return false;
	}
	return true;
}

[[nodiscard]] bool LoadSkeletonBindTransforms(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model
)
{
	FScopedReader IkData(Visual.open_chunk(OGF_S_IKDATA));
	if (!IkData)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF has no bind transforms");
		return false;
	}
	for (FTiramisuEditorOgfBoneSource& Bone : Model.Bones)
	{
		if (IkData->elapsed() < sizeof(u32))
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF IK data is truncated");
			return false;
		}
		const u32 Version = IkData->r_u32();
		xr_string GameMaterial;
		if (Version > 1u || !ReadBoundedString(
				*IkData, GameMaterial, MaxOgfBoneNameLength
			))
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF IK version is unsupported");
			return false;
		}
		const size_t IkRecordSize = Version > 0u
			? SerializedIkDataVersion1Size
			: SerializedIkDataVersion0Size;
		const size_t RequiredBytes = SerializedBoneShapeSize +
			IkRecordSize + 3u * sizeof(Fvector) + sizeof(float);
		if (IkData->elapsed() < RequiredBytes)
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF IK payload is truncated");
			return false;
		}
		IkData->advance(SerializedBoneShapeSize + IkRecordSize);
		Fvector Rotation;
		Fvector Translation;
		IkData->r_fvector3(Rotation);
		IkData->r_fvector3(Translation);
		IkData->advance(sizeof(float) + sizeof(Fvector));
		if (!std::isfinite(Rotation.x) ||
			!std::isfinite(Rotation.y) ||
			!std::isfinite(Rotation.z) ||
			!std::isfinite(Translation.x) ||
			!std::isfinite(Translation.y) ||
			!std::isfinite(Translation.z))
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF bind transform is invalid");
			return false;
		}
		Bone.BindLocal.setXYZi(Rotation);
		Bone.BindLocal.translate_over(Translation);
	}
	xr_vector<u8> VisitState(Model.Bones.size(), 0u);
	std::function<bool(u16)> BuildBone = [&](const u16 BoneIndex)
	{
		if (VisitState[BoneIndex] == 2u)
		{
			return true;
		}
		if (VisitState[BoneIndex] == 1u)
		{
			return false;
		}
		VisitState[BoneIndex] = 1u;
		FTiramisuEditorOgfBoneSource& Bone = Model.Bones[BoneIndex];
		if (Bone.ParentIndex == InvalidBoneIndex)
		{
			Bone.BindModel = Bone.BindLocal;
		}
		else
		{
			if (!BuildBone(Bone.ParentIndex))
			{
				return false;
			}
			Bone.BindModel.mul_43(
				Model.Bones[Bone.ParentIndex].BindModel,
				Bone.BindLocal
			);
		}
		if (!IsFinite(Bone.BindModel))
		{
			return false;
		}
		Bone.ModelToBone.invert(Bone.BindModel);
		VisitState[BoneIndex] = 2u;
		return IsFinite(Bone.ModelToBone);
	};
	for (u16 Index = 0; Index < Model.Bones.size(); ++Index)
	{
		if (!BuildBone(Index))
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Skeletal standalone OGF bone hierarchy is cyclic");
			return false;
		}
	}
	return true;
}

[[nodiscard]] bool LoadSkeletonMetadata(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model
)
{
	return LoadSkeletonBones(Visual, Model) &&
		LoadSkeletonBindTransforms(Visual, Model);
}

[[nodiscard]] bool ValidateSkinBindings(
	FTiramisuEditorOgfModelSource& Model
)
{
	if (Model.Bones.empty())
	{
		return true;
	}
	for (const FTiramisuEditorOgfMeshSource& Mesh : Model.Meshes)
	{
		for (const FTiramisuEditorOgfSkinBinding& Binding :
			 Mesh.SkinBindings)
		{
			for (size_t Influence = 0;
				 Influence < Binding.Weights.size(); ++Influence)
			{
				if (Binding.Weights[Influence] > 0.0f &&
					Binding.BoneIndices[Influence] >= Model.Bones.size())
				{
					SetFailure(Model,
						ETiramisuEditorOgfLoadStatus::InvalidData,
						"Skeletal standalone OGF bone index is out of range");
					return false;
				}
			}
		}
	}
	return true;
}

[[nodiscard]] bool LoadSkeletonMotions(
	IReader& Reader,
	FTiramisuEditorOgfModelSource& Model
)
{
	if (Model.Bones.empty())
	{
		return true;
	}
	xr_string MotionDiagnostic;
	if (!LoadTiramisuEditorOgfMotionReferences(
			Reader, Model.MotionReferences, &MotionDiagnostic
		))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			MotionDiagnostic.c_str());
		return false;
	}
	const bool HasParameters =
		Reader.find_chunk(OGF_S_SMPARAMS) != 0;
	const bool HasKeys = Reader.find_chunk(OGF_S_MOTIONS) != 0;
	if (!HasParameters && !HasKeys)
	{
		return true;
	}
	if (HasParameters != HasKeys)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF has incomplete embedded motions");
		return false;
	}
	xr_vector<xr_string> BoneNames;
	BoneNames.reserve(Model.Bones.size());
	for (const FTiramisuEditorOgfBoneSource& Bone : Model.Bones)
	{
		BoneNames.push_back(Bone.Name);
	}
	if (!LoadTiramisuEditorOgfMotions(
			Reader, BoneNames, Model.EmbeddedMotions
		))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			Model.EmbeddedMotions.Diagnostic.c_str());
		return false;
	}
	return true;
}

[[nodiscard]] u32 TextureCoordinateDimension(
	const u32 Fvf,
	const u32 CoordinateIndex
)
{
	const u32 Code =
		(Fvf >> (FvfTextureFormatShift + CoordinateIndex * 2u)) & 3u;
	constexpr u32 Dimensions[] = {2u, 3u, 4u, 1u};
	return Dimensions[Code];
}

void Normalize3(xr_array<float, 3>& Value)
{
	const float LengthSquared =
		Value[0] * Value[0] + Value[1] * Value[1] +
		Value[2] * Value[2];
	if (!std::isfinite(LengthSquared) || LengthSquared <= 1.0e-12f)
	{
		Value = {0.0f, 1.0f, 0.0f};
		return;
	}
	const float Scale = 1.0f / std::sqrt(LengthSquared);
	Value[0] *= Scale;
	Value[1] *= Scale;
	Value[2] *= Scale;
}

void BuildTangents(FTiramisuEditorOgfMeshSource& Mesh)
{
	xr_vector<xr_array<float, 3>> Tangents(Mesh.Vertices.size());
	for (size_t Index = 0; Index + 2 < Mesh.Indices.size(); Index += 3)
	{
		const u32 I0 = Mesh.Indices[Index];
		const u32 I1 = Mesh.Indices[Index + 1];
		const u32 I2 = Mesh.Indices[Index + 2];
		const auto& V0 = Mesh.Vertices[I0];
		const auto& V1 = Mesh.Vertices[I1];
		const auto& V2 = Mesh.Vertices[I2];
		const xr_array<float, 3> Edge1 = {
			V1.Position[0] - V0.Position[0],
			V1.Position[1] - V0.Position[1],
			V1.Position[2] - V0.Position[2]
		};
		const xr_array<float, 3> Edge2 = {
			V2.Position[0] - V0.Position[0],
			V2.Position[1] - V0.Position[1],
			V2.Position[2] - V0.Position[2]
		};
		const float Du1 = V1.TexCoord[0] - V0.TexCoord[0];
		const float Dv1 = V1.TexCoord[1] - V0.TexCoord[1];
		const float Du2 = V2.TexCoord[0] - V0.TexCoord[0];
		const float Dv2 = V2.TexCoord[1] - V0.TexCoord[1];
		const float Denominator = Du1 * Dv2 - Dv1 * Du2;
		if (!std::isfinite(Denominator) ||
			std::abs(Denominator) <= 1.0e-12f)
		{
			continue;
		}
		const float Scale = 1.0f / Denominator;
		const xr_array<float, 3> Tangent = {
			(Edge1[0] * Dv2 - Edge2[0] * Dv1) * Scale,
			(Edge1[1] * Dv2 - Edge2[1] * Dv1) * Scale,
			(Edge1[2] * Dv2 - Edge2[2] * Dv1) * Scale
		};
		for (const u32 VertexIndex : {I0, I1, I2})
		{
			for (size_t Axis = 0; Axis < 3; ++Axis)
			{
				Tangents[VertexIndex][Axis] += Tangent[Axis];
			}
		}
	}
	for (size_t Index = 0; Index < Mesh.Vertices.size(); ++Index)
	{
		Normalize3(Tangents[Index]);
		Mesh.Vertices[Index].Tangent = {
			Tangents[Index][0],
			Tangents[Index][1],
			Tangents[Index][2],
			1.0f
		};
	}
}

[[nodiscard]] bool LoadTextureSource(
	IReader& Visual,
	FTiramisuEditorOgfMeshSource& Mesh
)
{
	FScopedReader Texture(Visual.open_chunk(OGF_TEXTURE));
	if (!Texture)
	{
		Mesh.TextureName = "textures/default/default_white";
		Mesh.ShaderName = "default";
		return true;
	}
	Texture->r_stringZ(Mesh.TextureName);
	Texture->r_stringZ(Mesh.ShaderName);
	if (Mesh.TextureName.empty())
	{
		Mesh.TextureName = "textures/default/default_white";
	}
	if (Mesh.ShaderName.empty())
	{
		Mesh.ShaderName = "default";
	}
	return true;
}

[[nodiscard]] bool LoadStaticVertices(
	IReader& Visual,
	FTiramisuEditorOgfMeshSource& Mesh,
	FTiramisuEditorOgfModelSource& Model
)
{
	FScopedReader Vertices(Visual.open_chunk(OGF_VERTICES));
	if (!Vertices || Vertices->elapsed() < 2 * sizeof(u32))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF has no valid vertex chunk");
		return false;
	}
	const u32 Fvf = Vertices->r_u32();
	const u32 VertexCount = Vertices->r_u32();
	if ((Fvf & FvfPositionMask) != FvfXyz ||
		(Fvf & FvfNormal) == 0 || VertexCount == 0 ||
		VertexCount > MaxOgfVertexCount)
	{
		SetFailure(Model,
			ETiramisuEditorOgfLoadStatus::UnsupportedVertexFormat,
			"Standalone OGF uses an unsupported static vertex format");
		return false;
	}
	const u32 TextureCount =
		(Fvf & FvfTextureCountMask) >> FvfTextureCountShift;
	if (TextureCount == 0 || TextureCount > 8)
	{
		SetFailure(Model,
			ETiramisuEditorOgfLoadStatus::UnsupportedVertexFormat,
			"Standalone OGF has no supported texture coordinates");
		return false;
	}
	u32 Stride = 3u * sizeof(float);
	const u32 NormalOffset = Stride;
	Stride += 3u * sizeof(float);
	if ((Fvf & FvfPointSize) != 0)
	{
		Stride += sizeof(float);
	}
	const bool HasDiffuse = (Fvf & FvfDiffuse) != 0;
	const u32 DiffuseOffset = Stride;
	if (HasDiffuse)
	{
		Stride += sizeof(u32);
	}
	if ((Fvf & FvfSpecular) != 0)
	{
		Stride += sizeof(u32);
	}
	const u32 TexCoordOffset = Stride;
	for (u32 Coordinate = 0; Coordinate < TextureCount; ++Coordinate)
	{
		Stride += TextureCoordinateDimension(Fvf, Coordinate) *
			sizeof(float);
	}
	const u64 RequiredBytes = u64(VertexCount) * Stride;
	if (RequiredBytes > static_cast<u64>(Vertices->elapsed()))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF vertex payload is truncated");
		return false;
	}
	const auto* Source = static_cast<const u8*>(Vertices->pointer());
	Mesh.Vertices.resize(VertexCount);
	for (u32 Index = 0; Index < VertexCount; ++Index)
	{
		const u8* VertexSource = Source + u64(Index) * Stride;
		FEditorStaticMeshVertex& Vertex = Mesh.Vertices[Index];
		std::memcpy(Vertex.Position.data(), VertexSource,
			3 * sizeof(float));
		std::memcpy(Vertex.Normal.data(), VertexSource + NormalOffset,
			3 * sizeof(float));
		Normalize3(Vertex.Normal);
		if (HasDiffuse)
		{
			std::memcpy(&Vertex.Color, VertexSource + DiffuseOffset,
				sizeof(u32));
		}
		std::memcpy(Vertex.TexCoord.data(),
			VertexSource + TexCoordOffset, 2 * sizeof(float));
	}
	return true;
}

[[nodiscard]] bool LoadStaticIndices(
	IReader& Visual,
	FTiramisuEditorOgfMeshSource& Mesh,
	FTiramisuEditorOgfModelSource& Model
)
{
	FScopedReader Indices(Visual.open_chunk(OGF_INDICES));
	if (!Indices || Indices->elapsed() < sizeof(u32))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF has no valid index chunk");
		return false;
	}
	const u32 IndexCount = Indices->r_u32();
	if (IndexCount == 0 || IndexCount % 3 != 0 ||
		IndexCount > MaxOgfIndexCount ||
		u64(IndexCount) * sizeof(u16) >
			static_cast<u64>(Indices->elapsed()))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF index payload is invalid");
		return false;
	}
	Mesh.Indices.resize(IndexCount);
	for (u32 Index = 0; Index < IndexCount; ++Index)
	{
		const u16 SourceIndex = Indices->r_u16();
		if (SourceIndex >= Mesh.Vertices.size())
		{
			SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
				"Standalone OGF index is out of range");
			return false;
		}
		Mesh.Indices[Index] = SourceIndex;
	}
	return true;
}

[[nodiscard]] bool CopySkinBinding(
	const FSkeletalVertex1W& Source,
	FTiramisuEditorOgfSkinBinding& Destination
)
{
	if (Source.Bone > std::numeric_limits<u16>::max())
	{
		return false;
	}
	Destination.BoneIndices[0] = static_cast<u16>(Source.Bone);
	return true;
}

[[nodiscard]] bool CopySkinBinding(
	const FSkeletalVertex2W& Source,
	FTiramisuEditorOgfSkinBinding& Destination
)
{
	if (!std::isfinite(Source.Weight) ||
		Source.Weight < 0.0f || Source.Weight > 1.0f)
	{
		return false;
	}
	Destination.BoneIndices = {
		Source.Bones[0], Source.Bones[1], 0, 0
	};
	Destination.Weights = {
		1.0f - Source.Weight, Source.Weight, 0.0f, 0.0f
	};
	return true;
}

[[nodiscard]] bool CopySkinBinding(
	const FSkeletalVertex3W& Source,
	FTiramisuEditorOgfSkinBinding& Destination
)
{
	const float LastWeight =
		1.0f - Source.Weights[0] - Source.Weights[1];
	if (!std::isfinite(Source.Weights[0]) ||
		!std::isfinite(Source.Weights[1]) ||
		!std::isfinite(LastWeight) || Source.Weights[0] < 0.0f ||
		Source.Weights[1] < 0.0f || LastWeight < -1.0e-5f)
	{
		return false;
	}
	Destination.BoneIndices = {
		Source.Bones[0], Source.Bones[1], Source.Bones[2], 0
	};
	Destination.Weights = {
		Source.Weights[0],
		Source.Weights[1],
		std::max(0.0f, LastWeight),
		0.0f
	};
	return true;
}

[[nodiscard]] bool CopySkinBinding(
	const FSkeletalVertex4W& Source,
	FTiramisuEditorOgfSkinBinding& Destination
)
{
	const float LastWeight = 1.0f - Source.Weights[0] -
		Source.Weights[1] - Source.Weights[2];
	if (!std::isfinite(Source.Weights[0]) ||
		!std::isfinite(Source.Weights[1]) ||
		!std::isfinite(Source.Weights[2]) ||
		!std::isfinite(LastWeight) || Source.Weights[0] < 0.0f ||
		Source.Weights[1] < 0.0f || Source.Weights[2] < 0.0f ||
		LastWeight < -1.0e-5f)
	{
		return false;
	}
	Destination.BoneIndices = {
		Source.Bones[0], Source.Bones[1],
		Source.Bones[2], Source.Bones[3]
	};
	Destination.Weights = {
		Source.Weights[0],
		Source.Weights[1],
		Source.Weights[2],
		std::max(0.0f, LastWeight)
	};
	return true;
}

template <typename TVertex>
[[nodiscard]] bool CopySkeletalVertices(
	const TVertex* Source,
	const u32 VertexCount,
	FTiramisuEditorOgfMeshSource& Mesh
)
{
	Mesh.Vertices.resize(VertexCount);
	Mesh.SkinBindings.resize(VertexCount);
	for (u32 Index = 0; Index < VertexCount; ++Index)
	{
		if (!CopySkinBinding(Source[Index], Mesh.SkinBindings[Index]))
		{
			return false;
		}
		FEditorStaticMeshVertex& Destination = Mesh.Vertices[Index];
		Destination.Position = {
			Source[Index].Position.x,
			Source[Index].Position.y,
			Source[Index].Position.z
		};
		Destination.Normal = {
			Source[Index].Normal.x,
			Source[Index].Normal.y,
			Source[Index].Normal.z
		};
		Normalize3(Destination.Normal);
		xr_array<float, 3> Tangent = {
			Source[Index].Tangent.x,
			Source[Index].Tangent.y,
			Source[Index].Tangent.z
		};
		Normalize3(Tangent);
		Destination.Tangent = {
			Tangent[0], Tangent[1], Tangent[2], 1.0f
		};
		Destination.TexCoord = {Source[Index].U, Source[Index].V};
	}
	return true;
}

[[nodiscard]] bool LoadSkeletalVertices(
	IReader& Visual,
	FTiramisuEditorOgfMeshSource& Mesh,
	FTiramisuEditorOgfModelSource& Model
)
{
	FScopedReader Vertices(Visual.open_chunk(OGF_VERTICES));
	if (!Vertices || Vertices->elapsed() < 2 * sizeof(u32))
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF has no valid vertex chunk");
		return false;
	}
	const u32 VertexFormat = Vertices->r_u32();
	const u32 VertexCount = Vertices->r_u32();
	if (VertexCount == 0 || VertexCount > MaxOgfVertexCount)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Skeletal standalone OGF vertex count is invalid");
		return false;
	}
	const void* Source = Vertices->pointer();
	auto ValidatePayload = [&](const size_t Stride)
	{
		return u64(VertexCount) * Stride <=
			static_cast<u64>(Vertices->elapsed());
	};
	switch (VertexFormat)
	{
		case 1:
		case OGF_VERTEXFORMAT_FVF_1L:
			if (ValidatePayload(sizeof(FSkeletalVertex1W)))
			{
				if (!CopySkeletalVertices(
					static_cast<const FSkeletalVertex1W*>(Source),
					VertexCount, Mesh
				))
				{
					break;
				}
				return true;
			}
			break;
		case 2:
		case OGF_VERTEXFORMAT_FVF_2L:
			if (ValidatePayload(sizeof(FSkeletalVertex2W)))
			{
				if (!CopySkeletalVertices(
					static_cast<const FSkeletalVertex2W*>(Source),
					VertexCount, Mesh
				))
				{
					break;
				}
				return true;
			}
			break;
		case 3:
		case OGF_VERTEXFORMAT_FVF_3L:
			if (ValidatePayload(sizeof(FSkeletalVertex3W)))
			{
				if (!CopySkeletalVertices(
					static_cast<const FSkeletalVertex3W*>(Source),
					VertexCount, Mesh
				))
				{
					break;
				}
				return true;
			}
			break;
		case 4:
		case OGF_VERTEXFORMAT_FVF_4L:
			if (ValidatePayload(sizeof(FSkeletalVertex4W)))
			{
				if (!CopySkeletalVertices(
					static_cast<const FSkeletalVertex4W*>(Source),
					VertexCount, Mesh
				))
				{
					break;
				}
				return true;
			}
			break;
		default:
			SetFailure(Model,
				ETiramisuEditorOgfLoadStatus::UnsupportedVertexFormat,
				"Skeletal standalone OGF uses an unsupported weight format");
			return false;
	}
	SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
		"Skeletal standalone OGF vertex payload or weights are invalid");
	return false;
}

[[nodiscard]] bool ParseEmbeddedChildren(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model,
	const u32 Depth
);

[[nodiscard]] bool ParseVisual(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model,
	const u32 Depth
)
{
	if (Depth > MaxOgfHierarchyDepth)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF hierarchy is too deep");
		return false;
	}
	ogf_header Header = {};
	if (!Visual.r_chunk_safe(OGF_HEADER, &Header, sizeof(Header)) ||
		Header.format_version != xrOGF_FormatVersion)
	{
		SetFailure(Model, ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF header is invalid");
		return false;
	}
	switch (Header.type)
	{
		case MT_NORMAL:
		case MT_PROGRESSIVE:
		{
			FTiramisuEditorOgfMeshSource Mesh;
			if (!LoadTextureSource(Visual, Mesh) ||
				!LoadStaticVertices(Visual, Mesh, Model) ||
				!LoadStaticIndices(Visual, Mesh, Model))
			{
				return false;
			}
			BuildTangents(Mesh);
			Model.Meshes.push_back(std::move(Mesh));
			return true;
		}
		case MT_SKELETON_GEOMDEF_PM:
		case MT_SKELETON_GEOMDEF_ST:
		{
			FTiramisuEditorOgfMeshSource Mesh;
			if (!LoadTextureSource(Visual, Mesh) ||
				!LoadSkeletalVertices(Visual, Mesh, Model) ||
				!LoadStaticIndices(Visual, Mesh, Model))
			{
				return false;
			}
			Model.Meshes.push_back(std::move(Mesh));
			return true;
		}
		case MT_HIERRARHY:
			return ParseEmbeddedChildren(Visual, Model, Depth);
		case MT_SKELETON_ANIM:
		case MT_SKELETON_RIGID:
			if (Depth != 0u || !LoadSkeletonMetadata(Visual, Model))
			{
				if (Depth != 0u)
				{
					SetFailure(Model,
						ETiramisuEditorOgfLoadStatus::InvalidData,
						"Nested standalone OGF skeleton is invalid");
				}
				return false;
			}
			return ParseEmbeddedChildren(Visual, Model, Depth);
		default:
			SetFailure(Model,
				ETiramisuEditorOgfLoadStatus::UnsupportedVisualType,
				"Standalone OGF visual type is not supported by the editor model cache");
			return false;
	}
}

bool ParseEmbeddedChildren(
	IReader& Visual,
	FTiramisuEditorOgfModelSource& Model,
	const u32 Depth
)
{
	FScopedReader Children(Visual.open_chunk(OGF_CHILDREN));
	if (!Children)
	{
		SetFailure(Model,
			ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF hierarchy has no embedded children");
		return false;
	}
	const size_t FirstMesh = Model.Meshes.size();
	u32 ChildId = 0;
	IReader* Child = Children->open_chunk_iterator(ChildId);
	while (Child)
	{
		if (!ParseVisual(*Child, Model, Depth + 1))
		{
			Child->close();
			return false;
		}
		Child = Children->open_chunk_iterator(ChildId, Child);
	}
	if (Model.Meshes.size() == FirstMesh)
	{
		SetFailure(Model,
			ETiramisuEditorOgfLoadStatus::InvalidData,
			"Standalone OGF hierarchy is empty");
		return false;
	}
	return true;
}
} // namespace

bool LoadTiramisuEditorOgfModel(
	IReader& Reader,
	FTiramisuEditorOgfModelSource& OutModel
)
{
	OutModel = {};
	if (!ParseVisual(Reader, OutModel, 0))
	{
		OutModel.Meshes.clear();
		OutModel.Bones.clear();
		return false;
	}
	if (!LoadSkeletonMotions(Reader, OutModel) ||
		!ValidateSkinBindings(OutModel))
	{
		OutModel.Meshes.clear();
		OutModel.Bones.clear();
		return false;
	}
	OutModel.Status = ETiramisuEditorOgfLoadStatus::Success;
	OutModel.Diagnostic.clear();
	return true;
}

bool BuildTiramisuEditorOgfSkinningPalette(
	const FTiramisuEditorOgfModelSource& Model,
	const xr_span<const Fmatrix> LocalPose,
	xr_vector<Fmatrix>& OutPalette,
	xr_string* OutDiagnostic
)
{
	OutPalette.clear();
	if (OutDiagnostic)
	{
		OutDiagnostic->clear();
	}
	auto Fail = [&](const char* Diagnostic)
	{
		if (OutDiagnostic)
		{
			*OutDiagnostic = Diagnostic;
		}
		OutPalette.clear();
		return false;
	};
	if (Model.Bones.empty() ||
		Model.RootBoneIndex >= Model.Bones.size())
	{
		return Fail("Standalone OGF has no valid skeleton");
	}
	if (!LocalPose.empty() && LocalPose.size() != Model.Bones.size())
	{
		return Fail("Standalone OGF local pose size does not match skeleton");
	}
	OutPalette.resize(Model.Bones.size());
	xr_vector<Fmatrix> ModelPose(Model.Bones.size());
	xr_vector<u8> VisitState(Model.Bones.size(), 0u);
	std::function<bool(u16)> BuildBone = [&](const u16 BoneIndex)
	{
		if (BoneIndex >= Model.Bones.size())
		{
			return false;
		}
		if (VisitState[BoneIndex] == 2u)
		{
			return true;
		}
		if (VisitState[BoneIndex] == 1u)
		{
			return false;
		}
		VisitState[BoneIndex] = 1u;
		const FTiramisuEditorOgfBoneSource& Bone = Model.Bones[BoneIndex];
		const Fmatrix& Local = LocalPose.empty()
			? Bone.BindLocal
			: LocalPose[BoneIndex];
		if (!IsFinite(Local) || !IsFinite(Bone.ModelToBone))
		{
			return false;
		}
		if (Bone.ParentIndex == InvalidBoneIndex)
		{
			ModelPose[BoneIndex] = Local;
		}
		else
		{
			if (!BuildBone(Bone.ParentIndex))
			{
				return false;
			}
			ModelPose[BoneIndex].mul_43(
				ModelPose[Bone.ParentIndex], Local
			);
		}
		OutPalette[BoneIndex].mul_43(
			ModelPose[BoneIndex], Bone.ModelToBone
		);
		VisitState[BoneIndex] = 2u;
		return IsFinite(OutPalette[BoneIndex]);
	};
	for (size_t Index = 0; Index < Model.Bones.size(); ++Index)
	{
		if (!BuildBone(static_cast<u16>(Index)))
		{
			return Fail(
				"Standalone OGF local pose or bone hierarchy is invalid"
			);
		}
	}
	return true;
}

bool SampleTiramisuEditorOgfModelMotion(
	const FTiramisuEditorOgfModelSource& Model,
	const xr_string_view AnimationName,
	const float TimeSeconds,
	xr_vector<Fmatrix>& OutPalette,
	xr_string* OutDiagnostic
)
{
	OutPalette.clear();
	if (OutDiagnostic)
	{
		OutDiagnostic->clear();
	}
	const FTiramisuEditorOgfMotionSet* SelectedSet = nullptr;
	for (auto Slot = Model.ExternalMotions.rbegin();
		 Slot != Model.ExternalMotions.rend(); ++Slot)
	{
		const FTiramisuEditorOgfMotionClip* Clip =
			Slot->FindClip(AnimationName);
		if (Clip && !Clip->Fx)
		{
			SelectedSet = &*Slot;
			break;
		}
	}
	if (!SelectedSet)
	{
		const FTiramisuEditorOgfMotionClip* EmbeddedClip =
			Model.EmbeddedMotions.FindClip(AnimationName);
		if (EmbeddedClip && !EmbeddedClip->Fx)
		{
			SelectedSet = &Model.EmbeddedMotions;
		}
	}
	if (!SelectedSet)
	{
		if (OutDiagnostic)
		{
			*OutDiagnostic =
				"Standalone OGF startup animation is unavailable";
		}
		return false;
	}
	xr_vector<Fmatrix> LocalPose;
	return SampleTiramisuEditorOgfMotion(
			*SelectedSet,
			AnimationName,
			TimeSeconds,
			LocalPose,
			OutDiagnostic
		) && BuildTiramisuEditorOgfSkinningPalette(
			Model,
			LocalPose,
			OutPalette,
			OutDiagnostic
		);
}
