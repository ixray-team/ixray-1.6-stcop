#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorOgfModelLoader.h"
#include "../../xrEngine/Fmesh.h"

#include <cmath>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <ranges>

namespace
{
#pragma pack(push, 2)
struct FTestSkeletalVertex1W
{
	Fvector Position;
	Fvector Normal;
	Fvector Tangent;
	Fvector Binormal;
	float U = 0.0f;
	float V = 0.0f;
	u32 Bone = 0;
};
#pragma pack(pop)

static_assert(sizeof(FTestSkeletalVertex1W) == 60);

template <typename T>
void AppendValue(xr_vector<u8>& Bytes, const T& Value)
{
	const size_t Offset = Bytes.size();
	Bytes.resize(Offset + sizeof(T));
	std::memcpy(Bytes.data() + Offset, &Value, sizeof(T));
}

void AppendZeroBytes(xr_vector<u8>& Bytes, const size_t Count)
{
	Bytes.resize(Bytes.size() + Count, 0u);
}

void AppendString(xr_vector<u8>& Bytes, const xr_string_view Value)
{
	Bytes.insert(Bytes.end(), Value.begin(), Value.end());
	Bytes.push_back(0);
}

void AppendChunk(
	xr_vector<u8>& Parent,
	const u32 Id,
	const xr_vector<u8>& Payload
)
{
	AppendValue(Parent, Id);
	AppendValue(Parent, static_cast<u32>(Payload.size()));
	Parent.insert(Parent.end(), Payload.begin(), Payload.end());
}

[[nodiscard]] xr_vector<u8> MakeStaticVisual(
	const u8 Type = MT_NORMAL,
	const u32 Fvf = 0x112u,
	const bool InvalidIndex = false
)
{
	xr_vector<u8> Visual;
	ogf_header Header = {};
	Header.format_version = xrOGF_FormatVersion;
	Header.type = Type;
	Header.bb.min.set(-1.0f, -1.0f, 0.0f);
	Header.bb.max.set(1.0f, 1.0f, 0.0f);
	Header.bs.c.set(0.0f, 0.0f, 0.0f);
	Header.bs.r = 1.5f;
	xr_vector<u8> HeaderChunk;
	AppendValue(HeaderChunk, Header);
	AppendChunk(Visual, OGF_HEADER, HeaderChunk);

	xr_vector<u8> TextureChunk;
	AppendString(TextureChunk, "test\\diffuse");
	AppendString(TextureChunk, "default");
	AppendChunk(Visual, OGF_TEXTURE, TextureChunk);

	xr_vector<u8> VertexChunk;
	AppendValue(VertexChunk, Fvf);
	AppendValue(VertexChunk, 3u);
	constexpr float Vertices[3][8] = {
		{-1.0f, -1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f},
		{0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.5f, 0.0f},
		{1.0f, -1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f}
	};
	for (const auto& Vertex : Vertices)
	{
		for (const float Value : Vertex)
		{
			AppendValue(VertexChunk, Value);
		}
	}
	AppendChunk(Visual, OGF_VERTICES, VertexChunk);

	xr_vector<u8> IndexChunk;
	AppendValue(IndexChunk, 3u);
	AppendValue(IndexChunk, u16{0});
	AppendValue(IndexChunk, u16{1});
	AppendValue(IndexChunk, InvalidIndex ? u16{4} : u16{2});
	AppendChunk(Visual, OGF_INDICES, IndexChunk);
	return Visual;
}

[[nodiscard]] xr_vector<u8> MakeHierarchy()
{
	xr_vector<u8> Visual;
	ogf_header Header = {};
	Header.format_version = xrOGF_FormatVersion;
	Header.type = MT_HIERRARHY;
	xr_vector<u8> HeaderChunk;
	AppendValue(HeaderChunk, Header);
	AppendChunk(Visual, OGF_HEADER, HeaderChunk);
	xr_vector<u8> Children;
	AppendChunk(Children, 0, MakeStaticVisual());
	AppendChunk(Children, 1, MakeStaticVisual(MT_PROGRESSIVE));
	AppendChunk(Visual, OGF_CHILDREN, Children);
	return Visual;
}

[[nodiscard]] xr_vector<u8> MakeSkeletalVisual(const u32 BoneIndex)
{
	xr_vector<u8> Visual;
	ogf_header Header = {};
	Header.format_version = xrOGF_FormatVersion;
	Header.type = MT_SKELETON_GEOMDEF_ST;
	xr_vector<u8> HeaderChunk;
	AppendValue(HeaderChunk, Header);
	AppendChunk(Visual, OGF_HEADER, HeaderChunk);

	xr_vector<u8> TextureChunk;
	AppendString(TextureChunk, "test\\skinned");
	AppendString(TextureChunk, "default");
	AppendChunk(Visual, OGF_TEXTURE, TextureChunk);

	xr_vector<u8> VertexChunk;
	AppendValue(VertexChunk, 1u);
	AppendValue(VertexChunk, 3u);
	for (u32 Index = 0; Index < 3u; ++Index)
	{
		FTestSkeletalVertex1W Vertex = {};
		Vertex.Position.set(static_cast<float>(Index), 0.0f, 0.0f);
		Vertex.Normal.set(0.0f, 1.0f, 0.0f);
		Vertex.Tangent.set(1.0f, 0.0f, 0.0f);
		Vertex.Binormal.set(0.0f, 0.0f, 1.0f);
		Vertex.U = static_cast<float>(Index) * 0.5f;
		Vertex.Bone = BoneIndex;
		AppendValue(VertexChunk, Vertex);
	}
	AppendChunk(Visual, OGF_VERTICES, VertexChunk);

	xr_vector<u8> IndexChunk;
	AppendValue(IndexChunk, 3u);
	AppendValue(IndexChunk, u16{0});
	AppendValue(IndexChunk, u16{1});
	AppendValue(IndexChunk, u16{2});
	AppendChunk(Visual, OGF_INDICES, IndexChunk);
	return Visual;
}

void AppendSyntheticIkRecord(
	xr_vector<u8>& IkData,
	const Fvector& Translation
)
{
	AppendValue(IkData, 1u);
	AppendString(IkData, "default_object");
	AppendZeroBytes(IkData, 112u + 76u);
	Fvector Rotation;
	Rotation.set(0.0f, 0.0f, 0.0f);
	AppendValue(IkData, Rotation);
	AppendValue(IkData, Translation);
	AppendValue(IkData, 1.0f);
	Fvector Center;
	Center.set(0.0f, 0.0f, 0.0f);
	AppendValue(IkData, Center);
}

[[nodiscard]] xr_vector<u8> MakeSkeletonRoot(const u32 BoneIndex)
{
	xr_vector<u8> Visual;
	ogf_header Header = {};
	Header.format_version = xrOGF_FormatVersion;
	Header.type = MT_SKELETON_ANIM;
	xr_vector<u8> HeaderChunk;
	AppendValue(HeaderChunk, Header);
	AppendChunk(Visual, OGF_HEADER, HeaderChunk);

	xr_vector<u8> Children;
	AppendChunk(Children, 0u, MakeSkeletalVisual(BoneIndex));
	AppendChunk(Visual, OGF_CHILDREN, Children);

	xr_vector<u8> BoneNames;
	AppendValue(BoneNames, 2u);
	Fobb Obb = {};
	AppendString(BoneNames, "root");
	AppendString(BoneNames, "");
	AppendValue(BoneNames, Obb);
	AppendString(BoneNames, "child");
	AppendString(BoneNames, "root");
	AppendValue(BoneNames, Obb);
	AppendChunk(Visual, OGF_S_BONE_NAMES, BoneNames);

	xr_vector<u8> IkData;
	Fvector RootTranslation;
	RootTranslation.set(0.0f, 0.0f, 0.0f);
	AppendSyntheticIkRecord(IkData, RootTranslation);
	Fvector ChildTranslation;
	ChildTranslation.set(1.0f, 2.0f, 3.0f);
	AppendSyntheticIkRecord(IkData, ChildTranslation);
	AppendChunk(Visual, OGF_S_IKDATA, IkData);
	return Visual;
}

[[nodiscard]] bool Load(
	xr_vector<u8>& Bytes,
	FTiramisuEditorOgfModelSource& Model
)
{
	IReader Reader(Bytes.data(), Bytes.size());
	return LoadTiramisuEditorOgfModel(Reader, Model);
}

bool TestStaticVisual()
{
	xr_vector<u8> Bytes = MakeStaticVisual();
	FTiramisuEditorOgfModelSource Model;
	if (!Load(Bytes, Model) || !Model.IsValid() ||
		Model.Meshes.size() != 1)
	{
		std::cerr << "static OGF was not decoded\n";
		return false;
	}
	const FTiramisuEditorOgfMeshSource& Mesh = Model.Meshes.front();
	if (Mesh.ShaderName != "default" ||
		Mesh.TextureName != "test\\diffuse" ||
		Mesh.Vertices.size() != 3 || Mesh.Indices.size() != 3 ||
		Mesh.Indices[2] != 2 ||
		!std::isfinite(Mesh.Vertices[0].Tangent[0]))
	{
		std::cerr << "static OGF payload differs from the source\n";
		return false;
	}
	return true;
}

bool TestHierarchy()
{
	xr_vector<u8> Bytes = MakeHierarchy();
	FTiramisuEditorOgfModelSource Model;
	if (!Load(Bytes, Model) || Model.Meshes.size() != 2)
	{
		std::cerr << "embedded OGF hierarchy was not flattened\n";
		return false;
	}
	return true;
}

bool TestInvalidData()
{
	for (const auto& [BytesTemplate, ExpectedStatus] : {
			xr_pair{MakeStaticVisual(MT_NORMAL, 0x002u),
				ETiramisuEditorOgfLoadStatus::UnsupportedVertexFormat},
			xr_pair{MakeStaticVisual(MT_NORMAL, 0x112u, true),
				ETiramisuEditorOgfLoadStatus::InvalidData},
			xr_pair{MakeStaticVisual(MT_LOD),
				ETiramisuEditorOgfLoadStatus::UnsupportedVisualType}})
	{
		xr_vector<u8> Bytes = BytesTemplate;
		FTiramisuEditorOgfModelSource Model;
		if (Load(Bytes, Model) || Model.Status != ExpectedStatus ||
			Model.Diagnostic.empty())
		{
			std::cerr << "invalid OGF did not produce a typed diagnostic\n";
			return false;
		}
	}
	return true;
}

bool TestSyntheticSkeleton()
{
	xr_vector<u8> Bytes = MakeSkeletonRoot(1u);
	FTiramisuEditorOgfModelSource Model;
	if (!Load(Bytes, Model) || Model.Bones.size() != 2u ||
		Model.RootBoneIndex != 0u || Model.Bones[1].ParentIndex != 0u ||
		Model.Meshes.size() != 1u ||
		Model.Meshes[0].SkinBindings[0].BoneIndices[0] != 1u)
	{
		std::cerr << "synthetic skeletal OGF metadata was not decoded\n";
		return false;
	}
	const Fmatrix& BindModel = Model.Bones[1].BindModel;
	const Fmatrix& ModelToBone = Model.Bones[1].ModelToBone;
	if (std::abs(BindModel._41 - 1.0f) > 1.0e-5f ||
		std::abs(BindModel._42 - 2.0f) > 1.0e-5f ||
		std::abs(BindModel._43 - 3.0f) > 1.0e-5f ||
		std::abs(ModelToBone._41 + 1.0f) > 1.0e-5f ||
		std::abs(ModelToBone._42 + 2.0f) > 1.0e-5f ||
		std::abs(ModelToBone._43 + 3.0f) > 1.0e-5f)
	{
		std::cerr << "synthetic skeleton inverse bind palette is invalid\n";
		return false;
	}
	xr_vector<Fmatrix> Palette;
	xr_string Diagnostic;
	if (!BuildTiramisuEditorOgfSkinningPalette(
			Model, {}, Palette, &Diagnostic
		) || Palette.size() != Model.Bones.size())
	{
		std::cerr << "bind-pose skinning palette was not built: "
			<< Diagnostic.c_str() << '\n';
		return false;
	}
	Fmatrix Identity;
	Identity.identity();
	for (const Fmatrix& Matrix : Palette)
	{
		for (size_t Element = 0; Element < 16u; ++Element)
		{
			if (std::abs(Matrix.mm[Element] - Identity.mm[Element]) >
				1.0e-5f)
			{
				std::cerr << "bind-pose palette is not identity\n";
				return false;
			}
		}
	}
	xr_vector<Fmatrix> LocalPose;
	LocalPose.reserve(Model.Bones.size());
	for (const FTiramisuEditorOgfBoneSource& Bone : Model.Bones)
	{
		LocalPose.push_back(Bone.BindLocal);
	}
	LocalPose[1].translate_over(2.0f, 2.0f, 3.0f);
	if (!BuildTiramisuEditorOgfSkinningPalette(
			Model, LocalPose, Palette, &Diagnostic
		) || std::abs(Palette[1]._41 - 1.0f) > 1.0e-5f ||
		std::abs(Palette[1]._42) > 1.0e-5f ||
		std::abs(Palette[1]._43) > 1.0e-5f)
	{
		std::cerr << "animated local pose produced an invalid palette\n";
		return false;
	}
	LocalPose.pop_back();
	if (BuildTiramisuEditorOgfSkinningPalette(
			Model, LocalPose, Palette, &Diagnostic
		) || Diagnostic.empty())
	{
		std::cerr << "mismatched local pose was accepted\n";
		return false;
	}
	Model.Bones[0].ParentIndex = 1u;
	if (BuildTiramisuEditorOgfSkinningPalette(
			Model, {}, Palette, &Diagnostic
		) || Diagnostic.empty())
	{
		std::cerr << "cyclic pose hierarchy was accepted\n";
		return false;
	}
	Bytes = MakeSkeletonRoot(2u);
	if (Load(Bytes, Model) ||
		Model.Status != ETiramisuEditorOgfLoadStatus::InvalidData)
	{
		std::cerr << "out-of-range skeletal bone index was accepted\n";
		return false;
	}
	return true;
}

bool TestRealSkeletalBindPose()
{
	const std::filesystem::path Path =
		"gamedata/meshes/dynamics/scene_objects/part/part_none.ogf";
	std::ifstream Stream(Path, std::ios::binary);
	if (!Stream)
	{
		std::cerr << "real skeletal OGF fixture is missing\n";
		return false;
	}
	const xr_vector<char> FileBytes{
		std::istreambuf_iterator<char>(Stream),
		std::istreambuf_iterator<char>()
	};
	xr_vector<u8> Bytes(FileBytes.begin(), FileBytes.end());
	FTiramisuEditorOgfModelSource Model;
	if (!Load(Bytes, Model) || !Model.IsValid())
	{
		std::cerr << "real skeletal OGF bind pose was not decoded: "
			<< Model.Diagnostic.c_str() << '\n';
		return false;
	}
	if (Model.Bones.empty() ||
		Model.RootBoneIndex >= Model.Bones.size())
	{
		std::cerr << "real skeletal OGF lost its bone hierarchy\n";
		return false;
	}
	for (const FTiramisuEditorOgfMeshSource& Mesh : Model.Meshes)
	{
		if (Mesh.Vertices.empty() || Mesh.Indices.empty())
		{
			std::cerr << "real skeletal OGF contains an empty draw part\n";
			return false;
		}
		if (Mesh.SkinBindings.size() != Mesh.Vertices.size())
		{
			std::cerr << "real skeletal OGF lost vertex skin bindings\n";
			return false;
		}
		for (const FTiramisuEditorOgfSkinBinding& Binding :
			 Mesh.SkinBindings)
		{
			float WeightSum = 0.0f;
			for (size_t Influence = 0;
				 Influence < Binding.Weights.size(); ++Influence)
			{
				const float Weight = Binding.Weights[Influence];
				if (!std::isfinite(Weight) || Weight < 0.0f ||
					Weight > 1.0f)
				{
					std::cerr << "real skeletal OGF has invalid skin weights\n";
					return false;
				}
				WeightSum += Weight;
				if (Weight > 0.0f &&
					Binding.BoneIndices[Influence] >=
						Model.Bones.size())
				{
					std::cerr << "real skeletal OGF has invalid bone indices\n";
					return false;
				}
			}
			if (std::abs(WeightSum - 1.0f) > 1.0e-4f)
			{
				std::cerr << "real skeletal OGF skin weights are not normalized\n";
				return false;
			}
		}
	}
	return true;
}

bool TestOptionalRealEmbeddedMotions()
{
	const std::filesystem::path Path =
		"gamedata_soc/meshes/monsters/tushkano/tushkano.ogf";
	std::ifstream Stream(Path, std::ios::binary);
	if (!Stream)
	{
		return true;
	}
	const xr_vector<char> FileBytes{
		std::istreambuf_iterator<char>(Stream),
		std::istreambuf_iterator<char>()
	};
	xr_vector<u8> Bytes(FileBytes.begin(), FileBytes.end());
	FTiramisuEditorOgfModelSource Model;
	if (!Load(Bytes, Model) || Model.EmbeddedMotions.Clips.empty())
	{
		std::cerr << "real embedded OGF motions were not decoded: "
			<< Model.Diagnostic.c_str() << '\n';
		return false;
	}
	const FTiramisuEditorOgfMotionClip* Clip = nullptr;
	for (const FTiramisuEditorOgfMotionClip& Candidate :
		 Model.EmbeddedMotions.Clips)
	{
		if (!Candidate.Fx)
		{
			Clip = &Candidate;
			break;
		}
	}
	if (!Clip)
	{
		std::cerr << "real embedded OGF has no cycle motion\n";
		return false;
	}
	xr_vector<Fmatrix> Palette;
	xr_string Diagnostic;
	if (!SampleTiramisuEditorOgfModelMotion(
			Model,
			Clip->Name,
			Clip->DurationSeconds() * 0.5f,
			Palette,
			&Diagnostic
		) || Palette.size() != Model.Bones.size())
	{
		std::cerr << "real embedded OGF motion palette failed: "
			<< Diagnostic.c_str() << '\n';
		return false;
	}
	return true;
}

bool TestRealExternalMotions()
{
	const std::filesystem::path ModelPath =
		"gamedata/meshes/actors/stalker_bandit/stalker_bandit_1.ogf";
	std::ifstream ModelStream(ModelPath, std::ios::binary);
	if (!ModelStream)
	{
		std::cerr << "main external-motion OGF fixture is missing\n";
		return false;
	}
	const xr_vector<char> ModelFileBytes{
		std::istreambuf_iterator<char>(ModelStream),
		std::istreambuf_iterator<char>()
	};
	xr_vector<u8> ModelBytes(
		ModelFileBytes.begin(), ModelFileBytes.end()
	);
	FTiramisuEditorOgfModelSource Model;
	if (!Load(ModelBytes, Model) || Model.MotionReferences.empty())
	{
		std::cerr << "real external-motion OGF was not decoded: "
			<< Model.Diagnostic.c_str() << '\n';
		return false;
	}
	const auto MotionReference = std::ranges::find_if(
		Model.MotionReferences,
		[](const xr_string_view Reference)
		{
			return Reference.find("stalker_animation") !=
				xr_string_view::npos;
		}
	);
	if (MotionReference == Model.MotionReferences.end())
	{
		std::cerr << "main actor has no stalker_animation reference\n";
		return false;
	}
	std::filesystem::path MotionPath = "gamedata/meshes";
	MotionPath /= MotionReference->c_str();
	MotionPath += ".omf";
	std::ifstream MotionStream(MotionPath, std::ios::binary);
	if (!MotionStream)
	{
		std::cerr << "referenced real OMF fixture is missing: "
			<< MotionPath.string() << '\n';
		return false;
	}
	const xr_vector<char> MotionFileBytes{
		std::istreambuf_iterator<char>(MotionStream),
		std::istreambuf_iterator<char>()
	};
	xr_vector<u8> MotionBytes(
		MotionFileBytes.begin(), MotionFileBytes.end()
	);
	IReader MotionReader(MotionBytes.data(), MotionBytes.size());
	xr_vector<xr_string> BoneNames;
	BoneNames.reserve(Model.Bones.size());
	for (const FTiramisuEditorOgfBoneSource& Bone : Model.Bones)
	{
		BoneNames.push_back(Bone.Name);
	}
	FTiramisuEditorOgfMotionSet Motions;
	if (!LoadTiramisuEditorOgfMotions(
			MotionReader, BoneNames, Motions
		) || Motions.Clips.empty())
	{
		std::cerr << "real external OMF was not decoded: "
			<< Motions.Diagnostic.c_str() << '\n';
		return false;
	}
	constexpr xr_string_view ClipName = "norm_walk_fwd_0";
	const FTiramisuEditorOgfMotionClip* Clip =
		Motions.FindClip(ClipName);
	if (!Clip || Clip->Fx)
	{
		std::cerr << "main external OMF has no walk cycle\n";
		return false;
	}
	xr_vector<Fmatrix> FirstPalette;
	xr_vector<Fmatrix> SecondPalette;
	xr_string Diagnostic;
	Model.ExternalMotions.push_back(std::move(Motions));
	if (!SampleTiramisuEditorOgfModelMotion(
			Model, ClipName, 0.0f, FirstPalette, &Diagnostic
		) || !SampleTiramisuEditorOgfModelMotion(
			Model, ClipName, 0.125f, SecondPalette, &Diagnostic
		) || FirstPalette.size() != Model.Bones.size() ||
		SecondPalette.size() != Model.Bones.size())
	{
		std::cerr << "real external OMF palette failed: "
			<< Diagnostic.c_str() << '\n';
		return false;
	}
	bool Changed = false;
	for (size_t BoneIndex = 0; BoneIndex < FirstPalette.size(); ++BoneIndex)
	{
		for (size_t Element = 0; Element < 16u; ++Element)
		{
			Changed |= std::abs(
				FirstPalette[BoneIndex].mm[Element] -
				SecondPalette[BoneIndex].mm[Element]
			) > 1.0e-5f;
		}
	}
	if (!Changed)
	{
		std::cerr << "real external OMF walk palette did not change\n";
		return false;
	}
	return true;
}
} // namespace

int main()
{
	if (!TestStaticVisual() || !TestHierarchy() || !TestInvalidData() ||
		!TestSyntheticSkeleton() || !TestRealSkeletalBindPose() ||
		!TestOptionalRealEmbeddedMotions() ||
		!TestRealExternalMotions())
	{
		return 1;
	}
	std::cout << "Editor OGF model loader tests passed\n";
	return 0;
}
