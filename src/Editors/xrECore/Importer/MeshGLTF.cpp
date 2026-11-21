#include "stdafx.h"
#include "MeshGLTF.h"

#include "../Editor/EditObject.h"
#include "../Editor/EditMesh.h"

#define CGLTF_IMPLEMENTATION
#include <cgltf/cgltf.h>

namespace
{
	struct PointKey
	{
		u32 X;
		u32 Y;
		u32 Z;

		auto operator<=>(const PointKey&) const = default;
	};

	PointKey MakeKey(const Fvector& P)
	{
		PointKey Key;
		memcpy(&Key.X, &P.x, sizeof(u32));
		memcpy(&Key.Y, &P.y, sizeof(u32));
		memcpy(&Key.Z, &P.z, sizeof(u32));
		return Key;
	}

	// Column-major glTF matrix: M[Column * 4 + Row]
	struct NodeTransform
	{
		float M[16] = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1};
		// Cofactor matrix of the upper 3x3, signed by the determinant: inverse transpose up to scale
		float N[9] = {1, 0, 0, 0, 1, 0, 0, 0, 1};
		float Det = 1.0f;

		void Setup()
		{
			auto A = [this](int Row, int Column) { return M[Column * 4 + Row]; };
			const float C00 = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1);
			const float C01 = A(1, 2) * A(2, 0) - A(1, 0) * A(2, 2);
			const float C02 = A(1, 0) * A(2, 1) - A(1, 1) * A(2, 0);
			const float C10 = A(0, 2) * A(2, 1) - A(0, 1) * A(2, 2);
			const float C11 = A(0, 0) * A(2, 2) - A(0, 2) * A(2, 0);
			const float C12 = A(0, 1) * A(2, 0) - A(0, 0) * A(2, 1);
			const float C20 = A(0, 1) * A(1, 2) - A(0, 2) * A(1, 1);
			const float C21 = A(0, 2) * A(1, 0) - A(0, 0) * A(1, 2);
			const float C22 = A(0, 0) * A(1, 1) - A(0, 1) * A(1, 0);
			Det = A(0, 0) * C00 + A(0, 1) * C01 + A(0, 2) * C02;
			const float Sign = Det < 0.0f ? -1.0f : 1.0f;
			const float Cof[9] = {C00, C01, C02, C10, C11, C12, C20, C21, C22};
			for (int i = 0; i < 9; ++i)
			{
				N[i] = Cof[i] * Sign;
			}
		}

		// glTF is right-handed: mirror Z into the engine left-handed space
		Fvector Point(const float* P) const
		{
			Fvector R;
			R.x = M[0] * P[0] + M[4] * P[1] + M[8] * P[2] + M[12];
			R.y = M[1] * P[0] + M[5] * P[1] + M[9] * P[2] + M[13];
			R.z = -(M[2] * P[0] + M[6] * P[1] + M[10] * P[2] + M[14]);
			return R;
		}

		Fvector Normal(const float* P) const
		{
			Fvector R;
			R.x = N[0] * P[0] + N[1] * P[1] + N[2] * P[2];
			R.y = N[3] * P[0] + N[4] * P[1] + N[5] * P[2];
			R.z = -(N[6] * P[0] + N[7] * P[1] + N[8] * P[2]);
			const float Len = R.magnitude();
			if (Len > EPS_S)
			{
				R.div(Len);
			}
			else
			{
				R.set(0.0f, 1.0f, 0.0f);
			}
			return R;
		}
	};

	xr_string TextureName(const cgltf_material* Material)
	{
		if (!Material)
		{
			return {};
		}

		const cgltf_texture* Texture = Material->pbr_metallic_roughness.base_color_texture.texture;
		if (!Texture && Material->has_pbr_specular_glossiness)
		{
			Texture = Material->pbr_specular_glossiness.diffuse_texture.texture;
		}
		if (!Texture || !Texture->image)
		{
			return {};
		}

		xr_string Name;
		if (Texture->image->uri && strncmp(Texture->image->uri, "data:", 5) != 0)
		{
			Name = Texture->image->uri;
			cgltf_decode_uri(Name.data());
			Name = Name.c_str();
		}
		else if (Texture->image->name)
		{
			Name = Texture->image->name;
		}

		const size_t Slash = Name.find_last_of("/\\");
		if (Slash != xr_string::npos)
		{
			Name = Name.substr(Slash + 1);
		}
		const size_t Dot = Name.find_last_of('.');
		if (Dot != xr_string::npos)
		{
			Name = Name.substr(0, Dot);
		}
		return Name;
	}

	class GLTFBuilder
	{
	public:
		GLTFBuilder(const cgltf_data* InData, CEditableObject* InOut) :
			Data(InData),
			Out(InOut)
		{
		}

		void AddNode(const cgltf_node* Node, const xr_string& Name)
		{
			NodeTransform Transform;
			cgltf_node_transform_world(Node, Transform.M);
			Transform.Setup();
			AddMesh(Node->mesh, Transform, Name);
		}

		void AddMesh(const cgltf_mesh* Mesh, const NodeTransform& Transform, const xr_string& Name)
		{
			CEditableMesh* EMesh = new CEditableMesh(Out);
			EMesh->m_Name = Name.c_str();
			EMesh->m_VMaps.push_back(xr_make_unique<st_VMap>("Texture", vmtUV, false));
			st_VMap* UVMap = EMesh->m_VMaps.back().get();

			xr_map<PointKey, int> PointMap;
			xr_vector<Fvector> Normals;
			bool HasAllNormals = true;

			for (cgltf_size p = 0; p < Mesh->primitives_count; ++p)
			{
				const cgltf_primitive& Prim = Mesh->primitives[p];
				if (Prim.type != cgltf_primitive_type_triangles && Prim.type != cgltf_primitive_type_triangle_strip &&
					Prim.type != cgltf_primitive_type_triangle_fan)
				{
					Msg("! glTF: mesh '%s' primitive %u is not triangles, skipped", Name.c_str(), u32(p));
					continue;
				}

				const cgltf_accessor* PosAcc = nullptr;
				const cgltf_accessor* NormAcc = nullptr;
				const cgltf_accessor* UVAcc = nullptr;
				for (cgltf_size a = 0; a < Prim.attributes_count; ++a)
				{
					const cgltf_attribute& Attr = Prim.attributes[a];
					if (Attr.type == cgltf_attribute_type_position)
					{
						PosAcc = Attr.data;
					}
					else if (Attr.type == cgltf_attribute_type_normal)
					{
						NormAcc = Attr.data;
					}
					else if (Attr.type == cgltf_attribute_type_texcoord && Attr.index == 0)
					{
						UVAcc = Attr.data;
					}
				}

				if (!PosAcc || PosAcc->count == 0)
				{
					continue;
				}

				const cgltf_size VertCount = PosAcc->count;
				xr_vector<float> Positions(VertCount * 3);
				cgltf_accessor_unpack_floats(PosAcc, Positions.data(), Positions.size());

				xr_vector<float> PrimNormals;
				if (NormAcc && NormAcc->count == VertCount)
				{
					PrimNormals.resize(VertCount * 3);
					cgltf_accessor_unpack_floats(NormAcc, PrimNormals.data(), PrimNormals.size());
				}
				else
				{
					HasAllNormals = false;
				}

				xr_vector<float> UVs;
				if (UVAcc && UVAcc->count == VertCount)
				{
					UVs.resize(VertCount * 2);
					cgltf_accessor_unpack_floats(UVAcc, UVs.data(), UVs.size());
				}

				// Points are welded by position; each glTF vertex keeps its own UV entry
				const int VMBase = int(EMesh->m_VMRefs.size());
				xr_vector<int> PointOf(VertCount);
				for (cgltf_size v = 0; v < VertCount; ++v)
				{
					const Fvector P = Transform.Point(&Positions[v * 3]);
					auto [It, Inserted] = PointMap.try_emplace(MakeKey(P), int(EMesh->m_Vertices.size()));
					if (Inserted)
					{
						EMesh->m_Vertices.push_back(P);
					}
					PointOf[v] = It->second;

					UVMap->appendUV(UVs.empty() ? 0.0f : UVs[v * 2], UVs.empty() ? 0.0f : UVs[v * 2 + 1]);
					UVMap->appendVI(It->second);

					st_VMapPtLst& Ref = EMesh->m_VMRefs.emplace_back();
					Ref.resize(1);
					Ref[0].vmap_index = 0;
					Ref[0].index = VMBase + int(v);
				}

				xr_vector<u32> Indices;
				if (Prim.indices)
				{
					Indices.resize(Prim.indices->count);
					cgltf_accessor_unpack_indices(Prim.indices, Indices.data(), sizeof(u32), Indices.size());
				}
				else
				{
					Indices.resize(VertCount);
					for (cgltf_size v = 0; v < VertCount; ++v)
					{
						Indices[v] = u32(v);
					}
				}

				// Triangle list in glTF counter-clockwise order
				xr_vector<u32> Tris;
				if (Prim.type == cgltf_primitive_type_triangles)
				{
					Tris.assign(Indices.begin(), Indices.begin() + (Indices.size() / 3) * 3);
				}
				else if (Prim.type == cgltf_primitive_type_triangle_strip)
				{
					for (size_t i = 2; i < Indices.size(); ++i)
					{
						const bool Odd = (i & 1) != 0;
						Tris.push_back(Indices[i - 2]);
						Tris.push_back(Odd ? Indices[i] : Indices[i - 1]);
						Tris.push_back(Odd ? Indices[i - 1] : Indices[i]);
					}
				}
				else
				{
					for (size_t i = 2; i < Indices.size(); ++i)
					{
						Tris.push_back(Indices[0]);
						Tris.push_back(Indices[i - 1]);
						Tris.push_back(Indices[i]);
					}
				}

				// Mirroring Z reverses the winding; a mirrored node transform reverses it back
				const bool Flip = Transform.Det >= 0.0f;
				IntVec& SurfFaces = EMesh->m_SurfFaces[GetSurface(Prim.material)];

				for (size_t t = 0; t + 2 < Tris.size(); t += 3)
				{
					u32 Corner[3] = {Tris[t], Tris[t + 1], Tris[t + 2]};
					if (Corner[0] >= VertCount || Corner[1] >= VertCount || Corner[2] >= VertCount)
					{
						continue;
					}
					if (Flip)
					{
						std::swap(Corner[1], Corner[2]);
					}

					st_Face Face;
					for (int k = 0; k < 3; ++k)
					{
						Face.pv[k].pindex = PointOf[Corner[k]];
						Face.pv[k].vmref = VMBase + int(Corner[k]);
					}
					if (Face.pv[0].pindex == Face.pv[1].pindex || Face.pv[1].pindex == Face.pv[2].pindex ||
						Face.pv[0].pindex == Face.pv[2].pindex)
					{
						continue;
					}

					SurfFaces.push_back(int(EMesh->m_Faces.size()));
					EMesh->m_Faces.push_back(Face);
					for (int k = 0; k < 3; ++k)
					{
						Normals.push_back(PrimNormals.empty() ? Fvector().set(0.0f, 1.0f, 0.0f) : Transform.Normal(&PrimNormals[Corner[k] * 3]));
					}
				}
			}

			for (auto It = EMesh->m_SurfFaces.begin(); It != EMesh->m_SurfFaces.end();)
			{
				It = It->second.empty() ? EMesh->m_SurfFaces.erase(It) : std::next(It);
			}

			if (EMesh->m_Faces.empty())
			{
				Msg("! glTF: mesh '%s' has no triangles, skipped", Name.c_str());
				xr_delete(EMesh);
				return;
			}

			// One smoothing group: welded points are smoothed, file normals are kept for the Normals smoothing mode
			EMesh->m_Flags.set(CEditableMesh::flSGMask, TRUE);
			if (!HasAllNormals)
			{
				Normals.clear();
			}
			EMesh->SetSmoothing(xr_vector<u32>(EMesh->m_Faces.size(), 1), std::move(Normals));

			EMesh->GenerateFNormals();
			EMesh->GenerateVNormals(nullptr, true);
			EMesh->GenerateAdjacency();
			EMesh->RecomputeBBox();
			Out->m_Meshes.push_back(EMesh);
			FaceCount += u32(EMesh->m_Faces.size());
		}

		u32 FaceCount = 0;

	private:
		CSurface* GetSurface(const cgltf_material* Material)
		{
			auto It = Surfaces.find(Material);
			if (It != Surfaces.end())
			{
				return It->second;
			}

			xr_string Name;
			if (Material && Material->name && Material->name[0])
			{
				Name = Material->name;
			}
			else if (Material)
			{
				Name = "material_" + xr_string::ToString(u32(Material - Data->materials));
			}
			else
			{
				Name = "default";
			}

			CSurface* Surface = new CSurface();
			Surface->SetSharedData(nullptr);
			Surface->SetName(Name.c_str());
			Surface->SetShader("default");
			Surface->SetShaderXRLC("default");
			Surface->SetGameMtl("default");

			const xr_string Texture = TextureName(Material);
			Surface->SetTexture(Texture.c_str());
			if (Material && Material->double_sided)
			{
				Surface->_flags().set(SSurfaceData::sf2Sided, TRUE);
			}
			Surface->m_VMap = "Texture";
			Surface->m_dwFVF = D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1;

			Out->m_Surfaces.push_back(Surface);
			Surfaces[Material] = Surface;
			return Surface;
		}

		const cgltf_data* Data;
		CEditableObject* Out;
		xr_map<const cgltf_material*, CSurface*> Surfaces;
	};

	xr_string NodeName(const cgltf_node* Node, u32 Index)
	{
		if (Node->name && Node->name[0])
		{
			return Node->name;
		}
		if (Node->mesh->name && Node->mesh->name[0])
		{
			return Node->mesh->name;
		}
		return "mesh_" + xr_string::ToString(Index);
	}

	void AddNodeRecursive(GLTFBuilder& Builder, const cgltf_node* Node, u32& Index)
	{
		if (Node->mesh)
		{
			Builder.AddNode(Node, NodeName(Node, Index++));
		}
		for (cgltf_size i = 0; i < Node->children_count; ++i)
		{
			AddNodeRecursive(Builder, Node->children[i], Index);
		}
	}
}

bool XRay::Importer::Mesh::IsGLTF(const char* FileName)
{
	const char* Ext = FileName ? strext(FileName) : nullptr;
	return Ext && (0 == stricmp(Ext, ".gltf") || 0 == stricmp(Ext, ".glb"));
}

bool XRay::Importer::Mesh::ImportGLTF(const char* FileName, CEditableObject* Out)
{
	cgltf_options Options = {};
	cgltf_data* Data = nullptr;

	cgltf_result Result = cgltf_parse_file(&Options, FileName, &Data);
	if (Result != cgltf_result_success)
	{
		Msg("! glTF: can't parse '%s' (error %d)", FileName, int(Result));
		return false;
	}

	struct DataGuard
	{
		cgltf_data* Data;
		~DataGuard() { cgltf_free(Data); }
	} Guard{Data};

	for (cgltf_size i = 0; i < Data->extensions_required_count; ++i)
	{
		Msg("! glTF: '%s' requires unsupported extension '%s'", FileName, Data->extensions_required[i]);
		return false;
	}

	Result = cgltf_load_buffers(&Options, Data, FileName);
	if (Result != cgltf_result_success)
	{
		Msg("! glTF: can't load buffers of '%s' (error %d)", FileName, int(Result));
		return false;
	}

	Result = cgltf_validate(Data);
	if (Result != cgltf_result_success)
	{
		Msg("! glTF: '%s' is invalid (error %d)", FileName, int(Result));
		return false;
	}

	GLTFBuilder Builder(Data, Out);
	u32 Index = 0;

	const cgltf_scene* Scene = Data->scene ? Data->scene : (Data->scenes_count ? &Data->scenes[0] : nullptr);
	if (Scene)
	{
		for (cgltf_size i = 0; i < Scene->nodes_count; ++i)
		{
			AddNodeRecursive(Builder, Scene->nodes[i], Index);
		}
	}
	else if (Data->nodes_count)
	{
		for (cgltf_size i = 0; i < Data->nodes_count; ++i)
		{
			if (Data->nodes[i].mesh)
			{
				Builder.AddNode(&Data->nodes[i], NodeName(&Data->nodes[i], Index++));
			}
		}
	}
	else
	{
		for (cgltf_size i = 0; i < Data->meshes_count; ++i)
		{
			const cgltf_mesh& Mesh = Data->meshes[i];
			xr_string Name = (Mesh.name && Mesh.name[0]) ? xr_string(Mesh.name) : ("mesh_" + xr_string::ToString(Index));
			++Index;
			NodeTransform Identity;
			Builder.AddMesh(&Mesh, Identity, Name);
		}
	}

	if (Out->m_Meshes.empty())
	{
		Msg("! glTF: '%s' has no triangle meshes", FileName);
		return false;
	}

	Out->VerifyMeshNames();
	Out->UpdateBox();
	Msg("* glTF: '%s' imported, %u meshes, %u faces", FileName, u32(Out->m_Meshes.size()), Builder.FaceCount);
	return true;
}
