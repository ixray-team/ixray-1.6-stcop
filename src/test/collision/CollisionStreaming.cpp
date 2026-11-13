#include "../../xrCore/stdafx.h"
#include "../../xrCore/Collision/xr_area.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrCore/Collision/Frustum.h"

namespace
{
u32 g_Failures = 0;

bool Check(bool Value, const char* Description)
{
	if (!Value)
	{
		std::fprintf(stderr, "FAIL: %s\n", Description);
		++g_Failures;
	}
	return Value;
}

void CheckRay(CDB::MODEL& Model, const Fvector& Start)
{
	CDB::COLLIDER Collider;
	Collider.ray_options(CDB::OPT_ONLYNEAREST);
	Collider.ray_query(Model, Start, Fvector{0.f, -1.f, 0.f}, 20.f);
	Check(Collider.r_count() == 1, "loaded geometry answers ray queries");
	Check(Model.root != nullptr, "single-leaf BVH has a root");
}

void CheckSpatialQueries(CObjectSpace& Space, bool Instanced)
{
	CDB::COLLIDER Collider;
	const Fvector Center{2.f, 0.f, 2.f};
	Collider.box_options(CDB::OPT_FULL_TEST);
	Collider.box_query(Space, Center, Fvector{1.f, 1.f, 1.f});
	Check(Collider.r_count() == 1, "AABB query returns one world triangle across resident models");
	Fmatrix Rotation;
	Rotation.setHPB(0.7f, 0.2f, 0.f);
	Rotation.c = Center;
	Fobb Box;
	Box.xform_set(Rotation);
	Box.m_halfsize.set(1.f, 1.f, 1.f);
	Collider.obb_options(CDB::OPT_FULL_TEST);
	Collider.obb_query(Space, Box);
	Check(Collider.r_count() == 1, "rotated OBB query does not duplicate boundary triangles");
	Collider.sphere_options(CDB::OPT_FULL_TEST);
	Collider.sphere_query(Space, Fsphere{Center, 1.f});
	Check(Collider.r_count() == 1, "sphere query traverses resident models once");
	Fcylinder Cylinder;
	Cylinder.m_center = Center;
	Cylinder.m_direction.set(0.f, 1.f, 0.f);
	Cylinder.m_radius = 1.f;
	Cylinder.m_height = 2.f;
	Collider.cylinder_options(CDB::OPT_FULL_TEST);
	Collider.cylinder_query(Space, Cylinder);
	Check(Collider.r_count() == 1, "cylinder query traverses resident models once");
	Fcone Cone;
	Cone.m_translate.set(2.f, 2.f, 2.f);
	Cone.m_direction.set(0.f, -1.f, 0.f);
	Cone.m_radius = 2.f;
	Cone.m_height = 3.f;
	Collider.cone_options(CDB::OPT_FULL_TEST);
	Collider.cone_query(Space, Cone);
	Check(Collider.r_count() == 1, "cone query traverses resident models once");
	Fmatrix View, Projection, Combined;
	View.build_camera_dir(Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, Fvector{0.f, 0.f, 1.f});
	Projection.build_projection(PI_DIV_2, 1.f, 0.1f, 12.f);
	Combined.mul(Projection, View);
	CFrustum Frustum;
	Frustum.CreateFromMatrix(Combined, FRUSTUM_P_ALL);
	Collider.box_options(CDB::OPT_ONLYFIRST);
	Collider.frustum_options(CDB::OPT_FULL_TEST);
	Collider.frustum_query(Space, Frustum);
	Check(Collider.r_count() == (Instanced ? 2 : 1), "frustum options are independent of box options and include instances");
	Collider.frustum_options(CDB::OPT_ONLYFIRST);
	Collider.frustum_query(Space, Frustum);
	Check(Collider.r_count() == 1, "frustum only-first mode survives resident-model aggregation");
	Collider.sphere_options(CDB::OPT_ONLYFIRST);
	Collider.sphere_query(Space, Fsphere{Fvector{2.f, 2.f, 2.f}, 10.f});
	Check(Collider.r_count() == 1, "sphere only-first mode is passed to per-model queries");
	Cylinder.m_height = 20.f;
	Collider.cylinder_options(CDB::OPT_ONLYFIRST);
	Collider.cylinder_query(Space, Cylinder);
	Check(Collider.r_count() == 1, "cylinder only-first mode is passed to per-model queries");
	Cone.m_translate.y = 10.f;
	Cone.m_height = 12.f;
	Cone.m_radius = 10.f;
	Collider.cone_options(CDB::OPT_ONLYFIRST);
	Collider.cone_query(Space, Cone);
	Check(Collider.r_count() == 1, "cone only-first mode is passed to per-model queries");
	if (Instanced)
	{
		Collider.sphere_options(CDB::OPT_FULL_TEST);
		Collider.sphere_query(Space, Fsphere{Fvector{2.f, 5.f, 2.f}, 0.25f});
		Check(Collider.r_count() == 1 && Collider.r_any().ModelWorldTransform.c.y == 5.f, "shape results retain the instance world transform for inspector rendering");
	}
}

void TestVanilla(const xr_string& Base)
{
	xr_vector<Fvector> Verts{{0.f, 0.f, 0.f}, {0.f, 0.f, 4.f}, {4.f, 0.f, 0.f}};
	xr_vector<CDB::TRI> Tris(1);
	Tris[0].verts[0] = 0;
	Tris[0].verts[1] = 1;
	Tris[0].verts[2] = 2;
	Tris[0].dummy = 0xABCD1234;
	Tris[0].StreamedSectorID = 42;
	XRay::CForm::CFormatVanilla Format;
	Format.AddStaticGeom(Verts, Tris);
	if (!Check(Format.Write(Base.c_str()), "write vanilla cform"))
	{
		return;
	}
	auto Reader = FS.rg_open((Base + ".cform").c_str());
	Check(Reader && Reader->length() == sizeof(hdrCFORM) + 3 * sizeof(Fvector) + 16, "vanilla disk triangles stay 16 bytes");
	auto Read = XRay::CForm::Read(Base.c_str());
	if (!Check(bool(Read), "read vanilla cform"))
	{
		return;
	}
	xr_vector<Fvector> LoadedVerts;
	xr_vector<CDB::TRI> LoadedTris;
	Read->GetStaticGeom(LoadedVerts, LoadedTris);
	Check(LoadedTris.size() == 1 && LoadedTris[0].dummy == Tris[0].dummy && LoadedTris[0].StreamedSectorID == 0, "unpack legacy triangles without losing material/sector bits");
	CDB::MODEL Model;
	Read->ReadData(Model);
	CheckRay(Model, Fvector{1.f, 10.f, 1.f});
	CObjectSpace Space;
	Space.Load(nullptr, Base.c_str(), nullptr);
	CheckSpatialQueries(Space, false);
}

void TestStreamed(const xr_string& Base, bool Instanced)
{
	// The large triangle covers tile (0,0) although none of its vertices is in it.
	xr_vector<Fvector> Verts{{-20.f, 0.f, -20.f}, {-20.f, 0.f, 40.f}, {40.f, 0.f, -20.f}, {1.f, 2.f, 1.f}, {1.f, 2.f, 4.f}, {4.f, 2.f, 1.f}};
	xr_vector<CDB::TRI> Tris(2);
	for (u32 Index = 0; Index < 2; ++Index)
	{
		Tris[Index].verts[0] = Index * 3;
		Tris[Index].verts[1] = Index * 3 + 1;
		Tris[Index].verts[2] = Index * 3 + 2;
		Tris[Index].dummy = 7;
	}
	Tris[1].StreamedSectorID = 99;
	xr_unique_ptr<XRay::CForm::CFormatStreamed> Format;
	if (Instanced)
	{
		Format.reset(new XRay::CForm::CFormatStreamedInstanced(10.f));
	}
	else
	{
		Format.reset(new XRay::CForm::CFormatStreamed(10.f));
	}
	Format->AddStaticGeom(Verts, Tris);
	CDB::MODEL Prototype;
	if (Instanced)
	{
		Prototype.verts = {{0.f, 0.f, 0.f}, {0.f, 0.f, 3.f}, {3.f, 0.f, 0.f}, {3.f, 0.f, 3.f}};
		Prototype.tris.assign(1, Tris[0]);
		Fmatrix Transform{Fmatrix::EIdentity::Identity};
		Transform.c.set(1.f, 5.f, 1.f);
		Fbox Bounds;
		Bounds.set(Fvector{1.f, 5.f, 1.f}, Fvector{4.f, 5.f, 4.f});
		Format->AddInstanceRef("test-prototype", Transform, Bounds, Prototype, 123, 0);
		Format->AddInstanceRef("test-prototype", Transform, Bounds, Prototype, 321, 99);
		Format->AddInstanceRef("test-prototype", Transform, Bounds, Prototype, 999, 101);
	}
	if (!Check(Format->Write(Base.c_str()), "write streamed cform"))
	{
		return;
	}
	Format.reset();
	auto Read = XRay::CForm::Read(Base.c_str());
	if (!Check(Read && Read->IsStreamed(), "read streamed metadata"))
	{
		return;
	}
	Check(Read->GetTileSize() == 10.f, "tile size round trip");
	xr_vector<u32> IDs;
	Read->GetStreamedSectorIDs(IDs);
	Check(IDs.size() == (Instanced ? 2 : 1) && std::find(IDs.begin(), IDs.end(), 99) != IDs.end(), "manual sector index round trip");
	CDB::MODEL Tile;
	if (Check(Read->LoadStreamedTile(0, 0, Tile), "load interior tile of a large triangle"))
	{
		CheckRay(Tile, Fvector{2.f, 10.f, 2.f});
		if (Instanced)
		{
			Check(Tile.instances.size() == 1 && Tile.instances[0].Sector == 123, "instance render sector survives serialization");
			Check(Tile.models.size() == 1 && Tile.models[0]->verts.size() == 4, "prototype vertex count is independent of triangle count");
		}
	}
	CDB::MODEL NegativeTile;
	Check(Read->LoadStreamedTile(-1, -1, NegativeTile), "negative tile coordinates");
	CDB::MODEL Missing;
	Check(!Read->LoadStreamedTile(999, 999, Missing), "absent tile is not loaded");
	CDB::MODEL Manual;
	Check(Read->LoadStreamedSector(99, Manual), "load a manual sector");
	if (Instanced)
	{
		Check(Manual.instances.size() == 1 && Manual.instances[0].Sector == 321, "manual streaming sector does not overwrite the render sector");
		CDB::MODEL InstanceOnly;
		if (Check(Read->LoadStreamedSector(101, InstanceOnly), "load a chunk with instances but no static mesh"))
		{
			Check(InstanceOnly.tris.empty() && InstanceOnly.instances.size() == 1, "instance-only chunk preserves its layout");
			CheckRay(InstanceOnly, Fvector{2.f, 10.f, 2.f});
		}
	}

	CObjectSpace Space;
	Space.Load(nullptr, Base.c_str(), nullptr);
	Check(Space.IsStreamingEnabled() && Space.GetActiveStreamedModels().empty(), "master load does not eagerly load geometry");
	Space.UpdateStreaming(Fvector{0.f, 0.f, 0.f}, 0.f, 20.f);
	Check(Space.IsStreamedTileLoaded(0, 0) && Space.IsStreamedTileLoaded(-1, -1), "zero-radius streaming loads touching tiles");
	Check(!Space.IsStreamedSectorLoaded(99), "automatic update leaves manual sectors unloaded");
	const u64 InitialRevision = Space.GetStreamingRevision();
	xr_unique_ptr<CObjectSpace::StreamingUpdate> Prepared;
	xr_task_group Tasks;
	Tasks.run([&]() { Prepared = Space.PrepareStreamingUpdate(Fvector{1000.f, 0.f, 1000.f}, 0.f, 20.f); });
	for (u32 Query = 0; Query < 32; ++Query)
	{
		CDB::COLLIDER Reader;
		Reader.ray_query(Space, Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f);
		Check(Reader.r_count() > 0, "background preparation leaves in-flight collision queries unchanged");
	}
	Tasks.wait();
	Check(Space.IsStreamedTileLoaded(0, 0) && Space.GetStreamingRevision() == InitialRevision, "finished worker cannot publish or unload collision");
	Check(Space.ApplyStreamingUpdate(std::move(Prepared)), "publish a completed update on the caller thread");
	Check(Space.GetActiveStreamedModels().empty() && Space.GetStreamingRevision() == InitialRevision + 1, "publication replaces the whole tile snapshot with one revision");
	Tasks.run([&]() { Prepared = Space.PrepareStreamingUpdate(Fvector{0.f, 0.f, 0.f}, 0.f, 20.f); });
	for (u32 Query = 0; Query < 32; ++Query)
	{
		CDB::COLLIDER Reader;
		Reader.ray_query(Space, Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f);
		Check(Reader.r_count() == 0 && Space.GetStreamingRevision() == InitialRevision + 1, "background loading cannot expose partially built geometry");
	}
	Tasks.wait();
	Check(Space.GetActiveStreamedModels().empty(), "newly built tiles stay invisible until publication");
	Check(Space.ApplyStreamingUpdate(std::move(Prepared)) && Space.IsStreamedTileLoaded(0, 0), "publish fully built tiles prepared by a worker");
	const u64 LoadedRevision = Space.GetStreamingRevision();
	Space.UpdateStreaming(Fvector{0.f, 0.f, 0.f}, 0.f, 20.f);
	Check(Space.GetStreamingRevision() == LoadedRevision, "unchanged updates preserve tile identities and revision");
	Prepared = Space.PrepareStreamingUpdate(Fvector{1000.f, 0.f, 1000.f}, 0.f, 20.f);
	Prepared.reset();
	Check(Space.IsStreamedTileLoaded(0, 0) && Space.GetStreamingRevision() == LoadedRevision, "discarding a pending update does not change live residency");
	CObjectSpace OtherSpace;
	Prepared = Space.PrepareStreamingUpdate(Fvector{1000.f, 0.f, 1000.f}, 0.f, 20.f);
	Check(!OtherSpace.ApplyStreamingUpdate(std::move(Prepared)), "updates cannot be published into a different level");
	Check(!Space.PrepareStreamingUpdate(Fvector{std::numeric_limits<float>::quiet_NaN(), 0.f, 0.f}, 0.f, 20.f), "invalid camera positions cannot create a pending update");
	CDB::COLLIDER Collider;
	Collider.ray_options(0);
	Collider.ray_query(Space, Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f);
	Check(Collider.r_count() == (Instanced ? 2 : 1), "multi-tile ray results do not duplicate boundary triangles");
	Collider.ray_options(CDB::OPT_ONLYNEAREST);
	Collider.ray_query(Space, Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f);
	Check(Collider.r_count() == 1 && std::abs(Collider.r_any().range - (Instanced ? 5.f : 10.f)) < EPS, "nearest ray hit is selected across all resident tiles");
	collide::ray_cache Cache;
	Check(Space.RayTest(Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f, collide::rqtStatic, &Cache, nullptr), "object space ray test sees streamed geometry");
	Collider.box_options(0);
	Collider.box_query(Space, Fvector{2.f, 0.f, 2.f}, Fvector{1.f, 1.f, 1.f});
	Check(Collider.r_count() == 1, "physics box query traverses streamed models once");
	CheckSpatialQueries(Space, Instanced);
	Prepared = Space.PrepareStreamingUpdate(Fvector{1000.f, 0.f, 1000.f}, 0.f, 20.f);
	Check(Space.LoadStreamedSector(99), "object space loads a manual sector");
	Check(!Space.ApplyStreamingUpdate(std::move(Prepared)), "manual residency changes reject a stale background update");
	Check(Space.IsStreamedTileLoaded(0, 0) && Space.IsStreamedSectorLoaded(99), "stale publication cannot undo a manual change");
	auto Snapshot = Space.GetActiveStreamedModels();
	const u64 Revision = Space.GetStreamingRevision();
	Space.UpdateStreaming(Fvector{1000.f, 0.f, 1000.f}, 0.f, 20.f);
	Check(!Space.IsStreamedTileLoaded(0, 0) && Space.IsStreamedSectorLoaded(99), "distance unload preserves manual sectors");
	Check(Space.GetStreamingRevision() > Revision, "unloading invalidates physics caches");
	for (const auto& Model : Snapshot)
	{
		Check(Model->IsBuilt, "snapshots retain unloaded geometry for in-flight queries");
	}
	Check(Space.UnloadStreamedSector(99), "manual sector unload");
	Check(Space.GetActiveStreamedModels().empty(), "all streamed geometry can be unloaded");
	Collider.ray_query(Space, Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f);
	Check(Collider.r_count() == 0, "empty residency clears previous collision results");
	Collider.sphere_query(Space, Fsphere{Fvector{2.f, 0.f, 2.f}, 10.f});
	Check(Collider.r_count() == 0, "sphere queries clear results after all models unload");
	Fcylinder EmptyCylinder;
	EmptyCylinder.m_center.set(2.f, 0.f, 2.f);
	EmptyCylinder.m_direction.set(0.f, 1.f, 0.f);
	EmptyCylinder.m_radius = 2.f;
	EmptyCylinder.m_height = 10.f;
	Collider.cylinder_query(Space, EmptyCylinder);
	Check(Collider.r_count() == 0, "cylinder queries clear results after all models unload");
	Fcone EmptyCone;
	EmptyCone.m_translate.set(2.f, 10.f, 2.f);
	EmptyCone.m_direction.set(0.f, -1.f, 0.f);
	EmptyCone.m_radius = 2.f;
	EmptyCone.m_height = 10.f;
	Collider.cone_query(Space, EmptyCone);
	Check(Collider.r_count() == 0, "cone queries clear results after all models unload");
	Check(!Space.RayTest(Fvector{2.f, 10.f, 2.f}, Fvector{0.f, -1.f, 0.f}, 20.f, collide::rqtStatic, &Cache, nullptr), "ray cache cannot return a hit from unloaded geometry");
}

template <class Shape>
void CheckInstancedCurvedQuery(CObjectSpace& Space, const CDB::MODEL& Flat, const Shape& Query, const char* Scenario)
{
	for (u32 Options : {0u, u32(CDB::OPT_FULL_TEST), u32(CDB::OPT_ONLYFIRST), u32(CDB::OPT_FULL_TEST | CDB::OPT_ONLYFIRST)})
	{
		xr_vector<u32> Expected;
		for (const auto& Triangle : Flat.tris)
		{
			const Fvector Vertices[]{Flat.verts[Triangle.verts[0]], Flat.verts[Triangle.verts[1]], Flat.verts[Triangle.verts[2]]};
			if (Query.intersectTri(Vertices, bool(Options & CDB::OPT_FULL_TEST)))
			{
				Expected.push_back(Triangle.material);
			}
		}
		CDB::COLLIDER Collider;
		if constexpr (std::is_same_v<Shape, Fcylinder>)
		{
			Collider.cylinder_options(Options);
			Collider.cylinder_query(Space, Query);
		}
		else
		{
			Collider.cone_options(Options);
			Collider.cone_query(Space, Query);
		}
		xr_vector<u32> Actual;
		for (const auto& Result : Collider.r_vec())
		{
			const auto& Triangle = Result.model->tris[Result.tris_id];
			Actual.push_back(Triangle.material);
			for (u32 Vertex = 0; Vertex < 3; ++Vertex)
			{
				Fvector WorldVertex;
				Result.ModelWorldTransform.transform_tiny(WorldVertex, Result.model->verts[Triangle.verts[Vertex]]);
				Check(WorldVertex.similar(Flat.verts[Flat.tris[Triangle.material].verts[Vertex]], EPS_L), "curved instance queries preserve source world transforms");
			}
		}
		std::sort(Expected.begin(), Expected.end());
		std::sort(Actual.begin(), Actual.end());
		const bool First = Options & CDB::OPT_ONLYFIRST;
		const bool Matches = First ? (Actual.size() == (Expected.empty() ? 0u : 1u) && (Actual.empty() || std::binary_search(Expected.begin(), Expected.end(), Actual.front()))) : Actual == Expected;
		if (!Check(Matches, "instanced cylinder/cone results match direct world-space triangle tests"))
		{
			std::fprintf(stderr, "  %s, %s, options=%u: %zu hits, %zu expected\n", Scenario, std::is_same_v<Shape, Fcylinder> ? "cylinder" : "cone", Options, Actual.size(), Expected.size());
		}
	}
}

void TestInstancedCurvedQueries(const xr_string& Base, bool Streaming)
{
	Fmatrix QueryFrame;
	QueryFrame.setHPB(0.45f, -0.3f, 0.2f);
	QueryFrame.c.set(13.f, -4.f, 21.f);
	for (u32 Variant = 0; Variant < 7; ++Variant)
	{
		Fmatrix Transform{Fmatrix::EIdentity::Identity};
		if (Variant >= 2)
		{
			Transform.setHPB(0.37f * Variant, -0.19f * Variant, 0.13f * Variant);
		}
		if (Variant >= 1)
		{
			Transform.c.set(-8.f + Variant, 6.f, 3.f);
		}
		if (Variant >= 3)
		{
			Transform.i.mul(Variant == 5 ? -0.4f : 0.2f);
			Transform.j.mul(Variant == 4 ? 10.f : 3.f);
			Transform.k.mul(1.7f);
		}
		if (Variant == 6)
		{
			Transform.i.set(1.f, 0.2f, -0.4f);
			Transform.j.set(0.7f, 2.f, 0.1f);
			Transform.k.set(-0.3f, 0.4f, 0.8f);
		}
		Fmatrix Inverse;
		Inverse.invert(Transform);
		CDB::MODEL Prototype, Flat;
		const auto AddTriangle = [&](const Fvector& A, const Fvector& B, const Fvector& C)
		{
			CDB::TRI Triangle{};
			Triangle.material = u16(Prototype.tris.size());
			const Fvector Vertices[]{A, B, C};
			for (u32 Index = 0; Index < 3; ++Index)
			{
				Fvector World, Local;
				QueryFrame.transform_tiny(World, Vertices[Index]);
				Inverse.transform_tiny(Local, World);
				Transform.transform_tiny(World, Local);
				Triangle.verts[Index] = u32(Prototype.verts.size());
				Prototype.verts.push_back(Local);
				Flat.verts.push_back(World);
			}
			Prototype.tris.push_back(Triangle);
			Flat.tris.push_back(Triangle);
		};
		// The first face is inside the bounding box/pyramid but outside both curved
		// volumes. Only-first must continue after a rejected broad-phase candidate.
		AddTriangle(Fvector{1.7f, 1.7f, 2.f}, Fvector{1.8f, 1.7f, 2.f}, Fvector{1.7f, 1.8f, 2.f});
		AddTriangle(Fvector{-0.1f, -0.1f, 2.f}, Fvector{0.1f, -0.1f, 2.f}, Fvector{0.f, 0.1f, 2.f});
		AddTriangle(Fvector{-3.f, 0.f, 2.f}, Fvector{3.f, 0.f, 2.f}, Fvector{0.f, 3.f, 2.f});
		for (float Z : {-1.f, 0.3f, 2.f, 3.8f, 5.f})
		{
			for (float X : {-3.f, -1.5f, 0.f, 1.5f, 3.f})
			{
				for (float Y : {-3.f, -1.5f, 0.f, 1.5f, 3.f})
				{
					AddTriangle(Fvector{X, Y, Z}, Fvector{X + 0.08f, Y, Z}, Fvector{X, Y + 0.08f, Z});
				}
			}
		}
		Fbox Bounds;
		Bounds.invalidate();
		for (const auto& Vertex : Flat.verts)
		{
			Bounds.modify(Vertex);
		}
		xr_unique_ptr<XRay::CForm::IFormat> Format;
		if (Streaming)
		{
			Format.reset(new XRay::CForm::CFormatStreamedInstanced(5.f));
		}
		else
		{
			Format.reset(new XRay::CForm::CFormatInstanced());
		}
		xr_vector<Fvector> EmptyVertices;
		xr_vector<CDB::TRI> EmptyTriangles;
		Format->AddStaticGeom(EmptyVertices, EmptyTriangles);
		Format->AddInstanceRef("curved-query-prototype", Transform, Bounds, Prototype, 17, 0);
		const xr_string Name = Base + "_" + std::to_string(Variant).c_str();
		if (!Check(Format->Write(Name.c_str()), "write transformed cylinder/cone query fixture"))
		{
			continue;
		}
		CObjectSpace Space;
		Space.Load(nullptr, Name.c_str(), nullptr);
		if (Streaming)
		{
			Space.UpdateStreaming(QueryFrame.c, 20.f, 30.f);
		}
		Fcylinder Cylinder;
		QueryFrame.transform_tiny(Cylinder.m_center, Fvector{0.f, 0.f, 2.f});
		Cylinder.m_direction = QueryFrame.k;
		Cylinder.m_radius = 2.f;
		Cylinder.m_height = 4.f;
		CheckInstancedCurvedQuery(Space, Flat, Cylinder, Name.c_str());
		Fcone Cone;
		Cone.m_translate = QueryFrame.c;
		Cone.m_direction = QueryFrame.k;
		Cone.m_radius = 2.f;
		Cone.m_height = 4.f;
		CheckInstancedCurvedQuery(Space, Flat, Cone, Name.c_str());
		Cylinder.m_center.mad(QueryFrame.i, 30.f);
		Cone.m_translate.mad(QueryFrame.i, 30.f);
		CheckInstancedCurvedQuery(Space, Flat, Cylinder, Name.c_str());
		CheckInstancedCurvedQuery(Space, Flat, Cone, Name.c_str());
	}
}

void TestInvalidMaster(const xr_string& Base)
{
	{
		auto Writer = FS.wg_open((Base + ".cform").c_str());
		hdrCFORM Header{};
		Header.version = CFormVersions::Streamed;
		Writer->w(&Header, sizeof(Header));
		Writer->w_float(0.f);
		Writer->w_u32(0);
		Writer->w_u32(0);
	}
	Check(!XRay::CForm::Read(Base.c_str()), "zero tile size is rejected");
	{
		auto Writer = FS.wg_open((Base + "_truncated.cform").c_str());
		Writer->w_u32(u32(CFormVersions::Streamed));
	}
	Check(!XRay::CForm::Read((Base + "_truncated").c_str()), "truncated master header is rejected");
}
} // namespace

int main(int ArgCount, char** Arguments)
{
	if (ArgCount != 2)
	{
		std::fprintf(stderr, "Usage: TestCollisionStreaming <empty test directory>\n");
		return 2;
	}
	// The caller supplies a dedicated empty directory; no game data is modified.
	const std::filesystem::path Directory = std::filesystem::absolute(Arguments[1]);
	std::filesystem::create_directories(Directory);
	if (!std::filesystem::is_empty(Directory))
	{
		std::fprintf(stderr, "Test directory must be empty\n");
		return 2;
	}
	Core._initialize("CollisionStreamingTests", nullptr, false);
	FS._initialize(CLocatorAPI::flTargetFolderOnly, Directory.string().c_str());
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	{
		const xr_string Base = (Directory / "collision").string().c_str();
		TestVanilla(Base + "_vanilla");
		TestStreamed(Base + "_streamed", false);
		TestStreamed(Base + "_instanced", true);
		TestInstancedCurvedQueries(Base + "_curved_instanced", false);
		TestInstancedCurvedQueries(Base + "_curved_streamed", true);
		TestInvalidMaster(Base + "_invalid");
	}
	xr_delete(g_SpatialSpace);
	xr_delete(g_SpatialSpacePhysic);
	std::printf("Collision streaming: %u failure(s)\n", g_Failures);
	Core._destroy();
	return g_Failures == 0 ? 0 : 1;
}
