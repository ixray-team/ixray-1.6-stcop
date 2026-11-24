/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
#include "r1_GlowManager.h"
#include <numeric>
#include "GpuSceneInternal.h"

namespace GpuSceneInternal
{
namespace
{
constexpr u32 NoSector = u32(-1);

// A skeleton only the shadow views cast is posed every frame within ShadowPoseNear of the eye and keeps its pose for up to
// UCalc_Interval beyond ShadowPoseFar, the interval growing between
constexpr float ShadowPoseNear = 30.f;
constexpr float ShadowPoseFar = 40.f;

// How far the eye may have moved since a cascade sampled now was drawn
constexpr float SunReachMargin = 1.f;

// How far along the sun a shadow can fall: to the widest cascade's far end (Direction . a point there), not below the level
struct SunReach
{
	Fvector Direction;
	float FarEnd;
	float Floor;
};

// An object of the frame's table: the sun cascades whose collection would have called it, a bit each, and whether a lamp
// part casts it
struct FrameObject
{
	IRenderable* Renderable;
	u32 Sector;
	u32 Cascades;
	bool IsMain;
	bool IsLamp = false;
};

// What the main view's tests keep, in the order of its spatial query: a light to add, or an object or a particle system
// to call. Sector: of an object of the table, NoSector for a particle system
struct MainItem
{
	IRenderable* Renderable;
	light* Light;
	CKinematics* Skeleton; // posed before the calls, its wallmarks updated before its call
	u32 Sector;
};

// An object a drawn cascade's collection would have called, in the order the cascades met them. Cascades: zero for one
// met before
struct CascadeHit
{
	IRenderable* Renderable;
	u32 Sector;
	u32 Cascades;
};

// The object only the shadow views call whose leaves AddShadowLeaf casts: its object of the table, the cascades that
// cast it, whether a lamp part does
struct ShadowCall
{
	IRenderable* Renderable;
	u32 Object;
	u32 Sector;
	u32 Cascades;
	bool IsLamp;
};

// An object a lamp part would have called, in the order the parts met them: its decor object, or one its portals reached
struct LampHit
{
	IRenderable* Renderable;
	u32 Sector;
	u32 Part;
	u32 Order;
	bool IsDecor;
};

// What the batch cannot draw of an object a lamp part casts: a pass of its shadow element, or every pass (Pass -1)
struct LampLeft
{
	u32 Part;
	u32 Pass;
	dxRender_Visual* Visual;
	IRenderable* Object;
	Fmatrix World;
	float Ssa;
};

using ObjectId = std::pair<const IRenderable*, u32>;
xr_vector<FrameObject> FrameObjects;
xr_vector<ObjectId> MainIds; // the main view's objects by address
xr_vector<ObjectId> ShadowIds; // those only the shadow views called
xr_vector<LampHit> LampHits; // by object once CollectCasters has called them
xr_vector<const LampHit*> LampCalls;
xr_vector<CKinematics*> LampSkeletons;
xr_vector<LampLeft> LampLefts; // by part
xr_vector<MainItem> MainItems;
xr_vector<CKinematics*> MainSkeletons;
xr_vector<CascadeHit> CascadeHits;
xr_vector<u32> HitOrder;
ShadowCall Call = {};
R_dsgraph_structure* CascadeGraphs = nullptr;
u32 ShadowRuns = 0; // what the shadow views' batch has left of SCENE_DYNAMIC_CLUSTERS
bool IsCast = false; // the batch casts the table
xr_vector<ISpatialShared> CascadeQuery;
u32 ObjectsOver = 0;
u32 ObjectsFrame = u32(-1);
xr_vector<const IRenderable*> ShadowDrops; // objects a shadow view left out, their shadow off the screen

// Of the objects only the shadow views would call: called, left out by every view, their shadow off the screen, and
// skeletons that kept their pose
u32 ShadowCalled = 0;
u32 ShadowSkipped = 0;
u32 ShadowHeld = 0;

// The renderable_Render calls of the views: of this frame so far and of the whole frame before. A particle system's
// are counted apart
xr_vector<std::pair<ObjectView, const IRenderable*>> ViewCalls, LastViewCalls;
u32 ParticleCalls = 0;
u32 LastParticleCalls = 0;
u32 ViewCallsFrame = u32(-1);
u32 ObjectStatsStep = 0;
xr_vector<const IRenderable*> CallScratch;

u32 FindId(const xr_vector<ObjectId>& Ids, const IRenderable* Object)
{
	const auto Found = std::lower_bound(Ids.begin(), Ids.end(), Object, [](const ObjectId& Item, const IRenderable* Key) { return Item.first < Key; });
	return Found != Ids.end() && Found->first == Object ? Found->second : SCENE_OBJECT_NONE;
}

bool IsFound(const xr_vector<ObjectId>& Ids, const IRenderable* Object)
{
	return std::binary_search(Ids.begin(), Ids.end(), ObjectId(Object, 0), [](const ObjectId& Left, const ObjectId& Right) { return Left.first < Right.first; });
}

// The lamp parts that cast an object
std::pair<const LampHit*, const LampHit*> HitsOf(const IRenderable* Object)
{
	const auto [First, Last] = std::equal_range(LampHits.begin(), LampHits.end(), LampHit{ const_cast<IRenderable*>(Object) }, [](const LampHit& Left, const LampHit& Right) { return Left.Renderable < Right.Renderable; });
	return { LampHits.data() + (First - LampHits.begin()), LampHits.data() + (Last - LampHits.begin()) };
}

// A lamp part's cast of a record, or what its graph draws instead
void LeaveLamps(const IRenderable* Object, CasterLeft Why, const LampLeft& Left)
{
	const auto [First, Last] = HitsOf(Object);
	CountsOf(ShadowKind::Lamp).Left[u32(Why)] += u32(Last - First);

	for (const LampHit* Hit = First; Hit != Last && Why != CasterLeft::Taken; ++Hit)
	{
		LampLefts.push_back(Left);
		LampLefts.back().Part = Hit->Part;
	}
}

void RollCalls()
{
	const u32 Frame = RFrame.dwFrame;

	if (ViewCallsFrame == Frame)
	{
		return;
	}

	const bool IsNext = ViewCallsFrame + 1 == Frame;
	LastViewCalls.swap(ViewCalls);
	LastParticleCalls = IsNext ? ParticleCalls : 0;
	ViewCalls.clear();
	ParticleCalls = 0;
	ViewCallsFrame = Frame;

	if (!IsNext)
	{
		LastViewCalls.clear();
	}
}

// In a sector the view's portals reached, inside one of the frustums they left of the view there
bool IsInPortals(const CPortalTraverser& Traverser, const CSector& Sector, const Fsphere& Sphere)
{
	if (!Traverser.IsVisited(Sector.index))
	{
		return false;
	}

	for (const CFrustum& View : Traverser.local_frustums[Sector.index])
	{
		if (View.testSphere_dirty(Sphere.P, Sphere.R))
		{
			return true;
		}
	}

	return false;
}

// Large enough on the screen, its distance as the field of view of 67 degrees would show it: a scope does not drop what it
// magnifies
bool IsLarge(const ISpatial& Spatial)
{
	const Fsphere& Sphere = Spatial.sphere;
	Fvector4 Centre, Edge;
	RFrame.mFullTransform.transform(Centre, Sphere.P);
	RFrame.mFullTransform.transform(Edge, Fvector(Sphere.P).mad(RFrame.vCameraRight, Sphere.R));
	return Centre.sub(Edge).magnitude() > Spatial.ssa_dyn_factor && RFrame.vCameraPosition.distance_to(Sphere.P) * RFrame.fFOV / 67.f < Spatial.ssa_d_cam;
}

// Whether the hull of two spheres can be inside the main view: outside a plane only when both are
bool IsHullSeen(const Fvector& First, float FirstRadius, const Fvector& Second, float SecondRadius)
{
	const CFrustum& View = RImplementation.ViewBase;

	for (int Plane = 0; Plane < View.p_count; ++Plane)
	{
		if (View.planes[Plane].classify(First) > FirstRadius && View.planes[Plane].classify(Second) > SecondRadius)
		{
			return false;
		}
	}

	return true;
}

// The capsule a sphere sweeps along the sun until no receiver can be: past the cascades' far end or below the level
bool IsSunShadowSeen(const Fsphere& Sphere, const SunReach& Sun)
{
	float Length = Sun.FarEnd - Sun.Direction.dotproduct(Sphere.P) + Sphere.R;

	if (Sun.Direction.y < -EPS_L)
	{
		Length = std::min(Length, (Sphere.P.y + Sphere.R - Sun.Floor) / -Sun.Direction.y);
	}

	return IsHullSeen(Sphere.P, Sphere.R, Fvector().mad(Sphere.P, Sun.Direction, std::max(Length, 0.f)), Sphere.R);
}

// The cone a sphere shades away from a lamp, out to its range: inside the hull of a sphere around the cone where the
// sphere is (its radius there and the sphere's) and one around the cone's far end. A lamp inside the sphere shades all
bool IsLampShadowSeen(const Fsphere& Sphere, const Fvector& Lamp, float Range)
{
	const Fvector Axis = Fvector().sub(Sphere.P, Lamp);
	const float Distance = Axis.magnitude();
	const float Tangent = Distance * Distance - Sphere.R * Sphere.R;

	if (Tangent <= 0.f)
	{
		return true;
	}

	const float Slope = Sphere.R / _sqrt(Tangent);
	const float Far = std::max(Range, Distance);
	return IsHullSeen(Sphere.P, Sphere.R + Distance * Slope, Fvector().mad(Lamp, Axis, Far / Distance), Far * Slope);
}

void CountCascades(u32 Cascades, CasterLeft Why)
{
	for (u32 Cascade = 0; Cascade < SCENE_CASTER_VIEWS; ++Cascade)
	{
		CascadeCounts[Cascade].Left[u32(Why)] += (Cascades >> Cascade) & 1;
	}
}

// A record of the table only the shadow views draw, of a caster the batch draws as it is
CasterLeft AddShadow(const SPass& Pass, dxRender_Visual* Visual, const Fmatrix& World, u32 Object, u32 Sector, u32& Runs, bool IsTranslucent = false)
{
	Hlsl::SceneModel Record = {};
	CSkeletonX* Skin = nullptr;
	const CasterLeft Why = MakeCaster(Pass, Visual, World, true, ModelRecords.size() < FrameModels, Runs, Record, Skin);

	if (Why != CasterLeft::Taken)
	{
		return Why;
	}

	Record.bin = SCENE_MATERIAL_NO_PIPELINE;
	Record.flags = SCENE_OBJECT_CASTER | SCENE_OBJECT_SHADOW_ONLY | (IsTranslucent ? SCENE_OBJECT_TRANSLUCENT : 0u);
	Record.sector = Sector;
	Record.object = Object;
	ModelRecords.push_back(Record);
	ModelSkins.push_back(Skin);
	ShadowSpheres.emplace_back(World, &PartSphere(Visual));
	return CasterLeft::Taken;
}

// The table of the frame before against the calls the views made for it
void LogObjects()
{
	const char* const ViewNames[u32(ObjectView::Count)] = { "main", "lamps", "decor", "sun cascades" };
	string512 Views = {};

	for (u32 View = 0; View < u32(ObjectView::Count); ++View)
	{
		CallScratch.clear();

		for (const auto& [Kind, Object] : LastViewCalls)
		{
			if (u32(Kind) == View)
			{
				CallScratch.push_back(Object);
			}
		}

		const u32 Count = u32(CallScratch.size());
		std::sort(CallScratch.begin(), CallScratch.end());
		CallScratch.erase(std::unique(CallScratch.begin(), CallScratch.end()), CallScratch.end());
		string64 Part;
		xr_sprintf(Part, "%s%s %u of %u", View ? ", " : "", ViewNames[View], Count, u32(CallScratch.size()));
		xr_strcat(Views, Part);
	}

	const u32 Main = u32(std::count_if(FrameObjects.begin(), FrameObjects.end(), [](const FrameObject& Object) { return Object.IsMain; }));
	u32 Kinds[4] = {};
	u32 Casters = 0;
	u32 Translucent = 0;

	for (const Hlsl::SceneModel& Record : ModelRecords)
	{
		++Kinds[(Record.flags & SCENE_OBJECT_SHADOW_ONLY) ? 3 : (Record.flags & SCENE_OBJECT_SORTED) ? 2 : (Record.flags & SCENE_OBJECT_HUD) ? 1 : 0];
		Casters += (Record.flags & SCENE_OBJECT_CASTER) != 0;
		Translucent += (Record.flags & SCENE_OBJECT_TRANSLUCENT) != 0;
	}

	Msg("* [scene] frame objects in the frame before: unique %u (main %u, only in the shadow views %u, over the cap %u); renderable_Render calls by view: %s, %u of particles; shadow-only objects called %u, left out with their shadow off the screen %u, skeletons keeping their pose %u",
		u32(FrameObjects.size()), Main, u32(FrameObjects.size()) - Main, ObjectsOver, Views, LastParticleCalls, ShadowCalled, ShadowSkipped, ShadowHeld);
	Msg("* [scene] frame objects' parts %u (main opaque %u, HUD %u, sorted %u, shadow-only %u), casters %u (translucent %u)",
		u32(ModelRecords.size()), Kinds[0], Kinds[1], Kinds[2], Kinds[3], Casters, Translucent);
}
} // namespace

void AddShadowLeaf(const MovingLeaf& Leaf, bool IsInvisible, const bool (&Mask)[3])
{
	ShaderElement* Element = Leaf.Visual->shader->E[ShaderVariant::Shadow]._get();

	if (!Element || IsInvisible)
	{
		return;
	}

	RequestMovingSkin(Leaf.Visual, Leaf.World);

	// The graph kept these in lists no shadow view draws
	if (Mask[Element->flags.iPriority / 2] && (Leaf.IsHud || Element->flags.bStrictB2F || Leaf.Visual->dcast_ParticleCustom()))
	{
		return;
	}

	for (u32 Index = 0; Index < Element->passes.size(); ++Index)
	{
		SPass* Pass = Element->passes[Index]._get();
		const u32 List = Pass ? (Pass->iPriority != u8(-1) ? Pass->iPriority : Element->flags.iPriority) / 2 : 2;

		// Only a lamp casts the second priority
		if (List > 1 || !Mask[List] || (List && !Call.IsLamp))
		{
			continue;
		}

		const CasterLeft Why = !IsCast ? CasterLeft::Shader : Call.Object == SCENE_OBJECT_NONE ? CasterLeft::Caps : AddShadow(*Pass, Leaf.Visual, Leaf.World, Call.Object, Call.Sector, ShadowRuns, List == 1);

		if (Call.IsLamp)
		{
			LeaveLamps(Call.Renderable, Why, { 0, Index, Leaf.Visual, Leaf.Object, Leaf.World, Leaf.Ssa });
		}

		if (List)
		{
			continue;
		}

		CountCascades(IsCast ? Call.Cascades : 0u, Why);

		for (u32 Cascade = 0; Cascade < SCENE_CASTER_VIEWS && Why != CasterLeft::Taken; ++Cascade)
		{
			if (Call.Cascades & (1u << Cascade))
			{
				InsertPasses(CascadeGraphs[Cascade], Element, Leaf.Visual, Leaf.Object, Leaf.World, Leaf.Ssa, 1, Index);
			}
		}
	}
}
} // namespace GpuSceneInternal

void GpuScene::CountRendered(ObjectView View, IRenderable* Object)
{
	RollCalls();

	if (!Object)
	{
		return;
	}

	const ESPATIAL_TYPE Type = Object->SpatialComponent ? Object->SpatialComponent->type : ESPATIAL_TYPE::NONE;

	if ((Type & (ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW)) == ESPATIAL_TYPE::NONE)
	{
		++ParticleCalls;
		return;
	}

	ViewCalls.emplace_back(View, Object);
}

void GpuScene::BeginLamps()
{
	LampHits.clear();
	LampLefts.clear();
	ShadowDrops.clear();
}

void GpuScene::HitLamp(const R_dsgraph_structure& Graph, const xr_vector<ISpatialShared>& Found, const IRenderable* Ignore, u32 Part, GpuTileView& View)
{
	const CPortalTraverser& Traverser = Graph.PortalTraverser;
	const CRenderTarget::SpotShadowPart& Lamp = RImplementation.LampParts[Part];

	// A bake keeps every object it holds: one left out would change its hash and build the cache again
	const bool IsTested = ObjectsFrame == RFrame.dwFrame && !(Lamp.Entry && Lamp.Entry->BakedCasters);

	for (u32 Sector = 0; Sector < std::min(SectorCount, SCENE_VIEW_SECTORS); ++Sector)
	{
		View.Sectors[Sector >> 5] |= Traverser.IsVisited(Sector) ? 1u << (Sector & 31) : 0u;
	}

	for (const ISpatialShared& Item : Found)
	{
		ISpatial* Spatial = Item.get();
		Spatial->spatial_updatesector();
		const CSector* Sector = static_cast<const CSector*>(Spatial->sector);
		IRenderable* Renderable = Spatial->dcast_Renderable();

		if (!Sector || !Renderable || Renderable == Ignore || !IsInPortals(Traverser, *Sector, Spatial->sphere))
		{
			continue;
		}

		if (IsTested && !IsFound(MainIds, Renderable) && !IsLampShadowSeen(Spatial->sphere, Lamp.Light->position, Lamp.Light->range))
		{
			ShadowDrops.push_back(Renderable);
			View.IsPartial = true;
			continue;
		}

		LampHits.push_back({ Renderable, u32(Sector->index), Part, u32(LampHits.size()), false });
	}
}

void GpuScene::HitDecor(IRenderable* Object, u32 Part)
{
	const CSector* Sector = Object->SpatialComponent ? static_cast<const CSector*>(Object->SpatialComponent->sector) : nullptr;
	LampHits.push_back({ Object, Sector ? u32(Sector->index) : SCENE_VIEW_SECTORS, Part, u32(LampHits.size()), true });
}

u32 GpuScene::ObjectOf(const IRenderable* Object) const
{
	const u32 Main = ObjectsFrame == RFrame.dwFrame ? FindId(MainIds, Object) : SCENE_OBJECT_NONE;
	return Main != SCENE_OBJECT_NONE ? Main : FindId(ShadowIds, Object);
}

u32 GpuScene::TestTile(const GpuTileView& View, u64& Hash) const
{
	Hash = 0;

	if (!TakesCasters() || ModelFrame != RFrame.dwFrame)
	{
		return 0;
	}

	Fmatrix Transform = View.Transform;
	CFrustum Frustum;
	Frustum.CreateFromMatrix(Transform, FRUSTUM_P_ALL & ~FRUSTUM_P_NEAR);
	const bool IsTranslucent = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS);
	bool IsStill = true;
	u32 Count = 0;

	// As AddTiles of scene_cull tests them
	for (const Hlsl::SceneModel& Record : ModelRecords)
	{
		const bool IsCast = View.IsOnly ? Record.object == View.Object : Record.object != View.Object || Record.object == SCENE_OBJECT_NONE;
		const bool IsSeen = Record.sector >= SCENE_VIEW_SECTORS || (View.Sectors[Record.sector >> 5] & (1u << (Record.sector & 31))) != 0;

		if (!(Record.flags & SCENE_OBJECT_CASTER) || ((Record.flags & SCENE_OBJECT_TRANSLUCENT) && !IsTranslucent) || !IsCast || !IsSeen ||
			!Frustum.testSphere_dirty(Fvector().set(Record.bounds.x, Record.bounds.y, Record.bounds.z), Record.bounds.w))
		{
			continue;
		}

		// A record a cast, by what it draws and where: added, so their order does not count
		u64 Own = 1469598103934665603ull;
		const u8* Bytes = reinterpret_cast<const u8*>(&Record.world);

		for (size_t Byte = 0; Byte < sizeof(Record.world); ++Byte)
		{
			Own = (Own ^ Bytes[Byte]) * 1099511628211ull;
		}

		for (const u32 Word : { Record.index_start, Record.triangle_count, Record.vertices, Record.source })
		{
			Own = (Own ^ Word) * 1099511628211ull;
		}

		Hash += Own;
		IsStill = IsStill && !(Record.source & SCENE_DYNAMIC_SKINNED);
		++Count;
	}

	Hash = !IsStill || View.IsPartial ? 0 : Hash ? Hash : 1;
	return Count;
}

void GpuScene::InsertLampLeftovers(R_dsgraph_structure& Graph, u32 Part) const
{
	const auto [First, Last] = std::equal_range(LampLefts.begin(), LampLefts.end(), LampLeft{ Part }, [](const LampLeft& Left, const LampLeft& Right) { return Left.Part < Right.Part; });

	for (auto Left = First; Left != Last; ++Left)
	{
		InsertPasses(Graph, Left->Visual->shader->E[ShaderVariant::Shadow]._get(), Left->Visual, Left->Object, Left->World, Left->Ssa, 3, Left->Pass);
	}
}

void GpuSceneInternal::LogFrameObjects()
{
	RollCalls();

	// render_memory_stats asks for the forward cull's lines: these go with them, of the frame before, whose views are all done
	if (CullStatsStep == 1 && ObjectStatsStep != 1)
	{
		LogObjects();
	}

	ObjectStatsStep = CullStatsStep;
}

void GpuScene::CollectObjects(R_dsgraph_structure& Main)
{
	ShadowCalled = ShadowSkipped = ShadowHeld = 0;
	FrameObjects.clear();
	MainIds.clear();
	MainItems.clear();
	MainSkeletons.clear();
	ObjectsOver = 0;
	ObjectsFrame = RFrame.dwFrame;

	PROF_EVENT("FrameObjects");
	CRender& Renderer = RImplementation;
	const GraphView& Eye = Main.ViewParams;
	const bool IsSingle = Renderer.Sectors.size() <= 1;
	const bool IsDynamic = psDeviceFlags.test(rsDrawDynamic);
	const float FullDetail = ps_r1_full_detail_distance_scale < 1.f ? std::max(100.f, Eye.FarPlane * ps_r1_full_detail_distance_scale) : 0.f;

	for (const ISpatialShared& Item : Main.lstRenderablesMain)
	{
		ISpatial* Spatial = Item.get();

		if (!Spatial)
		{
			continue;
		}

		const Fsphere& Sphere = Spatial->sphere;
		const bool IsLight = (Spatial->type & ESPATIAL_TYPE::LIGHTSOURCE) != ESPATIAL_TYPE::NONE;
		light* Light = IsLight ? (light*)Spatial->dcast_Light() : nullptr;
		const bool IsHudLight = Light && Light->flags.bHudMode;

		if (FullDetail > 0.f && !IsHudLight && Eye.CameraPosition.distance_to_sqr(Sphere.P) > _sqr(FullDetail + Sphere.R))
		{
			continue;
		}

		Spatial->spatial_updatesector();
		const CSector* Sector = static_cast<const CSector*>(Spatial->sector);

		if (!Sector)
		{
			continue;
		}

		if (IsHudLight)
		{
			MainItems.push_back({ nullptr, Light, nullptr, NoSector });
			continue;
		}

		if (!Renderer.HOM.visible(Spatial->sphere))
		{
			continue;
		}

		if (CGlow* Glow = Renderer.L_Glows && Renderer.phase == CRender::PHASE_NORMAL ? Spatial->dcast_CGlow() : nullptr)
		{
			Renderer.L_Glows->add(Glow);
			continue;
		}

		if (IsLight)
		{
			if (Light && Light->get_LOD() > EPS_L)
			{
				MainItems.push_back({ nullptr, Light, nullptr, NoSector });
			}

			continue;
		}

		IRenderable* Renderable = Spatial->dcast_Renderable();

		if (!Renderable || (!IsSingle && !IsInPortals(Main.PortalTraverser, *Sector, Sphere)))
		{
			continue;
		}

		const bool IsNear = (Spatial->type & ESPATIAL_TYPE::RENDERABLE) != ESPATIAL_TYPE::NONE && IsDynamic && Eye.CameraPosition.distance_to_sqr(Sphere.P) < _sqr(Eye.FogDistance);
		const bool IsSized = IsNear && IsLarge(*Spatial);
		const bool IsObject = IsNear && (IsSized || Sphere.R <= 1.f);

		if (IsObject || (Spatial->type & ESPATIAL_TYPE::PARTICLE) != ESPATIAL_TYPE::NONE)
		{
			MainItems.push_back({ Renderable, nullptr, IsSized ? PCKinematics((dxRender_Visual*)Renderable->renderable.visual) : nullptr, IsObject ? u32(Sector->index) : NoSector });
		}
	}

	// Posed once the collection is done: nothing in it reads the bones
	for (const MainItem& Item : MainItems)
	{
		if (Item.Skeleton && (MainSkeletons.empty() || MainSkeletons.back() != Item.Skeleton))
		{
			MainSkeletons.push_back(Item.Skeleton);
		}
	}

	PoseSkeletons(MainSkeletons);
	PROF_EVENT("ObjectsRender");

	for (const MainItem& Item : MainItems)
	{
		if (Item.Light)
		{
			Renderer.Lights.add_light(Item.Light);
			continue;
		}

		if (Item.Skeleton)
		{
			Item.Skeleton->CalculateWallmarks();
		}

		// One over the cap keeps no id: it casts no shadow, and is not called again
		const bool IsObject = Item.Sector != NoSector;
		const bool IsHeld = IsObject && FrameObjects.size() < SCENE_OBJECTS;
		Collector.Id = IsHeld ? u32(FrameObjects.size()) : SCENE_OBJECT_NONE;
		Collector.set_Object(Item.Renderable);
		Item.Renderable->renderable_Render(&Collector);
		CountRendered(ObjectView::Main, Item.Renderable);
		Collector.set_Object(nullptr);
		Collector.Id = SCENE_OBJECT_NONE;

		if (!IsObject)
		{
			continue;
		}

		ObjectsOver += !IsHeld;
		MainIds.emplace_back(Item.Renderable, IsHeld ? u32(FrameObjects.size()) : SCENE_OBJECT_NONE);

		if (IsHeld)
		{
			FrameObjects.push_back({ Item.Renderable, Item.Sector, 0, true });
		}
	}

	std::sort(MainIds.begin(), MainIds.end());
}

void GpuScene::CollectCasters(R_dsgraph_structure* Cascades, const u32* Drawn, u32 DrawnCount, xr_vector<ISpatialShared>* Queries)
{
	PROF_EVENT("SceneCollectCasters");
	const u32 Frame = RFrame.dwFrame;
	const bool HasMain = ObjectsFrame == Frame;
	ShadowCalled = ShadowSkipped = ShadowHeld = 0;

	// A frame the main view did not collect has no objects of its own
	if (!HasMain)
	{
		FrameObjects.clear();
		MainIds.clear();
		ObjectsFrame = Frame;
	}

	CascadeHits.clear();

	for (u32 Item = 0; Item < DrawnCount; ++Item)
	{
		CascadeCounts[Drawn[Item]] = {};
	}

	// What each drawn cascade's collection would have called: from its spatial query, in a sector its portals reached,
	// inside a frustum they left there
	for (u32 Item = 0; Item < DrawnCount && psDeviceFlags.test(rsDrawDynamic); ++Item)
	{
		const u32 Index = Drawn[Item];
		R_dsgraph_structure& Graph = Cascades[Index];
		CascadeQuery.clear();

		if (!Queries)
		{
			g_SpatialSpace->q_frustum(CascadeQuery, ISpatial_DB::O_ORDERED, ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW, Graph.ViewParams.Frustum);
		}

		for (const ISpatialShared& Found : Queries ? Queries[Index] : CascadeQuery)
		{
			ISpatial* Spatial = Found.get();
			Spatial->spatial_updatesector();
			const CSector* Sector = static_cast<const CSector*>(Spatial->sector);
			IRenderable* Renderable = Spatial->dcast_Renderable();

			if (Sector && Renderable && IsInPortals(Graph.PortalTraverser, *Sector, Spatial->sphere))
			{
				CascadeHits.push_back({ Renderable, u32(Sector->index), 1u << Index });
			}
		}
	}

	// Held, the query would keep what it found alive past its level
	CascadeQuery.clear();

	// An object once, at the first cascade that met it, with every cascade that did
	HitOrder.resize(CascadeHits.size());
	std::iota(HitOrder.begin(), HitOrder.end(), 0u);
	std::sort(HitOrder.begin(), HitOrder.end(), [](u32 Left, u32 Right) { return CascadeHits[Left].Renderable < CascadeHits[Right].Renderable || (CascadeHits[Left].Renderable == CascadeHits[Right].Renderable && Left < Right); });

	for (size_t At = 0, Next = 0; At < HitOrder.size(); At = Next)
	{
		CascadeHit& First = CascadeHits[HitOrder[At]];

		for (Next = At + 1; Next < HitOrder.size() && CascadeHits[HitOrder[Next]].Renderable == First.Renderable; ++Next)
		{
			First.Cascades |= CascadeHits[HitOrder[Next]].Cascades;
			CascadeHits[HitOrder[Next]].Cascades = 0;
		}
	}

	// One the main view did not call casts where its shadow can reach the main view
	const light* Sun = (const light*)RImplementation.Lights.sun_adapted._get();

	if (HasMain && Sun && !CascadeHits.empty())
	{
		float Size = 0.f;
		float Floor = flt_max;

		for (const sun::cascade& Cascade : RImplementation.m_sun_cascades)
		{
			Size = std::max(Size, Cascade.size);
		}

		for (IRender_Sector* Sector : RImplementation.Sectors)
		{
			const dxRender_Visual* Root = static_cast<CSector*>(Sector)->root();
			Floor = Root ? std::min(Floor, Root->vis.box.min.y) : Floor;
		}

		SunReach Reach = { Sun->direction, 0.f, Floor < flt_max ? Floor : -flt_max };
		Reach.Direction.normalize_safe();
		Reach.FarEnd = Reach.Direction.dotproduct(RFrame.vCameraPosition) + 1.41421f * Size + SunReachMargin;

		for (CascadeHit& Hit : CascadeHits)
		{
			const ISpatial* Spatial = Hit.Renderable->SpatialComponent.get();

			if (Hit.Cascades && Spatial && !IsFound(MainIds, Hit.Renderable) && !IsSunShadowSeen(Spatial->sphere, Reach))
			{
				ShadowDrops.push_back(Hit.Renderable);
				Hit.Cascades = 0;
			}
		}
	}

	// The lamp parts' objects together, in the order the parts met them; the first hit of each is where it is called
	std::stable_sort(LampHits.begin(), LampHits.end(), [](const LampHit& Left, const LampHit& Right) { return Left.Renderable < Right.Renderable; });
	LampCalls.clear();
	LampSkeletons.clear();

	for (size_t At = 0; At < LampHits.size(); ++At)
	{
		const LampHit& Hit = LampHits[At];

		if (At && LampHits[At - 1].Renderable == Hit.Renderable)
		{
			continue;
		}

		LampCalls.push_back(&Hit);
		CKinematics* Skeleton = PCKinematics((dxRender_Visual*)Hit.Renderable->renderable.visual);

		// What the main view did not call is posed within 100 m, as a lamp's own collection did
		if (Skeleton && !IsFound(MainIds, Hit.Renderable) && RFrame.vCameraPosition.distance_to_sqr(Hit.Renderable->renderable.xform.c) <= 10000.f)
		{
			LampSkeletons.push_back(Skeleton);
		}
	}

	std::sort(LampCalls.begin(), LampCalls.end(), [](const LampHit* Left, const LampHit* Right) { return Left->Order < Right->Order; });
	std::sort(LampSkeletons.begin(), LampSkeletons.end());
	LampSkeletons.erase(std::unique(LampSkeletons.begin(), LampSkeletons.end()), LampSkeletons.end());

	// Before anything poses them: the skeletons of those to be called keep their pose by their distance from the eye
	const auto Hold = [](IRenderable* Renderable)
	{
		CKinematics* Skeleton = PCKinematics((dxRender_Visual*)Renderable->renderable.visual);
		const float Share = clampr((RFrame.vCameraPosition.distance_to(Renderable->renderable.xform.c) - ShadowPoseNear) / (ShadowPoseFar - ShadowPoseNear), 0.f, 1.f);
		ShadowHeld += Skeleton && !IsFound(MainIds, Renderable) && Skeleton->HoldPose(u32(Share * float(UCalc_Interval)));
	};

	for (const CascadeHit& Hit : CascadeHits)
	{
		if (Hit.Cascades)
		{
			Hold(Hit.Renderable);
		}
	}

	for (const LampHit* Hit : LampCalls)
	{
		Hold(Hit->Renderable);
	}

	PoseSkeletons(LampSkeletons);

	// The main view's objects the cascades and lamp parts cast
	for (const CascadeHit& Hit : CascadeHits)
	{
		if (const u32 Main = FindId(MainIds, Hit.Renderable); Hit.Cascades && Main != SCENE_OBJECT_NONE)
		{
			FrameObjects[Main].Cascades |= Hit.Cascades;
		}
	}

	for (const LampHit* Hit : LampCalls)
	{
		if (const u32 Main = FindId(MainIds, Hit->Renderable); Main != SCENE_OBJECT_NONE)
		{
			FrameObjects[Main].IsLamp = true;
		}
	}

	// What the batch cannot draw as it is goes to the graphs of the cascades and lamp parts that would have collected it:
	// first of the main view's objects, which cast as they stand
	const bool IsLampTranslucent = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS);
	IsCast = TakesCasters();
	ShadowRuns = SCENE_DYNAMIC_CLUSTERS;
	CascadeGraphs = Cascades;

	for (const MainPart& Part : MainParts)
	{
		const FrameObject& Object = FrameObjects[Part.Object];
		const u32 Mask = Object.Cascades;
		const ShaderElement* Shadow = Part.Visual->shader ? Part.Visual->shader->E[ShaderVariant::Shadow]._get() : nullptr;

		if ((!Mask && !Object.IsLamp) || !Shadow || Shadow->passes.empty())
		{
			continue;
		}

		// The cascades draw no shadow pass of the second priority: a record the main view draws keeps it for a view that does
		const SPass* Pass = Shadow->passes.size() == 1 ? Shadow->passes[0]._get() : nullptr;
		const bool IsTranslucent = Pass && (Pass->iPriority != u8(-1) ? Pass->iPriority : Shadow->flags.iPriority) / 2 == 1;
		const bool IsLamp = Object.IsLamp && (!IsTranslucent || IsLampTranslucent);
		const bool IsPart = Part.Visual->Type == MT_SKELETON_GEOMDEF_ST || Part.Visual->Type == MT_SKELETON_GEOMDEF_PM;
		CasterLeft Why = CasterLeft::Shader;

		if (IsCast && Pass && Part.Record != NoRecord)
		{
			Hlsl::SceneModel& Record = ModelRecords[Part.Record];
			const u32 Count = (Record.triangle_count + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES;
			u32 Alpha = SCENE_TEXTURE_ABSENT;
			Why = CasterPass(*Pass, IsPart, Alpha);
			Why = Why == CasterLeft::Taken && Count > ShadowRuns ? CasterLeft::Caps : Why;

			if (Why == CasterLeft::Taken)
			{
				Record.flags |= SCENE_OBJECT_CASTER | (IsTranslucent ? SCENE_OBJECT_TRANSLUCENT : 0u);
				Record.alpha = Alpha;
				Record.sector = Object.Sector;
				ShadowRuns -= Count;
			}
		}
		else if (IsCast && Pass && (!IsTranslucent || IsLamp))
		{
			Why = AddShadow(*Pass, Part.Visual, Part.World, Part.Object, Object.Sector, ShadowRuns, IsTranslucent);
		}

		if (IsLamp)
		{
			LeaveLamps(Object.Renderable, Why, { 0, u32(-1), Part.Visual, nullptr, Part.World, Part.Ssa });
		}

		if (IsTranslucent || !Mask)
		{
			continue;
		}

		CountCascades(IsCast ? Mask : 0u, Why);

		for (u32 Cascade = 0; Cascade < SCENE_CASTER_VIEWS && Why != CasterLeft::Taken; ++Cascade)
		{
			if (Mask & (1u << Cascade))
			{
				InsertPasses(Cascades[Cascade], Shadow, Part.Visual, nullptr, Part.World, Part.Ssa);
			}
		}
	}

	// The others are called once, in the shadow phase the shadow views called them in, the cascades' first, their leaves
	// cast as the collector adds them (AddShadowLeaf). Their detail is chosen by the main view's eye. The lamps cast the
	// second priority with translucent shadows on
	GraphView View = RImplementation.GraphMain.ViewParams;
	View.Phase = CRender::PHASE_SMAP;
	View.SsaDiscard = 0.f;
	View.FullDetail = true;
	View.Distortion = false;
	View.TrackStats = false;
	const bool Passes[3] = { true, IsLampTranslucent && !LampCalls.empty(), false };
	Collector.Begin(View, Passes, nullptr);
	ShadowIds.clear();

	const auto CallOnce = [&](IRenderable* Renderable, u32 Sector, u32 Cascades, ObjectView By)
	{
		ObjectsOver += FrameObjects.size() == SCENE_OBJECTS;
		const u32 Object = FrameObjects.size() < SCENE_OBJECTS ? u32(FrameObjects.size()) : SCENE_OBJECT_NONE;
		const auto [FirstHit, LastHit] = HitsOf(Renderable);
		Call = { Renderable, Object, Sector, Cascades, FirstHit != LastHit };

		if (Object != SCENE_OBJECT_NONE)
		{
			FrameObjects.push_back({ Renderable, Sector, Cascades, false });
		}

		ShadowIds.emplace_back(Renderable, Object);
		Renderable->renderable_Render(&Collector);
		CountRendered(By, Renderable);
	};

	for (const CascadeHit& Hit : CascadeHits)
	{
		if (Hit.Cascades && !IsFound(MainIds, Hit.Renderable))
		{
			CallOnce(Hit.Renderable, Hit.Sector, Hit.Cascades, ObjectView::Cascade);
		}
	}

	std::sort(ShadowIds.begin(), ShadowIds.end());
	const size_t CascadeCalls = ShadowIds.size();

	for (const LampHit* Hit : LampCalls)
	{
		const auto Called = ShadowIds.begin() + CascadeCalls;

		if (!IsFound(MainIds, Hit->Renderable) && !std::binary_search(ShadowIds.begin(), Called, ObjectId(Hit->Renderable, 0), [](const ObjectId& Left, const ObjectId& Right) { return Left.first < Right.first; }))
		{
			CallOnce(Hit->Renderable, Hit->Sector, 0, Hit->IsDecor ? ObjectView::Decor : ObjectView::Lamp);
		}
	}

	std::sort(ShadowIds.begin(), ShadowIds.end());
	std::sort(ShadowDrops.begin(), ShadowDrops.end());
	ShadowDrops.erase(std::unique(ShadowDrops.begin(), ShadowDrops.end()), ShadowDrops.end());
	ShadowCalled = u32(ShadowIds.size());
	ShadowSkipped = u32(std::count_if(ShadowDrops.begin(), ShadowDrops.end(), [](const IRenderable* Object) { return !IsFound(ShadowIds, Object); }));
	ShadowDrops.clear();
	std::stable_sort(LampLefts.begin(), LampLefts.end(), [](const LampLeft& Left, const LampLeft& Right) { return Left.Part < Right.Part; });
	LampHits.clear();
}
