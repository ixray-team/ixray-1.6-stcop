#include "stdafx.h"
#include "xrCDB.h"

#include "embree4/rtcore_ray.h"
#include "embree4/rtcore_scene.h"
#include "embree4/rtcore_common.h"

#include "Frustum.h"
#include "override/Model.h"
#include "xrServerEntities/object_destroyer.h"

using namespace CDB;

static const char* GetDeviceConfig()
{
	bool avx_test = CPU::ID().hasFeature(CPUFeature::AVX2);
	bool sse = CPU::ID().hasFeature(CPUFeature::SSE);

	const char* config = "";
	if (avx_test)
	{
		config = "isa=avx2";
	}
	else if (sse)
	{
		config = "isa=sse4.2";
	}
	else
	{
		config = "isa=sse2";
	}

	return config;
}

struct EmbreeDeviceWrapper
{
	EmbreeDeviceWrapper()
	{
		auto fError = [](void* userPtr, enum RTCError code, const char* str)
		{
			I_ASSERT_M(false, "Code: [%s], Reason: [%s]", magic_enum::enum_name(code).data(), str);
		};

		EmbreeDevice = rtcNewDevice(GetDeviceConfig());
		rtcSetDeviceErrorFunction(EmbreeDevice, fError, nullptr);
	}
	~EmbreeDeviceWrapper()
	{
		rtcReleaseDevice(EmbreeDevice);
	}
	
	RTCDevice EmbreeDevice;
};

RTCDevice CDB::GetEmbreeDevice()
{
	static EmbreeDeviceWrapper Wrapper;
	return Wrapper.EmbreeDevice;
}

CDB::MODEL::~MODEL()
{
	load_task.wait();
	delete_data(verts);
	delete_data(tris);
	rtcReleaseBVH(tree);
}

void MODEL::build_simple()
{
	load_task.run([&]()
	{
		PROF_START_THREAD("build collision")
		VERIFY(!IsBuilt);
		auto EmbreeDevice = GetEmbreeDevice();
		InstaceScene = rtcNewScene(EmbreeDevice);
		rtcSetSceneBuildQuality(InstaceScene, RTC_BUILD_QUALITY_HIGH);

		for(auto& elem : instances)
		{
			auto InstModel = models[elem.ModelIndex];
			while (!InstModel->IsBuilt){}
			VERIFY(InstModel->models.empty());
			auto InstanceOnLevel = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_INSTANCE);
			rtcSetGeometryInstancedScene(InstanceOnLevel, InstModel->InstaceScene);

			float matrix[16];
		
			matrix[0] = elem.Transform._11;
			matrix[1] = elem.Transform._12;
			matrix[2] = elem.Transform._13;
			matrix[3] = elem.Transform._14;

			matrix[4] = elem.Transform._21;
			matrix[5] = elem.Transform._22;
			matrix[6] = elem.Transform._23;
			matrix[7] = elem.Transform._24;

			matrix[8] = elem.Transform._31;
			matrix[9] = elem.Transform._32;
			matrix[10] = elem.Transform._33;
			matrix[11] = elem.Transform._34;

			matrix[12] = elem.Transform._41;
			matrix[13] = elem.Transform._42;
			matrix[14] = elem.Transform._43;
			matrix[15] = elem.Transform._44;
		
			rtcSetGeometryTransform(InstanceOnLevel, 0, RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR, &matrix);
			rtcSetGeometryUserData(InstanceOnLevel, &elem);
			
			rtcCommitGeometry(InstanceOnLevel);
		
			rtcAttachGeometry(InstaceScene, InstanceOnLevel);
			rtcReleaseGeometry(InstanceOnLevel);
		}
	
		auto BatchedGeometry = rtcNewGeometry(EmbreeDevice, RTC_GEOMETRY_TYPE_TRIANGLE);
	
		rtcSetSharedGeometryBuffer(BatchedGeometry, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, verts.data(), 0, sizeof(Fvector), verts.size());
		rtcSetSharedGeometryBuffer(BatchedGeometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, tris.data(), 0, sizeof(CDB::TRI), tris.size());
		rtcSetGeometryUserData(BatchedGeometry, this);
	
		rtcCommitGeometry(BatchedGeometry);
	
		rtcAttachGeometry(InstaceScene, BatchedGeometry);
		rtcReleaseGeometry(BatchedGeometry);
	
		rtcCommitScene(InstaceScene);
	
		CDB::BuilderConfig::Data UserData = {&root, tris.size()};
	
		CDB::BuilderConfig Config;
		Config.Vertices = &verts;
		Config.Faces = &tris;
		Config.Instances = &instances;
		Config.UserData = &UserData;
		tree = CDB::BuildModel(Config);
	
		IsBuilt = true;
		PROF_STOP_THREAD()
	});
}

// Collision queries



void COLLIDER::ray_query(const MODEL& m_def, const Fvector& r_start, const Fvector& r_dir, float r_range)
{
	PROF_EVENT("COLLIDER::ray_query");
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.InstaceScene)
	{
		return;
	}
	
	r_clear();
	r_vec().reserve(16);
	
	struct ColliderContext : public RTCRayQueryContext
	{
		COLLIDER* collider = nullptr;
		const MODEL* model = nullptr;
	};
	ColliderContext context;
	rtcInitRayQueryContext(&context);
	context.collider = this;
	context.model = &m_def;
	
	if(!!(ray_mode & OPT_ONLYFIRST))
	{
		PROF_EVENT("COLLIDER::ray_query::Occluded");
		RTCRay ray;
		ray.org_x = r_start.x;
		ray.org_y = r_start.y;
		ray.org_z = r_start.z;
		ray.dir_x = r_dir.x;
		ray.dir_y = r_dir.y;
		ray.dir_z = r_dir.z;
		ray.tnear = 0.0f;
		ray.tfar = r_range;
		ray.mask = -1;
		ray.flags = 0;
		ray.time = 0.0f;
		
		RTCOccludedArguments args;
		rtcInitOccludedArguments(&args);
		struct Filter
		{
			static void Execute(const RTCFilterFunctionNArguments* args)
			{
				PROF_EVENT("COLLIDER::ray_query::OccludedExecute");
				VERIFY(args->N == 1);
				if (*args->valid != -1)
				{
					return;
				}
				auto model = (MODEL*)args->geometryUserPtr;
				VERIFY(model);
				
				auto context = (ColliderContext*)args->context;
				auto self = context->collider;
				if(!!(self->ray_mode & OPT_CULL))
				{
					auto Nx = RTCHitN_Ng_x(args->hit, 1, 0);
					auto Ny = RTCHitN_Ng_y(args->hit, 1, 0);
					auto Nz = RTCHitN_Ng_z(args->hit, 1, 0);
					auto Dx = RTCRayN_dir_x(args->ray, 1, 0);
					auto Dy = RTCRayN_dir_y(args->ray, 1, 0);
					auto Dz = RTCRayN_dir_z(args->ray, 1, 0);
					float dot = Nx*Dx+Ny*Dy+Nz*Dz;
					if(dot > 0)
					{
						args->valid[0] = 0;
						return;
					}
				}
				
				auto PrimID = RTCHitN_primID(args->hit, 1, 0);
				VERIFY(PrimID < model->tris.size());
				
				RESULT& R = self->r_add();
				R.model = model;
				R.tris_id = PrimID;
				R.range = RTCRayN_tfar(args->ray, 1, 0);
				R.u = RTCHitN_u(args->hit, 1, 0);
				R.v = RTCHitN_v(args->hit, 1, 0);

				auto InstID = RTCHitN_instID(args->hit, 1, 0, 0);
				if (InstID != u32(-1))
				{
					auto Geom = rtcGetGeometry(context->model->InstaceScene, InstID);
					auto Data = (CDB::InstanceData*)rtcGetGeometryUserData(Geom);
					R.ModelWorldTransform = Data->Transform;
					R.Sector = Data->Sector;
				}
			}
		};
		args.filter = Filter::Execute;
		args.context = &context;
		args.flags = RTC_RAY_QUERY_FLAG_INVOKE_ARGUMENT_FILTER;
		rtcOccluded1(m_def.InstaceScene, &ray, &args);
	} else
	{
		PROF_EVENT("COLLIDER::ray_query::Intersect");
		RTCRayHit ray;
		ray.ray.org_x = r_start.x;
		ray.ray.org_y = r_start.y;
		ray.ray.org_z = r_start.z;
		ray.ray.dir_x = r_dir.x;
		ray.ray.dir_y = r_dir.y;
		ray.ray.dir_z = r_dir.z;
		ray.ray.tnear = 0.0f;
		ray.ray.tfar = r_range;
		ray.ray.mask = -1;
		ray.ray.flags = 0;
		ray.ray.time = 0.0f;
		ray.hit.geomID = RTC_INVALID_GEOMETRY_ID;
		
		RTCIntersectArguments args;
		rtcInitIntersectArguments(&args);
		struct Filter
		{
			static void Execute(const RTCFilterFunctionNArguments* args)
			{
				PROF_EVENT("COLLIDER::ray_query::IntersectExecute");
				VERIFY(args->N == 1);
				if (*args->valid != -1)
				{
					return;
				}
				auto model = (MODEL*)args->geometryUserPtr;
				VERIFY(model);

				auto context = (ColliderContext*)args->context;
				auto self = context->collider;
				if(!!(self->ray_mode & OPT_CULL))
				{
					auto Nx = RTCHitN_Ng_x(args->hit, 1, 0);
					auto Ny = RTCHitN_Ng_y(args->hit, 1, 0);
					auto Nz = RTCHitN_Ng_z(args->hit, 1, 0);
					auto Dx = RTCRayN_dir_x(args->ray, 1, 0);
					auto Dy = RTCRayN_dir_y(args->ray, 1, 0);
					auto Dz = RTCRayN_dir_z(args->ray, 1, 0);
					float dot = Nx*Dx+Ny*Dy+Nz*Dz;
					if(dot > 0)
					{
						args->valid[0] = 0;
						return;
					}
				}

				auto PrimID = RTCHitN_primID(args->hit, 1, 0);
				VERIFY(PrimID < model->tris.size());
				
				float Range = RTCRayN_tfar(args->ray, 1, 0);
				RESULT* R;
				if (!!(self->ray_mode & OPT_ONLYNEAREST) && !self->rd.empty())
				{
					R = self->r_any().range > Range ? self->rd.data() : self->rd.data()+1;
				} else
				{
					R = &self->r_add();
				}
				
				R->model = model;
				R->tris_id = PrimID;
				R->range = Range;
				R->u = RTCHitN_u(args->hit, 1, 0);
				R->v = RTCHitN_v(args->hit, 1, 0);

				auto InstID = RTCHitN_instID(args->hit, 1, 0, 0);
				if (InstID != u32(-1))
				{
					auto Geom = rtcGetGeometry(context->model->InstaceScene, InstID);
					auto Data = (CDB::InstanceData*)rtcGetGeometryUserData(Geom);
					R->ModelWorldTransform = Data->Transform;
					R->Sector = Data->Sector;
				} else
				{
					R->Sector = model->tris[PrimID].sector;
				}
				
				/*if(!!(self->ray_mode & OPT_ONLYNEAREST) && self->rd.size() > 1)
				{
					auto& First = self->rd[0];
					auto& Last = self->rd[self->rd.size() - 1];
					if (First.range > Last.range)
					{
						First = Last;
					}
					self->rd.pop_back();
				}*/
			}
		};
		args.filter = Filter::Execute;
		args.context = &context;
		args.flags = RTC_RAY_QUERY_FLAG_INVOKE_ARGUMENT_FILTER;
		rtcIntersect1(m_def.InstaceScene, &ray, &args);
	}
}

constexpr size_t MAX_INSTANCE_DEPTH = 4;

struct cform_stack final
{
	xr_array<const MODEL*, MAX_INSTANCE_DEPTH> m_def_array = {};
	size_t CurrentIndex = 0;
	
	cform_stack(const MODEL& RootModel){ m_def_array[0] = &RootModel; }
	
	ICF const MODEL& GetCurrentTree()
	{
		VERIFY(CurrentIndex < m_def_array.size());
		VERIFY(m_def_array[CurrentIndex]);
		return *m_def_array[CurrentIndex];
	}
	
	ICF void Push(const MODEL& NewTree){ CurrentIndex++; VERIFY(CurrentIndex < m_def_array.size()); m_def_array[CurrentIndex] = &NewTree; }
	ICF void Pop(){ m_def_array[CurrentIndex] = nullptr; VERIFY(CurrentIndex > 0); CurrentIndex--;  }
};

struct cform_frustum_collider final
{
	xr_vector<RESULT>* dest = nullptr;
	cform_stack* stack = nullptr;
	const CFrustum* F = nullptr;
	ColliderCallback OnCheckNode = nullptr;

	bool bClass3, bFirst;

	ICF void Prim(ElementID InPrim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(InPrim.IsNotPointer);
		if (InPrim.IsInstance)
		{
			auto& CurModel = stack->GetCurrentTree();
			auto& Instances = CurModel.get_instances();
			auto& Prototype = Instances[InPrim.Index];
			auto& Models = CurModel.get_models();
			auto& ChildModel = Models[Prototype.ModelIndex];
			stack->Push(*ChildModel);
			xr_scope_exit g = [&]()
			{
				stack->Pop();
			};

			CFrustum LocalF;
			for(int i = 0; i < F->p_count; ++i)
			{
				Fplane LocalPlane = F->planes[i];
				Fvector worldPoint = LocalPlane.n * (-LocalPlane.d);
				Prototype.InvTransform.transform_dir(LocalPlane.n);
				Prototype.InvTransform.transform_tiny(worldPoint);
				LocalPlane.d = -LocalPlane.n.dotproduct(worldPoint);
				float len = LocalPlane.n.magnitude();
				if (len > EPS) {
					LocalPlane.n.div(len);
					LocalPlane.d /= len;
				}
				LocalF._add(LocalPlane);
			}
			
			Fmatrix NewToWorld;
			NewToWorld.mul(ToWorldTransform, Prototype.Transform);
			cform_frustum_collider FC{
				dest,
				stack,
				&LocalF,
				OnCheckNode,
				bClass3,
				bFirst
			};
			FC.Stab(*ChildModel->root, LocalF.getMask(), NewToWorld);
			return;
		}
		
		auto& CurModel = stack->GetCurrentTree();
		auto& Tri = CurModel.tris[InPrim.Index];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = {
			CurModel.verts[TriVerts[0]],
			CurModel.verts[TriVerts[1]],
			CurModel.verts[TriVerts[2]]
		};

		if (bClass3)
		{
			thread_local sPoly Src, Dst;
			Src.resize(3);
			Fvector* src = Src.begin();
			src[0] = tri_verts[0];
			src[1] = tri_verts[1];
			src[2] = tri_verts[2];
			if (F->ClipPoly(Src, Dst))
			{
				RESULT& R = dest->emplace_back();
				R.ModelWorldTransform = ToWorldTransform;
				R.model = &CurModel;
				R.tris_id = InPrim.Index;
			}
		}
		else
		{
			RESULT& R = dest->emplace_back();
			R.ModelWorldTransform = ToWorldTransform;
			R.model = &CurModel;
			R.tris_id = InPrim.Index;
		}
	}

	void Stab(const BVHNode& node, u32 mask, const Fmatrix& ToWorldTransform)
	{
		if (OnCheckNode)
		{
			OnCheckNode(node, ToWorldTransform);
		}

		for(size_t i = 0; i < node.GetSize(); ++i)
		{
			if(node.HasNode(i))
			{
				// Actual frustum/aabb test		
				if (fcvNone == F->testAABB(node.GetAABB(i).data(), mask))
				{
					continue;
				}
				Stab(node.GetNode(i), mask, ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
		
				// Early exit for "only first"
				if (bFirst && !dest->empty())
				{
					return;
				}
			}
		}
	}
};

void COLLIDER::frustum_query(const MODEL& m_def, const CFrustum& F, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::frustum_query");
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.InstaceScene)
	{
		return;
	}

	r_clear();
	r_vec().reserve(16);

	cform_stack S = m_def;
	cform_frustum_collider BC{};
	BC.dest = &rd;
	BC.stack = &S;
	BC.F = &F;
	BC.bClass3 = box_mode & OPT_FULL_TEST;
	BC.bFirst = box_mode & OPT_ONLYFIRST;
	BC.OnCheckNode = OnCheckNode;
	BC.Stab(*m_def.root, F.getMask(), Fmatrix{Fmatrix::EIdentity::Identity});
}

template<class Shape>
struct cform_shape_collider;

template<class Shape>
static void ColliderSubstep(cform_shape_collider<Shape>& GlobalShape, ElementID InPrim, const Fmatrix& ToWorldTransform);

template <class Shape>
struct cform_shape_collider final
{
	xr_vector<RESULT>* dest = nullptr;
	cform_stack* stack = nullptr;
	ColliderCallback OnCheckNode = nullptr;
	Shape shape;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void Prim(ElementID InPrim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(InPrim.IsNotPointer);
		if (InPrim.IsInstance)
		{
			ColliderSubstep(*this, InPrim, ToWorldTransform);
			return;
		}
		
		auto& CurModel = stack->GetCurrentTree();
		auto& Tri = CurModel.tris[InPrim.Index];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = {
			CurModel.verts[TriVerts[0]],
			CurModel.verts[TriVerts[1]],
			CurModel.verts[TriVerts[2]]
		};
		if (!shape.intersectTri(tri_verts, bClass3))
		{
			return;
		}

		RESULT& R = dest->emplace_back();
		R.ModelWorldTransform = ToWorldTransform;
		R.model = &CurModel;
		R.tris_id = InPrim.Index;
	}

	void Stab(const BVHNode& node, const Fmatrix& ToWorldTransform)
	{		
		if (OnCheckNode)
		{
			OnCheckNode(node, ToWorldTransform);
		}

		for(size_t i = 0; i < node.GetSize(); ++i)
		{
			if(node.HasNode(i))
			{
				// Actual box-box test
				if (!shape.intersectAABB(node.GetAABB(i)))
				{
					continue;
				}
				Stab(node.GetNode(i), ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
		
				// Early exit for "only first"
				if (bFirst && !dest->empty())
				{
					return;
				}
			}
		}
	}
};

template<typename T>
static void ColliderSubstep(cform_shape_collider<_box3<T>>& GlobalCollider, ElementID InPrim, const Fmatrix& ToWorldTransform)
{
	auto& CurModel = GlobalCollider.stack->GetCurrentTree();
	auto& Instances = CurModel.get_instances();
	auto& Prototype = Instances[InPrim.Index];
	if (!GlobalCollider.shape.intersectAABB(Prototype.GlobalAABB))
	{
		return;
	}
	auto& Models = CurModel.get_models();
	auto& ChildModel = Models[Prototype.ModelIndex];
	GlobalCollider.stack->Push(*ChildModel);
	xr_scope_exit g = [&]()
	{
		GlobalCollider.stack->Pop();
	};

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	thread_local xr_vector<RESULT> LocResult(100);
	LocResult.clear();
	cform_shape_collider<_box3<T>> BC;
	BC.OnCheckNode = GlobalCollider.OnCheckNode;
	BC.dest = &LocResult;
	BC.stack = GlobalCollider.stack;
	BC.bClass3 = GlobalCollider.bClass3;
	BC.bFirst = GlobalCollider.bFirst;
	BC.shape.invalidate();
	for (int i = 0; i < 8; i++)
	{
		Fvector AABBPoint;
		GlobalCollider.shape.getpoint(i, AABBPoint);
		Prototype.InvTransform.transform_tiny(AABBPoint);
		BC.shape.modify(AABBPoint);
	}
	BC.Stab(*ChildModel->root, NewToWorld);
	for (auto& elem : LocResult)
	{
		auto& Tri = elem.model->tris[elem.tris_id];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = {
			elem.model->verts[TriVerts[0]],
			elem.model->verts[TriVerts[1]],
			elem.model->verts[TriVerts[2]]
		};
		elem.ModelWorldTransform.transform_tiny(tri_verts[0]);
		elem.ModelWorldTransform.transform_tiny(tri_verts[1]);
		elem.ModelWorldTransform.transform_tiny(tri_verts[2]);
		if (GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->emplace_back(elem);
		}
	}
}

template<typename T>
static void ColliderSubstep(cform_shape_collider<_obb<T>>& GlobalCollider, ElementID InPrim, const Fmatrix& ToWorldTransform)
{
	auto& CurModel = GlobalCollider.stack->GetCurrentTree();
	auto& Instances = CurModel.get_instances();
	auto& Prototype = Instances[InPrim.Index];
	auto& Models = CurModel.get_models();
	auto& ChildModel = Models[Prototype.ModelIndex];
	GlobalCollider.stack->Push(*ChildModel);
	xr_scope_exit g = [&]()
	{
		GlobalCollider.stack->Pop();
	};
	
	cform_frustum_collider FC;
	FC.OnCheckNode = GlobalCollider.OnCheckNode;
	FC.dest = GlobalCollider.dest;
	FC.stack = GlobalCollider.stack;
	FC.bClass3 = GlobalCollider.bClass3;
	FC.bFirst = GlobalCollider.bFirst;

	CFrustum FLocal;
	FC.F = &FLocal;
	auto PlaneFunc = [&](Fvector n, float d)
	{
		Fvector worldPoint = n * (-d);
		Fplane LocalPlane;
		Prototype.InvTransform.transform_dir(LocalPlane.n, n);
		Prototype.InvTransform.transform_tiny(worldPoint);
		LocalPlane.d = -LocalPlane.n.dotproduct(worldPoint);
		float len = LocalPlane.n.magnitude();
		if (len > EPS) {
			LocalPlane.n.div(len);
			LocalPlane.d /= len;
		}
		FLocal._add(LocalPlane);
	};
	PlaneFunc(GlobalCollider.shape.m_rotate.i, -(GlobalCollider.shape.m_rotate.i*GlobalCollider.shape.m_translate + GlobalCollider.shape.m_halfsize.x));
	PlaneFunc(-GlobalCollider.shape.m_rotate.i, GlobalCollider.shape.m_rotate.i*GlobalCollider.shape.m_translate - GlobalCollider.shape.m_halfsize.x);
	PlaneFunc(GlobalCollider.shape.m_rotate.j, -(GlobalCollider.shape.m_rotate.j*GlobalCollider.shape.m_translate + GlobalCollider.shape.m_halfsize.y));
	PlaneFunc(-GlobalCollider.shape.m_rotate.j, GlobalCollider.shape.m_rotate.j*GlobalCollider.shape.m_translate - GlobalCollider.shape.m_halfsize.y);
	PlaneFunc(GlobalCollider.shape.m_rotate.k, -(GlobalCollider.shape.m_rotate.k*GlobalCollider.shape.m_translate + GlobalCollider.shape.m_halfsize.z));
	PlaneFunc(-GlobalCollider.shape.m_rotate.k, GlobalCollider.shape.m_rotate.k*GlobalCollider.shape.m_translate - GlobalCollider.shape.m_halfsize.z);

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	FC.Stab(*ChildModel->root, FLocal.getMask(), NewToWorld);
}

template<typename T>
static void ColliderSubstep(cform_shape_collider<_sphere<T>>& GlobalCollider, ElementID InPrim, const Fmatrix& ToWorldTransform)
{
	auto& CurModel = GlobalCollider.stack->GetCurrentTree();
	auto& Instances = CurModel.get_instances();
	auto& Prototype = Instances[InPrim.Index];
	auto& Models = CurModel.get_models();
	auto& ChildModel = Models[Prototype.ModelIndex];
	GlobalCollider.stack->Push(*ChildModel);
	xr_scope_exit g = [&]()
	{
		GlobalCollider.stack->Pop();
	};

	_obb<T> obb;
	Prototype.InvTransform.transform_tiny(obb.m_translate, GlobalCollider.shape.P);
	obb.m_rotate.i = Prototype.InvTransform.i;
	obb.m_rotate.j = Prototype.InvTransform.j;
	obb.m_rotate.k = Prototype.InvTransform.k;
	obb.m_halfsize.x = GlobalCollider.shape.R*obb.m_rotate.i.magnitude();
	obb.m_halfsize.y = GlobalCollider.shape.R*obb.m_rotate.j.magnitude();
	obb.m_halfsize.z = GlobalCollider.shape.R*obb.m_rotate.k.magnitude();
	obb.m_rotate.i.normalize();
	obb.m_rotate.j.normalize();
	obb.m_rotate.k.normalize();
	VERIFY(_valid(obb));

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	xr_vector<RESULT> local_results;
	cform_shape_collider<_obb<T>> OC{
		&local_results,
		GlobalCollider.stack,
		GlobalCollider.OnCheckNode,
		obb,
		GlobalCollider.bClass3,
		GlobalCollider.bFirst
	};
	OC.Stab(*ChildModel->root, NewToWorld);

	for(auto& R : local_results)
	{
		auto& CurrentTris = R.model->tris[R.tris_id];
				
		Fvector tri_verts[3] = {};
		NewToWorld.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
		NewToWorld.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
		NewToWorld.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);
				
		if (!GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->push_back(R);
		}
	}
}

template<typename T>
static void ColliderSubstep(cform_shape_collider<_cylinder<T>>& GlobalCollider, ElementID InPrim, const Fmatrix& ToWorldTransform)
{
	auto& CurModel = GlobalCollider.stack->GetCurrentTree();
	auto& Instances = CurModel.get_instances();
	auto& Prototype = Instances[InPrim.Index];
	auto& Models = CurModel.get_models();
	auto& ChildModel = Models[Prototype.ModelIndex];
	GlobalCollider.stack->Push(*ChildModel);
	xr_scope_exit g = [&]()
	{
		GlobalCollider.stack->Pop();
	};

	// ---------------------------------------------------------------
	// 1. A = M^{-1}  (4x3 invert; M - аффинное "локальные <- глобальные")
	// ---------------------------------------------------------------
	_matrix<T> A;
	A.invert(ToWorldTransform);

	// ---------------------------------------------------------------
	// 2. Образ центра и оси цилиндра
	// ---------------------------------------------------------------
	_vector3<T> c_prime, v;
	A.transform_tiny(c_prime, GlobalCollider.shape.m_center);
	A.transform_dir(v, GlobalCollider.shape.m_direction);

	const auto half_h = 0.5 * GlobalCollider.shape.m_height;
	const auto r = GlobalCollider.shape.m_radius;

	// ---------------------------------------------------------------
	// 3. Единичная ось u в глобальных координатах (для проектора P)
	// ---------------------------------------------------------------
	auto u = GlobalCollider.shape.m_direction;
	if (u.square_magnitude() > T(1e-12))
	{
		u.normalize();
	}
	else
	{
		u.set(T(0), T(1), T(0));
	}

	// Базис плоскости, перпендикулярной u (в глобальных координатах).
	// Нужен только для построения P через u1,u2 - но фактически в
	// дальнейшем используется только сама u (см. ext_perp ниже).
	// Оставлено для ясности; можно убрать, если не нужно.

	// ---------------------------------------------------------------
	// 4. Ось OBB вдоль v (образа оси цилиндра)
	// ---------------------------------------------------------------
	const auto v_len = v.magnitude();
	_vector3<T> ev;
	if (v_len > T(1e-6))
	{
		ev = v / v_len;
	}
	else
	{
		ev = u; // вырожденный случай: цилиндр схлопнулся
	}

	const auto ext_v = half_h * v_len; // полуразмер вдоль оси v
	
	// ---------------------------------------------------------------
	// 5. Базис плоскости, перпендикулярной ev
	//    (две оставшиеся оси OBB лежат в этой плоскости)
	// ---------------------------------------------------------------
	_vector3<T> p1, p2;
	_vector3<T>::generate_orthonormal_basis(ev, p1, p2);

	// ---------------------------------------------------------------
	// 6. Опорная функция "эллиптической" части:
	//    ext_perp(f) = r * || P A^T f ||,  P = I - u u^T
	//    где f - единичный вектор в плоскости, перпендикулярной v.
	// ---------------------------------------------------------------
	auto ext_perp = [&](const _vector3<T>& f) -> T
	{
		// w = A^T f
		_vector3<T> w;
		A.transform_dirT(w, f);

		// ||P w||^2 = ||w||^2 - (w.u)^2
		const auto du = w.dotproduct(u);
		const auto sq = w.square_magnitude() - du * du;

		return r * _sqrt(sq > T(0) ? sq : T(0));
	};

	// ---------------------------------------------------------------
	// 7. 2x2 матрица эллипса поперечного сечения в базисе (p1, p2):
	//    C_ij = (P A^T p_i) . (P A^T p_j)
	//    (без множителя r^2 - он не влияет на собственные векторы)
	// ---------------------------------------------------------------
	_vector3<T> w1, w2;
	A.transform_dirT(w1, p1);
	A.transform_dirT(w2, p2);

	// Проекции на плоскость, перпендикулярную u:
	const auto du1 = w1.dotproduct(u);
	const auto du2 = w2.dotproduct(u);
	w1.x -= u.x * du1;  w1.y -= u.y * du1;  w1.z -= u.z * du1;
	w2.x -= u.x * du2;  w2.y -= u.y * du2;  w2.z -= u.z * du2;

	const auto C11 = w1.dotproduct(w1);
	const auto C22 = w2.dotproduct(w2);
	const auto C12 = w1.dotproduct(w2);

	// ---------------------------------------------------------------
	// 8. Главная ось 2x2 эллипса - замкнутая формула.
	//    theta - угол поворота главной оси относительно p1 в базисе (p1, p2).
	//    atan2 устойчив: при C11 == C22 и C12 == 0 даёт theta = 0
	//    (любой выбор осей в вырожденном случае корректен).
	// ---------------------------------------------------------------
	const auto theta = T(0.5) * std::atan2(T(2) * C12, C11 - C22);
	const auto ct = std::cos(theta);
	const auto st = std::sin(theta);
	
	// Оси OBB в плоскости, перпендикулярной ev:
	auto f1 = p1 * ct + p2 * st;
	auto f2 = p1 * (-st) + p2 * ct;
	
	// ---------------------------------------------------------------
	// 9. Полуразмеры по точной опорной функции образа цилиндра:
	//    ext(f) = (h/2) * |f . v| + r * || P A^T f ||
	//    Для f1, f2, лежащих в плоскости _|_ v, f.v = 0 (с точностью
	//    до округления), поэтому ext(f1) = ext_perp(f1), ext(f2) = ext_perp(f2).
	//    Для ev: f.v != 0, но P A^T ev может быть ненулевым из-за
	//    неортогональности ev и u после трансформации.
	//    Поэтому ev считаем по полной формуле.
	// ---------------------------------------------------------------
	const auto ext1 = ext_perp(f1);
	const auto ext2 = ext_perp(f2);
	
	// Полная опорная функция для ev:
	auto ext_ev = half_h * std::abs(ev.dotproduct(v));
	{
		_vector3<T> w;
		A.transform_dirT(w, ev);
		const auto du = w.dotproduct(u);
		const auto sq = w.square_magnitude() - du * du;
		ext_ev += r * _sqrt(sq > T(0) ? sq : T(0));
	}

	
	// ---------------------------------------------------------------
	// 10. Сборка результата.
	//     Оси OBB должны быть ортонормированными:
	//     i = ev, j = f1, k = f2 - уже единичные и ортогональные,
	//     но для страховки от накопленной ошибки пересчитаем k = i x j.
	// ---------------------------------------------------------------
	_obb<T> obb;
	obb.m_translate = c_prime;
	obb.m_rotate.i  = ev;
	obb.m_rotate.j  = f1;
	obb.m_rotate.k.crossproduct(ev, f1);   // = f2 с точностью до знака
	// Убедимся, что k сонаправлен f2 (не противоположен) -
	// это не влияет на OBB, но полезно для консистентности:
	if (obb.m_rotate.k.dotproduct(f2) < T(0))
	{
		obb.m_rotate.k.invert();
	}
	obb.m_rotate.k.normalize_safe();
	obb.m_halfsize.set(ext_ev, ext1, ext2);
	VERIFY(_valid(obb));

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	xr_vector<RESULT> local_results;
	cform_shape_collider<_obb<T>> OC{
		&local_results,
		GlobalCollider.stack,
		GlobalCollider.OnCheckNode,
		obb,
		GlobalCollider.bClass3,
		GlobalCollider.bFirst
	};
	OC.Stab(*ChildModel->root, NewToWorld);

	for(auto& R : local_results)
	{
		auto& CurrentTris = R.model->tris[R.tris_id];
				
		Fvector tri_verts[3] = {};
		NewToWorld.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
		NewToWorld.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
		NewToWorld.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);
				
		if (!GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->push_back(R);
		}
	}
}

template<typename T>
static void ColliderSubstep(cform_shape_collider<_cone<T>>& GlobalCollider, ElementID InPrim, const Fmatrix& ToWorldTransform)
{
	auto& CurModel = GlobalCollider.stack->GetCurrentTree();
	auto& Instances = CurModel.get_instances();
	auto& Prototype = Instances[InPrim.Index];
	auto& Models = CurModel.get_models();
	auto& ChildModel = Models[Prototype.ModelIndex];
	GlobalCollider.stack->Push(*ChildModel);
	xr_scope_exit g = [&]()
	{
		GlobalCollider.stack->Pop();
	};

	
    // ---- 1. Обратная матрица (4x3 достаточно для аффинных, но 4x4 надёжнее
    //         для случая, когда M содержит перспективу) -------------------
    Fmatrix Minv;
    Minv.invert_b(ToWorldTransform);   // 4x3 invert; для аффинных матриц ок

    // Линейная часть и сдвиг локального преобразования
    // L(p) = Minv₃ₓ₃ * p + Minv.c
    const Fmatrix& Mi = Minv;

    // ---- 2. Apex и основание оси в локальном пространстве ---------------
    auto xform_point = [&](const _vector3<T>& p) -> _vector3<T> {
        _vector3<T> r;
        r.x = p.x*Mi._11 + p.y*Mi._21 + p.z*Mi._31 + Mi._41;
        r.y = p.x*Mi._12 + p.y*Mi._22 + p.z*Mi._32 + Mi._42;
        r.z = p.x*Mi._13 + p.y*Mi._23 + p.z*Mi._33 + Mi._43;
        return r;
    };

	const auto& apex = GlobalCollider.shape.m_translate;
	const auto height = GlobalCollider.shape.m_height;
	const auto& d = GlobalCollider.shape.m_direction;
	const auto radius = GlobalCollider.shape.m_radius;
	
    const auto A_l   = xform_point(apex);
    const auto Base_g = apex; // (A + h*d)
    auto base_g = apex;
    base_g.mad(d, height);
    const auto B_l   = xform_point(base_g);

    _vector3<T> d_l;
	d_l.sub(B_l, A_l);
    const auto len_AB = d_l.magnitude();

    // d' — единичная ось; h' — проекция AB на d' (совпадает с len_AB,
    // т.к. base_g лежит ровно на оси; на случай вырожденного Minv оставим
    // именно проекцию — она устойчивее).
    d_l.mul(1.f / len_AB);
    const auto h_l = len_AB;

    // ---- 3. Базис сечения (u, v ⊥ d) в ГЛОБАЛЬНОМ пространстве ----------
    _vector3<T> u, v;
    _vector3<T>::generate_orthonormal_basis(d, u, v);

    // ---- 4. Образ базиса сечения в локальном пространстве ---------------
    // W = L * U, где U = [u v] (3x2), L — линейная часть Minv.
    // Столбцы W:  w0 = L*u,  w1 = L*v
    auto xform_dir = [&](const _vector3<T>& p) -> _vector3<T> {
        _vector3<T> r;
        r.x = p.x*Mi._11 + p.y*Mi._21 + p.z*Mi._31;
        r.y = p.x*Mi._12 + p.y*Mi._22 + p.z*Mi._32;
        r.z = p.x*Mi._13 + p.y*Mi._23 + p.z*Mi._33;
        return r;
    };
    const auto w0 = xform_dir(u);
    const auto w1 = xform_dir(v);

    // ---- 5. 2x2 SVD WᵀW замкнутой формулой -------------------------------
    // G = WᵀW (2x2):
    //   g00 = w0·w0,  g01 = w0·w1,  g11 = w1·w1
    const auto g00 = w0.dotproduct(w0);
    const auto g01 = w0.dotproduct(w1);
    const auto g11 = w1.dotproduct(w1);

    // Собственные значения G = [[g00,g01],[g01,g11]]
    const auto tr   = g00 + g11;
    const auto det  = g00*g11 - g01*g01;
    auto disc = tr*tr*0.25f - det;
    if (disc < 0.f)
    {
	    disc = 0.f; // клампим численный шум
    }
	const auto sq = std::sqrt(disc);

    const auto lam_max = tr*0.5f + sq;          // σ_max²
    const auto lam_min = tr*0.5f - sq;          // σ_min²

    // σ (сингулярные числа) — это и есть максимальные полуоси эллипса
    // в локальном пространстве, соответствующие направлениям u и v.
    const auto sig_max = std::sqrt(std::max(lam_max, 0.f));
    const auto sig_min = std::sqrt(std::max(lam_min, 0.f));

    // Собственные векторы G (направления в пространстве (u,v))
    // Для lam_max: (g01, lam_max - g00) либо (lam_max - g11, g01)
    T c0, s0;   // cos/sin в базисе (u, v) для первой главной оси
    if (std::abs(g01) > 1e-6f) {
        c0 = g01;
        s0 = lam_max - g00;
    } else {
        // g01 ≈ 0 — оси эллипса уже совпадают с u,v
        if (g00 >= g11) { c0 = 1.f; s0 = 0.f; }
        else            { c0 = 0.f; s0 = 1.f; }
    }
    auto n0 = std::sqrt(c0*c0 + s0*s0);
    if (n0 < 1e-6f) { c0 = 1.f; s0 = 0.f; n0 = 1.f; }
    c0 /= n0; s0 /= n0;
    // Вторая ось — ортогональна первой
    const auto c1 = -s0;
    const auto s1 =  c0;

    // Главные направления в ЛОКАЛЬНОМ пространстве:
    //   e0 = c0*w0 + s0*w1,  e1 = c1*w0 + s1*w1
    _vector3<T> e0, e1;
    e0.mul(w0, c0); e0.mad(w1, s0);
    e1.mul(w0, c1); e1.mad(w1, s1);

    // Нормируем (должны быть единичными, но подстрахуемся)
    VERIFY(e0.square_magnitude() < EPS || e1.square_magnitude() < EPS);
    e0.normalize();
    e1.normalize();

    // Полуоси эллипса в локальном пространстве = sig_max * r, sig_min * r.
    // Это НАСТОЯЩИЕ полуоси эллипса (после min-max по параметру).
    const auto a_l = radius * sig_max;
    const auto b_l = radius * sig_min;

    // Если эллипс слишком сплющен (sig_min ~ 0), зажимаем b_l, чтобы
    // не получить вырожденный фрустум — это допустимое «расширение»,
    // которое делает фрустум больше, но не ломает геометрию.
    const auto min_extent = 1e-3f * (a_l > EPS ? a_l : 1.f);
    const auto b_l_safe = std::max(b_l, min_extent);

    // ---- 6. Боковые плоскости ------------------------------------------
    // Касательные к эллипсу с полуосями (a_l, b_l), центр в B_l,
    // оси (e0, e1). Ось конуса — d_l (единичная).
    //
    // В базисе (e0, e1, d_l) конус имеет apex A_l и основание B_l с
    // эллипсом (a_l, b_l). Боковая плоскость, касающаяся эллипса в точке
    // на направлении phi (в плоскости e0,e1), имеет нормаль:
    //
    //   n_side = ( cos(phi)/a_l * e0 + sin(phi)/b_l * e1 ) * cos(beta)
    //          - sin(beta) * d_l
    //
    // где beta — угол наклона образующей к оси:  tan(beta) = a_eff/h_l,
    // а a_eff — расстояние от оси до точки касания в направлении phi.
    //
    // Для минимального фрустума берём phi = 0, 90, 180, 270 градусов
    // (касание в вершинах главных осей эллипса).

    Fplane planes[5];

    // Вспомогательные: для каждой из 4 сторон — точка касания в локальных
    // координатах (в плоскости основания) и нормаль.
    // Точка касания на эллипсе в направлении phi:
    //   P(phi) = B_l + a_l*cos(phi)*e0 + b_l*sin(phi)*e1
    // Расстояние от оси до P(phi) в плоскости (e0,e1):
    //   rad(phi) = sqrt( (a_l*cos)^2 + (b_l*sin)^2 )
    // Наклон образующей в этом сечении:
    //   tan(beta_phi) = rad(phi) / h_l
    // Нормаль боковой плоскости (внутрь смотрит -n, наружу +n):
    //   n = cos(beta_phi) * dir_radial - sin(beta_phi) * d_l
    // где dir_radial — единичный вектор от оси к точке касания в плоскости
    // основания:  dir_radial = normalize( a_l*cos*e0 + b_l*sin*e1 ).

    const float inv_h = 1.f / h_l;

    for (int i = 0; i < 4; ++i)
    {
        // phi = i * 90 град:  0, PI/2, PI, 3PI/2
        float phi = (float)(i * PI_DIV_2);
        float cs = std::cos(phi);
        float sn = std::sin(phi);

        // Вектор от оси до точки касания (в локальных координатах, но
        // масштабированный: a_l*cs*e0 + b_l*sn*e1)
        Fvector rad_vec;
        rad_vec.mul(e0, a_l * cs);
        rad_vec.mad(e1, b_l_safe * sn);

        float rad_len = rad_vec.magnitude();
        if (rad_len < 1e-6f) {
            // Вырожденный случай (a_l ~ 0 или b_l ~ 0) — пропускаем,
            // поставим плоскость-заглушку далеко наружу.
            planes[i].n.set(0.f, 0.f, 1.f);
            planes[i].d = 1e30f;
            continue;
        }

        // Единичный радиальный вектор в локальном пространстве
        Fvector dir_rad;
        dir_rad.mul(rad_vec, 1.f / rad_len);

        // Угол beta: tan(beta) = rad_len / h_l
        // cos(beta) = h_l / sqrt(h_l^2 + rad_len^2)
        // sin(beta) = rad_len / sqrt(h_l^2 + rad_len^2)
        float denom = std::sqrt(h_l*h_l + rad_len*rad_len);
        float cb = h_l    / denom;
        float sb = rad_len / denom;

        // Нормаль: n = cos(beta)*dir_rad - sin(beta)*d_l
        Fvector n;
        n.mul(dir_rad, cb);
        n.mad(d_l, -sb);
        n.normalize_safe();

        // Плоскость проходит через apex A_l:
        //   n·(p - A_l) = 0  =>  n·p = n·A_l
        planes[i].n = n;
        planes[i].d = n.dotproduct(A_l);
    }

    // ---- 7. Плоскость основания ----------------------------------------
    // Внешняя нормаль — вдоль +d_l (от apex к основанию).
    // Уравнение:  d_l · (p - B_l) = 0  =>  d_l·p = d_l·B_l
    planes[4].n = d_l;
    planes[4].d = d_l.dotproduct(B_l);

    // ---- 8. Проверка знака нормалей ------------------------------------
    // Точка строго внутри конуса (центр основания, чуть смещённый):
    Fvector inside = B_l;
    inside.mad(d_l, -0.5f * h_l);   // середина оси
    for (int i = 0; i < 5; ++i) {
        float s = planes[i].n.dotproduct(inside) - planes[i].d;
        if (s > 0.f) {
            // Нормаль смотрит не туда — инвертируем
            planes[i].n.invert();
            planes[i].d = -planes[i].d;
        }
    }

    // ---- 9. Заполняем фрустум ------------------------------------------
	CFrustum FLocal;
	FLocal.CreateFromPlanes(planes, 5);
	xr_vector<RESULT> local_results;
	cform_frustum_collider FC;
	FC.OnCheckNode = GlobalCollider.OnCheckNode;
	FC.dest = &local_results;
	FC.stack = GlobalCollider.stack;
	FC.bClass3 = GlobalCollider.bClass3;
	FC.bFirst = GlobalCollider.bFirst;
	FC.F = &FLocal;
	
	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	FC.Stab(*ChildModel->root, FLocal.getMask(), NewToWorld);

	for(auto& R : local_results)
	{
		auto& CurrentTris = R.model->tris[R.tris_id];
				
		Fvector tri_verts[3] = {};
		NewToWorld.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
		NewToWorld.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
		NewToWorld.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);
				
		if (!GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->push_back(R);
		}
	}
}

template <class Shape>
void COLLIDER::shape_query(const MODEL& m_def, const Shape& shape, u32 mode, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::shape_query");
	
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.InstaceScene)
	{
		return;
	}

	r_clear();
	r_vec().reserve(16);

	cform_stack S = m_def;
	cform_shape_collider<Shape> SC{};
	SC.dest = &rd;
	SC.stack = &S;
	SC.shape = shape;
	SC.bClass3 = !!(mode & OPT_FULL_TEST);
	SC.bFirst = !!(mode & OPT_ONLYFIRST);
	SC.OnCheckNode = OnCheckNode;
	SC.Stab(*m_def.root, Fmatrix{Fmatrix::EIdentity::Identity});
}

void COLLIDER::box_query(const MODEL& m_def, const Fbox& _box, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::box_query");
	shape_query(m_def, _box, box_mode);
}

void COLLIDER::obb_query(const MODEL& m_def, const Fobb& obb, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::obb_query");
	shape_query(m_def, obb, obb_mode);
}

void COLLIDER::sphere_query(const MODEL& m_def, const Fsphere& sphere, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::sphere_query");
	shape_query(m_def, sphere, sphere_mode);
}

void COLLIDER::cylinder_query(const MODEL& m_def, const Fcylinder& cylinder, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::cylinder_query");
	shape_query(m_def, cylinder, cylinder_mode);
}

void COLLIDER::cone_query(const MODEL& m_def, const Fcone& cone, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::cone_query");
	shape_query(m_def, cone, cone_mode);
}

struct cform_custom_collider final
{
	cform_stack* stack = nullptr;
	CDB::COLLIDER::CheckFunc AABBCheck = nullptr;
	void* paabbc = nullptr;
	CDB::COLLIDER::TrisFunc GetTris = nullptr;
	void* ptric = nullptr;

	void Prim(ElementID prim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(prim.IsNotPointer);
		if(prim.IsInstance)
		{
			auto& CurModel = stack->GetCurrentTree();
			auto& Instances = CurModel.get_instances();
			auto& Prototype = Instances[prim.Index];
			auto& Models = CurModel.get_models();
			auto& ChildModel = Models[Prototype.ModelIndex];
			stack->Push(*ChildModel);
			xr_scope_exit g = [&]()
			{
				stack->Pop();
			};

			Fmatrix NewToWorld;
			NewToWorld.mul(ToWorldTransform, Prototype.Transform);
			Stab(*ChildModel->root, NewToWorld);
			return;
		}
		
		if (GetTris)
		{
			GetTris(stack->GetCurrentTree(), ToWorldTransform, prim, ptric);
		}
	}
	
	void Stab(const BVHNode& node, const Fmatrix& ToWorldTransform)
	{
		if (nullptr==AABBCheck || !AABBCheck(stack->GetCurrentTree(), ToWorldTransform, node, paabbc))
		{
			return;
		}

		for(size_t i = 0; i < node.GetSize(); ++i)
		{
			if(node.HasNode(i))
			{
				Stab(node.GetNode(i), ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
			}
		}
	}
};

void COLLIDER::custom_query(const MODEL& m_def, CheckFunc AABBCheckF, void* paabbc, TrisFunc GetTrisF, void* ptric)
{
	PROF_EVENT("COLLIDER::custom_query");
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.InstaceScene)
	{
		return;
	}

	cform_stack S = m_def;
	cform_custom_collider CC
	{
		&S,
		AABBCheckF,
		paabbc,
		GetTrisF,
		ptric
	};
	CC.Stab(*m_def.root, Fmatrix{Fmatrix::EIdentity::Identity});
}