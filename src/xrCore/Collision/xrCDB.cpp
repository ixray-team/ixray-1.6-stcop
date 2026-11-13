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
	if (InstaceScene)
	{
		rtcReleaseScene(InstaceScene);
	}
	for (auto*& Model : models)
	{
		xr_delete(Model);
	}
	delete_data(verts);
	delete_data(tris);
	if (tree)
	{
		rtcReleaseBVH(tree);
	}
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
	r_clear();
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.InstaceScene)
	{
		return;
	}
	
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
					if (Range >= self->rd.front().range)
					{
						args->valid[0] = 0;
						return;
					}
					R = self->rd.data();
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
					R->ModelWorldTransform.identity();
					R->Sector = model->tris[PrimID].sector;
				}
				if (!(self->ray_mode & OPT_ONLYNEAREST))
				{
					// Continue traversal without shortening the ray to this hit.
					args->valid[0] = 0;
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
				if (bFirst && !dest->empty())
				{
					return;
				}
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
	r_clear();
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.root)
	{
		return;
	}

	r_vec().reserve(16);

	cform_stack S = m_def;
	cform_frustum_collider BC{};
	BC.dest = &rd;
	BC.stack = &S;
	BC.F = &F;
	BC.bClass3 = frustum_mode & OPT_FULL_TEST;
	BC.bFirst = frustum_mode & OPT_ONLYFIRST;
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
				if (bFirst && !dest->empty())
				{
					return;
				}
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
				
		if (GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
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
	// The query is expressed in the containing model's coordinates. Only the
	// instance inverse maps it to this prototype; ToWorldTransform is accumulated.
	A = Prototype.InvTransform;

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

	// ---------------------------------------------------------------
	// 5. Базис плоскости, перпендикулярной ev
	//    (две оставшиеся оси OBB лежат в этой плоскости)
	// ---------------------------------------------------------------
	_vector3<T> p1, p2;
	_vector3<T>::generate_orthonormal_basis(ev, p1, p2);

	// ---------------------------------------------------------------
	// Support of the transformed enclosing square cross-section.
	// ---------------------------------------------------------------
	auto ext_perp = [&](const _vector3<T>& f) -> T
	{
		// w = A^T f
		_vector3<T> w;
		A.transform_dirT(w, f);

		// intersectTri uses a finite SAT axis set, including the cylinder's
		// enclosing square. Preserve those conservative hits as well: a tighter
		// circular support bound could reject triangles accepted by that test.
		_vector3<T> Side1, Side2;
		_vector3<T>::generate_orthonormal_basis(u, Side1, Side2);
		return r * (std::abs(w.dotproduct(Side1)) + std::abs(w.dotproduct(Side2)));
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
	// Include both axial and transverse support on every OBB axis.
	// ---------------------------------------------------------------
	const auto ext1 = half_h * std::abs(f1.dotproduct(v)) + ext_perp(f1);
	const auto ext2 = half_h * std::abs(f2.dotproduct(v)) + ext_perp(f2);

	// Полная опорная функция для ev:
	const auto ext_ev = half_h * std::abs(ev.dotproduct(v)) + ext_perp(ev);


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
	obb.m_halfsize.add(EPS_L);
	VERIFY(_valid(obb));

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	xr_vector<RESULT> local_results;
	cform_shape_collider<_obb<T>> OC{
		&local_results,
		GlobalCollider.stack,
		GlobalCollider.OnCheckNode,
		obb,
		false,
		false // Only-first applies after the curved-volume filter, not to candidates.
	};
	OC.Stab(*ChildModel->root, NewToWorld);

	Fmatrix WorldToQuery;
	WorldToQuery.invert(ToWorldTransform);
	for (auto& R : local_results)
	{
		auto& CurrentTris = R.model->tris[R.tris_id];

		Fvector tri_verts[3] = {};
		Fmatrix TriangleToQuery;
		TriangleToQuery.mul(WorldToQuery, R.ModelWorldTransform);
		TriangleToQuery.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
		TriangleToQuery.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
		TriangleToQuery.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);

		if (GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->push_back(R);
			if (GlobalCollider.bFirst)
			{
				return;
			}
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


	const auto& Cone = GlobalCollider.shape;

	if (Cone.m_height <= T(1e-6))
	{
		return;
	}

	_vector3<T> Side1, Side2;
	_vector3<T>::generate_orthonormal_basis(Cone.m_direction, Side1, Side2);
	const T Slope = Cone.m_radius / Cone.m_height;

	// A square pyramid circumscribes the cone in query coordinates. Transforming
	// its plane equations preserves containment under nonuniform scale and shear.
	Fplane Planes[6];

	for (u32 Index = 0; Index < 4; ++Index)
	{
		const auto& Side = Index < 2 ? Side1 : Side2;
		Planes[Index].n.mul(Side, Index % 2 ? -1.f : 1.f).mad(Cone.m_direction, -Slope);
		Planes[Index].d = -Planes[Index].n.dotproduct(Cone.m_translate);
	}

	Planes[4].n.invert(Cone.m_direction);
	Planes[4].d = -Planes[4].n.dotproduct(Cone.m_translate);
	Fvector Base;
	Base.mad(Cone.m_translate, Cone.m_direction, Cone.m_height);
	Planes[5].n = Cone.m_direction;
	Planes[5].d = -Planes[5].n.dotproduct(Base);
	// For x_query = L * x_local + t, n_query dot x_query + d <= 0
	// becomes (L^T * n_query) dot x_local + d + n_query dot t <= 0.
	for (auto& Plane : Planes)
	{
		const Fvector Normal = Plane.n;
		Plane.n.set(Normal.dotproduct(Prototype.Transform.i), Normal.dotproduct(Prototype.Transform.j), Normal.dotproduct(Prototype.Transform.k));
		Plane.d += Normal.dotproduct(Prototype.Transform.c);
		Plane.d -= EPS_L * Plane.n.magnitude();
	}
	CFrustum FLocal;
	FLocal.CreateFromPlanes(Planes, 6);
	xr_vector<RESULT> local_results;
	cform_frustum_collider FC;
	FC.OnCheckNode = GlobalCollider.OnCheckNode;
	FC.dest = &local_results;
	FC.stack = GlobalCollider.stack;
	FC.bClass3 = false;
	FC.bFirst = false; // Only-first applies after the curved-volume filter.
	FC.F = &FLocal;

	Fmatrix NewToWorld;
	NewToWorld.mul(ToWorldTransform, Prototype.Transform);
	FC.Stab(*ChildModel->root, FLocal.getMask(), NewToWorld);

	Fmatrix WorldToQuery;
	WorldToQuery.invert(ToWorldTransform);
	for (auto& R : local_results)
	{
		auto& CurrentTris = R.model->tris[R.tris_id];

		Fvector tri_verts[3] = {};
		Fmatrix TriangleToQuery;
		TriangleToQuery.mul(WorldToQuery, R.ModelWorldTransform);
		TriangleToQuery.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
		TriangleToQuery.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
		TriangleToQuery.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);

		if (GlobalCollider.shape.intersectTri(tri_verts, GlobalCollider.bClass3))
		{
			GlobalCollider.dest->push_back(R);
			if (GlobalCollider.bFirst)
			{
				return;
			}
		}
	}
}

template <class Shape>
void COLLIDER::shape_query(const MODEL& m_def, const Shape& shape, u32 mode, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::shape_query");
	r_clear();
	
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.root)
	{
		return;
	}

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
	r_clear();
	if (!m_def.IsBuilt)
	{
		m_def.load_task.wait();
	}
	if (!m_def.root)
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
