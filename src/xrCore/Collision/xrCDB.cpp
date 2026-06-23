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
	delete_data(verts);
	delete_data(tris);
	rtcReleaseBVH(tree);
}

void MODEL::build_simple()
{
	VERIFY(!IsBuilt);
	auto EmbreeDevice = GetEmbreeDevice();
	InstaceScene = rtcNewScene(EmbreeDevice);
	rtcSetSceneBuildQuality(InstaceScene, RTC_BUILD_QUALITY_HIGH);

	for(auto& elem : instances)
	{
		auto InstModel = models[elem.ModelIndex];
		VERIFY(InstModel->IsBuilt);
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
}

// Collision queries



void COLLIDER::ray_query(const MODEL* m_def, const Fvector& r_start, const Fvector& r_dir, float r_range)
{
	PROF_EVENT("COLLIDER::ray_query");
	if (!m_def || !m_def->IsBuilt || !m_def->InstaceScene)
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
	context.model = m_def;
	
	if(!!(ray_mode & OPT_ONLYFIRST))
	{
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
		rtcOccluded1(m_def->InstaceScene, &ray, &args);
	} else
	{
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
				
				RESULT& R	= self->r_add();
				R.model = model;
				R.tris_id = PrimID;
				R.range		= RTCRayN_tfar(args->ray, 1, 0);
				R.u			= RTCHitN_u(args->hit, 1, 0);
				R.v			= RTCHitN_v(args->hit, 1, 0);

				auto InstID = RTCHitN_instID(args->hit, 1, 0, 0);
				if (InstID != u32(-1))
				{
					auto Geom = rtcGetGeometry(context->model->InstaceScene, InstID);
					auto Data = (CDB::InstanceData*)rtcGetGeometryUserData(Geom);
					R.ModelWorldTransform = Data->Transform;
					R.Sector = Data->Sector;
				} else
				{
					R.Sector = model->tris[PrimID].sector;
				}
				
				if(!!(self->ray_mode & OPT_ONLYNEAREST) && self->rd.size() > 1)
				{
					auto& First = self->rd[0];
					auto& Last = self->rd[self->rd.size() - 1];
					if (First.range > Last.range)
					{
						First = Last;
					}
					self->rd.pop_back();
				}
			}
		};
		args.filter = Filter::Execute;
		args.context = &context;
		args.flags = RTC_RAY_QUERY_FLAG_INVOKE_ARGUMENT_FILTER;
		rtcIntersect1(m_def->InstaceScene, &ray, &args);
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

// ultimate solution for non-uniform instances - SAT and converted AABB, OBB and Frustum as 6 planes
// will be slower than simple box check, but no additional filter check required
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
				if (bFirst && dest->size())
				{
					return;
				}
			}
		}
	}
};

void COLLIDER::frustum_query(const MODEL* m_def, const CFrustum& F, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::frustum_query");
	/*if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();*/

	r_clear();
	r_vec().reserve(16);

	cform_stack S = *m_def;
	cform_frustum_collider BC{};
	BC.dest = &rd;
	BC.stack = &S;
	BC.F = &F;
	BC.bClass3 = box_mode & OPT_FULL_TEST;
	BC.bFirst = box_mode & OPT_ONLYFIRST;
	BC.OnCheckNode = OnCheckNode;
	BC.Stab(*m_def->root, F.getMask(), Fmatrix{Fmatrix::EIdentity::Identity});
}

struct cform_box_collider final
{
	xr_vector<RESULT>* dest = nullptr;
	cform_stack* stack = nullptr;
	ColliderCallback OnCheckNode = nullptr;
	Fbox box;
	bool bClass3, bFirst;

	ICF void Prim(ElementID InPrim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(InPrim.IsNotPointer);
		if (InPrim.IsInstance)
		{
			auto& CurModel = stack->GetCurrentTree();
			auto& Instances = CurModel.get_instances();
			auto& Prototype = Instances[InPrim.Index];
			if (!box.intersect(Prototype.GlobalAABB))
			{
				return;
			}
			auto& Models = CurModel.get_models();
			auto& ChildModel = Models[Prototype.ModelIndex];
			stack->Push(*ChildModel);
			xr_scope_exit g = [&]()
			{
				stack->Pop();
			};

			Fmatrix NewToWorld;
			NewToWorld.mul(ToWorldTransform, Prototype.Transform);
			thread_local xr_vector<RESULT> LocResult(100);
			LocResult.clear();
			cform_box_collider BC;
			BC.OnCheckNode = OnCheckNode;
			BC.dest = &LocResult;
			BC.stack = stack;
			BC.bClass3 = bClass3;
			BC.bFirst = bFirst;
			BC.box.invalidate();
			for (int i = 0; i < 8; i++)
			{
				Fvector AABBPoint;
				box.getpoint(i, AABBPoint);
				Prototype.InvTransform.transform_tiny(AABBPoint);
				BC.box.modify(AABBPoint);
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
				if (box.intersectTri(tri_verts, bClass3))
				{
					dest->emplace_back(elem);
				}
			}
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
		if (!box.intersectTri(tri_verts, bClass3))
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
				if (!box.intersect(node.GetAABB(i)))
				{
					continue;
				}
				Stab(node.GetNode(i), ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
		
				// Early exit for "only first"
				if (bFirst && dest->size())
				{
					return;
				}
			}
		}
	}
};

void COLLIDER::box_query(const MODEL *m_def, const Fbox& _box, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::box_query");
	/*if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();*/

	r_clear();
	r_vec().reserve(16);

	cform_stack S = *m_def;
	cform_box_collider BC{};
	BC.dest = &rd;
	BC.stack = &S;
	BC.box = _box;
	BC.bClass3 = box_mode & OPT_FULL_TEST;
	BC.bFirst = box_mode & OPT_ONLYFIRST;
	BC.OnCheckNode = OnCheckNode;
	BC.Stab(*m_def->root, Fmatrix{Fmatrix::EIdentity::Identity});
}

struct cform_obb_collider final
{
	xr_vector<RESULT>* dest = nullptr;
	cform_stack* stack = nullptr;
	ColliderCallback OnCheckNode = nullptr;
	Fobb obb;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void Prim(ElementID prim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(prim.IsNotPointer);
		if (prim.IsInstance)
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

			CFrustum LocalF;
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
				LocalF._add(LocalPlane);
			};
			PlaneFunc(obb.m_rotate.i, -(obb.m_rotate.i*obb.m_translate + obb.m_halfsize.x));
			PlaneFunc(-obb.m_rotate.i, obb.m_rotate.i*obb.m_translate - obb.m_halfsize.x);
			PlaneFunc(obb.m_rotate.j, -(obb.m_rotate.j*obb.m_translate + obb.m_halfsize.y));
			PlaneFunc(-obb.m_rotate.j, obb.m_rotate.j*obb.m_translate - obb.m_halfsize.y);
			PlaneFunc(obb.m_rotate.k, -(obb.m_rotate.k*obb.m_translate + obb.m_halfsize.z));
			PlaneFunc(-obb.m_rotate.k, obb.m_rotate.k*obb.m_translate - obb.m_halfsize.z);

			Fmatrix NewToWorld;
			NewToWorld.mul(ToWorldTransform, Prototype.Transform);
			cform_frustum_collider BC{
				dest,
				stack,
				&LocalF,
				OnCheckNode,
				bClass3,
				bFirst
			};
			BC.Stab(*ChildModel->root, LocalF.getMask(), NewToWorld);
			return;
		}
		
		auto& CurModel = stack->GetCurrentTree();
		auto& Tri = CurModel.tris[prim.Index];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = {
			CurModel.verts[TriVerts[0]],
			CurModel.verts[TriVerts[1]],
			CurModel.verts[TriVerts[2]]
		};

		if (!obb.intersectTri(tri_verts, bClass3))
		{
			return;
		}

		RESULT& R = dest->emplace_back();
		R.ModelWorldTransform = ToWorldTransform;
		R.model = &CurModel;
		R.tris_id = prim.Index;
	}

	void Stab(const BVHNode& node, const Fmatrix& ToWorldTransform)
	{
		VERIFY(dest);
		VERIFY(stack);
		
		if (OnCheckNode)
		{
			OnCheckNode(node, ToWorldTransform);
		}

		for(size_t i = 0; i < node.GetSize(); ++i)
		{
			if(node.HasNode(i))
			{
				// Actual OBB-AABB test
				if (!obb.intersectAABB(node.GetAABB(i)))
				{
					continue;
				}
				Stab(node.GetNode(i), ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
		
				// Early exit for "only first"
				if (bFirst && dest->size())
				{
					return;
				}
			}
		}
	}
};

void COLLIDER::obb_query(const MODEL* m_def, const Fobb& obb, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::obb_query");
	/*if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();*/

	r_clear();
	r_vec().reserve(16);

	cform_stack S = *m_def;
	cform_obb_collider OC
	{
		&rd,
		&S,
		OnCheckNode,
		obb,
		!!(obb_mode & OPT_FULL_TEST),
		!!(obb_mode & OPT_ONLYFIRST)
	};
	OC.Stab(*m_def->root, Fmatrix{Fmatrix::EIdentity::Identity});
}

struct cform_sphere_collider final
{
	xr_vector<RESULT>* dest = nullptr;
	cform_stack* stack = nullptr;
	ColliderCallback OnCheckNode = nullptr;
	Fsphere sphere;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void Prim(ElementID prim, const Fmatrix& ToWorldTransform)
	{
		VERIFY(prim.IsNotPointer);
		if (prim.IsInstance)
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

			// TODO: SAT test will always works fine, but there are some ways to optimize this
			Fobb obb;
			Prototype.InvTransform.transform_tiny(obb.m_translate, sphere.P);
			obb.m_rotate.i = Prototype.InvTransform.i;
			obb.m_rotate.j = Prototype.InvTransform.j;
			obb.m_rotate.k = Prototype.InvTransform.k;
			obb.m_halfsize.x = sphere.R*obb.m_rotate.i.magnitude();
			obb.m_halfsize.y = sphere.R*obb.m_rotate.j.magnitude();
			obb.m_halfsize.z = sphere.R*obb.m_rotate.k.magnitude();
			obb.m_rotate.i.normalize();
			obb.m_rotate.j.normalize();
			obb.m_rotate.k.normalize();
			VERIFY(_valid(obb));

			Fmatrix NewToWorld;
			NewToWorld.mul(ToWorldTransform, Prototype.Transform);
			xr_vector<RESULT> local_results;
			cform_obb_collider OC{
				&local_results,
				stack,
				OnCheckNode,
				obb,
				bClass3,
				bFirst
			};
			OC.Stab(*ChildModel->root, NewToWorld);

			for(auto& R : local_results)
			{
				auto& CurrentTris = R.model->tris[R.tris_id];
				
				Fvector tri_verts[3] = {};
				NewToWorld.transform_tiny(tri_verts[0], R.model->verts[CurrentTris.verts[0]]);
				NewToWorld.transform_tiny(tri_verts[1], R.model->verts[CurrentTris.verts[1]]);
				NewToWorld.transform_tiny(tri_verts[2], R.model->verts[CurrentTris.verts[2]]);
				
				if (!sphere.intersectTri(tri_verts, bClass3))
				{
					dest->push_back(R);
				}
			}			
			return;
		}
		
		auto& CurModel = stack->GetCurrentTree();
		auto& Tri = CurModel.tris[prim.Index];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = {
			CurModel.verts[TriVerts[0]],
			CurModel.verts[TriVerts[1]],
			CurModel.verts[TriVerts[2]]
		};

		if (!sphere.intersectTri(tri_verts, bClass3))
		{
			return;
		}

		RESULT& R = dest->emplace_back();
		R.ModelWorldTransform = ToWorldTransform;
		R.model = &CurModel;
		R.tris_id = prim.Index;
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
				// Actual Sphere-AABB test
				Fvector center, extents;
				node.GetAABB(i).get_CD(center, extents);
				if (!sphere.intersectAABB(center, extents))
				{
					return;
				}
				Stab(node.GetNode(i), ToWorldTransform);
			}
			else
			{
				Prim(node.GetElement(i), ToWorldTransform);
		
				// Early exit for "only first"
				if (bFirst && dest->size())
				{
					return;
				}
			}
		}
	}
};

void COLLIDER::sphere_query(const MODEL* m_def, const Fsphere& sphere, ColliderCallback OnCheckNode)
{
	PROF_EVENT("COLLIDER::sphere_query");
	/*if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();*/

	r_clear();
	r_vec().reserve(16);

	cform_stack S = *m_def;
	cform_sphere_collider SC
	{
		&rd,
		&S,
		OnCheckNode,
		sphere,
		!!(sphere_mode & OPT_FULL_TEST),
		!!(sphere_mode & OPT_ONLYFIRST)
	};
	SC.Stab(*m_def->root, Fmatrix{Fmatrix::EIdentity::Identity});
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

void COLLIDER::custom_query(const MODEL* m_def, CheckFunc AABBCheckF, void* paabbc, TrisFunc GetTrisF, void* ptric)
{
	PROF_EVENT("COLLIDER::custom_query");
	/*if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();*/

	cform_stack S = *m_def;
	cform_custom_collider CC
	{
		&S,
		AABBCheckF,
		paabbc,
		GetTrisF,
		ptric
	};
	CC.Stab(*m_def->root, Fmatrix{Fmatrix::EIdentity::Identity});
}