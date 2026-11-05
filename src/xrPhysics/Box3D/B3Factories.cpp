#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "PHContact.h"
#include "IPHCapture.h"
#include "IPHStaticGeomShell.h"
#include "IActivationShape.h"
#include "ActorCameraCollision.h"
#include "MovementBoxDynamicActivate.h"
#include "IPhysicsShellHolder.h"
#include "iclimableobject.h"
#include "icollisiondamagereceiver.h"
#include "debug_output.h"
#include "phvalide.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/bone.h"

CPhysicsElement* P_create_Element()
{
	return new CPHElement();
}

CPhysicsShell* P_create_Shell()
{
	return new CPHShell();
}

CPhysicsShell* P_create_splited_Shell()
{
	B3_NOT_IMPLEMENTED();
	return new CPHShell();
}

CPhysicsJoint* P_create_Joint(CPhysicsJoint::enumType type, CPhysicsElement* first, CPhysicsElement* second)
{
	return new CPHJoint(type, first, second);
}

// Shell building below only uses the public API and matches the ODE backend.
CPhysicsShell* P_build_Shell(IPhysicsShellHolder* obj, bool not_active_state, BONE_P_MAP* bone_map, bool not_set_bone_callbacks)
{
	VERIFY(obj);
	phys_shell_verify_object_model(*obj);
	IKinematics* pKinematics = obj->ObjectKinematics();

	CPhysicsShell* pPhysicsShell = P_create_Shell();
#ifdef DEBUG
	pPhysicsShell->dbg_obj = obj;
#endif
	pPhysicsShell->build_FromKinematics(pKinematics, bone_map);

	pPhysicsShell->set_PhysicsRefObject(obj);
	pPhysicsShell->mXFORM.set(obj->ObjectXFORM());
	pPhysicsShell->Activate(not_active_state, not_set_bone_callbacks);
	pPhysicsShell->SetAirResistance();

	return pPhysicsShell;
}

void fix_bones(const char* fixed_bones, CPhysicsShell* shell)
{
	VERIFY(fixed_bones);
	VERIFY(shell);
	IKinematics* pKinematics = shell->PKinematics();
	VERIFY(pKinematics);
	const int Count = _GetItemCount(fixed_bones);
	for (int I = 0; I < Count; ++I)
	{
		string64 FixedBone;
		_GetItem(fixed_bones, I, FixedBone);
		const u16 FixedBoneId = pKinematics->LL_BoneID(FixedBone);
		R_ASSERT2(BI_NONE != FixedBoneId, make_string<const char*>("wrong fixed bone [%s] for object with visual [%s]", FixedBone, pKinematics->getDebugName().c_str()));
		if (CPhysicsElement* E = shell->get_Element(FixedBoneId))
		{
			E->Fix();
		}
	}
}

CPhysicsShell* P_build_Shell(IPhysicsShellHolder* obj, bool not_active_state, BONE_P_MAP* p_bone_map, const char* fixed_bones)
{
	CPhysicsShell* pPhysicsShell = nullptr;
	IKinematics* pKinematics = obj->ObjectKinematics();
	if (fixed_bones)
	{
		const int Count = _GetItemCount(fixed_bones);
		for (int I = 0; I < Count; ++I)
		{
			string64 FixedBone;
			_GetItem(fixed_bones, I, FixedBone);
			const u16 FixedBoneId = pKinematics->LL_BoneID(FixedBone);
			R_ASSERT2(BI_NONE != FixedBoneId, make_string<const char*>("wrong fixed bone [%s] for object with visual [%s]", FixedBone, pKinematics->getDebugName().c_str()));
			p_bone_map->insert(std::make_pair(FixedBoneId, physicsBone()));
		}

		pPhysicsShell = P_build_Shell(obj, not_active_state, p_bone_map);
	}
	else
	{
		pPhysicsShell = P_build_Shell(obj, not_active_state);
	}

	if (!p_bone_map->empty())
	{
		pPhysicsShell->SetPrefereExactIntegration();
	}
	for (auto& [BoneId, Bone] : *p_bone_map)
	{
		R_ASSERT2(Bone.element, "fixed bone has no physics");
		Bone.element->Fix();
	}
	return pPhysicsShell;
}

CPhysicsShell* P_build_Shell(IPhysicsShellHolder* obj, bool not_active_state, const char* fixed_bones)
{
	U16Vec FixedBones;
	if (fixed_bones)
	{
		IKinematics* K = obj->ObjectKinematics();
		VERIFY(K);
		const int Count = _GetItemCount(fixed_bones);
		for (int I = 0; I < Count; ++I)
		{
			string64 FixedBone;
			_GetItem(fixed_bones, I, FixedBone);
			FixedBones.push_back(K->LL_BoneID(FixedBone));
			R_ASSERT2(BI_NONE != FixedBones.back(), make_string<const char*>("wrong fixed bone [%s] for object [%s] with visual [%s]", FixedBone, obj->ObjectName(), obj->ObjectNameVisual()));
		}
	}
	return P_build_Shell(obj, not_active_state, FixedBones);
}

CPhysicsShell* P_build_Shell(IPhysicsShellHolder* obj, bool not_active_state, U16Vec& fixed_bones)
{
	BONE_P_MAP BoneMap;
	for (u16 BoneId : fixed_bones)
	{
		BoneMap.insert(std::make_pair(BoneId, physicsBone()));
	}
	CPhysicsShell* pPhysicsShell = P_build_Shell(obj, not_active_state, &BoneMap);

	for (auto& [BoneId, Bone] : BoneMap)
	{
		if (Bone.element)
		{
			Bone.element->Fix();
		}
	}
	return pPhysicsShell;
}

CPhysicsShell* P_build_SimpleShell(IPhysicsShellHolder* obj, float mass, bool not_active_state)
{
	CPhysicsShell* pPhysicsShell = P_create_Shell();
#ifdef DEBUG
	pPhysicsShell->dbg_obj = obj;
#endif
	VERIFY(obj);
	VERIFY(obj->ObjectKinematics());

	Fobb Box;
	obj->ObjectKinematics()->GetBox().get_CD(Box.m_translate, Box.m_halfsize);
	Box.m_rotate.identity();
	CPhysicsElement* E = P_create_Element();
	R_ASSERT(E);
	E->add_Box(Box);
	pPhysicsShell->add_Element(E);
	pPhysicsShell->setMass(mass);
	pPhysicsShell->set_PhysicsRefObject(obj);
	if (!obj->has_parent_object())
	{
		pPhysicsShell->Activate(obj->ObjectXFORM(), 0, obj->ObjectXFORM(), not_active_state);
	}
	return pPhysicsShell;
}

void ApplySpawnIniToPhysicShell(CInifile const* ini, CPhysicsShell* physics_shell, bool fixed)
{
	if (!ini)
	{
		return;
	}

	if (ini->section_exist("physics_common"))
	{
		fixed = fixed || (ini->line_exist("physics_common", "fixed_bones"));
		fix_bones(ini->r_string("physics_common", "fixed_bones"), physics_shell);
	}
	if (ini->section_exist("collide"))
	{
		if ((ini->line_exist("collide", "ignore_static") && fixed) || (ini->line_exist("collide", "ignore_static") && ini->section_exist("animated_object")))
		{
			physics_shell->SetIgnoreStatic();
		}
		if (ini->line_exist("collide", "small_object"))
		{
			physics_shell->SetSmall();
		}
		if (ini->line_exist("collide", "ignore_small_objects"))
		{
			physics_shell->SetIgnoreSmall();
		}
		if (ini->line_exist("collide", "ignore_ragdoll"))
		{
			physics_shell->SetIgnoreRagDoll();
		}
		if (ini->line_exist("collide", "ignore_animated_objects"))
		{
			physics_shell->SetIgnoreAnimated();
		}
		if (ini->line_exist("collide", "self_collision"))
		{
			physics_shell->SetSelfCollision(true);
		}
	}

	if (ini->section_exist("animated_object"))
	{
		physics_shell->CreateShellAnimator(ini, "animated_object");
	}
}

void get_box(const CPhysicsBase* shell, const Fmatrix& form, Fvector& sz, Fvector& c)
{
	t_get_box(shell, form, sz, c);
}

void destroy_physics_shell(CPhysicsShell*& p)
{
	if (p)
	{
		p->Deactivate();
	}
	xr_delete(p);
}

static bool BoneHasPhysics(IKinematics& K, u16 BoneId)
{
	return K.LL_GetBoneVisible(BoneId) && shape_is_physic(K.GetBoneData(BoneId).get_shape());
}

bool has_physics_collision_shapes(IKinematics& K)
{
	const u16 Count = K.LL_BoneCount();
	for (u16 I = 0; I < Count; ++I)
	{
		if (BoneHasPhysics(K, I))
		{
			return true;
		}
	}
	return false;
}

void phys_shell_verify_model(IKinematics& K)
{
	VERIFY2(has_physics_collision_shapes(K), make_string<const char*>("Can not create physics shell for model %s because it has no physics collision shapes set", K.getDebugName().c_str()));
}

void phys_shell_verify_object_model(IPhysicsShellHolder& O)
{
	IKinematics* K = O.ObjectKinematics();
	VERIFY2(K, make_string<const char*>("Can not create physics shell for object %s, model %s is not skeleton", O.ObjectName(), O.ObjectNameVisual()));
	VERIFY2(has_physics_collision_shapes(*K), make_string<const char*>("Can not create physics shell for object %s, model %s has no physics collision shapes set", O.ObjectName(), O.ObjectNameVisual()));
	VERIFY2(_valid(O.ObjectXFORM()), make_string<const char*>("create physics shell: object matrix is not valid"));
	VERIFY2(valid_pos(O.ObjectXFORM().c), dbg_valide_pos_string(O.ObjectXFORM().c, &O, "create physics shell").c_str());
}

bool can_create_phys_shell(string1024& reason, IPhysicsShellHolder& O)
{
	xr_strcpy(reason, "ok");
	IKinematics* K = O.ObjectKinematics();
	if (!K)
	{
		xr_strcpy(reason, make_string<const char*>("Can not create physics shell for object %s, model %s is not skeleton", O.ObjectName(), O.ObjectNameVisual()));
		return false;
	}
	if (!has_physics_collision_shapes(*K))
	{
		xr_strcpy(reason, make_string<const char*>("Can not create physics shell for object %s, model %s has no physics collision shapes set", O.ObjectName(), O.ObjectNameVisual()));
		return false;
	}
	if (!_valid(O.ObjectXFORM()))
	{
		xr_strcpy(reason, make_string<const char*>("create physics shell: object matrix is not valid"));
		return false;
	}
	if (!valid_pos(O.ObjectXFORM().c))
	{
		xr_strcpy(reason, make_string<const char*>("~ create physics shell: object position is not valid, or missing 'level.cform', or the level is not loaded."));
		return false;
	}
	return true;
}

// ODE turns this contact into a one-sided static contact; Box3D keeps the normal contact.
void StaticEnvironmentCB(bool& do_colide, bool bo1, CPHContact& contact, SGameMtl* material_1, SGameMtl* material_2)
{
}

static float ContactEnergy(const dContact& Contact, bool First)
{
	// Kinetic energy lost along the normal, as E_NL in the ODE backend.
	float Mass[2] = {0.f, 0.f};
	float Velocity[2] = {0.f, 0.f};
	for (int I = 0; I < 2; ++I)
	{
		CODEGeom* Geom = Contact.Geom[I];
		if (Geom == nullptr || Geom->OwnerForm == nullptr || !b3Shape_IsValid(Geom->Shape))
		{
			continue;
		}

		const b3BodyId Body = b3Shape_GetBody(Geom->Shape);
		if (b3Body_GetType(Body) != b3_dynamicBody)
		{
			continue;
		}

		Mass[I] = b3Body_GetMass(Body);
		Velocity[I] = B3Math::FromB3(b3Body_GetLinearVelocity(Body)).dotproduct(Contact.Normal);
	}

	// Normal points from the first side to the second: approaching means V0 > V1.
	if (Velocity[0] <= Velocity[1])
	{
		return 0.f;
	}
	if (Mass[0] <= 0.f || Mass[1] <= 0.f)
	{
		const int Moving = Mass[0] > 0.f ? 0 : 1;
		return Mass[Moving] * Velocity[Moving] * Velocity[Moving] * 0.5f;
	}

	const float Total = Mass[0] + Mass[1];
	const float Center = (Mass[0] * Velocity[0] + Mass[1] * Velocity[1]) / Total;
	const float Start = 0.5f * (Mass[0] * Velocity[0] * Velocity[0] + Mass[1] * Velocity[1] * Velocity[1]);
	return Start - 0.5f * Total * Center * Center;
}

void DamageReceiverCollisionCallback(bool& do_colide, bool bo1, CPHContact& contact, SGameMtl* material_1, SGameMtl* material_2)
{
	if (material_1->Flags.test(SGameMtl::flPassable) || material_2->Flags.test(SGameMtl::flPassable))
	{
		return;
	}

	dContact& Native = contact.Native();
	IPhysicsShellHolder* Self = contact.GetObject(bo1);
	IPhysicsShellHolder* Damager = contact.GetObject(!bo1);
	if (Self == nullptr)
	{
		return;
	}

	ICollisionDamageReceiver* Receiver = Self->ObjectPhCollisionDamageReceiver();
	if (Receiver == nullptr)
	{
		return;
	}

	SGameMtl* MaterialSelf = bo1 ? material_1 : material_2;
	SGameMtl* MaterialDamager = bo1 ? material_2 : material_1;
	float DamagerFactor = MaterialDamager->fBounceDamageFactor;
	if (Damager && contact.GetCharacter(!bo1))
	{
		Damager->BonceDamagerCallback(DamagerFactor);
	}

	const float Sum = MaterialSelf->fBounceDamageFactor + DamagerFactor;
	if (fis_zero(Sum))
	{
		return;
	}

	CODEGeom* SelfGeom = Native.Geom[bo1 ? 0 : 1];
	Fvector Pos;
	Fmatrix Form;
	SelfGeom->GetGlobalForm(Form);
	Pos.sub(Native.Position, Form.c);
	const ALife::_OBJECT_ID SourceId = Damager ? Damager->ObjectID() : ALife::INVALID_OBJECT_ID;
	Receiver->CollisionHit(SourceId, SelfGeom->BoneId, ContactEnergy(Native, true) * DamagerFactor / Sum, Native.Normal, Pos);
}

void BreakableObjectCollisionCallback(bool& do_colide, bool bo1, CPHContact& contact, SGameMtl* material_1, SGameMtl* material_2)
{
	dContact& Native = contact.Native();
	IPhysicsShellHolder* Self = contact.GetObject(bo1);
	if (Self == nullptr)
	{
		return;
	}

	ICollisionDamageReceiver* Receiver = Self->ObjectPhCollisionDamageReceiver();
	if (Receiver == nullptr)
	{
		return;
	}

	const float NormalSign = bo1 ? -1.f : 1.f;
	Fvector Dir;
	Dir.mul(Native.Normal, -NormalSign);
	Fvector Pos = Native.Position;
	Receiver->CollisionHit(ALife::INVALID_OBJECT_ID, u16(-1), ContactEnergy(Native, true), Dir, Pos);
}

// Static geom shell: a static Box3D body with one box, owned by an IPhysicsShellHolder.
class CB3StaticGeomShell : public IPHStaticGeomShell
{
public:
	SB3GeomOwner Owner;
	Fmatrix Form = Fidentity;
	CODEGeom* Geom = nullptr;
	b3BodyId Body = b3_nullBodyId;

	void Build(const Fobb& Box, const Fmatrix& InForm, u64 Mask, u64 Category = B3Category::Static)
	{
		Form.set(InForm);
		Geom = CODEGeom::MakeBox(Box);
		Geom->Owner = &Owner;
		Geom->OwnerForm = &Form;
		if (g_B3World == nullptr)
		{
			return;
		}

		const b3Transform Transform = B3Math::ToB3(Form);
		b3BodyDef Def = b3DefaultBodyDef();
		Def.type = b3_staticBody;
		Def.position = Transform.p;
		Def.rotation = Transform.q;
		Body = b3CreateBody(g_B3World->Id(), &Def);
		Geom->CreateShape(Body, 1.f, false, Mask);
		if (Category != B3Category::Static && b3Shape_IsValid(Geom->Shape))
		{
			// invokeContacts rebuilds the broadphase proxy: queries filter on the category stored in the tree.
			b3Filter Filter = b3Shape_GetFilter(Geom->Shape);
			Filter.categoryBits = Category;
			B3_SAFE(b3Shape_SetFilter(Geom->Shape, Filter, true));
		}
	}

	virtual ~CB3StaticGeomShell() override
	{
		if (Geom)
		{
			Geom->Shape = b3_nullShapeId;
		}
		if (b3Body_IsValid(Body))
		{
			b3DestroyBody(Body);
		}
		xr_delete(Geom);
	}
};

static void _BCL StaticGeomBoneCallback(CBoneInstance* B)
{
}

IPHStaticGeomShell* P_BuildStaticGeomShell(IPhysicsShellHolder* obj, ObjectContactCallbackFun* object_contact_callback)
{
	IKinematics* K = obj->ObjectKinematics();
	R_ASSERT2(K, "need visual to build");

	K->CalculateBones(true);
	Fobb Box;
	K->GetBox().getradius(Box.m_halfsize);
	Box.xform_set(Fidentity);

	CB3StaticGeomShell* Shell = new CB3StaticGeomShell();
	Shell->Owner.RefObject = obj;
	Shell->Owner.AddObjectCallback(object_contact_callback);
	Shell->Build(Box, obj->ObjectXFORM(), B3Category::Dynamic);

	K->CalculateBones(true);
	for (u16 I = 0; I < K->LL_BoneCount(); ++I)
	{
		K->LL_GetBoneInstance(I).set_callback(bctPhysics, StaticGeomBoneCallback, K->LL_GetBoneInstance(I).callback_param(), true);
	}
	return Shell;
}

IPHStaticGeomShell* P_BuildLeaderGeomShell(IClimableObject* obj, ObjectContactCallbackFun* callback, const Fobb& b)
{
	IPhysicsShellHolder* Holder = smart_cast<IPhysicsShellHolder*>(obj);
	CB3StaticGeomShell* Shell = new CB3StaticGeomShell();
	Shell->Owner.RefObject = Holder;
	Shell->Owner.Material = obj->Material();
	Shell->Owner.Climable = obj;
	Shell->Owner.AddObjectCallback(callback);
	// Ladders do not block anything; characters find them with queries and climb.
	Shell->Build(b, Holder->ObjectXFORM(), B3Category::Ladder, B3Category::Ladder);
	return Shell;
}

void DestroyStaticGeomShell(IPHStaticGeomShell*& p)
{
	CB3StaticGeomShell* Shell = static_cast<CB3StaticGeomShell*>(p);
	xr_delete(Shell);
	p = nullptr;
}

#ifdef DEBUG_DRAW
class CB3DebugOutput : public IDebugOutput
{
	Flags32 Mask;
	Flags32 Mask1;
	u32 Counters[9] = {};

public:
	CB3DebugOutput()
	{
		Mask.zero();
		Mask1.zero();
	}

	virtual const Flags32& ph_dbg_draw_mask() const override
	{
		return Mask;
	}
	virtual const Flags32& ph_dbg_draw_mask1() const override
	{
		return Mask1;
	}
	virtual void DBG_DrawStatBeforeFrameStep() override {}
	virtual void DBG_DrawStatAfterFrameStep() override {}
	virtual void DBG_OpenCashedDraw() override {}
	virtual void DBG_ClosedCashedDraw(u32 remove_time) override {}
	virtual void DBG_DrawPHObject(const CPHObject* obj) override {}
	virtual void DBG_DrawContact(const CPHContact& c) override {}
	virtual void DBG_DrawTri(CDB::RESULT* T, u32 c) override {}
	virtual void DBG_DrawTri(CDB::TRI* T, const Fvector* V_verts, u32 c) override {}
	virtual void DBG_DrawLine(const Fvector& p0, const Fvector& p1, u32 c) override {}
	virtual void DBG_DrawAABB(const Fvector& center, const Fvector& AABB, u32 c) override {}
	virtual void DBG_DrawOBB(const Fmatrix& m, const Fvector h, u32 c) override {}
	virtual void DBG_DrawPoint(const Fvector& p, float size, u32 c) override {}
	virtual void DBG_DrawMatrix(const Fmatrix& m, float size, u8 a = 255) override {}
	virtual void _cdecl DBG_OutText(const char* s, ...) override {}
	virtual void DBG_DrawFrameStart() override {}
	virtual void PH_DBG_Render() override {}
	virtual void PH_DBG_Clear() override {}
	virtual const char* PH_DBG_ObjectTrackName() override
	{
		return "";
	}
	virtual u32& dbg_tries_num() override
	{
		return Counters[0];
	}
	virtual u32& dbg_saved_tries_for_active_objects() override
	{
		return Counters[1];
	}
	virtual u32& dbg_total_saved_tries() override
	{
		return Counters[2];
	}
	virtual u32& dbg_reused_queries_per_step() override
	{
		return Counters[3];
	}
	virtual u32& dbg_new_queries_per_step() override
	{
		return Counters[4];
	}
	virtual u32& dbg_bodies_num() override
	{
		return Counters[5];
	}
	virtual u32& dbg_joints_num() override
	{
		return Counters[6];
	}
	virtual u32& dbg_islands_num() override
	{
		return Counters[7];
	}
	virtual u32& dbg_contacts_num() override
	{
		return Counters[8];
	}
	virtual float dbg_vel_collid_damage_to_display() override
	{
		return 7.5f;
	}
	virtual void DBG_ObjAfterPhDataUpdate(CPHObject* obj) override {}
	virtual void DBG_ObjBeforePhDataUpdate(CPHObject* obj) override {}
	virtual void DBG_ObjAfterStep(CPHObject* obj) override {}
	virtual void DBG_ObjBeforeStep(CPHObject* obj) override {}
	virtual void DBG_ObjeAfterPhTune(CPHObject* obj) override {}
	virtual void DBG_ObjBeforePhTune(CPHObject* obj) override {}
	virtual void DBG_ObjAfterCollision(CPHObject* obj) override {}
	virtual void DBG_ObjBeforeCollision(CPHObject* obj) override {}
};

static CB3DebugOutput g_B3DebugOutput;
IDebugOutput* ph_debug_output = &g_B3DebugOutput;

IPhysicsShellHolder* PHObjectRefObject(CPHObject* Object)
{
	return nullptr;
}

void PHObjectGetBounds(const CPHObject* Object, Fvector& AABB, Fvector& Center)
{
	AABB.set(0.f, 0.f, 0.f);
	Center.set(0.f, 0.f, 0.f);
}
#endif
