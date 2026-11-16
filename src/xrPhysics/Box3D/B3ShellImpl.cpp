#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "IPhysicsShellHolder.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/bone.h"
#include "PHShellBuildJoint.h"

extern Fbox phBoundaries;

CPhysicsShell::~CPhysicsShell()
{
}

static void _BCL EmptyBoneCallback(CBoneInstance* B)
{
}

static void _BCL ElementBonesCallback(CBoneInstance* B);

CPHShell::CPHShell()
{
	mXFORM.identity();
	m_pKinematics = nullptr;
#ifdef DEBUG
	dbg_obj = nullptr;
#endif
	CollideClassBits.assign(B3CollideClass::Dynamic);
}

CPHShell::~CPHShell()
{
	if (Active)
	{
		Deactivate();
	}

	for (CPHJoint* Joint : JointsList)
	{
		xr_delete(Joint);
	}
	JointsList.clear();

	for (CPHElement* Element : ElementsList)
	{
		xr_delete(Element);
	}
	ElementsList.clear();
}

void CPHShell::PhTune(float Step)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->PhTune(Step);
	}

	for (CPHJoint* Joint : JointsList)
	{
		Joint->PhTune(Step);
	}
}

void CPHShell::PhDataUpdate(float Step)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->CutVelocity();
		Element->PullState(true);
	}

	CPHElement* RootElement = Root();
	if (RootElement && RootElement->IsAwake() && RootElement->CurForm.c.y < phBoundaries.y1)
	{
		Disable();
	}

	const bool Enabled = isEnabled();
	if (Enabled != WasEnabled)
	{
		WasEnabled = Enabled;
		if (Enabled)
		{
			vis_update_activate();
		}
		else
		{
			--ActiveCount;
		}
	}

	SelfContacts = 0;
	for (CPHElement* Element : ElementsList)
	{
		b3ContactData Contacts[32];
		const int Count = SelfCollision && Element->IsAwake() ? b3Body_GetContactData(Element->Body, Contacts, 32) : 0;
		for (int I = 0; I < Count; ++I)
		{
			const b3ContactData& Contact = Contacts[I];
			const b3BodyId BodyA = b3Shape_GetBody(Contact.shapeIdA);
			if (B3_ID_EQUALS(BodyA, Element->Body) && OwnsBody(b3Shape_GetBody(Contact.shapeIdB)))
			{
				for (int M = 0; M < Contact.manifoldCount; ++M)
				{
					SelfContacts += u32(Contact.manifolds[M].pointCount);
				}
			}
		}
	}
}

void CPHShell::set_DynamicLimits(float l_limit, float w_limit)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_DynamicLimits(l_limit, w_limit);
	}
}

void CPHShell::set_DynamicScales(float l_scale, float w_scale)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_DynamicScales(l_scale, w_scale);
	}
}

void CPHShell::SetGeomFilter()
{
	const u64 Mask = CollisionDisabled ? 0ull : CollideMask;
	for (CPHElement* Element : ElementsList)
	{
		for (CODEGeom* Geom : Element->Geoms)
		{
			Geom->SetMask(Mask);
		}
	}
}

void CPHShell::Activate(const Fmatrix& m0, float dt01, const Fmatrix& m2, bool disable)
{
	if (Active)
	{
		return;
	}

	mXFORM.set(m0);
	for (CPHElement* Element : ElementsList)
	{
		Element->Activate(mXFORM, disable);
	}
	for (CPHJoint* Joint : JointsList)
	{
		Joint->Activate();
	}

	Active = true;
	if (g_B3World && !InWorld)
	{
		g_B3World->AddShell(this);
		InWorld = true;
	}
	FinishActivate(disable);

	Fvector LinearVel;
	LinearVel.sub(m2.c, m0.c);
	set_LinearVel(LinearVel);
	if (!SkipBoneCallbacks)
	{
		SetCallbacks();
	}
}

void CPHShell::Activate(const Fmatrix& transform, const Fvector& lin_vel, const Fvector& ang_vel, bool disable)
{
	if (Active)
	{
		return;
	}

	mXFORM.set(transform);
	for (CPHElement* Element : ElementsList)
	{
		Fmatrix Global;
		Global.mul_43(transform, Element->mXFORM);
		Element->Activate(Global, lin_vel, ang_vel, disable);
	}
	for (CPHJoint* Joint : JointsList)
	{
		Joint->Activate();
	}

	Active = true;
	if (g_B3World && !InWorld)
	{
		g_B3World->AddShell(this);
		InWorld = true;
	}
	FinishActivate(disable);
	SetCallbacks();
}

// As ODE activate(): a shell activated disabled puts its object to processing sleep. Ragdoll
// inertia smoothing waits for the bodies.
void CPHShell::FinishActivate(bool Disable)
{
	WasEnabled = !Disable;
	if (Disable)
	{
		--ActiveCount;
	}
	if (SmoothInertia <= 0.f)
	{
		return;
	}

	b3Matrix3 Mean = b3Mat3_zero;
	for (CPHElement* Element : ElementsList)
	{
		if (b3Body_IsValid(Element->Body))
		{
			Mean = b3AddMM(Mean, b3Body_GetMassData(Element->Body).inertia);
		}
	}
	Mean = b3MulSM(SmoothInertia / float(ElementsList.size()), Mean);
	for (CPHElement* Element : ElementsList)
	{
		Element->InertiaKeep *= 1.f - SmoothInertia;
		Element->ExtraInertia = b3AddMM(b3MulSM(1.f - SmoothInertia, Element->ExtraInertia), Mean);
		Element->ApplyMassData();
	}
	SmoothInertia = 0.f;
}

void CPHShell::Activate(bool disable, bool not_set_bone_callbacks)
{
	SkipBoneCallbacks = not_set_bone_callbacks;
	Activate(mXFORM, 0.f, mXFORM, disable);
	SkipBoneCallbacks = false;
}

void CPHShell::Activate(const Fmatrix& form, bool disable)
{
	Activate(form, 0.f, form, disable);
}

void CPHShell::Build(bool disable)
{
	// ODE Build does not touch the processing count; RunSimulation enables the shell.
	ActiveCount += Active ? 0 : 1;
	Activate(true, true);
}

void CPHShell::RunSimulation(bool place_current_forms)
{
	if (!Active)
	{
		Activate(false, true);
	}
	// Start from the animated pose, not the bind pose the elements were built in (ODE flActivating).
	if (place_current_forms && m_pKinematics != nullptr)
	{
		ActivatingBonePoses(*m_pKinematics);
	}
	Enable();
	WasEnabled = true;
}

// As ODE: disabled callbacks let the animation drive the bones (imotion_position death animation).
void CPHShell::EnabledCallbacks(bool val)
{
	CallbacksEnabled = val;
	if (!val)
	{
		ZeroCallbacks();
		return;
	}

	SetCallbacks();
	if (m_pKinematics != nullptr)
	{
		for (CPHElement* Element : ElementsList)
		{
			if (Element->m_SelfID != BI_NONE)
			{
				m_pKinematics->LL_GetBoneInstance(Element->m_SelfID).set_callback_overwrite(true);
			}
		}
	}
}

bool CPHShell::AnimToVelocityState(float dt, float l_limit, float a_limit)
{
	bool Result = true;
	for (CPHElement* Element : ElementsList)
	{
		Result = Element->AnimToVel(dt, l_limit, a_limit) && Result;
	}
	return Result;
}

void CPHShell::ActivatingBonePoses(IKinematics& K)
{
	for (CPHElement* Element : ElementsList)
	{
		if (Element->m_SelfID != BI_NONE)
		{
			Element->ToBonePos(K.LL_GetTransform(Element->m_SelfID));
			if (Element == Root())
			{
				ObjectInRootForm.invert(K.LL_GetTransform(Element->m_SelfID));
			}
		}
	}
}

void CPHShell::Deactivate()
{
	if (g_B3World)
	{
		g_B3World->NetRelcase(this);
	}

	if (!Active)
	{
		return;
	}

	R_ASSERT2(!g_B3World || !g_B3World->Processing(), "can not deactivate physics shell during physics processing!!!");
	if (g_B3World && InWorld)
	{
		g_B3World->RemoveShell(this);
	}
	InWorld = false;

	ZeroCallbacks();
	xr_delete(Animator);
	for (CPHJoint* Joint : JointsList)
	{
		Joint->Deactivate();
	}
	for (CPHElement* Element : ElementsList)
	{
		Element->Deactivate();
	}
	Active = false;
}

void CPHShell::InterpolateGlobalTransform(Fmatrix* m)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->InterpolateGlobalTransform(&Element->mXFORM);
	}

	if (CPHElement* RootElement = Root())
	{
		m->set(RootElement->mXFORM);
	}
	m->mulB_43(ObjectInRootForm);
	mXFORM.set(*m);
	VERIFY2(_valid(*m), "not valide transform");

	IPhysicsShellHolder* Object = Root() ? Root()->PhysicsRefObject() : nullptr;
	if (Object && ActiveCount < 0)
	{
		Object->ObjectProcessingDeactivate();
		Object->ObjectSpatialMove();
		ActiveCount = 0;
	}
}

void CPHShell::vis_update_activate()
{
	IPhysicsShellHolder* Object = Root() ? Root()->PhysicsRefObject() : nullptr;
	if (++ActiveCount > 0 && Object)
	{
		ActiveCount = 0;
		Object->ObjectProcessingActivate();
	}
}

void CPHShell::GetGlobalTransformDynamic(Fmatrix* m)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->GetGlobalTransformDynamic(&Element->mXFORM);
	}

	if (CPHElement* RootElement = Root())
	{
		m->set(RootElement->mXFORM);
	}
	m->mulB_43(ObjectInRootForm);
	mXFORM.set(*m);
}

void CPHShell::InterpolateGlobalPosition(Fvector* v)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->InterpolateGlobalPosition(v);
		v->add(ObjectInRootForm.c);
	}
}

void CPHShell::GetGlobalPositionDynamic(Fvector* v)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->GetGlobalPositionDynamic(v);
	}
}

void CPHShell::UpdateRoot()
{
	if (CPHElement* RootElement = Root(); RootElement && RootElement->isActive())
	{
		RootElement->InterpolateGlobalTransform(&mXFORM);
	}
}

void CPHShell::Update()
{
	if (!Active)
	{
		return;
	}

	Fmatrix Form;
	InterpolateGlobalTransform(&Form);
}

void CPHShell::ObjectToRootForm(const Fmatrix& form)
{
	mXFORM.set(form);
}

void CPHShell::net_Import(NET_Packet& P)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->net_Import(P);
	}
}

void CPHShell::net_Export(NET_Packet& P)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->net_Export(P);
	}
}

bool CPHShell::isEnabled() const
{
	for (CPHElement* Element : ElementsList)
	{
		if (Element->isEnabled())
		{
			return true;
		}
	}
	return false;
}

void CPHShell::Enable()
{
	for (CPHElement* Element : ElementsList)
	{
		Element->Enable();
	}
}

// ODE disables only this shell's bodies; Box3D sleeps the whole island, so the shell is put to sleep
// only when no other dynamic body touches it. Otherwise it stops and Box3D sleeps it later.
void CPHShell::Disable()
{
	if (g_B3World && g_B3World->IsStepping())
	{
		g_B3World->Defer([this]() { Disable(); });
		return;
	}

	bool Alone = true;
	for (CPHElement* Element : ElementsList)
	{
		if (!b3Body_IsValid(Element->Body))
		{
			continue;
		}

		b3Body_SetLinearVelocity(Element->Body, b3Vec3_zero);
		b3Body_SetAngularVelocity(Element->Body, b3Vec3_zero);
		b3ContactData Contacts[32];
		const int Count = b3Body_GetContactData(Element->Body, Contacts, 32);
		for (int I = 0; I < Count && Alone; ++I)
		{
			const b3BodyId BodyA = b3Shape_GetBody(Contacts[I].shapeIdA);
			const b3BodyId Other = B3_ID_EQUALS(BodyA, Element->Body) ? b3Shape_GetBody(Contacts[I].shapeIdB) : BodyA;
			Alone = b3Body_GetType(Other) != b3_dynamicBody || OwnsBody(Other);
		}
	}
	for (CPHElement* Element : ElementsList)
	{
		if (Alone && b3Body_IsValid(Element->Body))
		{
			b3Body_SetAwake(Element->Body, false);
		}
	}
}

void CPHShell::setMass(float M)
{
	const float Volume = getVolume();
	for (CPHElement* Element : ElementsList)
	{
		const float Part = Volume > EPS_S ? Element->getVolume() / Volume : 1.f / ElementsList.size();
		Element->setMass(M * Part);
	}
}

void CPHShell::setMass1(float M)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->setMass(M / ElementsList.size());
	}
}

void CPHShell::setDensity(float M)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->setDensity(M);
	}
}

float CPHShell::getMass()
{
	float Mass = 0.f;
	for (CPHElement* Element : ElementsList)
	{
		Mass += Element->getMass();
	}
	return Mass;
}

float CPHShell::getDensity()
{
	const float Volume = getVolume();
	return Volume > EPS_S ? getMass() / Volume : 0.f;
}

float CPHShell::getVolume()
{
	float Volume = 0.f;
	for (CPHElement* Element : ElementsList)
	{
		Volume += Element->getVolume();
	}
	return Volume;
}

// As ODE: mass is the box density; every element gets the box mass and inertia at its center.
void CPHShell::MassAddBox(float mass, const Fvector& full_size)
{
	const float M = mass * full_size.x * full_size.y * full_size.z;
	const float X = full_size.x * full_size.x / 12.f;
	const float Y = full_size.y * full_size.y / 12.f;
	const float Z = full_size.z * full_size.z / 12.f;
	const b3Matrix3 Inertia = {{M * (Y + Z), 0.f, 0.f}, {0.f, M * (X + Z), 0.f}, {0.f, 0.f, M * (X + Y)}};
	for (CPHElement* Element : ElementsList)
	{
		Element->AddedMass += M;
		Element->ExtraInertia = b3AddMM(Element->ExtraInertia, Inertia);
		Element->ApplyMassData();
	}
}

// As ODE: before the bodies exist the masses are blended at once and the inertia once they are
// created; on a built shell (BreakableObject) ODE leaves the bodies alone.
void CPHShell::SmoothElementsInertia(float k)
{
	float Mean = 0.f;
	for (CPHElement* Element : ElementsList)
	{
		if (b3Body_IsValid(Element->Body))
		{
			return;
		}
		Mean += Element->getMass();
	}
	Mean *= k / float(ElementsList.size());
	for (CPHElement* Element : ElementsList)
	{
		const float Scale = 1.f - k + Mean / std::max(Element->Mass, EPS_S);
		Element->Mass *= Scale;
		for (auto& Entry : Element->GeomMasses)
		{
			Entry.second *= Scale;
		}
	}
	SmoothInertia = k;
}

void CPHShell::get_Extensions(const Fvector& axis, float center_prg, float& lo_ext, float& hi_ext) const
{
	lo_ext = flt_max;
	hi_ext = -flt_max;
	for (CPHElement* Element : ElementsList)
	{
		float Lo, Hi;
		Element->get_Extensions(axis, center_prg, Lo, Hi);
		lo_ext = std::min(lo_ext, Lo);
		hi_ext = std::max(hi_ext, Hi);
	}
}

void CPHShell::applyForce(const Fvector& dir, float val)
{
	const float Mass = getMass();
	for (CPHElement* Element : ElementsList)
	{
		Element->applyForce(dir, Mass > EPS_S ? val * Element->getMass() / Mass : val);
	}
}

void CPHShell::applyForce(float x, float y, float z)
{
	Fvector Force;
	Force.set(x, y, z);
	const float Magnitude = Force.magnitude();
	if (Magnitude > EPS_S)
	{
		applyForce(Force.div(Magnitude), Magnitude);
	}
}

void CPHShell::applyImpulse(const Fvector& dir, float val)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->applyImpulse(dir, val);
	}
}

void CPHShell::applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->applyImpulseTrace(pos, dir, val, 0);
	}
}

void CPHShell::applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val, const u16 id)
{
	if (CPhysicsElement* Element = id == BI_NONE ? nullptr : get_Element(id))
	{
		Element->applyImpulseTrace(pos, dir, val, id);
	}
}

// As ODE ShellHit.cpp: a blast pushes every geom of the shell at a random point in its element, and
// its hit position (often the world center of the target) is never a lever arm.
void CPHShell::applyHit(const Fvector& pos, const Fvector& dir, float val, const u16 id, ALife::EHitType hit_type)
{
	if (id == BI_NONE)
	{
		return;
	}
	if (m_pKinematics == nullptr)
	{
		applyImpulseTrace(pos, dir, val);
		return;
	}
	if (hit_type != ALife::eHitTypeExplosion)
	{
		applyImpulseTrace(pos, dir, val, id);
		return;
	}
	if (!Active)
	{
		return;
	}

	Enable();
	const float Impulse = val / _sqrt(_sqrt(float(ElementsList.size())));
	for (CPHElement* Element : ElementsList)
	{
		const float Radius = Element->getRadius();
		for (CODEGeom* Geom : Element->Geoms)
		{
			Fvector Point, Dir;
			Point.random_point(Fvector().set(Radius, Radius, Radius));
			Dir.random_dir();
			if (!fis_zero(pos.magnitude(), EPS_L))
			{
				Dir.mul(0.5f).add(dir);
			}
			Dir.normalize_safe();
			Element->applyImpulseTrace(Point, Dir, Impulse / float(Element->Geoms.size()), Geom->BoneId);
		}
	}
}

void CPHShell::setTorque(const Fvector& torque)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->setTorque(torque);
	}
}

void CPHShell::setForce(const Fvector& force)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->setForce(force);
	}
}

void CPHShell::applyGravityAccel(const Fvector& accel)
{
	if (!Active)
	{
		return;
	}

	const Fvector Accel = Fvector().mul(accel, float(ElementsList.size()));
	for (CPHElement* Element : ElementsList)
	{
		Element->applyGravityAccel(Accel);
	}
}

void CPHShell::SetAirResistance(float linear, float angular)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->SetAirResistance(linear, angular);
	}
}

void CPHShell::GetAirResistance(float& linear, float& angular)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->GetAirResistance(linear, angular);
	}
}

void CPHShell::set_ContactCallback(ContactCallbackFun* callback)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_ContactCallback(callback);
	}
}

void CPHShell::set_ObjectContactCallback(ObjectContactCallbackFun* callback)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_ObjectContactCallback(callback);
	}
}

void CPHShell::add_ObjectContactCallback(ObjectContactCallbackFun* callback)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->add_ObjectContactCallback(callback);
	}
}

void CPHShell::remove_ObjectContactCallback(ObjectContactCallbackFun* callback)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->remove_ObjectContactCallback(callback);
	}
}

void CPHShell::SetAnimated(bool v)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->SetAnimated(v);
	}
}

void CPHShell::set_CallbackData(void* cd)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_CallbackData(cd);
	}
}

void* CPHShell::get_CallbackData()
{
	CPHElement* RootElement = Root();
	return RootElement ? RootElement->get_CallbackData() : nullptr;
}

void CPHShell::set_PhysicsRefObject(IPhysicsShellHolder* ref_object)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_PhysicsRefObject(ref_object);
	}
}

void CPHShell::set_LinearVel(const Fvector& velocity)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_LinearVel(velocity);
	}
}

void CPHShell::set_AngularVel(const Fvector& velocity)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_AngularVel(velocity);
	}
}

void CPHShell::get_LinearVel(Fvector& velocity) const
{
	velocity.set(0.f, 0.f, 0.f);
	if (!ElementsList.empty())
	{
		ElementsList.front()->get_LinearVel(velocity);
	}
}

void CPHShell::get_AngularVel(Fvector& velocity) const
{
	velocity.set(0.f, 0.f, 0.f);
	if (!ElementsList.empty())
	{
		ElementsList.front()->get_AngularVel(velocity);
	}
}

void CPHShell::TransformPosition(const Fmatrix& form, motion_history_state history_state)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->TransformPosition(form, history_state);
	}
	mXFORM.mulA_43(form);
}

void CPHShell::SetTransform(const Fmatrix& m0, motion_history_state history_state)
{
	if (!Active)
	{
		mXFORM.set(m0);
		return;
	}

	Fmatrix InvShell;
	InvShell.invert(mXFORM);
	for (CPHElement* Element : ElementsList)
	{
		Fmatrix Current;
		Element->GetGlobalTransformDynamic(&Current);
		Fmatrix Relative;
		Relative.mul_43(InvShell, Current);
		Fmatrix Global;
		Global.mul_43(m0, Relative);
		Element->SetTransform(Global, history_state);
	}
	mXFORM.set(m0);
}

void CPHShell::SetGlTransformDynamic(const Fmatrix& form)
{
	SetTransform(form, mh_unspecified);
}

void CPHShell::set_ApplyByGravity(bool flag)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_ApplyByGravity(flag);
	}
}

bool CPHShell::get_ApplyByGravity()
{
	CPHElement* RootElement = Root();
	return RootElement ? RootElement->get_ApplyByGravity() : false;
}

void CPHShell::SetMaterial(u16 m)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->SetMaterial(m);
	}
}

void CPHShell::SetMaterial(const char* m)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->SetMaterial(m);
	}
}

void CPHShell::set_DisableParams(const SAllDDOParams& params)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->set_DisableParams(params);
	}
}

void CPHShell::add_Element(CPhysicsElement* E)
{
	CPHElement* Element = static_cast<CPHElement*>(E);
	Element->Shell = this;
	Element->Owner.Shell = this;
	ElementsList.push_back(Element);
}

void CPHShell::add_Joint(CPhysicsJoint* E)
{
	if (E)
	{
		CPHJoint* Joint = static_cast<CPHJoint*>(E);
		Joint->OwnerShell = this;
		JointsList.push_back(Joint);
	}
}

void CPHShell::SetIgnoreStatic()
{
	CollideMask &= ~B3Category::Level;
	SetGeomFilter();
}

// ODE: not dynamic class, so characters too; static geom shells still collide.
void CPHShell::SetIgnoreDynamic()
{
	CollideMask &= ~(B3Category::Dynamic | B3Category::Character);
	SetGeomFilter();
}

// Character exclusion lives in the mask: character mover queries skip shapes that do not accept characters.
void CPHShell::DisableCharacterCollision()
{
	CollideMask &= ~B3Category::Character;
	CollideClassBits.set(B3CollideClass::NCCharacter, true);
	SetGeomFilter();
}

void CPHShell::ResetPairs()
{
	const u64 Mask = CollisionDisabled ? 0ull : CollideMask;
	for (CPHElement* Element : ElementsList)
	{
		for (CODEGeom* Geom : Element->Geoms)
		{
			Geom->SetMask(0ull);
			Geom->SetMask(Mask);
		}
	}
}

void CPHShell::SetClassBit(CLClassBits Bit)
{
	if (!CollideClassBits.test(Bit))
	{
		CollideClassBits.set(Bit, true);
		ResetPairs();
	}
}

void CPHShell::SetRagDoll()
{
	if (CollideClassBits.test(B3CollideClass::RagDoll))
	{
		return;
	}

	// Recycled contacts keep their old friction: the death skin friction must reach resting contacts.
	for (CPHElement* Element : ElementsList)
	{
		const b3BodyId Body = Element->Body;
		if (b3Body_IsValid(Body))
		{
			B3_SAFE(b3Body_EnableContactRecycling(Body, false));
		}
	}
	SetClassBit(B3CollideClass::RagDoll);
}

void CPHShell::SetIgnoreRagDoll()
{
	SetClassBit(B3CollideClass::NCRagDoll);
}

void CPHShell::SetIgnoreAnimated()
{
	SetClassBit(B3CollideClass::NCAnimated);
}

void CPHShell::SetSmall()
{
	SetClassBit(B3CollideClass::Small);
}

void CPHShell::SetIgnoreSmall()
{
	SetClassBit(B3CollideClass::NCSmall);
}

void CPHShell::SetBullets()
{
	const bool Bullet = IsBullet();
	for (CPHElement* Element : ElementsList)
	{
		const b3BodyId Body = Element->Body;
		if (b3Body_IsValid(Body))
		{
			B3_SAFE(b3Body_SetBullet(Body, Bullet));
		}
	}
}

void CPHShell::BuildSelfCollisionPairs()
{
	SelfCollisionDirty = false;
	SelfCollisionExcluded.clear();
	if (SelfCollision && g_B3World)
	{
		struct SContext
		{
			CPHShell* Shell;
			const SB3GeomOwner* Owner;
		};
		const auto Callback = [](b3ShapeId ShapeId, void* Context) -> bool
		{
			const SContext& Self = *static_cast<SContext*>(Context);
			const CODEGeom* Other = static_cast<const CODEGeom*>(b3Shape_GetUserData(ShapeId));
			if (Other && Other->Owner && Other->Owner->Shell == Self.Shell && Other->Owner != Self.Owner)
			{
				const SB3GeomOwner* OtherOwner = Other->Owner;
				Self.Shell->SelfCollisionExcluded.emplace_back(std::min(Self.Owner, OtherOwner), std::max(Self.Owner, OtherOwner));
			}
			return true;
		};
		// Overlap deeper than 1 cm, as ODE SelfCollisionOverlap.
		for (CPHElement* Element : ElementsList)
		{
			SContext Context{this, &Element->Owner};
			for (CODEGeom* Geom : Element->Geoms)
			{
				g_B3World->OverlapGeom(*Geom, Element->CurForm, 0.01f, B3Category::Dynamic, Callback, &Context);
			}
		}
	}
	ResetPairs();
}

bool CPHShell::SelfCollides(const SB3GeomOwner* A, const SB3GeomOwner* B) const
{
	const std::pair<const SB3GeomOwner*, const SB3GeomOwner*> Pair(std::min(A, B), std::max(A, B));
	return SelfCollision && std::find(SelfCollisionExcluded.begin(), SelfCollisionExcluded.end(), Pair) == SelfCollisionExcluded.end();
}

void CPHShell::CollideAll()
{
	if (g_B3World == nullptr || CollisionDisabled)
	{
		return;
	}

	for (CPHElement* Element : ElementsList)
	{
		for (CODEGeom* Geom : Element->Geoms)
		{
			g_B3World->CollideGeom(*Geom, Element->CurForm, CollideMask & (B3Category::Level | B3Category::Static | B3Category::Dynamic | B3Category::Character));
		}
	}
	if (!g_B3World->IsStepping())
	{
		g_B3World->RunDeferred();
	}
}

void CPHShell::CreateShellAnimator(CInifile const* ini, const char* section)
{
	SetClassBit(B3CollideClass::Animated);
	xr_delete(Animator);
	Animator = new CPhysicsShellAnimator(this, ini, section);
	if (IPhysicsShellHolder* Object = Root() ? Root()->PhysicsRefObject() : nullptr)
	{
		Object->ObjectProcessingActivate();
	}
}

void CPHShell::AnimatorOnFrame(bool calculate_bones)
{
	if (Animator)
	{
		Animator->OnFrame(calculate_bones);
	}
}

bool CPHShell::isBreakable()
{
	if (BreakingBlocked)
	{
		return false;
	}
	for (CPHJoint* Joint : JointsList)
	{
		if (Joint->isBreakable())
		{
			return true;
		}
	}
	return false;
}

bool CPHShell::isFractured()
{
	if (BreakingBlocked)
	{
		return false;
	}
	for (CPHJoint* Joint : JointsList)
	{
		if (Joint->Broken)
		{
			return true;
		}
	}
	return false;
}

void CPHShell::RemoveJoint(CPHJoint* Joint)
{
	auto It = std::find(JointsList.begin(), JointsList.end(), Joint);
	if (It != JointsList.end())
	{
		JointsList.erase(It);
	}
}

static bool IsBoneInSubtree(IKinematics& K, u16 Id, u16 Root)
{
	while (Id != BI_NONE)
	{
		if (Id == Root)
		{
			return true;
		}
		if (Id == K.LL_GetBoneRoot())
		{
			return false;
		}
		Id = K.GetBoneData(Id).GetParentID();
	}
	return false;
}

// Every broken joint cuts the subtree below its bone into a new shell. The new shell keeps the
// Box3D bodies; the game gives it a kinematics and a holder (CPHSkeleton::UnsplitSingle).
// As ODE, from the last joint to the first: children were added after their parents, so a broken
// child leaves first and a broken parent then takes the rest of its subtree.
void CPHShell::SplitProcess(PHSHELL_PAIR_VECTOR& out_shels)
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	const JOINT_STORAGE Joints = JointsList;
	for (auto Rit = Joints.rbegin(); Rit != Joints.rend(); ++Rit)
	{
		CPHJoint* Broken = *Rit;
		if (!Broken->Broken || Broken->OwnerShell != this)
		{
			continue;
		}

		const u16 SplitBone = Broken->BoneID();
		RemoveJoint(Broken);
		Broken->Deactivate();
		xr_delete(Broken);

		CPHShell* NewShell = static_cast<CPHShell*>(P_create_Shell());
		NewShell->CollideMask = CollideMask;
		NewShell->CollideClassBits = CollideClassBits;
		NewShell->RemoveCharCollOnDisable = RemoveCharCollOnDisable;
		NewShell->TracedGeoms = TracedGeoms;
		NewShell->GeomTrace = GeomTrace;
		if (IsGroupObject())
		{
			NewShell->RegisterToCLGroup(GetCLGroup());
		}
		for (auto It = ElementsList.begin(); It != ElementsList.end();)
		{
			CPHElement* Element = *It;
			if (Element != Root() && IsBoneInSubtree(*m_pKinematics, Element->m_SelfID, SplitBone))
			{
				It = ElementsList.erase(It);
				NewShell->add_Element(Element);
				continue;
			}
			++It;
		}
		for (auto It = JointsList.begin(); It != JointsList.end();)
		{
			CPHJoint* Joint = *It;
			if (Joint->PSecond_element() && static_cast<CPHElement*>(Joint->PSecond_element())->Shell == NewShell)
			{
				It = JointsList.erase(It);
				NewShell->add_Joint(Joint);
				continue;
			}
			++It;
		}
		for (auto It = BoneElements.begin(); It != BoneElements.end();)
		{
			if (IsBoneInSubtree(*m_pKinematics, It->first, SplitBone))
			{
				NewShell->BoneElements.insert(*It);
				It = BoneElements.erase(It);
				continue;
			}
			++It;
		}

		if (NewShell->ElementsList.empty())
		{
			xr_delete(NewShell);
			continue;
		}

		// ODE SplitJoint: the part keeps none of the source object's callbacks.
		NewShell->set_ObjectContactCallback(nullptr);
		NewShell->set_CallbackData(nullptr);
		NewShell->set_PhysicsRefObject(nullptr);
		NewShell->Active = true;
		NewShell->Root()->GetGlobalTransformDynamic(&NewShell->mXFORM);
		if (g_B3World)
		{
			g_B3World->AddShell(NewShell);
			NewShell->InWorld = true;
		}
		out_shels.emplace_back(NewShell, SplitBone);
	}
}

CPhysicsShellAnimator::CPhysicsShellAnimator(CPHShell* InShell, CInifile const* Ini, const char* Section) :
	Shell(InShell)
{
	VERIFY(Ini->section_exist(Section));
	IPhysicsShellHolder* Object = Shell->Root() ? Shell->Root()->PhysicsRefObject() : nullptr;
	StartXFORM.set(Object ? Object->ObjectXFORM() : Shell->mXFORM);

	bool AllBones = true;
	if (Ini->line_exist(Section, "controled_bones"))
	{
		const char* Controlled = Ini->r_string(Section, "controled_bones");
		AllBones = xr_strcmp(Controlled, "all") == 0;
		if (!AllBones && Shell->PKinematics())
		{
			const int Count = _GetItemCount(Controlled);
			for (int I = 0; I < Count; ++I)
			{
				string64 Name;
				_GetItem(Controlled, I, Name);
				if (CPhysicsElement* E = Shell->get_Element(Shell->PKinematics()->LL_BoneID(Name)))
				{
					ControlledElements.push_back(static_cast<CPHElement*>(E));
				}
			}
		}
	}
	if (AllBones)
	{
		ControlledElements = Shell->ElementsList;
	}

	for (CPHElement* Element : ControlledElements)
	{
		Element->Fix();
	}

	if (Ini->line_exist(Section, "leave_joints") && xr_strcmp(Ini->r_string(Section, "leave_joints"), "all") == 0)
	{
		return;
	}

	for (CPHJoint* Joint : Shell->JointsList)
	{
		Joint->Deactivate();
		xr_delete(Joint);
	}
	Shell->JointsList.clear();
}

CPhysicsShellAnimator::~CPhysicsShellAnimator()
{
	for (CPHElement* Element : ControlledElements)
	{
		Element->ReleaseFixed();
	}
}

void CPhysicsShellAnimator::OnFrame(bool CalculateBones)
{
	IKinematics* K = Shell->PKinematics();
	if (K == nullptr)
	{
		return;
	}

	Shell->Enable();
	if (CalculateBones)
	{
		K->CalculateBones_Invalidate();
		K->CalculateBones(true);
	}

	for (CPHElement* Element : ControlledElements)
	{
		CBoneInstance& B = K->LL_GetBoneInstance(Element->m_SelfID);
		B.set_callback(B.callback_type(), nullptr, B.callback_param(), false);

		Fmatrix Target;
		Target.mul_43(StartXFORM, B.mTransform);
		Element->SetAnimTarget(Target);
	}
}

void CPHShell::DisableCollision()
{
	CollisionDisabled = true;
	SetGeomFilter();
}

void CPHShell::EnableCollision()
{
	CollisionDisabled = false;
	SetGeomFilter();
}

void CPHShell::PureStep(float step)
{
	B3_NOT_IMPLEMENTED();
}

BoneCallbackFun* CPHShell::GetBonesCallback()
{
	return &ElementBonesCallback;
}

BoneCallbackFun* CPHShell::GetStaticObjectBonesCallback()
{
	return &EmptyBoneCallback;
}

CPhysicsElement* CPHShell::get_Element(u16 bone_id)
{
	auto It = BoneElements.find(bone_id);
	if (It != BoneElements.end())
	{
		return It->second;
	}

	for (CPHElement* Element : ElementsList)
	{
		if (Element->m_SelfID == bone_id)
		{
			return Element;
		}
	}
	return nullptr;
}

CPhysicsElement* CPHShell::get_Element(const shared_str& bone_name)
{
	return m_pKinematics ? get_Element(m_pKinematics->LL_BoneID(bone_name)) : nullptr;
}

CPhysicsElement* CPHShell::get_Element(const char* bone_name)
{
	return m_pKinematics ? get_Element(m_pKinematics->LL_BoneID(bone_name)) : nullptr;
}

CPhysicsElement* CPHShell::get_ElementByStoreOrder(u16 num)
{
	R_ASSERT2(num < ElementsList.size(), "argument is out of range");
	return ElementsList[num];
}

const CPhysicsElement* CPHShell::get_ElementByStoreOrder(u16 num) const
{
	R_ASSERT2(num < ElementsList.size(), "argument is out of range");
	return ElementsList[num];
}

CPhysicsElement* CPHShell::get_PhysicsParrentElement(u16 bone_id)
{
	CPhysicsElement* Element = get_Element(bone_id);
	return Element ? Element : Root();
}

CPHSynchronize* CPHShell::get_ElementSync(u16 element)
{
	return ElementsList[element];
}

CPhysicsJoint* CPHShell::get_Joint(u16 bone_id)
{
	for (CPHJoint* Joint : JointsList)
	{
		if (Joint->BoneID() == bone_id)
		{
			return Joint;
		}
	}
	return nullptr;
}

CPhysicsJoint* CPHShell::get_Joint(const shared_str& bone_name)
{
	return m_pKinematics ? get_Joint(m_pKinematics->LL_BoneID(bone_name)) : nullptr;
}

CPhysicsJoint* CPHShell::get_Joint(const char* bone_name)
{
	return m_pKinematics ? get_Joint(m_pKinematics->LL_BoneID(bone_name)) : nullptr;
}

CPhysicsJoint* CPHShell::get_JointByStoreOrder(u16 num)
{
	return JointsList[num];
}

CODEGeom* CPHShell::get_GeomByID(u16 bone_id)
{
	for (CPHElement* Element : ElementsList)
	{
		for (CODEGeom* Geom : Element->Geoms)
		{
			if (Geom->BoneId == bone_id)
			{
				return Geom;
			}
		}
	}
	return nullptr;
}

CPhysicsElement* CPHShell::NearestToPoint(const Fvector& point, NearestToPointCallback* cb)
{
	CPhysicsElement* Nearest = nullptr;
	float NearestDistance = flt_max;
	for (CPHElement* Element : ElementsList)
	{
		if (cb && !(*cb)(Element))
		{
			continue;
		}

		const float Distance = Element->mass_Center().distance_to_sqr(point);
		if (Distance < NearestDistance)
		{
			NearestDistance = Distance;
			Nearest = Element;
		}
	}
	return Nearest;
}

static CPhysicsElement* ElementOfBone(CBoneInstance* B)
{
	return static_cast<CPhysicsElement*>(B->callback_param());
}

static void _BCL ElementBonesCallback(CBoneInstance* B)
{
	static_cast<CPHElement*>(ElementOfBone(B))->BonesCallBack(B);
}

static bool NoPhysicsShape(const SBoneShape& Shape)
{
	return Shape.type == SBoneShape::stNone || Shape.flags.test(SBoneShape::sfNoPhysics);
}

// Same layout as the ODE backend: rigid bones join the parent element, other bones get
// their own element and a joint built from the bone IK data.
void CPHShell::AddElementRecursive(CPhysicsElement* RootElement, u16 Id, Fmatrix GlobalParent, BONE_P_MAP* GettingMap)
{
	const IBoneData& BoneData = m_pKinematics->GetBoneData(Id);
	const SJointIKData& JointData = BoneData.get_IK_data();

	Fmatrix Position = BoneData.get_bind_transform();
	Position.mulA_43(GlobalParent);

	if (!m_pKinematics->LL_GetBonesVisible().is(Id))
	{
		for (u16 I = 0; I < BoneData.GetNumChildren(); ++I)
		{
			AddElementRecursive(RootElement, BoneData.GetChild(I).GetSelfID(), Position, GettingMap);
		}
		return;
	}

	CPhysicsElement* E = RootElement;
	CPhysicsJoint* J = nullptr;
	const bool HasShape = !NoPhysicsShape(BoneData.get_shape());
	const bool Breakable = JointData.ik_flags.test(SJointIKData::flBreakable);
	if (HasShape || !RootElement)
	{
		// A rigid breakable bone (ODE CPHFracture) gets its own element, welded to the parent.
		if (JointData.type == jtRigid && RootElement && !Breakable)
		{
			Fmatrix VsRoot;
			VsRoot.invert(RootElement->mXFORM);
			VsRoot.mulB_43(Position);
			RootElement->add_Shape(BoneData.get_shape(), VsRoot);
			RootElement->add_Mass(BoneData.get_shape(), VsRoot, BoneData.get_center_of_mass(), BoneData.get_mass());
		}
		else
		{
			E = P_create_Element();
			E->m_SelfID = Id;
			E->mXFORM.set(Position);
			E->SetMaterial(BoneData.get_game_mtl_idx());
			E->set_ParentElement(RootElement);
			if (HasShape)
			{
				E->add_Shape(BoneData.get_shape());
				E->setMassMC(BoneData.get_mass(), BoneData.get_center_of_mass());
			}
			add_Element(E);

			if (RootElement)
			{
				const bool Fracture = Breakable && JointData.type == jtRigid;
				J = Fracture ? P_create_Joint(CPhysicsJoint::ball, RootElement, E) : BuildJoint(BoneData, RootElement, E);
				if (J)
				{
					J->SetBoneID(Id);
					add_Joint(J);
					if (Fracture)
					{
						J->SetAnchorVsSecondElement(0.f, 0.f, 0.f);
						static_cast<CPHJoint*>(J)->SetFracture(JointData.break_force, JointData.break_torque);
					}
					else if (Breakable)
					{
						J->SetBreakable(JointData.break_force, JointData.break_torque);
					}
				}
			}
		}
	}

	if (HasShape && E->last_geom())
	{
		E->last_geom()->BoneId = Id;
	}
	BoneElements[Id] = static_cast<CPHElement*>(E);

	if (GettingMap)
	{
		auto It = GettingMap->find(Id);
		if (It != GettingMap->end())
		{
			It->second.joint = J;
			It->second.element = E;
		}
	}

	for (u16 I = 0; I < BoneData.GetNumChildren(); ++I)
	{
		AddElementRecursive(E, BoneData.GetChild(I).GetSelfID(), Position, GettingMap);
	}
}

void CPHShell::build_FromKinematics(IKinematics* K, BONE_P_MAP* p_geting_map)
{
	VERIFY(K);
	phys_shell_verify_model(*K);
	m_pKinematics = K;
	AddElementRecursive(nullptr, K->LL_GetBoneRoot(), Fidentity, p_geting_map);
}

void CPHShell::preBuild_FromKinematics(IKinematics* K, BONE_P_MAP* p_geting_map)
{
	build_FromKinematics(K, p_geting_map);
	m_pKinematics = nullptr;
}

void CPHShell::SetCallbacks()
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	for (CPHElement* Element : ElementsList)
	{
		if (Element->m_SelfID != BI_NONE)
		{
			m_pKinematics->LL_GetBoneInstance(Element->m_SelfID).set_callback(bctPhysics, ElementBonesCallback, static_cast<CPhysicsElement*>(Element));
		}
	}

	// Bones without an element follow the element of their nearest physics parent.
	const u16 Count = m_pKinematics->LL_BoneCount();
	for (u16 Id = 0; Id < Count; ++Id)
	{
		CBoneInstance& Instance = m_pKinematics->LL_GetBoneInstance(Id);
		if (Instance.callback() && Instance.callback_type() == bctPhysics)
		{
			continue;
		}

		auto It = BoneElements.find(Id);
		if (It != BoneElements.end() && m_pKinematics->LL_GetBoneVisible(Id))
		{
			Instance.set_callback(bctPhysics, nullptr, static_cast<CPhysicsElement*>(It->second));
		}
	}
}

void CPHShell::ZeroCallbacks()
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	const u16 Count = m_pKinematics->LL_BoneCount();
	for (u16 Id = 0; Id < Count; ++Id)
	{
		CBoneInstance& Instance = m_pKinematics->LL_GetBoneInstance(Id);
		if (Instance.callback_type() == bctPhysics)
		{
			Instance.reset_callback();
		}
	}
}

void CPHShell::ResetCallbacks(u16 id, VisMask& mask)
{
	ZeroCallbacks();
	SetCallbacks();
}

void CPHShell::SetBonesCallbacksOverwrite(bool v)
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	for (CPHElement* Element : ElementsList)
	{
		if (Element->m_SelfID != BI_NONE)
		{
			m_pKinematics->LL_GetBoneInstance(Element->m_SelfID).set_callback_overwrite(v);
		}
	}
}

void CPHShell::ToAnimBonesPositions(motion_history_state history_state)
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	for (CPHElement* Element : ElementsList)
	{
		if (Element->m_SelfID != BI_NONE)
		{
			Element->ToBonePos(m_pKinematics->LL_GetTransform(Element->m_SelfID));
		}
	}
}

bool shape_is_physic(const SBoneShape& shape)
{
	return shape.type != SBoneShape::stNone && !shape.flags.test(SBoneShape::sfNoPhysics);
}

CPHJoint::CPHJoint(enumType Type, CPhysicsElement* InFirst, CPhysicsElement* InSecond) :
	First(InFirst),
	Second(InSecond)
{
	eType = Type;
	bActive = false;
	if (Type == hinge2 && InSecond)
	{
		static_cast<CPHElement*>(InSecond)->AllowFastRotation = true;
	}
}

CPHJoint::~CPHJoint()
{
	DestroyNative();
	if (BackRef)
	{
		*BackRef = nullptr;
	}
}

u16 CPHJoint::GetAxesNumber()
{
	switch (eType)
	{
	case ball:
		return 0;
	case hinge:
	case slider:
		return 1;
	case hinge2:
		return 2;
	case full_control:
		return 3;
	}
	return 0;
}

void CPHJoint::SetAnchor(const Fvector& position)
{
	Anchor.set(position);
	AnchorVs = vs_global;
}

void CPHJoint::SetAnchorVsFirstElement(const Fvector& position)
{
	Anchor.set(position);
	AnchorVs = vs_first;
}

void CPHJoint::SetAnchorVsSecondElement(const Fvector& position)
{
	Anchor.set(position);
	AnchorVs = vs_second;
}

void CPHJoint::SetAnchor(const float x, const float y, const float z)
{
	SetAnchor(Fvector().set(x, y, z));
}

void CPHJoint::SetAnchorVsFirstElement(const float x, const float y, const float z)
{
	SetAnchorVsFirstElement(Fvector().set(x, y, z));
}

void CPHJoint::SetAnchorVsSecondElement(const float x, const float y, const float z)
{
	SetAnchorVsSecondElement(Fvector().set(x, y, z));
}

static int ClampAxis(int AxisNum)
{
	return clampr(AxisNum, 0, 2);
}

// ODE re-aims a live joint (SetAxisDirDynamic); Box3D rebuilds the frames from the current poses.
void CPHJoint::SetAxisDir(const Fvector& orientation, const int axis_num)
{
	Axes[ClampAxis(axis_num)].Direction.set(orientation);
	Axes[ClampAxis(axis_num)].Vs = vs_global;
	if (b3Joint_IsValid(Joint))
	{
		DestroyNative();
		CreateNative();
	}
}

void CPHJoint::SetAxisDirVsFirstElement(const Fvector& orientation, const int axis_num)
{
	Axes[ClampAxis(axis_num)].Direction.set(orientation);
	Axes[ClampAxis(axis_num)].Vs = vs_first;
}

void CPHJoint::SetAxisDirVsSecondElement(const Fvector& orientation, const int axis_num)
{
	Axes[ClampAxis(axis_num)].Direction.set(orientation);
	Axes[ClampAxis(axis_num)].Vs = vs_second;
}

void CPHJoint::SetAxisDir(const float x, const float y, const float z, const int axis_num)
{
	SetAxisDir(Fvector().set(x, y, z), axis_num);
}

void CPHJoint::SetAxisDirVsFirstElement(const float x, const float y, const float z, const int axis_num)
{
	SetAxisDirVsFirstElement(Fvector().set(x, y, z), axis_num);
}

void CPHJoint::SetAxisDirVsSecondElement(const float x, const float y, const float z, const int axis_num)
{
	SetAxisDirVsSecondElement(Fvector().set(x, y, z), axis_num);
}

void CPHJoint::SetLimits(const float low, const float high, const int axis_num)
{
	Axes[ClampAxis(axis_num)].Low = low;
	Axes[ClampAxis(axis_num)].High = high;
	UpdateLimits();
}

void CPHJoint::SetLimitsVsFirstElement(const float low, const float high, const int axis_num)
{
	SetLimits(low, high, axis_num);
}

void CPHJoint::SetLimitsVsSecondElement(const float low, const float high, const int axis_num)
{
	SetLimits(low, high, axis_num);
}

void CPHJoint::SetHiLimitDynamic(int axis_num, float limit)
{
	Axes[ClampAxis(axis_num)].High = limit;
	UpdateLimits();
}

void CPHJoint::SetLoLimitDynamic(int axis_num, float limit)
{
	Axes[ClampAxis(axis_num)].Low = limit;
	UpdateLimits();
}

void CPHJoint::SetAxisSDfactors(float spring_factor, float damping_factor, int axis_num)
{
	Axes[ClampAxis(axis_num)].Spring = spring_factor;
	Axes[ClampAxis(axis_num)].Damping = damping_factor;
}

void CPHJoint::SetJointSDfactors(float spring_factor, float damping_factor)
{
	Spring = spring_factor;
	Damping = damping_factor;
	// ODE puts them only on the motor CFM (Box3D motors are hard) and the hinge2 suspension, never
	// on the joint constraint itself.
	UpdateMotor();
}

void CPHJoint::GetAxisSDfactors(float& spring_factor, float& damping_factor, int axis_num)
{
	spring_factor = Axes[ClampAxis(axis_num)].Spring;
	damping_factor = Axes[ClampAxis(axis_num)].Damping;
}

void CPHJoint::GetJointSDfactors(float& spring_factor, float& damping_factor)
{
	spring_factor = Spring;
	damping_factor = Damping;
}

void CPHJoint::SetForceAndVelocity(const float force, const float velocity, const int axis_num)
{
	SetForce(force, axis_num);
	SetVelocity(velocity, axis_num);
}

void CPHJoint::SetForce(const float force, const int axis_num)
{
	if (axis_num < 0)
	{
		for (SAxis& Axis : Axes)
		{
			Axis.Force = force;
		}
		UpdateMotor();
		return;
	}
	Axes[ClampAxis(axis_num)].Force = force;
	UpdateMotor();
}

void CPHJoint::SetVelocity(const float velocity, const int axis_num)
{
	if (axis_num < 0)
	{
		for (SAxis& Axis : Axes)
		{
			Axis.Velocity = velocity;
		}
		UpdateMotor();
		return;
	}
	Axes[ClampAxis(axis_num)].Velocity = velocity;
	UpdateMotor();
}

void CPHJoint::GetMaxForceAndVelocity(float& force, float& velocity, int axis_num)
{
	force = Axes[ClampAxis(axis_num)].Force;
	velocity = Axes[ClampAxis(axis_num)].Velocity;
}

void CPHJoint::GetLimits(float& lo_limit, float& hi_limit, int axis_num)
{
	lo_limit = Axes[ClampAxis(axis_num)].Low;
	hi_limit = Axes[ClampAxis(axis_num)].High;
	if (FirstIsReversed())
	{
		lo_limit = -Axes[ClampAxis(axis_num)].High;
		hi_limit = -Axes[ClampAxis(axis_num)].Low;
	}
}

void CPHJoint::GetAxisDir(int num, Fvector& axis, eVs& vs)
{
	axis.set(Axes[ClampAxis(num)].Direction);
	vs = Axes[ClampAxis(num)].Vs;
}
