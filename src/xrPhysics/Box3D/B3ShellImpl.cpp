#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "IPhysicsShellHolder.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/bone.h"
#include "PHShellBuildJoint.h"

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
	CollideClassBits.assign(0);
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
}

void CPHShell::PhDataUpdate(float Step)
{
	for (CPHElement* Element : ElementsList)
	{
		Element->CutVelocity();
		Element->PullState(true);
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
	SetCallbacks();
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

void CPHShell::Disable()
{
	for (CPHElement* Element : ElementsList)
	{
		if (b3Body_IsValid(Element->Body))
		{
			B3_SAFE(b3Body_SetAwake(Element->Body, false));
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

void CPHShell::MassAddBox(float mass, const Fvector& full_size)
{
	if (CPHElement* RootElement = Root())
	{
		RootElement->setMass(RootElement->getMass() + mass);
	}
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
	CPhysicsElement* Element = id == BI_NONE ? Root() : get_Element(id);
	if (Element == nullptr)
	{
		Element = Root();
	}
	if (Element)
	{
		Element->applyImpulseTrace(pos, dir, val, id);
	}
}

void CPHShell::applyHit(const Fvector& pos, const Fvector& dir, float val, const u16 id, ALife::EHitType hit_type)
{
	applyImpulseTrace(pos, dir, val, id);
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
	for (CPHElement* Element : ElementsList)
	{
		Element->applyGravityAccel(accel);
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

void CPHShell::SetIgnoreDynamic()
{
	CollideMask &= ~(B3Category::Dynamic | B3Category::Static);
	SetGeomFilter();
}

void CPHShell::SetRagDoll()
{
}

void CPHShell::SetIgnoreRagDoll()
{
}

void CPHShell::SetIgnoreAnimated()
{
}

void CPHShell::SetSmall()
{
}

void CPHShell::SetIgnoreSmall()
{
}

void CPHShell::CreateShellAnimator(CInifile const* ini, const char* section)
{
	xr_delete(Animator);
	Animator = new CPhysicsShellAnimator(this, ini, section);
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
void CPHShell::SplitProcess(PHSHELL_PAIR_VECTOR& out_shels)
{
	if (m_pKinematics == nullptr)
	{
		return;
	}

	const JOINT_STORAGE Joints = JointsList;
	for (CPHJoint* Broken : Joints)
	{
		if (!Broken->Broken)
		{
			continue;
		}

		const u16 SplitBone = Broken->BoneID();
		RemoveJoint(Broken);
		Broken->Deactivate();
		xr_delete(Broken);

		CPHShell* NewShell = static_cast<CPHShell*>(P_create_Shell());
		NewShell->CollideMask = CollideMask;
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
	if (HasShape || !RootElement)
	{
		if (JointData.type == jtRigid && RootElement)
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
				J = BuildJoint(BoneData, RootElement, E);
				if (J)
				{
					J->SetBoneID(Id);
					add_Joint(J);
					if (JointData.ik_flags.test(SJointIKData::flBreakable))
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

void CPHJoint::SetAxisDir(const Fvector& orientation, const int axis_num)
{
	Axes[ClampAxis(axis_num)].Direction.set(orientation);
	Axes[ClampAxis(axis_num)].Vs = vs_global;
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
