#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "matrix_utils.h"
#include "IPhysicsShellHolder.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/bone.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrServerEntities/PHNetState.h"

static u16 DefaultElementMaterial()
{
	static u16 Material = u16(-1);
	if (Material == u16(-1))
	{
		auto It = GMLibrary().GetMaterialIt("objects\\small_box");
		Material = It == GMLibrary().GetMaterials().end() ? 0 : u16(It - GMLibrary().FirstMaterial());
	}
	return Material;
}

CPHElement::CPHElement()
{
	mXFORM.identity();
	m_SelfID = 0;
	Owner.Material = DefaultElementMaterial();
	Owner.ContactCallback = g_B3World ? g_B3World->DefaultContactShotMark() : nullptr;
	DisableParams.Reset();
}

CPHElement::~CPHElement()
{
	DestroyBody();
	for (CODEGeom* Geom : Geoms)
	{
		xr_delete(Geom);
	}
	Geoms.clear();
}

void CPHElement::CreateBody(const Fmatrix& Form, bool Disable)
{
	DestroyBody();
	if (g_B3World == nullptr)
	{
		return;
	}

	mXFORM.set(Form);
	PrevForm.set(Form);
	CurForm.set(Form);

	const b3Transform Transform = B3Math::ToB3(Form);
	b3BodyDef Def = b3DefaultBodyDef();
	Def.type = Fixed ? b3_kinematicBody : b3_dynamicBody;
	Def.position = Transform.p;
	Def.rotation = Transform.q;
	Def.gravityScale = ApplyGravity ? 1.f : 0.f;
	Def.isAwake = !Disable;
	Def.userData = this;
	Body = b3CreateBody(g_B3World->Id(), &Def);

	const u64 Mask = Shell ? (Shell->CollisionDisabled ? 0ull : Shell->CollideMask) : B3Category::All;
	const float Volume = getVolume();
	const float Density = Volume > EPS_S ? Mass / Volume : 1.f;
	for (CODEGeom* Geom : Geoms)
	{
		Geom->Owner = &Owner;
		Geom->OwnerForm = &mXFORM;
		Geom->CreateShape(Body, Density, true, Mask);
	}

	if (!Geoms.empty())
	{
		b3MassData MassData = b3Body_GetMassData(Body);
		if (MassData.mass > EPS_S)
		{
			const float Scale = Mass / MassData.mass;
			MassData.mass = Mass;
			MassData.inertia = b3MulSM(Scale, MassData.inertia);
			B3_SAFE(b3Body_SetMassData(Body, MassData));
		}
		// The center of mass comes from the shapes; trace impulses are relative to it, as in ODE.
		MassCenterLocal = B3Math::FromB3(MassData.center);
	}
}

void CPHElement::DestroyBody()
{
	for (CODEGeom* Geom : Geoms)
	{
		Geom->Shape = b3_nullShapeId;
	}
	if (b3Body_IsValid(Body))
	{
		b3DestroyBody(Body);
	}
	Body = b3_nullBodyId;
}

bool CPHElement::IsAwake() const
{
	return b3Body_IsValid(Body) && b3Body_IsAwake(Body);
}

void CPHElement::PullState(bool StorePrev)
{
	if (!b3Body_IsValid(Body))
	{
		return;
	}

	if (StorePrev)
	{
		PrevForm.set(CurForm);
	}
	CurForm = B3Math::FromB3(b3Body_GetTransform(Body));
	VERIFY(_valid(CurForm));
}

void CPHElement::ToBonePos(const Fmatrix& BoneTransform)
{
	VERIFY(Shell);
	Fmatrix Global;
	Global.mul_43(Shell->mXFORM, BoneTransform);
	VERIFY(_valid(Global));

	xrCriticalSectionGuard Guard(PendingLock);
	PendingForm.set(Global);
	HasPendingForm = true;
	mXFORM.set(Global);
	PrevForm.set(Global);
	CurForm.set(Global);
}

// As CPHElement::AnimToVel in ODE: velocities that bring the element to the animated bone pose in dt.
bool CPHElement::AnimToVel(float dt, float l_limit, float a_limit)
{
	IKinematics* K = Shell ? Shell->PKinematics() : nullptr;
	IPhysicsShellHolder* Holder = PhysicsRefObject();
	if (K == nullptr || Holder == nullptr || m_SelfID == BI_NONE || !b3Body_IsValid(Body))
	{
		return true;
	}

	Fmatrix BonePose;
	BonePose.mul_43(Holder->ObjectXFORM(), K->LL_GetTransform(m_SelfID));
	Fmatrix Current;
	GetGlobalTransformDynamic(&Current);
	Fmatrix Diff;
	Diff.mul_43(Fmatrix().invert(Current), BonePose);
	dt = std::max(dt, EPS_S);

	Fvector Linear, Angular;
	linear_diff(Linear, BonePose.c, Current.c, dt);
	angular_diff(Angular, Diff, dt);
	const bool Result = Angular.square_magnitude() < a_limit * a_limit && Linear.square_magnitude() < l_limit * l_limit;
	if (Linear.square_magnitude() > l_limit * l_limit)
	{
		Linear.normalize().mul(l_limit);
	}
	if (Angular.square_magnitude() > a_limit * a_limit)
	{
		Angular.normalize().mul(a_limit);
	}
	set_LinearVel(Linear);
	set_AngularVel(Angular);
	return Result;
}

// As ODE BodyCutForce: one impulse changes linear velocity by at most l_limit and angular velocity
// by at most w_limit. A grenade hit on a bone or the rocket engine lever would otherwise spin the
// body so fast that it tumbles inside the step or tunnels through the level mesh.
void CPHElement::ApplyLimitedImpulse(const Fvector& Impulse, const Fvector* Point)
{
	const b3BodyId Id = Body;
	const float LLimit = LinearLimit;
	const float WLimit = AngularLimit;
	const b3Vec3 J = B3Math::ToB3(Impulse);
	const bool AtPoint = Point != nullptr;
	const b3Pos P = AtPoint ? B3Math::ToB3(*Point) : b3Pos{};
	B3_SAFE(
		if (!b3Body_IsValid(Id)) { return; }
		const b3Vec3 Linear0 = b3Body_GetLinearVelocity(Id);
		const b3Vec3 Angular0 = b3Body_GetAngularVelocity(Id);
		if (AtPoint) { b3Body_ApplyLinearImpulse(Id, J, P, true); }
		else { b3Body_ApplyLinearImpulseToCenter(Id, J, true); }
		b3Vec3 DL = b3Sub(b3Body_GetLinearVelocity(Id), Linear0);
		b3Vec3 DW = b3Sub(b3Body_GetAngularVelocity(Id), Angular0);
		const float DLM = b3Length(DL);
		const float DWM = b3Length(DW);
		if (DLM > LLimit) { b3Body_SetLinearVelocity(Id, b3MulAdd(Linear0, LLimit / DLM, DL)); }
		if (DWM > WLimit) { b3Body_SetAngularVelocity(Id, b3MulAdd(Angular0, WLimit / DWM, DW)); }
	);
}

// As ODE PhDataUpdate: velocities over limit * scale are cut to the limit. Without it a stiff
// stabilizing torque (rocket engine trace impulse) makes the body spin up and tumble.
void CPHElement::CutVelocity()
{
	if (!Active || Fixed || !b3Body_IsValid(Body))
	{
		return;
	}

	Fvector Linear, Angular;
	get_LinearVel(Linear);
	get_AngularVel(Angular);
	if (Linear.magnitude() > LinearLimit * LinearScale)
	{
		Linear.normalize().mul(LinearLimit);
		set_LinearVel(Linear);
	}
	if (Angular.magnitude() > AngularLimit * AngularScale)
	{
		Angular.normalize().mul(AngularLimit);
		set_AngularVel(Angular);
	}
}

void CPHElement::ApplyPendingForm()
{
	xrCriticalSectionGuard Guard(PendingLock);
	if (!HasPendingForm)
	{
		return;
	}

	HasPendingForm = false;
	if (b3Body_IsValid(Body))
	{
		const b3Transform Transform = B3Math::ToB3(PendingForm);
		B3_SAFE(b3Body_SetTransform(Body, Transform.p, Transform.q));
		B3_SAFE(b3Body_SetAwake(Body, true));
	}
}

void CPHElement::SetAnimTarget(const Fmatrix& Form)
{
	xrCriticalSectionGuard Guard(PendingLock);
	AnimTarget.set(Form);
	HasAnimTarget = true;
}

void CPHElement::BonesCallBack(CBoneInstance* B)
{
	VERIFY(Shell);
	if (Activating)
	{
		Activating = false;
		ToBonePos(B->mTransform);
		if (Shell->Root() == this)
		{
			Shell->ObjectInRootForm.invert(B->mTransform);
		}
		B->set_callback_overwrite(true);
		return;
	}

	Fmatrix Parent;
	Parent.invert(Shell->mXFORM);
	B->mTransform.mul_43(Parent, mXFORM);
	VERIFY2(_valid(B->mTransform), "Bones callback returns bad matrix");
}

void CPHElement::PhTune(float Step)
{
	ApplyPendingForm();
	{
		xrCriticalSectionGuard Guard(PendingLock);
		if (HasAnimTarget && b3Body_IsValid(Body))
		{
			HasAnimTarget = false;
			B3_SAFE(b3Body_SetTargetTransform(Body, B3Math::ToB3(AnimTarget), Step, true));
		}
	}
	if (!b3Body_IsValid(Body) || Fixed || !b3Body_IsAwake(Body))
	{
		return;
	}

	// Square air resistance as in ODE.
	const b3Vec3 Linear = b3Body_GetLinearVelocity(Body);
	const float Speed = b3Length(Linear);
	if (Speed > EPS_L && AirLinear > 0.f)
	{
		B3_SAFE(b3Body_ApplyForceToCenter(Body, b3MulSV(-AirLinear * Speed, Linear), false));
	}
	const b3Vec3 Angular = b3Body_GetAngularVelocity(Body);
	if (AirAngular > 0.f)
	{
		const float Damp = std::min(AirAngular * Step, 0.5f);
		B3_SAFE(b3Body_SetAngularVelocity(Body, b3MulSV(1.f - Damp, Angular)));
	}
}

void CPHElement::Activate(const Fmatrix& transform, const Fvector& lin_vel, const Fvector& ang_vel, bool disable)
{
	if (Active)
	{
		return;
	}

	CreateBody(transform, disable);
	set_LinearVel(lin_vel);
	set_AngularVel(ang_vel);
	Active = true;
	Activating = Shell && Shell->PKinematics() && m_SelfID != BI_NONE;
}

void CPHElement::Activate(const Fmatrix& m0, float dt01, const Fmatrix& m2, bool disable)
{
	Fvector LinearVel;
	LinearVel.sub(m2.c, m0.c);
	Activate(m0, LinearVel, Fvector().set(0.f, 0.f, 0.f), disable);
}

void CPHElement::Activate(bool disable, bool not_set_bone_callbacks)
{
	const Fvector Zero = {0.f, 0.f, 0.f};
	Activate(mXFORM, Zero, Zero, disable);
}

void CPHElement::Activate(const Fmatrix& form, bool disable)
{
	Fmatrix Global;
	Global.mul_43(form, mXFORM);
	const Fvector Zero = {0.f, 0.f, 0.f};
	Activate(Global, Zero, Zero, disable);
}

void CPHElement::Deactivate()
{
	if (!Active)
	{
		return;
	}

	PullState(false);
	mXFORM.set(CurForm);
	DestroyBody();
	Active = false;
}

void CPHElement::InterpolateGlobalTransform(Fmatrix* m)
{
	if (!Active)
	{
		m->set(mXFORM);
		return;
	}

	const float Factor = g_B3World ? g_B3World->InterpolationFactor() : 1.f;
	Fquaternion Q0, Q1, Q;
	Q0.set(PrevForm);
	Q1.set(CurForm);
	Q.slerp(Q0, Q1, Factor);
	Fvector Position;
	Position.lerp(PrevForm.c, CurForm.c, Factor);
	m->mk_xform(Q, Position);
	VERIFY(_valid(*m));
}

void CPHElement::InterpolateGlobalPosition(Fvector* v)
{
	const float Factor = g_B3World ? g_B3World->InterpolationFactor() : 1.f;
	v->lerp(PrevForm.c, CurForm.c, Factor);
}

void CPHElement::GetGlobalTransformDynamic(Fmatrix* m) const
{
	m->set(Active ? CurForm : mXFORM);
}

void CPHElement::GetGlobalPositionDynamic(Fvector* v)
{
	v->set(Active ? CurForm.c : mXFORM.c);
}

bool CPHElement::isEnabled() const
{
	return IsAwake();
}

void CPHElement::Enable()
{
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetAwake(Body, true));
	}
}

void CPHElement::setMass(float M)
{
	Mass = std::max(M, EPS_S);
	if (b3Body_IsValid(Body))
	{
		b3MassData MassData = b3Body_GetMassData(Body);
		if (MassData.mass > EPS_S)
		{
			MassData.inertia = b3MulSM(Mass / MassData.mass, MassData.inertia);
			MassData.mass = Mass;
			B3_SAFE(b3Body_SetMassData(Body, MassData));
		}
	}
}

void CPHElement::setDensity(float M)
{
	setMass(M * getVolume());
}

float CPHElement::getDensity()
{
	const float Volume = getVolume();
	return Volume > EPS_S ? Mass / Volume : 0.f;
}

float CPHElement::getVolume()
{
	float Volume = 0.f;
	for (CODEGeom* Geom : Geoms)
	{
		Volume += Geom->Volume();
	}
	return Volume;
}

void CPHElement::get_Extensions(const Fvector& axis, float center_prg, float& lo_ext, float& hi_ext) const
{
	lo_ext = flt_max;
	hi_ext = -flt_max;
	for (CODEGeom* Geom : Geoms)
	{
		float Lo, Hi;
		Geom->GetExtensions(axis, center_prg, Lo, Hi);
		lo_ext = std::min(lo_ext, Lo);
		hi_ext = std::max(hi_ext, Hi);
	}
}

void CPHElement::applyForce(const Fvector& dir, float val)
{
	applyForce(dir.x * val, dir.y * val, dir.z * val);
}

void CPHElement::applyForce(float x, float y, float z)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(b3Body_ApplyForceToCenter(Body, B3Math::ToB3(Fvector().set(x, y, z)), true));
	}
}

void CPHElement::applyImpulse(const Fvector& dir, float val)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		ApplyLimitedImpulse(Fvector().mul(dir, val), nullptr);
	}
}

void CPHElement::setTorque(const Fvector& torque)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(b3Body_ApplyTorque(Body, B3Math::AngularToB3(torque), true));
	}
}

void CPHElement::setForce(const Fvector& force)
{
	applyForce(force.x, force.y, force.z);
}

void CPHElement::applyGravityAccel(const Fvector& accel)
{
	applyForce(accel.x * Mass, accel.y * Mass, accel.z * Mass);
}

void CPHElement::SetAirResistance(float linear, float angular)
{
	AirLinear = linear;
	AirAngular = angular;
}

void CPHElement::GetAirResistance(float& linear, float& angular)
{
	linear = AirLinear;
	angular = AirAngular;
}

void CPHElement::set_ObjectContactCallback(ObjectContactCallbackFun* callback)
{
	Owner.ObjectCallbacks.clear();
	Owner.AddObjectCallback(callback);
}

void CPHElement::set_LinearVel(const Fvector& velocity)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(b3Body_SetLinearVelocity(Body, B3Math::ToB3(velocity)));
	}
}

void CPHElement::set_AngularVel(const Fvector& velocity)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(b3Body_SetAngularVelocity(Body, B3Math::AngularToB3(velocity)));
	}
}

void CPHElement::get_LinearVel(Fvector& velocity) const
{
	if (b3Body_IsValid(Body))
	{
		velocity = B3Math::FromB3(b3Body_GetLinearVelocity(Body));
	}
	else
	{
		velocity.set(0.f, 0.f, 0.f);
	}
}

void CPHElement::get_AngularVel(Fvector& velocity) const
{
	if (b3Body_IsValid(Body))
	{
		velocity = B3Math::AngularFromB3(b3Body_GetAngularVelocity(Body));
	}
	else
	{
		velocity.set(0.f, 0.f, 0.f);
	}
}

void CPHElement::GetPointVel(Fvector& res_vel, const Fvector& point) const
{
	if (b3Body_IsValid(Body))
	{
		res_vel = B3Math::FromB3(b3Body_GetWorldPointVelocity(Body, B3Math::ToB3(point)));
	}
	else
	{
		res_vel.set(0.f, 0.f, 0.f);
	}
}

void CPHElement::TransformPosition(const Fmatrix& form, motion_history_state history_state)
{
	Fmatrix Current;
	GetGlobalTransformDynamic(&Current);
	Fmatrix Result;
	Result.mul_43(form, Current);
	SetTransform(Result, history_state);
}

void CPHElement::SetTransform(const Fmatrix& m0, motion_history_state history_state)
{
	mXFORM.set(m0);
	if (!b3Body_IsValid(Body))
	{
		return;
	}

	const b3Transform Transform = B3Math::ToB3(m0);
	B3_SAFE(b3Body_SetTransform(Body, Transform.p, Transform.q));
	CurForm.set(m0);
	if (history_state != mh_not_clear)
	{
		PrevForm.set(m0);
	}
}

void CPHElement::setQuaternion(const Fquaternion& quaternion)
{
	Fmatrix Form;
	Form.mk_xform(quaternion, (Active ? CurForm : mXFORM).c);
	SetTransform(Form, mh_unspecified);
}

void CPHElement::set_ApplyByGravity(bool flag)
{
	ApplyGravity = flag;
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetGravityScale(Body, flag ? 1.f : 0.f));
	}
}

void CPHElement::SetMaterial(u16 m)
{
	Owner.Material = m;
	SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(m);
	if (Mtl == nullptr)
	{
		return;
	}

	for (CODEGeom* Geom : Geoms)
	{
		if (b3Shape_IsValid(Geom->Shape))
		{
			b3SurfaceMaterial Material = b3Shape_GetSurfaceMaterial(Geom->Shape);
			Material.friction = Mtl->fPHFriction;
			Material.restitution = Mtl->Flags.test(SGameMtl::flBounceable) ? Mtl->fPHBouncing : 0.f;
			Material.userMaterialId = m;
			B3_SAFE(b3Shape_SetSurfaceMaterial(Geom->Shape, Material));
		}
	}
}

void CPHElement::SetMaterial(const char* m)
{
	SetMaterial(GMLibrary().GetMaterialIdx(m));
}

CPhysicsShell* CPHElement::PhysicsShell()
{
	return Shell;
}

void CPHElement::add_geom(CODEGeom* g)
{
	g->Owner = &Owner;
	g->OwnerForm = &mXFORM;
	Geoms.push_back(g);
	if (b3Body_IsValid(Body))
	{
		const u64 Mask = Shell ? Shell->CollideMask : B3Category::All;
		g->CreateShape(Body, getDensity(), true, Mask);
	}
}

void CPHElement::remove_geom(CODEGeom* g)
{
	auto It = std::find(Geoms.begin(), Geoms.end(), g);
	if (It != Geoms.end())
	{
		g->DestroyShape();
		Geoms.erase(It);
	}
}

void CPHElement::add_Sphere(const Fsphere& V)
{
	add_geom(CODEGeom::MakeSphere(V));
}

void CPHElement::add_Box(const Fobb& V)
{
	add_geom(CODEGeom::MakeBox(V));
}

void CPHElement::add_Cylinder(const Fcylinder& V)
{
	add_geom(CODEGeom::MakeCylinder(V));
}

void CPHElement::add_Shape(const SBoneShape& shape)
{
	add_Shape(shape, Fidentity);
}

void CPHElement::add_Shape(const SBoneShape& shape, const Fmatrix& offset)
{
	CODEGeom* Geom = nullptr;
	switch (shape.type)
	{
	case SBoneShape::stBox:
		Geom = CODEGeom::MakeBox(shape.box);
		break;
	case SBoneShape::stSphere:
		Geom = CODEGeom::MakeSphere(shape.sphere);
		break;
	case SBoneShape::stCylinder:
		Geom = CODEGeom::MakeCylinder(shape.cylinder);
		break;
	default:
		return;
	}

	Fmatrix Local = Geom->LocalForm;
	Geom->LocalForm.mul_43(offset, Local);
	add_geom(Geom);
}

void CPHElement::add_Mass(const SBoneShape& shape, const Fmatrix& offset, const Fvector& mass_center, float mass, CPHFracture* fracture)
{
	Mass += mass;
}

void CPHElement::set_BoxMass(const Fobb& box, float mass)
{
	setMass(mass);
}

void CPHElement::setMassMC(float M, const Fvector& mass_center)
{
	MassCenterLocal.set(mass_center);
	setMass(M);
}

void CPHElement::setDensityMC(float M, const Fvector& mass_center)
{
	MassCenterLocal.set(mass_center);
	setDensity(M);
}

void CPHElement::applyImpulseVsMC(const Fvector& pos, const Fvector& dir, float val)
{
	if (!b3Body_IsValid(Body) || Fixed)
	{
		return;
	}

	Fvector Point;
	CurForm.transform_dir(Point, pos);
	Point.add(mass_Center());
	ApplyLimitedImpulse(Fvector().mul(dir, val), &Point);
}

void CPHElement::applyImpulseVsGF(const Fvector& pos, const Fvector& dir, float val)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		ApplyLimitedImpulse(Fvector().mul(dir, val), &pos);
	}
}

// As ODE: pos is in the space of bone id; the impulse goes to that point relative to the mass center.
void CPHElement::applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val, const u16 id)
{
	Fvector BodyPos;
	BodyPos.set(0.f, 0.f, 0.f);
	if (id != BI_NONE)
	{
		if (id == m_SelfID)
		{
			BodyPos.sub(pos, local_mass_Center());
		}
		else if (IKinematics* K = Shell ? Shell->PKinematics() : nullptr)
		{
			Fmatrix().mul_43(Fmatrix().invert(K->LL_GetTransform(m_SelfID)), K->LL_GetTransform(id)).transform(BodyPos, pos);
			BodyPos.sub(local_mass_Center());
		}
	}
	applyImpulseVsMC(BodyPos, dir, val);
}

u16 CPHElement::setGeomFracturable(CPHFracture& fracture)
{
	B3_NOT_IMPLEMENTED();
	return 0;
}

CPHFracture& CPHElement::Fracture(u16 num)
{
	B3_NOT_IMPLEMENTED();
	R_ASSERT2(false, "[Box3D] fracture is not implemented");
	return *static_cast<CPHFracture*>(nullptr);
}

const Fvector& CPHElement::mass_Center() const
{
	if (b3Body_IsValid(Body))
	{
		MassCenterGlobal = B3Math::FromB3(b3Body_GetWorldCenterOfMass(Body));
	}
	else
	{
		mXFORM.transform_tiny(MassCenterGlobal, MassCenterLocal);
	}
	return MassCenterGlobal;
}

float CPHElement::getRadius()
{
	return Geoms.empty() ? 0.f : Geoms.back()->Radius();
}

void CPHElement::get_MaxAreaDir(Fvector& dir)
{
	dir.set(0.f, 1.f, 0.f);
	if (Geoms.empty() || Geoms.front()->Type != EB3GeomType::Box)
	{
		return;
	}

	const CODEGeom* Geom = Geoms.front();
	Fmatrix Form;
	Geom->GetGlobalForm(Form);
	const Fvector& H = Geom->HalfSize;
	if (H.x <= H.y && H.x <= H.z)
	{
		dir.set(Form.i);
	}
	else if (H.y <= H.z)
	{
		dir.set(Form.j);
	}
	else
	{
		dir.set(Form.k);
	}
}

void CPHElement::Fix()
{
	Fixed = true;
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetType(Body, b3_kinematicBody));
		B3_SAFE(b3Body_SetLinearVelocity(Body, b3Vec3_zero));
		B3_SAFE(b3Body_SetAngularVelocity(Body, b3Vec3_zero));
	}
}

void CPHElement::ReleaseFixed()
{
	Fixed = false;
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetType(Body, b3_dynamicBody));
		setMass(Mass);
	}
}

void CPHElement::get_State(SPHNetState& state)
{
	GetGlobalPositionDynamic(&state.position);
	state.previous_position.set(PrevForm.c);
	Fmatrix Form;
	GetGlobalTransformDynamic(&Form);
	state.quaternion.set(Form);
	state.previous_quaternion.set(PrevForm);
	get_LinearVel(state.linear_vel);
	get_AngularVel(state.angular_vel);
	state.force.set(0.f, 0.f, 0.f);
	state.torque.set(0.f, 0.f, 0.f);
	state.enabled = isEnabled();
}

void CPHElement::set_State(const SPHNetState& state)
{
	Fmatrix Form;
	Form.mk_xform(state.quaternion, state.position);
	SetTransform(Form, mh_unspecified);
	set_LinearVel(state.linear_vel);
	set_AngularVel(state.angular_vel);
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetAwake(Body, state.enabled));
	}
}

void CPHElement::cv2obj_Xfrom(const Fquaternion& q, const Fvector& pos, Fmatrix& xform)
{
	xform.mk_xform(q, pos);
}

void CPHElement::cv2bone_Xfrom(const Fquaternion& q, const Fvector& pos, Fmatrix& xform)
{
	xform.mk_xform(q, pos);
}

void CPHElement::net_Export(NET_Packet& P)
{
	SPHNetState State;
	get_State(State);
	State.net_Export(P);
}

void CPHElement::net_Import(NET_Packet& P)
{
	SPHNetState State;
	State.net_Import(P);
	set_State(State);
}
