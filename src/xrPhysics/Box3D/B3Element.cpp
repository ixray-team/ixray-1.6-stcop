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

static const float* FindGeomMass(const xr_vector<std::pair<CODEGeom*, float>>& Masses, const CODEGeom* Geom)
{
	auto It = std::find_if(Masses.begin(), Masses.end(), [Geom](const auto& Entry) { return Entry.first == Geom; });
	return It == Masses.end() ? nullptr : &It->second;
}

static Fvector ClampMagnitude(Fvector V, float Limit)
{
	const float Magnitude = V.magnitude();
	return Magnitude > Limit ? V.mul(Limit / Magnitude) : V;
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
	if (Geoms.empty())
	{
		Fixed = true;
	}

	const b3Transform Transform = B3Math::ToB3(Form);
	b3BodyDef Def = b3DefaultBodyDef();
	Def.type = Fixed ? b3_kinematicBody : b3_dynamicBody;
	Def.position = Transform.p;
	Def.rotation = Transform.q;
	Def.gravityScale = ApplyGravity ? 1.f : 0.f;
	Def.isAwake = !Disable;
	Def.isBullet = Shell && Shell->IsBullet();
	Def.sleepThreshold = SleepThreshold();
	Def.allowFastRotation = AllowFastRotation;
	Def.enableContactRecycling = !Shell || !Shell->CollideClassBits.test(B3CollideClass::RagDoll);
	Def.userData = this;
	Body = b3CreateBody(g_B3World->Id(), &Def);

	const u64 Mask = Shell ? (Shell->CollisionDisabled ? 0ull : Shell->CollideMask) : B3Category::All;
	float Explicit = 0.f;
	float Volume = 0.f;
	for (CODEGeom* Geom : Geoms)
	{
		const float* GeomMass = FindGeomMass(GeomMasses, Geom);
		Explicit += GeomMass ? *GeomMass : 0.f;
		Volume += GeomMass ? 0.f : Geom->Volume();
	}
	const float Density = Volume > EPS_S ? std::max(Mass - Explicit, 0.f) / Volume : 1.f;
	for (CODEGeom* Geom : Geoms)
	{
		const float* GeomMass = FindGeomMass(GeomMasses, Geom);
		Geom->Owner = &Owner;
		Geom->OwnerForm = &mXFORM;
		Geom->CreateShape(Body, GeomMass ? *GeomMass / std::max(Geom->Volume(), EPS_S) : Density, true, Mask);
	}
	ApplyMassData();
}

void CPHElement::ApplyMassData()
{
	if (Fixed || !b3Body_IsValid(Body))
	{
		return;
	}
	if (g_B3World->IsStepping())
	{
		g_B3World->Defer([this]() { ApplyMassData(); });
		return;
	}

	b3Body_ApplyMassFromShapes(Body);
	b3MassData Data = b3Body_GetMassData(Body);
	if (Data.mass <= EPS_S)
	{
		return;
	}

	Data.inertia = b3MulSM(Mass / Data.mass, Data.inertia);
	if (HasMassCenter)
	{
		const b3Vec3 Center = B3Math::ToB3(MassCenterLocal);
		const b3Vec3 D = b3Sub(Center, Data.center);
		const float DD = b3Dot(D, D);
		const b3Matrix3 Shift = {{DD - D.x * D.x, -D.y * D.x, -D.z * D.x}, {-D.x * D.y, DD - D.y * D.y, -D.z * D.y}, {-D.x * D.z, -D.y * D.z, DD - D.z * D.z}};
		Data.inertia = b3AddMM(Data.inertia, b3MulSM(Mass, Shift));
		Data.center = Center;
	}
	MassCenterLocal = B3Math::FromB3(Data.center);
	Data.mass = Mass + AddedMass;
	Data.inertia = b3AddMM(b3MulSM(InertiaKeep, Data.inertia), ExtraInertia);
	b3Body_SetMassData(Body, Data);
}

float CPHElement::SleepThreshold() const
{
	// Kinematic (animated) bodies keep Box3D's threshold: a slow animation must not fall asleep.
	if (Fixed)
	{
		return b3DefaultBodyDef().sleepThreshold;
	}

	float Radius = 0.f;
	for (CODEGeom* Geom : Geoms)
	{
		Radius = std::max(Radius, Geom->Radius());
	}
	// ODE disabling params are path lengths per 0.01 s; Box3D sleeps on |v| + |w x extent|.
	return (DisableParams.translational.velocity + DisableParams.rotational.velocity * Radius) / 0.01f;
}

void CPHElement::set_DisableParams(const SAllDDOParams& params)
{
	DisableParams = params;
	if (b3Body_IsValid(Body))
	{
		b3Body_SetSleepThreshold(Body, SleepThreshold());
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

	Fvector Linear, Angular, TargetCenter;
	BonePose.transform_tiny(TargetCenter, MassCenterLocal);
	linear_diff(Linear, TargetCenter, mass_Center(), dt);
	angular_diff(Angular, Diff, dt);
	const bool Result = Angular.square_magnitude() < a_limit * a_limit && Linear.square_magnitude() < l_limit * l_limit;
	set_LinearVel(ClampMagnitude(Linear, l_limit));
	set_AngularVel(ClampMagnitude(Angular, a_limit));
	return Result;
}

// As ODE PhDataUpdate: over limit * scale both velocities are cut to the limits, and the motion the
// step made with the excess is rolled back (dxStepBody with the negated excess).
void CPHElement::CutVelocity()
{
	if (!Active || Fixed || !b3Body_IsValid(Body) || !b3Body_IsAwake(Body))
	{
		return;
	}

	const b3Vec3 Linear = b3Body_GetLinearVelocity(Body);
	const b3Vec3 Angular = b3Body_GetAngularVelocity(Body);
	const float LinearMagnitude = b3Length(Linear);
	const float AngularMagnitude = b3Length(Angular);
	if (LinearMagnitude <= LinearLimit * LinearScale && AngularMagnitude <= AngularLimit * AngularScale)
	{
		return;
	}

	const b3Vec3 LinearCut = LinearMagnitude > LinearLimit ? b3MulSV(LinearLimit / LinearMagnitude, Linear) : Linear;
	const b3Vec3 AngularCut = AngularMagnitude > AngularLimit ? b3MulSV(AngularLimit / AngularMagnitude, Angular) : Angular;
	const b3Transform Transform = b3Body_GetTransform(Body);
	const b3Quat Spin = b3MulQuat({b3MulSV(0.5f * fixed_step, b3Sub(AngularCut, Angular)), 0.f}, Transform.q);
	const b3Quat Rotation = b3NormalizeQuat({b3Add(Transform.q.v, Spin.v), Transform.q.s + Spin.s});
	const b3Vec3 Center = b3MulAdd(b3Body_GetWorldCenterOfMass(Body), fixed_step, b3Sub(LinearCut, Linear));
	b3Body_SetTransform(Body, b3Sub(Center, b3RotateVector(Rotation, b3Body_GetLocalCenterOfMass(Body))), Rotation);
	b3Body_SetLinearVelocity(Body, LinearCut);
	b3Body_SetAngularVelocity(Body, AngularCut);
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
		// Re-targeted every step over the steps left in the frame: the body arrives with zero velocity.
		xrCriticalSectionGuard Guard(PendingLock);
		if (HasAnimTarget && b3Body_IsValid(Body))
		{
			B3_SAFE(b3Body_SetTargetTransform(Body, B3Math::ToB3(AnimTarget), Step * g_B3World->StepsLeft(), true));
		}
	}
	if (!b3Body_IsValid(Body) || Fixed || !b3Body_IsAwake(Body))
	{
		return;
	}

	// Air resistance exactly as in ODE (PHElement::PhTune): a quadratic linear force and a
	// torque proportional to the angular velocity. The old multiplicative decay was tens
	// of times weaker, so a ragdoll limb could swing for minutes.
	const b3Vec3 Linear = b3Body_GetLinearVelocity(Body);
	const float Speed = b3Length(Linear);
	if (Speed > EPS_L && AirLinear > 0.f)
	{
		float Drag = Speed * AirLinear;
		if (Drag > Mass / Step)
		{
			Drag = Mass / Step;
		}
		B3_SAFE(b3Body_ApplyForceToCenter(Body, b3MulSV(-Drag, Linear), false));
	}

	const b3Vec3 Angular = b3Body_GetAngularVelocity(Body);
	if (AirAngular > 0.f)
	{
		// Implicit step: an explicit torque multiplies the velocity of a low-inertia limb on every
		// step and reaches NaN. For ordinary bodies the result is the same as the explicit one.
		b3Matrix3 Damping = b3MulSM(Step * AirAngular, b3Body_GetWorldInverseRotationalInertia(Body));
		Damping.cx.x += 1.f;
		Damping.cy.y += 1.f;
		Damping.cz.z += 1.f;
		const b3Vec3 Damped = b3Solve3(Damping, Angular);
		B3_SAFE(b3Body_ApplyTorque(Body, b3MulSV(-AirAngular, Damped), false));
	}
}

void CPHElement::FlushForces(float Step)
{
	b3Vec3 F = B3Math::ToB3(Force);
	b3Vec3 T = B3Math::AngularToB3(Torque);
	Force.set(0.f, 0.f, 0.f);
	Torque.set(0.f, 0.f, 0.f);
	if (!b3Body_IsValid(Body) || Fixed || !b3Body_IsAwake(Body))
	{
		return;
	}

	const float FLimit = LinearLimit / Step * b3Body_GetMass(Body);
	const float FMagnitude = b3Length(F);
	if (FMagnitude > FLimit)
	{
		F = b3MulSV(FLimit / FMagnitude, F);
	}
	const float WLimit = AngularLimit / Step;
	const float WMagnitude = b3Length(b3MulMV(b3Body_GetWorldInverseRotationalInertia(Body), T));
	if (WMagnitude > WLimit)
	{
		T = b3MulSV(WLimit / WMagnitude, T);
	}
	B3_SAFE(b3Body_ApplyForceToCenter(Body, F, false); b3Body_ApplyTorque(Body, T, false));
}

void CPHElement::AddForce(const Fvector& F, const Fvector* Point)
{
	if (!b3Body_IsValid(Body) || Fixed)
	{
		return;
	}

	Fvector T = {0.f, 0.f, 0.f};
	if (Point)
	{
		T.crossproduct(Fvector().sub(*Point, mass_Center()), F);
	}
	B3_SAFE(Force.add(F); Torque.add(T); b3Body_SetAwake(Body, true));
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
	v->set(mass_Center());
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
	if (!b3Body_IsValid(Body))
	{
		HasMassCenter = false;
		std::erase_if(GeomMasses, [](const auto& Entry) { return Entry.second > 0.f; });
	}
	ApplyMassData();
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
	AddForce(Fvector().set(x, y, z));
}

void CPHElement::applyImpulse(const Fvector& dir, float val)
{
	AddForce(Fvector().mul(dir, val / fixed_step));
}

void CPHElement::setTorque(const Fvector& torque)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(Torque.set(torque); b3Body_SetAwake(Body, true));
	}
}

void CPHElement::setForce(const Fvector& force)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(Force.set(force); b3Body_SetAwake(Body, true));
	}
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
		B3_SAFE(b3Body_SetLinearVelocity(Body, B3Math::ToB3(ClampMagnitude(velocity, LinearLimit))));
	}
}

void CPHElement::set_AngularVel(const Fvector& velocity)
{
	if (b3Body_IsValid(Body) && !Fixed)
	{
		B3_SAFE(b3Body_SetAngularVelocity(Body, B3Math::AngularToB3(ClampMagnitude(velocity, AngularLimit))));
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
	xrCriticalSectionGuard Guard(PendingLock);
	HasPendingForm = false;
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
			const u64 Overrides = Material.userMaterialId & (B3MaterialFlag::Friction | B3MaterialFlag::Bounce);
			if ((Overrides & B3MaterialFlag::Friction) == 0)
			{
				Material.friction = Mtl->fPHFriction;
			}
			if ((Overrides & B3MaterialFlag::Bounce) == 0)
			{
				Material.restitution = Mtl->Flags.test(SGameMtl::flBounceable) ? Mtl->fPHBouncing : 0.f;
			}
			Material.userMaterialId = m | Overrides;
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

// As ODE: a geom moved in from another element keeps its global pose, and a geom added to a live
// body does not change its mass.
void CPHElement::add_geom(CODEGeom* g)
{
	if (g->OwnerForm && g->OwnerForm != &mXFORM)
	{
		Fmatrix Global;
		g->GetGlobalForm(Global);
		const Fmatrix BodyForm = b3Body_IsValid(Body) ? B3Math::FromB3(b3Body_GetTransform(Body)) : mXFORM;
		g->LocalForm.mul_43(Fmatrix().invert(BodyForm), Global);
	}
	g->Owner = &Owner;
	g->OwnerForm = &mXFORM;
	Geoms.push_back(g);
	if (b3Body_IsValid(Body))
	{
		const u64 Mask = Shell ? Shell->CollideMask : B3Category::All;
		GeomMasses.emplace_back(g, 0.f);
		g->CreateShape(Body, 0.f, true, Mask);
		ApplyMassData();
	}
}

void CPHElement::remove_geom(CODEGeom* g)
{
	auto It = std::find(Geoms.begin(), Geoms.end(), g);
	if (It != Geoms.end())
	{
		g->DestroyShape();
		Geoms.erase(It);
		std::erase_if(GeomMasses, [g](const auto& Entry) { return Entry.first == g; });
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

// As ODE: the rigid child bone keeps its own mass on its geom (the last one added) and moves the
// element center to the combined center. A geomless element starts from zero mass.
void CPHElement::add_Mass(const SBoneShape& shape, const Fmatrix& offset, const Fvector& mass_center, float mass, CPHFracture* fracture)
{
	Fvector Center;
	offset.transform_tiny(Center, mass_center);
	const float Base = Geoms.size() > 1 ? Mass : 0.f;
	Mass = Base + mass;
	MassCenterLocal.mul(Base).add(Center.mul(mass)).mul(1.f / std::max(Mass, EPS_S));
	HasMassCenter = true;
	GeomMasses.emplace_back(last_geom(), mass);
}

void CPHElement::set_BoxMass(const Fobb& box, float mass)
{
	setMass(mass);
}

void CPHElement::setMassMC(float M, const Fvector& mass_center)
{
	setMass(M);
	MassCenterLocal.set(mass_center);
	HasMassCenter = true;
	ApplyMassData();
}

void CPHElement::setDensityMC(float M, const Fvector& mass_center)
{
	setMassMC(M * getVolume(), mass_center);
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
	AddForce(Fvector().mul(dir, val / fixed_step), &Point);
}

void CPHElement::applyImpulseVsGF(const Fvector& pos, const Fvector& dir, float val)
{
	AddForce(Fvector().mul(dir, val / fixed_step), &pos);
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
		b3Body_SetSleepThreshold(Body, SleepThreshold());
	}
}

void CPHElement::ReleaseFixed()
{
	Fixed = false;
	HasAnimTarget = false;
	if (b3Body_IsValid(Body))
	{
		B3_SAFE(b3Body_SetType(Body, b3_dynamicBody));
		b3Body_SetSleepThreshold(Body, SleepThreshold());
		ApplyMassData();
	}
}

// As ODE the net state holds the center of mass, not the element origin.
void CPHElement::get_State(SPHNetState& state)
{
	GetGlobalPositionDynamic(&state.position);
	PrevForm.transform_tiny(state.previous_position, MassCenterLocal);
	Fmatrix Form;
	GetGlobalTransformDynamic(&Form);
	state.quaternion.set(Form);
	state.previous_quaternion.set(PrevForm);
	get_LinearVel(state.linear_vel);
	get_AngularVel(state.angular_vel);
	state.force.set(Force);
	state.torque.set(Torque);
	state.enabled = isEnabled();
}

void CPHElement::set_State(const SPHNetState& state)
{
	const auto OriginForm = [this](const Fquaternion& Q, const Fvector& Center)
	{
		Fmatrix Form;
		Form.mk_xform(Q, Center);
		Fvector Offset;
		Form.transform_dir(Offset, MassCenterLocal);
		Form.c.sub(Offset);
		return Form;
	};
	SetTransform(OriginForm(state.quaternion, state.position), mh_not_clear);
	PrevForm = OriginForm(state.previous_quaternion, state.previous_position);
	set_LinearVel(state.linear_vel);
	set_AngularVel(state.angular_vel);
	setForce(state.force);
	setTorque(state.torque);
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
