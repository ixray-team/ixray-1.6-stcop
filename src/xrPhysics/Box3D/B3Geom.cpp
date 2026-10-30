#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "../../xrEngine/GameMtlLib.h"

static constexpr float MinGeomSize = 0.005f;
static constexpr int CylinderSides = 12;

void SB3GeomOwner::AddObjectCallback(ObjectContactCallbackFun* Callback)
{
	if (Callback && std::find(ObjectCallbacks.begin(), ObjectCallbacks.end(), Callback) == ObjectCallbacks.end())
	{
		ObjectCallbacks.push_back(Callback);
	}
}

void SB3GeomOwner::RemoveObjectCallback(ObjectContactCallbackFun* Callback)
{
	auto It = std::find(ObjectCallbacks.begin(), ObjectCallbacks.end(), Callback);
	if (It != ObjectCallbacks.end())
	{
		ObjectCallbacks.erase(It);
	}
}

CODEGeom* CODEGeom::MakeSphere(const Fsphere& Sphere)
{
	CODEGeom* Geom = new CODEGeom();
	Geom->Type = EB3GeomType::Sphere;
	Geom->LocalForm.translate(Sphere.P);
	Geom->HalfSize.set(std::max(Sphere.R, MinGeomSize), 0.f, 0.f);
	return Geom;
}

CODEGeom* CODEGeom::MakeBox(const Fobb& Box)
{
	CODEGeom* Geom = new CODEGeom();
	Geom->Type = EB3GeomType::Box;
	Box.xform_get(Geom->LocalForm);
	Geom->HalfSize.set(std::max(Box.m_halfsize.x, MinGeomSize), std::max(Box.m_halfsize.y, MinGeomSize), std::max(Box.m_halfsize.z, MinGeomSize));
	return Geom;
}

CODEGeom* CODEGeom::MakeCylinder(const Fcylinder& Cylinder)
{
	CODEGeom* Geom = new CODEGeom();
	Geom->Type = EB3GeomType::Cylinder;

	// Cylinder axis is local Y.
	Fmatrix& Form = Geom->LocalForm;
	Form.identity();
	Form.j.normalize_safe(Fvector(Cylinder.m_direction));
	if (Form.j.square_magnitude() < EPS)
	{
		Form.j.set(0.f, 1.f, 0.f);
	}
	const Fvector Any = std::abs(Form.j.x) < 0.9f ? Fvector().set(1.f, 0.f, 0.f) : Fvector().set(0.f, 0.f, 1.f);
	Form.k.crossproduct(Any, Form.j).normalize();
	Form.i.crossproduct(Form.j, Form.k);
	Form.c.set(Cylinder.m_center);

	Geom->HalfSize.set(std::max(Cylinder.m_radius, MinGeomSize), std::max(Cylinder.m_height * 0.5f, MinGeomSize), 0.f);
	return Geom;
}

void CODEGeom::SetMask(u64 Mask)
{
	if (b3Shape_IsValid(Shape))
	{
		// invokeContacts drops contacts the new mask no longer allows (collision disable on shells).
		b3Filter Filter = b3Shape_GetFilter(Shape);
		Filter.maskBits = Mask;
		B3_SAFE(b3Shape_SetFilter(Shape, Filter, true));
	}
}

void CODEGeom::CreateShape(b3BodyId Body, float Density, bool Dynamic, u64 Mask)
{
	DestroyShape();

	b3ShapeDef Def = b3DefaultShapeDef();
	Def.density = Density;
	Def.userData = this;
	Def.enablePreSolveEvents = true;
	Def.enableHitEvents = Dynamic;
	Def.filter.categoryBits = Dynamic ? B3Category::Dynamic : B3Category::Static;
	Def.filter.maskBits = Mask;
	Def.enableCustomFiltering = Dynamic;
	if (Owner)
	{
		if (SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(Owner->Material))
		{
			Def.baseMaterial.friction = Mtl->fPHFriction;
			Def.baseMaterial.restitution = Mtl->Flags.test(SGameMtl::flBounceable) ? Mtl->fPHBouncing : 0.f;
		}
		Def.baseMaterial.userMaterialId = Owner->Material;
	}

	switch (Type)
	{
	case EB3GeomType::Sphere:
	{
		b3Sphere Sphere = {B3Math::ToB3(LocalForm.c), HalfSize.x};
		Shape = b3CreateSphereShape(Body, &Def, &Sphere);
		break;
	}
	case EB3GeomType::Box:
	{
		b3BoxHull Hull = b3MakeTransformedBoxHull(HalfSize.x, HalfSize.y, HalfSize.z, B3Math::ToB3(LocalForm));
		Shape = b3CreateHullShape(Body, &Def, &Hull.base);
		break;
	}
	case EB3GeomType::Cylinder:
	{
		b3HullData* Hull = b3CreateCylinder(HalfSize.y * 2.f, HalfSize.x, -HalfSize.y, CylinderSides);
		Shape = b3CreateTransformedHullShape(Body, &Def, Hull, B3Math::ToB3(LocalForm), b3Vec3{1.f, 1.f, 1.f});
		b3DestroyHull(Hull);
		break;
	}
	}
}

void CODEGeom::DestroyShape()
{
	if (b3Shape_IsValid(Shape))
	{
		b3DestroyShape(Shape, false);
	}
	Shape = b3_nullShapeId;
}

float CODEGeom::Volume() const
{
	switch (Type)
	{
	case EB3GeomType::Sphere:
		return 4.f / 3.f * PI * HalfSize.x * HalfSize.x * HalfSize.x;
	case EB3GeomType::Box:
		return 8.f * HalfSize.x * HalfSize.y * HalfSize.z;
	case EB3GeomType::Cylinder:
		return PI * HalfSize.x * HalfSize.x * HalfSize.y * 2.f;
	}
	return 0.f;
}

float CODEGeom::Radius() const
{
	switch (Type)
	{
	case EB3GeomType::Sphere:
		return HalfSize.x;
	case EB3GeomType::Box:
		return HalfSize.magnitude();
	case EB3GeomType::Cylinder:
		return _sqrt(HalfSize.x * HalfSize.x + HalfSize.y * HalfSize.y);
	}
	return 0.f;
}

void CODEGeom::GetGlobalForm(Fmatrix& Form) const
{
	if (OwnerForm)
	{
		Form.mul_43(*OwnerForm, LocalForm);
	}
	else
	{
		Form.set(LocalForm);
	}
}

void CODEGeom::GetExtensions(const Fvector& Axis, float Center, float& Lo, float& Hi) const
{
	Fmatrix Form;
	GetGlobalForm(Form);
	const float Dif = Form.c.dotproduct(Axis) - Center;

	float Ext = 0.f;
	switch (Type)
	{
	case EB3GeomType::Sphere:
		Ext = HalfSize.x;
		break;
	case EB3GeomType::Box:
		Ext = std::abs(Form.i.dotproduct(Axis)) * HalfSize.x + std::abs(Form.j.dotproduct(Axis)) * HalfSize.y + std::abs(Form.k.dotproduct(Axis)) * HalfSize.z;
		break;
	case EB3GeomType::Cylinder:
	{
		const float Cos = std::abs(Form.j.dotproduct(Axis));
		Ext = Cos * HalfSize.y + _sqrt(std::max(0.f, 1.f - Cos * Cos)) * HalfSize.x;
		break;
	}
	}
	Lo = Dif - Ext;
	Hi = Dif + Ext;
}

void CODEGeom::get_Box(Fmatrix& Form, Fvector& Size) const
{
	GetGlobalForm(Form);
	Fvector Center;
	Center.set(0.f, 0.f, 0.f);
	for (int I = 0; I < 3; ++I)
	{
		const Fvector& Axis = cast_fv(((const float*)&Form + I * 4));
		float Lo, Hi;
		GetExtensions(Axis, 0.f, Lo, Hi);
		Size[I] = Hi - Lo;
		Center.add(Fvector().mul(Axis, (Lo + Hi) / 2));
	}
	Form.c = Center;
}

void PHGeomGetXform(const CODEGeom* Geom, Fmatrix& Form)
{
	Geom->GetGlobalForm(Form);
}

void PHGeomSetBoneId(CODEGeom* Geom, u16 BoneId)
{
	Geom->BoneId = BoneId;
}

void PHGeomDestroy(CODEGeom*& Geom)
{
	if (Geom)
	{
		Geom->DestroyShape();
	}
	xr_delete(Geom);
}

void PHGeomAddObjectContactCallback(CODEGeom* Geom, ObjectContactCallbackFun* Callback)
{
	if (std::find(Geom->ObjectCallbacks.begin(), Geom->ObjectCallbacks.end(), Callback) == Geom->ObjectCallbacks.end())
	{
		Geom->ObjectCallbacks.push_back(Callback);
	}
}

#ifdef DEBUG
void PHGeomDbgDraw(const CODEGeom* Geom, float Scale, u32 Color)
{
}
#endif
