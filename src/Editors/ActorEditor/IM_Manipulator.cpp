#include "stdafx.h"

#include "IM_Manipulator.h"
#include "../xrEUI/ImGuizmo.h"

IM_Manipulator imManipulator;

static void BuildCylinderBasis(const Fvector& Dir, Fmatrix& Result)
{
	Fvector L_dir, L_up, L_right;
	L_dir.set(Dir);
	L_dir.normalize();

	L_up.set(0, 1, 0);
	if (std::abs(L_up.dotproduct(L_dir)) > .99f)
		L_up.set(0, 0, 1);

	L_right.crossproduct(L_up, L_dir);
	L_right.normalize();
	L_up.crossproduct(L_dir, L_right);
	L_up.normalize();

	Result.identity();
	Result.i.set(L_right);
	Result.j.set(L_up);
	Result.k.set(L_dir);
}

static float PickChangedAxis(const Fvector& Scale, float Current)
{
	float Result = Scale.x;
	if (std::abs(Scale.y - Current) > std::abs(Result - Current))
		Result = Scale.y;
	if (std::abs(Scale.z - Current) > std::abs(Result - Current))
		Result = Scale.z;
	return Result;
}

void IM_Manipulator::Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight)
{
	CBone* Bone = nullptr;
	const ETAction Action = ATools->GetAction();
	const bool CanEdit = ATools->GetEditMode() == emBone && !ATools->IsPhysics() && (Action == etaMove || Action == etaRotate || Action == etaScale);

	if (CanEdit)
		Bone = GetShapeBone();

	if (Bone == nullptr)
	{
		UpdateState();
		return;
	}

	ImGuizmo::SetRect(canvasX, canvasY, canvasWidth, canvasHeight);
	ImGuizmo::SetDrawlist();

	Fmatrix BoneMatrix;
	BoneMatrix.mul_43(ATools->GetAVTransform(), Bone->_LTransform());

	switch (Action)
	{
	case etaMove:
		CommandMove(Bone, BoneMatrix);
		break;
	case etaRotate:
		CommandRotate(Bone, BoneMatrix);
		break;
	case etaScale:
		CommandScale(Bone, BoneMatrix);
		break;
	}

	UpdateState();
}

void IM_Manipulator::UpdateState()
{
	if (ImGuizmo::IsUsing() && !m_active)
	{
		m_active = true;
	}

	if (!ImGuizmo::IsUsing() && m_active)
	{
		ATools->OnBoneModified();
		m_active = false;
	}
}

CBone* IM_Manipulator::GetShapeBone() const
{
	CEditableObject* Object = ATools->CurrentObject();
	if (Object == nullptr)
		return nullptr;

	BoneVec Selected;
	if (!Object->GetSelectedBones(Selected))
		return nullptr;

	for (CBone* Bone : Selected)
	{
		if (Bone->shape.type != SBoneShape::stNone && Bone->shape.Valid())
			return Bone;
	}

	return nullptr;
}

void IM_Manipulator::BuildShapeMatrix(CBone* Bone, Fmatrix& Result, bool WithScale) const
{
	const SBoneShape& Shape = Bone->shape;
	Fvector Scale = { 1.f, 1.f, 1.f };

	switch (Shape.type)
	{
	case SBoneShape::stBox:
		Shape.box.xform_get(Result);
		Scale.set(Shape.box.m_halfsize);
		break;
	case SBoneShape::stSphere:
		Result.translate(Shape.sphere.P);
		Scale.set(Shape.sphere.R, Shape.sphere.R, Shape.sphere.R);
		break;
	case SBoneShape::stCylinder:
		BuildCylinderBasis(Shape.cylinder.m_direction, Result);
		Result.c.set(Shape.cylinder.m_center);
		Scale.set(Shape.cylinder.m_radius, Shape.cylinder.m_radius, Shape.cylinder.m_height);
		break;
	default:
		Result.identity();
		break;
	}

	if (WithScale)
	{
		Result.i.mul(Scale.x);
		Result.j.mul(Scale.y);
		Result.k.mul(Scale.z);
	}
}

void IM_Manipulator::ApplyTransform(CBone* Bone, const Fmatrix& BoneMatrix, const Fmatrix& ShapeMatrix)
{
	Fmatrix InvBone;
	InvBone.invert(BoneMatrix);

	Fmatrix Local;
	Local.mul_43(InvBone, ShapeMatrix);
	Local.i.normalize();
	Local.j.normalize();
	Local.k.normalize();

	SBoneShape& Shape = Bone->shape;
	switch (Shape.type)
	{
	case SBoneShape::stBox:
		Shape.box.xform_set(Local);
		break;
	case SBoneShape::stSphere:
		Shape.sphere.P.set(Local.c);
		break;
	case SBoneShape::stCylinder:
		Shape.cylinder.m_center.set(Local.c);
		Shape.cylinder.m_direction.set(Local.k);
		break;
	}

	EContext.UI->RedrawScene();
}

void IM_Manipulator::CommandMove(CBone* Bone, const Fmatrix& BoneMatrix)
{
	float  MoveSnap[3];
	float* PtrMoveSnap = ATools->GetSettings(etfMSnap) ? MoveSnap : nullptr;

	if (PtrMoveSnap)
		std::fill_n(MoveSnap, std::size(MoveSnap), Tools->m_MoveSnap);

	Fmatrix ShapeLocal;
	BuildShapeMatrix(Bone, ShapeLocal, false);

	Fmatrix ShapeMatrix;
	ShapeMatrix.mul_43(BoneMatrix, ShapeLocal);

	const bool IsManipulated = ImGuizmo::Manipulate
	(
		(float*)&Device.mView, (float*)&Device.mProject,
		ImGuizmo::TRANSLATE, (ImGuizmo::MODE)MatrixMode,
		(float*)&ShapeMatrix, nullptr, PtrMoveSnap
	);

	if (IsManipulated)
		ApplyTransform(Bone, BoneMatrix, ShapeMatrix);
}

void IM_Manipulator::CommandRotate(CBone* Bone, const Fmatrix& BoneMatrix)
{
	if (Bone->shape.type == SBoneShape::stSphere)
		return;

	float RotateSnap;
	float* PtrRotateSnap = ATools->GetSettings(etfASnap) ? &RotateSnap : nullptr;
	if (PtrRotateSnap)
		RotateSnap = rad2deg(Tools->m_RotateSnapAngle);

	Fmatrix ShapeLocal;
	BuildShapeMatrix(Bone, ShapeLocal, false);

	Fmatrix ShapeMatrix;
	ShapeMatrix.mul_43(BoneMatrix, ShapeLocal);

	const bool IsManipulated = ImGuizmo::Manipulate
	(
		(float*)&Device.mView, (float*)&Device.mProject,
		ImGuizmo::ROTATE, (ImGuizmo::MODE)MatrixMode,
		(float*)&ShapeMatrix, nullptr, PtrRotateSnap
	);

	if (IsManipulated)
		ApplyTransform(Bone, BoneMatrix, ShapeMatrix);
}

void IM_Manipulator::CommandScale(CBone* Bone, const Fmatrix& BoneMatrix)
{
	float  ScaleSnap[3];
	float* PtrScaleSnap = ATools->GetSettings(etfScaleFixed) ? ScaleSnap : nullptr;

	if (PtrScaleSnap)
		std::fill_n(ScaleSnap, std::size(ScaleSnap), Tools->m_ScaleFixed);

	float  BoundsSnap[3];
	float* PtrBoundsSnap = ATools->GetSettings(etfMSnap) ? BoundsSnap : nullptr;

	if (PtrBoundsSnap)
		std::fill_n(BoundsSnap, std::size(BoundsSnap), Tools->m_MoveSnap);

	// ImGuizmo returns absolute axis lengths, so the shape sizes are packed into the matrix scale
	Fmatrix ShapeLocal;
	BuildShapeMatrix(Bone, ShapeLocal, true);

	Fmatrix ShapeMatrix;
	ShapeMatrix.mul_43(BoneMatrix, ShapeLocal);

	float LocalBounds[6] = { -1.f, -1.f, -1.f, 1.f, 1.f, 1.f };
	if (Bone->shape.type == SBoneShape::stCylinder)
	{
		LocalBounds[2] = -0.5f;
		LocalBounds[5] = 0.5f;
	}

	const bool IsManipulated = ImGuizmo::Manipulate
	(
		(float*)&Device.mView, (float*)&Device.mProject,
		ImGuizmo::SCALE, ImGuizmo::LOCAL,
		(float*)&ShapeMatrix, nullptr, PtrScaleSnap,
		LocalBounds, PtrBoundsSnap
	);

	// Bounds manipulation doesn't report changes, the matrix is updated while dragging
	if (!IsManipulated && !ImGuizmo::IsUsing())
		return;

	Fmatrix InvBone;
	InvBone.invert(BoneMatrix);

	Fmatrix Local;
	Local.mul_43(InvBone, ShapeMatrix);

	Fvector Size;
	Size.set(Local.i.magnitude(), Local.j.magnitude(), Local.k.magnitude());

	SBoneShape& Shape = Bone->shape;
	switch (Shape.type)
	{
	case SBoneShape::stBox:
		Shape.box.m_halfsize.set(std::max(Size.x, EPS), std::max(Size.y, EPS), std::max(Size.z, EPS));
		Shape.box.m_translate.set(Local.c);
		break;
	case SBoneShape::stSphere:
		Shape.sphere.R = std::max(PickChangedAxis(Size, Shape.sphere.R), EPS);
		Shape.sphere.P.set(Local.c);
		break;
	case SBoneShape::stCylinder:
	{
		float Radius = Size.x;
		if (std::abs(Size.y - Shape.cylinder.m_radius) > std::abs(Radius - Shape.cylinder.m_radius))
			Radius = Size.y;

		Shape.cylinder.m_radius = std::max(Radius, EPS);
		Shape.cylinder.m_height = std::max(Size.z, EPS);
		Shape.cylinder.m_center.set(Local.c);
	}
	break;
	}

	EContext.UI->RedrawScene();
}
