// Originally by B.O.R.S.C.H.T. team
// see https://bitbucket.org/stalker/xray-csky_borscht_sdk

#include "stdafx.h"

#include "IM_Manipulator.h"
#include "../xrEUI/imgui.h"
#include "../xrEUI/ImGuizmo.h"
#include "../../scene/scene.h"
#include "../../UI_LevelTools.h"
#include "../../Entry/CustomObject.h"

IM_Manipulator imManipulator;

void IM_Manipulator::Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight)
{
	ImGuizmo::SetRect(canvasX, canvasY, canvasWidth, canvasHeight);
	ImGuizmo::SetDrawlist();

	ESceneCustomOTool* tool = Scene->GetOTool(LTools->CurrentClassID());
	if (!tool)
		return;

	ObjectList lst;
	tool->GetQueryObjects(lst, TRUE, TRUE, FALSE);
	if (lst.size() < 1)
		return;

	const bool IsCSParent   = Tools->GetSettings(etfCSParent);
	Fmatrix    ObjectMatrix = lst.front()->FTransform;
	Fmatrix    DeltaMatrix  = Fidentity;

	switch (LTools->GetAction())
	{
	case etaMove:
	{
		float  MoveSnap[3];
		float* PtrMoveSnap = LTools->GetSettings(etfMSnap) ? MoveSnap : nullptr;

		if (PtrMoveSnap)
			std::fill_n(MoveSnap, std::size(MoveSnap), Tools->m_MoveSnap);

		const bool IsManipulated = ImGuizmo::Manipulate((float*)&Device.mView, (float*)&Device.mProject, ImGuizmo::TRANSLATE, ImGuizmo::WORLD, (float*)&ObjectMatrix, (float*)&DeltaMatrix, PtrMoveSnap);

		if (IsManipulated)
		{
			for (CCustomObject* ObjPtr : lst)
			{
				if (CWayObject* WayPtr = smart_cast<CWayObject*>(ObjPtr))
				{
					ObjPtr->Move(DeltaMatrix.c);
					ObjPtr->UpdateTransform();
				}
				else
				{
					ObjPtr->Move(DeltaMatrix.c);
				}
			}
		}
	}
	break;
	case etaRotate:
	{
		float  RotateSnap;
		float* PtrRotateSnap = LTools->GetSettings(etfASnap) ? &RotateSnap : nullptr;

		if (PtrRotateSnap)
			RotateSnap = rad2deg(Tools->m_RotateSnapAngle);

		Fvector OriginalRotation;

		ObjectMatrix.getXYZ(OriginalRotation);

		const bool IsManipulated = ImGuizmo::Manipulate((float*)&Device.mView, (float*)&Device.mProject, ImGuizmo::ROTATE, ImGuizmo::WORLD, (float*)&ObjectMatrix, (float*)&DeltaMatrix, PtrRotateSnap);

		if (IsManipulated)
		{
			Fvector DeltaXYZ;

			DeltaMatrix.getXYZ(DeltaXYZ);

			for (ObjectIt it = lst.begin(); it != lst.end(); it++)
			{
				void (CCustomObject:: * Handler)(Fvector&, float);

				if (IsCSParent)
					Handler = &CCustomObject::RotateParent;
				else
					Handler = &CCustomObject::RotateLocal;

				(*it->*Handler)(Fvector().set(0, 0, 1), -DeltaXYZ.z);
				(*it->*Handler)(Fvector().set(1, 0, 0), -DeltaXYZ.x);
				(*it->*Handler)(Fvector().set(0, 1, 0), -DeltaXYZ.y);
			}
			UI->UpdateScene();
		}
	}
	break;
	case etaScale:
	{
		float  ScaleSnap[3];
		float* PtrScaleSnap = LTools->GetSettings(etfScaleFixed) ? ScaleSnap : nullptr;

		if (PtrScaleSnap)
			std::fill_n(ScaleSnap, std::size(ScaleSnap), Tools->m_ScaleFixed);

		bool IsSingleObject = lst.size() == 1;

		Fbox test;
		u32 Flags = ImGuizmo::SCALE;

		if (IsSingleObject)
		{
			if (LTools->CurrentClassID() == OBJCLASS_SCENEOBJECT)
			{
				lst.front()->GetUTBox(test);
				Flags |= ImGuizmo::BOUNDS;
			}
			if (LTools->CurrentClassID() == OBJCLASS_SHAPE)
			{
				CEditShape* Shape = (CEditShape*)lst.front();
				if (Shape->shapes[0].type == CShapeData::cfBox)
				{
					test = Shape->m_Box;
					Flags |= ImGuizmo::BOUNDS;
				}
				else
				{
					IsSingleObject = false;
					Flags = ImGuizmo::SCALE_XU;
				}
			}
			else
			{
				IsSingleObject = false;
			}
		}

		const bool IsManipulated = ImGuizmo::Manipulate
		(
			(float*)&Device.mView, 
			(float*)&Device.mProject,
			(ImGuizmo::OPERATION)Flags, //ImGuizmo::SCALE | ImGuizmo::BOUNDS,
			IsSingleObject ? ImGuizmo::WORLD : ImGuizmo::LOCAL,
			(float*)&ObjectMatrix, 
			(float*)&DeltaMatrix, 
			PtrScaleSnap, 
			IsSingleObject ? (float*)&test.min.x : nullptr,
			IsSingleObject ? PtrScaleSnap : nullptr
		);

		if (IsManipulated)
		{
			Fvector Scale;
			Scale.x = DeltaMatrix.i.magnitude();
			Scale.y = DeltaMatrix.j.magnitude();
			Scale.z = DeltaMatrix.k.magnitude();

			for (ObjectIt it = lst.begin(); it != lst.end(); it++)
			{
				Scale.mul((*it)->GetScale());
				(*it)->SetScale(Scale);
			}
			UI->UpdateScene();
		}
		else if (IsSingleObject && ImGuizmo::IsUsing())
		{
			Fmatrix DeltaMatrixScale = lst.front()->FTransform;
			DeltaMatrixScale.invert();
			DeltaMatrixScale.mulA_44(ObjectMatrix);

			Fvector Scale;
			Scale.x = DeltaMatrixScale.i.magnitude();
			Scale.y = DeltaMatrixScale.j.magnitude();
			Scale.z = DeltaMatrixScale.k.magnitude();

			CCustomObject* Obj = lst.front();

			Scale.mul(Obj->GetScale());
			Obj->FPosition = ObjectMatrix.c;
			Obj->SetScale(Scale);
			UI->UpdateScene();
		}
	}
	break;
	}

	if (ImGuizmo::IsUsing() && !m_active)
	{
		// activate
		m_active = true;
	}

	if (!ImGuizmo::IsUsing() && m_active)
	{
		// deactivate
		Scene->UndoSave();
		m_active = false;
	}
}
