// Originally by B.O.R.S.C.H.T. team
// see https://bitbucket.org/stalker/xray-csky_borscht_sdk

#include "stdafx.h"

#include "IM_Manipulator.h"
#include "../xrEUI/ImGuizmo.h"
#include "../../Scene/scene.h"
#include "../../UI_LevelTools.h"
#include "../../Entry/CustomObject.h"

IM_Manipulator imManipulator;

void IM_Manipulator::Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight)
{
	ImGuizmo::SetRect(canvasX, canvasY, canvasWidth, canvasHeight);
	ImGuizmo::SetDrawlist();
	
	Fmatrix ObjectMatrix = Fidentity;

	ObjectList lst;
	SAINode* NodeObject = nullptr;

	if (ESceneCustomOTool* tool = Scene->GetOTool(LTools->CurrentClassID()))
	{
		tool->GetQueryObjects(lst, true, true, false);
		if (lst.size() < 1)
			return;

		ObjectMatrix = lst.front()->FTransform;
	}
	else if (ESceneAIMapTool* ToolBase = smart_cast<ESceneAIMapTool*>(Scene->GetTool(LTools->CurrentClassID())))
	{
		for (SAINode* Node : ToolBase->Nodes())
		{
			if (Node && Node->flags.test(SAINode::flSelected))
			{
				ObjectMatrix.c = Node->Pos;
				NodeObject = Node;
				break;
			}
		}

		if (NodeObject == nullptr)
			return;
	}
	else return;

	Fmatrix DeltaMatrix = Fidentity;

	switch (LTools->GetAction())
	{
		case etaMove:
		{
			CommandMove(lst, ObjectMatrix, DeltaMatrix, NodeObject);
			break;
		}
		case etaRotate:
		{
			const bool IsCSParent = Tools->GetSettings(etfCSParent);
			CommandRotate(ObjectMatrix, DeltaMatrix, lst, IsCSParent);
			break;
		}
		case etaScale:
		{
			bool retFlag;
			CommandScale(lst, ObjectMatrix, DeltaMatrix, retFlag);

			if (retFlag) 
				return;

			break;
		}
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

void IM_Manipulator::CommandScale(ObjectList& lst, Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, bool& retFlag)
{
	retFlag = true;

	float  ScaleSnap[3];
	float* PtrScaleSnap = LTools->GetSettings(etfScaleFixed) ? ScaleSnap : nullptr;

	if (PtrScaleSnap)
	{
		std::fill_n(ScaleSnap, std::size(ScaleSnap), Tools->m_ScaleFixed);
	}

	bool IsSingleObject = lst.size() == 1;

	Fbox localBox;
	Fbox worldBox;

	ImGuizmo::OPERATION Flags = ImGuizmo::SCALE;

	//----------------------------------------------------
	// Bounds
	//----------------------------------------------------
	if (IsSingleObject)
	{
		CCustomObject* Obj = lst.front();

		if (LTools->CurrentClassID() == OBJCLASS_SCENEOBJECT)
		{
			Obj->GetUTBox(localBox);
		}
		else if (LTools->CurrentClassID() == OBJCLASS_SHAPE || LTools->CurrentClassID() == OBJCLASS_PUDDLES)
		{
			CEditShape* Shape = (CEditShape*)Obj;

			if (Shape->shapes[0].type == CShapeData::cfBox)
			{
				localBox = Shape->m_Box;
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

		if (IsSingleObject)
		{
			worldBox = localBox;
		}
	}

	const bool IsManipulated = ImGuizmo::Manipulate
	(
		(float*)&Device.mView,
		(float*)&Device.mProject,
		Flags,
		IsSingleObject ? ImGuizmo::WORLD : ImGuizmo::LOCAL,
		(float*)&ObjectMatrix,
		(float*)&DeltaMatrix,
		PtrScaleSnap,
		IsSingleObject ? (float*)&worldBox.min.x : nullptr,
		IsSingleObject ? PtrScaleSnap : nullptr
	);

	if (IsManipulated)
	{
		Fvector scl;
		scl.set(
			DeltaMatrix.i.magnitude(),
			DeltaMatrix.j.magnitude(),
			DeltaMatrix.k.magnitude()
		);

		for (auto& obj : lst)
		{
			Fvector baseScale = obj->GetScale();
			Fvector newScale;

			newScale.x = baseScale.x * scl.x;
			newScale.y = baseScale.y * scl.y;
			newScale.z = baseScale.z * scl.z;

			obj->SetScale(newScale);
		}

		UI->UpdateScene();
	}
	else if (IsSingleObject && ImGuizmo::IsUsing())
	{
		CCustomObject* Obj = lst.front();

		Fmatrix invStart;
		invStart.invert(Obj->FTransform);

		Fmatrix delta;
		delta.mul(invStart, ObjectMatrix);

		Fvector scl;
		scl.set(
			delta.i.magnitude(),
			delta.j.magnitude(),
			delta.k.magnitude()
		);

		const float MIN_SCALE = 0.05f;
		if (scl.x < MIN_SCALE || scl.y < MIN_SCALE || scl.z < MIN_SCALE)
		{
			return;
		}

		Fvector baseScale = Obj->GetScale();
		Fvector newScale;

		newScale.x = baseScale.x * scl.x;
		newScale.y = baseScale.y * scl.y;
		newScale.z = baseScale.z * scl.z;

		Obj->SetScale(newScale);
		Obj->FPosition = ObjectMatrix.c;

		UI->UpdateScene();
	}

	retFlag = false;
}

void IM_Manipulator::CommandRotate(Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, ObjectList& lst, const bool IsCSParent)
{
	float RotateSnap;
	float* PtrRotateSnap = LTools->GetSettings(etfASnap) ? &RotateSnap : nullptr;
	if (PtrRotateSnap)
	{
		RotateSnap = rad2deg(Tools->m_RotateSnapAngle);
	}

	ImGuizmo::OPERATION Flags = ImGuizmo::ROTATE;
	if (LTools->CurrentClassID() == OBJCLASS_PUDDLES)
	{
		Flags = ImGuizmo::ROTATE_Y;
	}

	const bool IsManipulated = ImGuizmo::Manipulate(
		(float*)&Device.mView,
		(float*)&Device.mProject,
		Flags,
		(ImGuizmo::MODE)imManipulator.MatrixMode,
		(float*)&ObjectMatrix,
		(float*)&DeltaMatrix,
		PtrRotateSnap
	);

	if (!IsManipulated)
		return;

	Fvector DeltaXYZ;
	DeltaMatrix.getXYZ(DeltaXYZ);

	Fvector axisX(ObjectMatrix._11, ObjectMatrix._21, ObjectMatrix._31);
	Fvector axisY(ObjectMatrix._12, ObjectMatrix._22, ObjectMatrix._32);
	Fvector axisZ(ObjectMatrix._13, ObjectMatrix._23, ObjectMatrix._33);

	for (ObjectIt it = lst.begin(); it != lst.end(); it++)
	{
		void (CCustomObject::* Handler)(Fvector&, float) = IsCSParent ? &CCustomObject::RotateParent : &CCustomObject::RotateLocal;

		(*it->*Handler)(axisX, -DeltaXYZ.x);
		(*it->*Handler)(axisY, -DeltaXYZ.y);
		(*it->*Handler)(axisZ, -DeltaXYZ.z);
	}

	UI->UpdateScene();
}


void IM_Manipulator::CommandMove(ObjectList& lst, Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, SAINode* NodeObject)
{
	float  MoveSnap[3];
	float* PtrMoveSnap = LTools->GetSettings(etfMSnap) ? MoveSnap : nullptr;

	if (PtrMoveSnap)
		std::fill_n(MoveSnap, std::size(MoveSnap), Tools->m_MoveSnap);

	if (!lst.empty())
	{
		if (lst.size() == 1)
		{
			if (CWayObject* WayPtr = smart_cast<CWayObject*>(lst.front()))
			{
				ObjectMatrix = WayPtr->GetTransform();
			}
		}

		const bool IsManipulated = ImGuizmo::Manipulate
		(
			(float*)&Device.mView, (float*)&Device.mProject, 
			ImGuizmo::TRANSLATE, (ImGuizmo::MODE)imManipulator.MatrixMode, 
			(float*)&ObjectMatrix, (float*)&DeltaMatrix, PtrMoveSnap
		);

		if (IsManipulated)
		{
			for (CCustomObject* ObjPtr : lst)
			{
				ObjPtr->Move(DeltaMatrix.c);

				if (CWayObject* WayPtr = smart_cast<CWayObject*>(ObjPtr))
				{
					ObjPtr->UpdateTransform();
				}
				else if (ESoundSource* SndPtr = smart_cast<ESoundSource*>(ObjPtr))
				{
					ObjPtr->UpdateTransform();
				}
			}
		}
	}
	else if (NodeObject != nullptr)
	{
		const bool IsManipulated = ImGuizmo::Manipulate
		(
			(float*)&Device.mView, (float*)&Device.mProject, 
			ImGuizmo::TRANSLATE_Y, (ImGuizmo::MODE)imManipulator.MatrixMode, 
			(float*)&ObjectMatrix, (float*)&DeltaMatrix, PtrMoveSnap
		);

		if (IsManipulated)
		{
			if (lst.empty())
			{
				NodeObject->Pos.y += DeltaMatrix.c.y;
				NodeObject->Plane.build(NodeObject->Pos, NodeObject->Plane.n);
			}
		}
	}
}
