#include "stdafx.h"
#include "../Terrain/ESceneTerrainTools.h"
#include "../../../Renderer/Tiramisu/TiramisuEditorNativeScene.h"
#include "../../../../xrECore/Editor/EditorRenderBackend.h"

namespace
{
bool UsesNativeSceneObjectControl()
{
	return GetEditorNativeSceneDocument().IsOpen() &&
		LTools->CurrentClassID() == OBJCLASS_SCENEOBJECT;
}
} // namespace

TUI_CustomControl::TUI_CustomControl(int st, int act, ESceneToolBase* parent)
{
	parent_tool		= parent; VERIFY(parent);
	sub_target		= st;
	action			= act;
	bBoxSelection	= false;
}

bool TUI_CustomControl::Start(TShiftState _Shift)
{
	if (LUI->GetEState() == esEditLibrary)
		return false;

	switch(action)
	{
		case etaSelect:
			 return SelectStart(_Shift);
		 case etaAdd:
			 return AddStart(_Shift);
		 case etaMove:
			 return MovingStart(_Shift);
		 case etaRotate:
			 return RotateStart(_Shift);
		 case etaScale:
			 return ScaleStart(_Shift);
	}
	return false;
}
bool TUI_CustomControl::End(TShiftState _Shift)
{
	switch(action)
	{
		case etaSelect:
			return SelectEnd(_Shift);
		case etaAdd:
			return AddEnd(_Shift);
		case etaMove:
			return MovingEnd(_Shift);
		case etaRotate:
			return RotateEnd(_Shift);
		case etaScale:
			return ScaleEnd(_Shift);
	}
	return false;
}
void TUI_CustomControl::Move(TShiftState _Shift)
{
	switch(action)
	{
		case etaSelect:
			SelectProcess(_Shift);
		break;
		case etaAdd:
			AddProcess(_Shift);
		break;
		case etaMove:
			MovingProcess(_Shift);
		break;
		case etaRotate:
			RotateProcess(_Shift);
		break;
		case etaScale:
			ScaleProcess(_Shift);
		break;
	}
}
bool TUI_CustomControl::HiddenMode()
{
	switch(action)
	{
		case etaSelect:
			return false;
		case etaAdd:
			return false;
		case etaMove:
			return true;
		case etaRotate:
			return true;
		case etaScale:
			return true;
	}
	return false;
}

void DragDrop(const xr_string& Path, int Type)
{
	Fvector p, n;
	if (LUI->PickGround(p, UI->m_ContextRStart, UI->m_ContextRDir, 1, &n))
	{
		if (Path.ends_with(".static-mesh.json"))
		{
			TiramisuEditorNativeSceneDocument& Document =
				GetEditorNativeSceneDocument();
			if (!Document.IsEditableRenderScene())
			{
				Msg("! Open a native RenderScene before adding a native "
					"StaticMesh.");
				return;
			}
			xr_array<float, 16> Transform = {
				1.0f, 0.0f, 0.0f, 0.0f,
				0.0f, 1.0f, 0.0f, 0.0f,
				0.0f, 0.0f, 1.0f, 0.0f,
				p.x, p.y, p.z, 1.0f};
			xr_string Diagnostic;
			if (!Document.AddStaticMeshComponent(
					std::filesystem::path(Path.c_str()),
					Transform, Diagnostic))
			{
				Msg("! Cannot add native StaticMesh component: %s",
					Diagnostic.c_str());
				return;
			}
			UI->RedrawScene();
			return;
		}
		// before callback
		SBeforeAppendCallbackParams P;

		string_path fn = {};

		if (OBJCLASS_TERRAIN == Type)
		{
			FS.update_path(fn, "$server_data_root$", fn);
			xr_string NewPath = Path.substr(Path.find(fn) + xr_strlen(fn));
			NewPath = NewPath.substr(0, NewPath.length() - 4);

			string256 namebuffer;
			Scene->GenObjectName(OBJCLASS_TERRAIN, namebuffer, NewPath.data());
			CTerrain* obj = new CTerrain(nullptr, namebuffer);
			if (!obj->Valid()) {
				xr_delete(obj);
			}

			obj->SetLoadedState();
			Scene->SelectObjects(false, Type);
			Scene->AppendObject(obj);
		}
		else if (OBJCLASS_SCENEOBJECT == Type)
		{
			FS.update_path(fn, "$objects$", fn);
			xr_string NewPath = Path.substr(Path.find(fn) + xr_strlen(fn));
			NewPath = NewPath.substr(0, NewPath.length() - 7);

			string256 namebuffer;
			Scene->GenObjectName(OBJCLASS_SCENEOBJECT, namebuffer, NewPath.data());
			CSceneObject* obj = new CSceneObject(nullptr, namebuffer);
			CEditableObject* ref = obj->SetReference(NewPath.data());
			if (!obj->Valid()) {
				xr_delete(obj);
			}

			obj->MoveTo(p, n);
			obj->SetLoadedState();
			Scene->SelectObjects(false, Type);
			Scene->AppendObject(obj);
		}
		else if (Type == OBJCLASS_SPAWNPOINT)
		{
			xr_stack_string256 NameBuilder;
			if(Scene->LevelPrefix().c_str())
			{
				NameBuilder = Scene->LevelPrefix().c_str();
				NameBuilder += "_";
				NameBuilder += Path.c_str();
			} else
			{
				NameBuilder = Path;
			}
			string256 namebuffer;
			IVERIFY(NameBuilder.size()+10 < sizeof(namebuffer));
			Scene->GenObjectName(OBJCLASS_SPAWNPOINT, namebuffer, NameBuilder.data());
			auto obj = Scene->GetOTool(OBJCLASS_SPAWNPOINT)->CreateObject((void*)Path.data(), namebuffer);
			if (!obj->Valid())
			{
				xr_delete(obj);
				return;
			}

			obj->MoveTo(p, n);
			obj->SetLoadedState();
			Scene->SelectObjects(false, OBJCLASS_SPAWNPOINT);
			Scene->AppendObject(obj);
		}
		else if (Type == OBJCLASS_GROUP)
		{
			FS.update_path(fn, "$groups$", fn);
			xr_string NewPath = Path.substr(Path.find(fn) + xr_strlen(fn));
			NewPath = NewPath.substr(0, NewPath.length() - 6);

			string256 namebuffer;
			Scene->GenObjectName(OBJCLASS_GROUP, namebuffer, NewPath.data());
			CGroupObject* obj = new CGroupObject(nullptr, namebuffer);

			if (obj->SetReference(NewPath.data()))
			{
				string256 namebuffer;
				Scene->GenObjectName(OBJCLASS_GROUP, namebuffer, NewPath.data());
				obj->SetName(namebuffer);
			}

			if (!obj->Valid())
			{
				xr_delete(obj);
			}

			obj->MoveTo(p, n);
			obj->SetLoadedState();
			Scene->SelectObjects(false, Type);
			Scene->AppendObject(obj);
		}
		else return;

		ExecCommand(COMMAND_CHANGE_TARGET, Type);
	}
}

// add
CCustomObject* TUI_CustomControl::DefaultAddObject(TShiftState Shift, TBeforeAppendCallback before, TAfterAppendCallback after)
{
	if (Shift==ssRBOnly){ ExecCommand(COMMAND_SHOWCONTEXTMENU,parent_tool->FClassID); return 0;}
	Fvector p,n;
	CCustomObject* obj=0;
	if (LUI->PickGround(p,UI->m_CurrentRStart,UI->m_CurrentRDir,1,&n))
	{
		// before callback
		SBeforeAppendCallbackParams P;
		if (before&&!before(&P)) return 0;

		string256 namebuffer;
		Scene->GenObjectName(parent_tool->FClassID, namebuffer, P.name_prefix.c_str());
		obj=Scene->GetOTool(parent_tool->FClassID)->CreateObject(P.data, namebuffer);
		if (!obj->Valid()){
			xr_delete(obj);
			return 0;
		}
		// after callback
		if (after&&!after(Shift, obj)){
			xr_delete(obj);
			return 0;
		}
		obj->MoveTo(p,n);
		Scene->SelectObjects(false,parent_tool->FClassID);
		Scene->AppendObject(obj);
		if (Shift&ssCtrl) 
			ExecCommand(COMMAND_SHOW_PROPERTIES);
		if (!(Shift&ssAlt)) 
			ResetActionToSelect();
	}
	return obj;
}

bool  TUI_CustomControl::AddStart(TShiftState Shift)
{
	DefaultAddObject(Shift,0);
	return false;
}
void  TUI_CustomControl::AddProcess(TShiftState _Shift)
{
}
bool  TUI_CustomControl::AddEnd(TShiftState _Shift)
{
	return true;
}

bool TUI_CustomControl::CheckSnapList(TShiftState Shift)
{
	if ( MainForm->GetLeftBarForm()->IsSnapListMode()){
		CCustomObject* O=Scene->RayPickObject(UI->ZFar(),UI->m_CurrentRStart,UI->m_CurrentRDir,OBJCLASS_SCENEOBJECT,0,0);
		if (O){
			if (Scene->FindObjectInSnapList(O)){
				if (Shift&ssAlt){
					Scene->DelFromSnapList(O);
				}else if (Shift & ssCtrl){
					Scene->DelFromSnapList(O);
				}
			}else{
				if (!(Shift&(ssCtrl| ssAlt))){
					Scene->AddToSnapList(O);
				}else if (Shift&ssCtrl){
					Scene->AddToSnapList(O);
				}
			}
			return true;
		}else{
			return false;
		}
	}
	return false;
}

// total select
bool TUI_CustomControl::SelectStart(TShiftState Shift)
{
	ObjClassID cls = LTools->CurrentClassID();

	ESceneToolBase* mt = Scene->GetTool(cls);
	if (mt && !mt->IsLoaded)
	{
		return false;
	}

	if (UsesNativeSceneObjectControl())
	{
		if (Shift == ssRBOnly)
		{
			ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
			return false;
		}
		FEditorViewportPickRequest Request;
		Request.RayOrigin = {UI->m_CurrentRStart.x,
			UI->m_CurrentRStart.y, UI->m_CurrentRStart.z};
		Request.RayDirection = {UI->m_CurrentRDir.x,
			UI->m_CurrentRDir.y, UI->m_CurrentRDir.z};
		Request.MaxDistance = UI->ZFar();
		Request.CullBackFaces = false;
		const FEditorViewportPickResult Pick =
			GetEditorRenderBackend().PickViewport(
				static_cast<u32>(UI->ViewID), Request);
		TiramisuEditorNativeSceneDocument& Document =
			GetEditorNativeSceneDocument();
		if (Pick.Hit)
		{
			const EEditorNativeSceneSelectionMode Mode =
				(Shift & ssAlt)
					? EEditorNativeSceneSelectionMode::Remove
					: (Shift & ssCtrl)
						? EEditorNativeSceneSelectionMode::Toggle
						: EEditorNativeSceneSelectionMode::Replace;
			(void)Document.SelectObject(Pick.ObjectId.Value, Mode);
		}
		else if (!((Shift & ssCtrl) || (Shift & ssAlt)))
		{
			Document.ClearSelection();
		}
		bBoxSelection = !Pick.Hit ||
			((Shift & ssCtrl) || (Shift & ssAlt));
		if (bBoxSelection)
		{
			UI->EnableSelectionRect(true);
			UI->UpdateSelectionRect(UI->m_StartCp, UI->m_CurrentCp);
		}
		UI->RedrawScene();
		// A point miss starts native rectangle selection and must not fall
		// through to the empty transition EScene.
		return bBoxSelection;
	}

	if (CheckSnapList(Shift)) return false;
	if (Shift==ssRBOnly){ ExecCommand(COMMAND_SHOWCONTEXTMENU,parent_tool->FClassID); return false;}
	if (!((Shift&ssCtrl)||(Shift&ssAlt))) Scene->SelectObjects( false, cls);

	int cnt 		= Scene->RaySelect((Shift & ssCtrl)?-1:(Shift & ssAlt)?0:1,parent_tool->FClassID);
	bBoxSelection    = ((0!=cnt) && ((Shift & ssCtrl)||(Shift & ssAlt))) || (0==cnt);
	if( bBoxSelection )
	{
		UI->EnableSelectionRect( true );
		UI->UpdateSelectionRect(UI->m_StartCp,UI->m_CurrentCp);
		return true;
	}
	return false;
}

void  TUI_CustomControl::SelectProcess(TShiftState _Shift)
{
	if (bBoxSelection)
		UI->UpdateSelectionRect(UI->m_StartCp,UI->m_CurrentCp);
}

bool  TUI_CustomControl::SelectEnd(TShiftState _Shift)
{
	if (bBoxSelection)
	{
		UI->EnableSelectionRect( false );
		bBoxSelection = false;
		if (UsesNativeSceneObjectControl())
		{
			CFrustum LegacyFrustum;
			if (LUI->SelectionFrustum(LegacyFrustum))
			{
				FEditorNativeSceneSelectionFrustum NativeFrustum;
				NativeFrustum.Planes.reserve(LegacyFrustum.p_count);
				for (int Index = 0; Index < LegacyFrustum.p_count; ++Index)
				{
					const CFrustum::fplane& Plane =
						LegacyFrustum.planes[Index];
					NativeFrustum.Planes.push_back({
						{Plane.n.x, Plane.n.y, Plane.n.z}, Plane.d});
				}
				const EEditorNativeSceneSelectionMode Mode =
					(_Shift & ssAlt)
						? EEditorNativeSceneSelectionMode::Remove
						: EEditorNativeSceneSelectionMode::Add;
				(void)GetEditorNativeSceneDocument().SelectFrustum(
					NativeFrustum, Mode);
			}
			UI->RedrawScene();
			return true;
		}
		Scene->FrustumSelect(_Shift&ssAlt?0:1,LTools->CurrentClassID());
	}
	return true;
}

// moving
bool TUI_CustomControl::MovingStart(TShiftState Shift)
{
	ObjClassID cls = LTools->CurrentClassID();

	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}
	if (UsesNativeSceneObjectControl())
	{
		TiramisuEditorNativeSceneDocument& Document =
			GetEditorNativeSceneDocument();
		if (!Document.IsEditableRenderScene() ||
			Document.GetSelectionCount() == 0 || (Shift & ssCtrl))
		{
			return false;
		}
		if (etAxisY == Tools->GetAxis())
		{
			m_MovingXVector.set(0, 0, 0);
			m_MovingYVector.set(0, 1, 0);
		}
		else
		{
			m_MovingXVector.set(UI->CurrentView().m_Camera.GetRight());
			m_MovingXVector.y = 0;
			m_MovingYVector.set(
				UI->CurrentView().m_Camera.GetDirection());
			m_MovingYVector.y = 0;
			m_MovingXVector.normalize_safe();
			m_MovingYVector.normalize_safe();
		}
		m_MovingReminder.set(0, 0, 0);
		return Document.BeginEditTransaction();
	}
	if (Scene->SelectionCount(true, cls) == 0)
		return false;

	if (Shift & ssCtrl)
	{
		ObjectList lst;
		if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
		{
			if (lst.size() == 1)
			{
				Fvector p, n;
				// UI->IR_GetMousePosReal(EDevice->m_hWnd, UI->m_CurrentCp);
				UI->GetRenderMousePosition();
				UI->CurrentView().m_Camera.MouseRayFromPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, UI->m_CurrentCp);
				if (LUI->PickGround(p, UI->m_CurrentRStart, UI->m_CurrentRDir, 1, &n))
				{
					for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
						(*_F)->MoveTo(p, n);
					Scene->UndoSave();
				}
			}
			else
			{
				Fvector p, n;
				Fvector D = { 0, -1, 0 };
				for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
				{
					if (LUI->PickGround(p, (*_F)->GetPosition(), D, 1, &n))
					{
						(*_F)->MoveTo(p, n);
					}
				}
			}
		}
		return false;
	}
	else
	{
		if (etAxisY == Tools->GetAxis())
		{
			m_MovingXVector.set(0, 0, 0);
			m_MovingYVector.set(0, 1, 0);
		}
		else
		{
			m_MovingXVector.set(UI->CurrentView().m_Camera.GetRight());
			m_MovingXVector.y = 0;
			m_MovingYVector.set(UI->CurrentView().m_Camera.GetDirection());
			m_MovingYVector.y = 0;
			m_MovingXVector.normalize_safe();
			m_MovingYVector.normalize_safe();
		}
		m_MovingReminder.set(0, 0, 0);
	}
	return true;
}

bool TUI_CustomControl::DefaultMovingProcess(TShiftState Shift, Fvector& amount)
{
	if ((Shift & ssLeft) || (Shift & ssRight))
	{
		amount.mul(m_MovingXVector, UI->m_MouseSM * UI->m_DeltaCpH.x);
		amount.mad(amount, m_MovingYVector, -UI->m_MouseSM * UI->m_DeltaCpH.y);

		if (Tools->GetSettings(etfMSnap))
		{
			CHECK_SNAP(m_MovingReminder.x, amount.x, Tools->m_MoveSnap);
			CHECK_SNAP(m_MovingReminder.y, amount.y, Tools->m_MoveSnap);
			CHECK_SNAP(m_MovingReminder.z, amount.z, Tools->m_MoveSnap);
		}

		if (!(etAxisX == Tools->GetAxis()) && !(etAxisZX == Tools->GetAxis()))
			amount.x = 0.f;
		if (!(etAxisZ == Tools->GetAxis()) && !(etAxisZX == Tools->GetAxis()))
			amount.z = 0.f;
		if (!(etAxisY == Tools->GetAxis()))
			amount.y = 0.f;

		return (amount.square_magnitude() > EPS_S);
	}
	return false;
}

void TUI_CustomControl::MovingProcess(TShiftState _Shift)
{
	Fvector amount;
	if (DefaultMovingProcess(_Shift, amount))
	{
		if (UsesNativeSceneObjectControl())
		{
			(void)GetEditorNativeSceneDocument().TranslateSelected(
				{amount.x, amount.y, amount.z});
			UI->RedrawScene();
			return;
		}
		ObjectList lst;
		if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
			for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
				(*_F)->Move(amount);
	}
}

bool TUI_CustomControl::MovingEnd(TShiftState _Shift)
{
	if (UsesNativeSceneObjectControl())
	{
		const bool Ended =
			GetEditorNativeSceneDocument().EndEditTransaction();
		UI->RedrawScene();
		return Ended;
	}
	Scene->UndoSave();
	return true;
}

// rotate
bool TUI_CustomControl::RotateStart(TShiftState Shift)
{
	ObjClassID cls = LTools->CurrentClassID();

	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}
	if (UsesNativeSceneObjectControl())
	{
		if (!GetEditorNativeSceneDocument().IsEditableRenderScene() ||
			GetEditorNativeSceneDocument().GetSelectionCount() == 0)
		{
			return false;
		}
	}
	else if (Scene->SelectionCount(true, cls) == 0)
		return false;

	m_RotateVector.set(0, 0, 0);
	if (etAxisX == Tools->GetAxis())
		m_RotateVector.set(1, 0, 0);
	else if (etAxisY == Tools->GetAxis())
		m_RotateVector.set(0, 1, 0);
	else if (etAxisZ == Tools->GetAxis())
		m_RotateVector.set(0, 0, 1);
	m_fRotateSnapAngle = 0;
	if (UsesNativeSceneObjectControl())
		return GetEditorNativeSceneDocument().BeginEditTransaction();
	return true;
}

void TUI_CustomControl::RotateProcess(TShiftState _Shift)
{
	if (_Shift & ssLeft)
	{
		float amount = -UI->m_DeltaCpH.x * UI->m_MouseSR;

		if (Tools->GetSettings(etfASnap))
			CHECK_SNAP(m_fRotateSnapAngle, amount, Tools->m_RotateSnapAngle);

		if (UsesNativeSceneObjectControl())
		{
			const bool ParentSpace = Tools->GetSettings(etfCSParent);
			(void)GetEditorNativeSceneDocument().TransformSelected(
				[&](xr_array<float, 16>& Transform)
				{
					Fmatrix Matrix;
					std::copy_n(Transform.data(), Transform.size(),
						Matrix.mm);
					const Fvector Position = Matrix.c;
					Matrix.c.set(0.0f, 0.0f, 0.0f);
					Fmatrix Delta;
					Delta.rotation(m_RotateVector, amount);
					if (ParentSpace)
						Matrix.mulA_43(Delta);
					else
						Matrix.mulB_43(Delta);
					Matrix.c = Position;
					std::copy_n(Matrix.mm, Transform.size(),
						Transform.data());
				});
			UI->RedrawScene();
			return;
		}
		ObjectList lst;
		if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
			for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
				if (Tools->GetSettings(etfCSParent))
				{
					(*_F)->RotateParent(m_RotateVector, amount);
				}
				else
				{
					(*_F)->RotateLocal(m_RotateVector, amount);
				}
	}
}

bool TUI_CustomControl::RotateEnd(TShiftState _Shift)
{
	if (UsesNativeSceneObjectControl())
	{
		const bool Ended =
			GetEditorNativeSceneDocument().EndEditTransaction();
		UI->RedrawScene();
		return Ended;
	}
	Scene->UndoSave();
	return true;
}

// scale
bool TUI_CustomControl::ScaleStart(TShiftState Shift)
{
	ObjClassID cls = LTools->CurrentClassID();
	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}
	if (UsesNativeSceneObjectControl())
	{
		if (!GetEditorNativeSceneDocument().IsEditableRenderScene() ||
			GetEditorNativeSceneDocument().GetSelectionCount() == 0)
		{
			return false;
		}
		return GetEditorNativeSceneDocument().BeginEditTransaction();
	}
	if (Scene->SelectionCount(true, cls) == 0)
		return false;
	return true;
}

void TUI_CustomControl::ScaleProcess(TShiftState _Shift)
{
	float dy = UI->m_DeltaCpH.x * UI->m_MouseSS;
	if (dy > 1.f)
		dy = 1.f;
	else if (dy < -1.f)
		dy = -1.f;

	Fvector amount;
	amount.set(dy, dy, dy);

	if (Tools->GetSettings(etfScaleFixed))
	{
		CHECK_SNAP(Tools->m_fScaleFixedValue.x, amount.x, Tools->m_ScaleFixed);
		CHECK_SNAP(Tools->m_fScaleFixedValue.y, amount.y, Tools->m_ScaleFixed);
		CHECK_SNAP(Tools->m_fScaleFixedValue.z, amount.z, Tools->m_ScaleFixed);
	}

	if (Tools->GetSettings(etfNUScale))
	{
		if (!(etAxisX == Tools->GetAxis()) && !(etAxisZX == Tools->GetAxis()))
			amount.x = 0.f;
		if (!(etAxisZ == Tools->GetAxis()) && !(etAxisZX == Tools->GetAxis()))
			amount.z = 0.f;
		if (!(etAxisY == Tools->GetAxis()))
			amount.y = 0.f;
	}

	if (UsesNativeSceneObjectControl())
	{
		(void)GetEditorNativeSceneDocument().TransformSelected(
			[&](xr_array<float, 16>& Transform)
			{
				Fmatrix Matrix;
				std::copy_n(Transform.data(), Transform.size(), Matrix.mm);
				const float CurrentX = Matrix.i.magnitude();
				const float CurrentY = Matrix.j.magnitude();
				const float CurrentZ = Matrix.k.magnitude();
				if (CurrentX > EPS)
					Matrix.i.mul(std::max(EPS, CurrentX + amount.x) /
						CurrentX);
				if (CurrentY > EPS)
					Matrix.j.mul(std::max(EPS, CurrentY + amount.y) /
						CurrentY);
				if (CurrentZ > EPS)
					Matrix.k.mul(std::max(EPS, CurrentZ + amount.z) /
						CurrentZ);
				std::copy_n(Matrix.mm, Transform.size(), Transform.data());
			});
		UI->RedrawScene();
		return;
	}
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
		for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
			(*_F)->Scale(amount);
}

bool TUI_CustomControl::ScaleEnd(TShiftState _Shift)
{
	if (UsesNativeSceneObjectControl())
	{
		const bool Ended =
			GetEditorNativeSceneDocument().EndEditTransaction();
		UI->RedrawScene();
		return Ended;
	}
	Scene->UndoSave();
	return true;
}
