#include "stdafx.h"

TUI_CustomControl::TUI_CustomControl(int st, int act, ESceneToolBase* parent)
{
	parent_tool		= parent; VERIFY(parent);
	sub_target		= st;
    action			= act;
	bBoxSelection	= false;
}

bool TUI_CustomControl::Start  (TShiftState _Shift)
{
	switch(action){
	case etaSelect: return SelectStart(_Shift);
	case etaAdd: 	return AddStart(_Shift);
    }
    return false;
}
bool TUI_CustomControl::End    (TShiftState _Shift)
{
	switch(action){
	case etaSelect: return SelectEnd(_Shift);
	case etaAdd: 	return AddEnd(_Shift);
    }
    return false;
}
void TUI_CustomControl::Move   (TShiftState _Shift)
{
	switch(action){
	case etaSelect:	SelectProcess(_Shift); break;
	case etaAdd: 	AddProcess(_Shift);    break;
    }
}
bool TUI_CustomControl::HiddenMode()
{
	switch(action){
	case etaSelect:	return LTools->GetGimzo()->IsFixed()&& (LTools->GetGimzo()->GetType()==Gizmo::EType::Rotate&&IsSupportRotate());
	case etaAdd: 	return false;
    }
    return false;
}

// add
CCustomObject*  TUI_CustomControl::DefaultAddObject(TShiftState Shift, TBeforeAppendCallback before, TAfterAppendCallback after)
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

void TUI_CustomControl::MoveStart()
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
	{
		for (CCustomObject* Obj : lst)
		{
			Obj->PositionSave();
		}
	}
}

void TUI_CustomControl::ScaleStart()
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
	{
		for (CCustomObject* Obj : lst)
		{
			Obj->ScaleSave();
		}
	}
}

void TUI_CustomControl::RotateStart()
{
}

void TUI_CustomControl::MoveProcess(Fvector Delta, Fvector Vector)
{
	ObjectList lst;
    if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
    {
        for (CCustomObject* Obj : lst)
        {
            Fvector Pos = Obj->GetSavePosition();
            Pos.mad(Delta, Vector);
            if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move) && abs(Vector.x) > EPS)
            {
                Pos.x = snapto(Pos.x, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
            }
            if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move) && abs(Vector.y) > EPS)
            {
                Pos.y = snapto(Pos.y, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
            }
            if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move) && abs(Vector.z) > EPS)
            {
                Pos.z = snapto(Pos.z, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
            }
            Obj->Move(Pos);
            Tools->UpdateProperties();
        }
    }
}

void TUI_CustomControl::ScaleProcess(Fvector Delta, Fvector Vector)
{
	ObjectList lst;
    if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
    {
        for (CCustomObject* Obj : lst)
        {
            Fvector Pos = Obj->GetSaveScale();
            if (Obj->FClassID == OBJCLASS_GLOW)
            {
                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX)
				{
					Delta.set(Delta.x, Delta.x, Delta.x);
                }
				if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY)
				{
					Delta.set(Delta.y, Delta.y, Delta.y);
				}
				if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
				{
					Delta.set(Delta.z, Delta.z, Delta.z);
				}
                Vector.set(1, 1, 1);
            }
            Pos.mad(Delta, Vector);
            if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Scale) && abs(Vector.x) > EPS)
            {
                Pos.x = snapto(Pos.x, LTools->GetGimzo()->GetStep(Gizmo::EType::Scale));
            }
			if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Scale) && abs(Vector.y) > EPS)
			{
				Pos.y = snapto(Pos.y, LTools->GetGimzo()->GetStep(Gizmo::EType::Scale));
			}
			if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Scale) && abs(Vector.z) > EPS)
			{
				Pos.z = snapto(Pos.z, LTools->GetGimzo()->GetStep(Gizmo::EType::Scale));
			}
            if (fis_zero(Pos.x, EPS_S))
            {
                Pos.x = EPS;
            }
			if (fis_zero(Pos.y, EPS_S))
			{
				Pos.y = EPS;
			}
			if (fis_zero(Pos.z, EPS_S))
			{
				Pos.z = EPS;
			}
            Obj->Scale(Pos);
            Tools->UpdateProperties();
        }
    }
}

void TUI_CustomControl::RotateProcess(float Delta)
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
	{
		for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
		{
			(*_F)->RotateLocal(m_RotateVector, Delta);
			Fvector Rotate = (*_F)->GetRotation();
			Rotate.x = fmodf(Rotate.x, deg2rad(360.f));
			Rotate.y = fmodf(Rotate.y, deg2rad(360.f));
			Rotate.z = fmodf(Rotate.z, deg2rad(360.f));
			(*_F)->SetRotation(Rotate);
		}
	}
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
bool  TUI_CustomControl::SelectStart(TShiftState Shift)
{
	ObjClassID cls = LTools->CurrentClassID();



    if (LTools->GetGimzo()->GetType() == Gizmo::EType::Move && !IsSupportMove())
    {

    }
    else if (LTools->GetGimzo()->GetType() == Gizmo::EType::Rotate && !IsSupportRotate())
    {

    }
    else if (LTools->GetGimzo()->GetType() == Gizmo::EType::Scale && !IsSupportScale())
    {

    }
    else if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::None)
    {

        LTools->GetGimzo()->Fixed();

        if (LTools->GetGimzo()->GetType() == Gizmo::EType::Rotate)
        {
            m_RotateVector.set(0, 0, 0);
            if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX) 		m_RotateVector.set(1, 0, 0);
            else if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY) m_RotateVector.set(0, 1, 0);
            else if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ) m_RotateVector.set(0, 0, 1);
            m_fRotateSnapAngle = 0;
            RotateStart();
        }
        if (LTools->GetGimzo()->GetType() == Gizmo::EType::Scale)
        {
            ScaleStart();
        }
        if (LTools->GetGimzo()->GetType() == Gizmo::EType::Move)
        {
            MoveStart();
        }

		m_GizmoReminder.set(0, 0, 0);

		if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY)
		{
			m_GizmoXVector.set(0, 0, 0);
			m_GizmoYVector.set(0, 1, 0);
		}
		else
		{
			m_GizmoXVector.set(EDevice->m_Camera.GetRight());
			m_GizmoXVector.y = 0;
			m_GizmoYVector.set(EDevice->m_Camera.GetDirection());
			m_GizmoYVector.y = 0;
			m_GizmoXVector.normalize_safe();
			m_GizmoYVector.normalize_safe();
		}
        return true;

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
    if (LTools->GetGimzo()->IsFixed())
    {
        if (LTools->GetGimzo()->GetType() == Gizmo::EType::Move)
        {
            Fvector amount;

			amount.set(0, 0, 0);
			if ((_Shift & ssLeft) || (_Shift & ssRight))
			{
				Fvector Start;
				Fvector Dir;

				
                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX|| LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
				{
					Fplane XZPlane;  Fvector Position;  Fvector StartPosition;
					XZPlane.build_unit_normal(LTools->GetGimzo()->GetStartPosition(), Fvector().set(0, 1, 0));
                    if (XZPlane.intersectRayPoint(UI->m_StartRStart, UI->m_StartRDir, StartPosition) && XZPlane.intersectRayPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, Position))
                    {
						Fvector Delta;
						Delta.sub(Position, StartPosition);
						if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX)
						{
							MoveProcess(Delta, Fvector3().set(1, 0, 0));
						}
						else if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
						{
							MoveProcess(Delta, Fvector3().set(0, 0, 1));
						}
                    }
                }
                else  if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY)
				{
					Fplane YPlane;  Fvector Position; Fvector StartPosition;
                    Fvector Normal = EDevice->m_Camera.GetDirection();
                    Normal.mul(-1);
                    Normal.y = 0;
                    Normal.normalize_safe();
					YPlane.build_unit_normal(LTools->GetGimzo()->GetStartPosition(), Normal);
                    if (YPlane.intersectRayPoint(UI->m_StartRStart, UI->m_StartRDir, StartPosition) && YPlane.intersectRayPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, Position))
                    {
						Fvector Delta;
						Delta.sub(Position, StartPosition);
                        MoveProcess(Delta, Fvector3().set(0, 1, 0));
                    }
                }
             /*   else
                {
                    amount.mul(m_GizmoXVector, UI->m_MouseSM * UI->m_DeltaCpH.x);
                    amount.mad(amount, m_GizmoYVector, -UI->m_MouseSM * UI->m_DeltaCpH.y);

                    if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move))
                    {
                        CHECK_SNAP(m_GizmoReminder.x, amount.x, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                        CHECK_SNAP(m_GizmoReminder.y, amount.y, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                        CHECK_SNAP(m_GizmoReminder.z, amount.z, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                    }

                    if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedX) 	amount.x = 0.f;
                    if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedZ) 	amount.z = 0.f;
                    if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedY) 									amount.y = 0.f;
                }*/

			}

            /*if (amount.square_magnitude() > EPS_S)
            {
                ObjectList lst;
                if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
                    for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++) (*_F)->Move(amount);
            }*/
        }
        else   if (LTools->GetGimzo()->GetType() == Gizmo::EType::Scale)
        {
            if ((_Shift & ssLeft) || (_Shift & ssRight))
            {
                Fvector Start;
                Fvector Dir;
                Msg("%d,%d", UI->m_CurrentCp.x, UI->m_CurrentCp.y);


                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX || LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
                {
                    Fplane XZPlane;  Fvector Position; Fvector StartPosition;
                    XZPlane.build_unit_normal(LTools->GetGimzo()->GetStartPosition(), Fvector().set(0, 1, 0));
                    if (XZPlane.intersectRayPoint(UI->m_StartRStart, UI->m_StartRDir, StartPosition) && XZPlane.intersectRayPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, Position))
                    {
                        Fvector Delta;
                        Delta.sub(Position, StartPosition);
                        if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX)
                        {

                            ScaleProcess(Delta, Fvector().set(1, 0, 0));
                        }
                        else  if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
                        {

                            ScaleProcess(Delta, Fvector().set(0, 0, 1));
                        }
                    }
                }
                else  if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY)
                {
                    Fplane YPlane;  Fvector Position; Fvector StartPosition;
                    Fvector Normal = EDevice->m_Camera.GetDirection();
                    Normal.mul(-1);
                    Normal.y = 0;
                    Normal.normalize_safe();
                    YPlane.build_unit_normal(LTools->GetGimzo()->GetStartPosition(), Normal);
                    if (YPlane.intersectRayPoint(UI->m_StartRStart, UI->m_StartRDir, StartPosition) && YPlane.intersectRayPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, Position))
                    {
						Fvector Delta;
						Delta.sub(Position, StartPosition);
							ScaleProcess(Delta, Fvector().set(0, 1, 0));
                    }
                }
                else  if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedXYZ)
                {
					Fplane XYZPlane;  Fvector Position; Fvector StartPosition;
					Fvector Normal = EDevice->m_Camera.GetDirection();
                    Normal.mul(-1);
					Normal.normalize_safe();
                    XYZPlane.build_unit_normal(LTools->GetGimzo()->GetStartPosition(), Normal);
					if (XYZPlane.intersectRayPoint(UI->m_StartRStart, UI->m_StartRDir, StartPosition) && XYZPlane.intersectRayPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, Position))
					{
						EDevice->mView.transform(Position);
						EDevice->mView.transform(StartPosition);
						
						Fvector Delta;
                        Delta.set(1, 1, 1).mul(((Position.x - StartPosition.x) + (Position.y - StartPosition.y) + (Position.z - StartPosition.z)) * 0.7f);

						ScaleProcess(Delta, Fvector().set(1, 1, 1));
					}
                }
                /*   else
                   {
                       amount.mul(m_GizmoXVector, UI->m_MouseSM * UI->m_DeltaCpH.x);
                       amount.mad(amount, m_GizmoYVector, -UI->m_MouseSM * UI->m_DeltaCpH.y);

                       if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move))
                       {
                           CHECK_SNAP(m_GizmoReminder.x, amount.x, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                           CHECK_SNAP(m_GizmoReminder.y, amount.y, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                           CHECK_SNAP(m_GizmoReminder.z, amount.z, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
                       }

                       if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedX) 	amount.x = 0.f;
                       if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedZ) 	amount.z = 0.f;
                       if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedY) 									amount.y = 0.f;
                   }*/

            }
          /*  Fvector amount;
            amount.mul(m_GizmoXVector, UI->m_MouseSM * UI->m_DeltaCpH.x);
            amount.mad(amount, m_GizmoYVector, -UI->m_MouseSM * UI->m_DeltaCpH.y);
            
            if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedXYZ)
            {
                float dy = UI->m_DeltaCpH.x * UI->m_MouseSS;
                if (dy > 1.f) dy = 1.f; else if (dy < -1.f) dy = -1.f;

                amount.set(dy, dy, dy);
            }

            if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedXYZ)
            {
                if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedX) amount.x = 0.f;
                 if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedZ) amount.z = 0.f;
                 if (LTools->GetGimzo()->GetStatus() != Gizmo::EStatus::SelectedY) amount.y = 0.f;
            }

            ObjectList lst;
            if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, 1, 0))
                for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++) (*_F)->Scale(amount);*/
        }
        else    if (LTools->GetGimzo()->GetType() == Gizmo::EType::Rotate)
        {
            if (_Shift & ssLeft)
            {
                float famount = 0;
                Fvector amount;
                amount.set(1, 1, 1).mul(UI->m_MouseSR* UI->m_DeltaCpH.x + -UI->m_MouseSR * UI->m_DeltaCpH.y);

                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedX)
                {
                    famount =-amount.z;
                }
                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedZ)
                {
                    famount = amount.x;
                }
                if (LTools->GetGimzo()->GetStatus() == Gizmo::EStatus::SelectedY)
                {
                    famount = amount.y;
                }

                if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Rotate))
                {
                    float Mul = LTools->GetGimzo()->GetStep(Gizmo::EType::Rotate) / 22.f;
                    Mul = std::max(Mul, 1.f);
                    famount *= Mul;
                }
            
                famount = -famount;

                if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Rotate)) CHECK_SNAP(m_fRotateSnapAngle, famount,deg2rad( LTools->GetGimzo()->GetStep(Gizmo::EType::Rotate)));
                RotateProcess(famount);
              
            }
        }
        return;
    }

    if (bBoxSelection) UI->UpdateSelectionRect(UI->m_StartCp,UI->m_CurrentCp);
}

bool  TUI_CustomControl::SelectEnd(TShiftState _Shift)
{
    if (LTools->GetGimzo()->IsFixed())
    {
        LTools->GetGimzo()->Clear();
        Scene->UndoSave();
        return true;
    }
    else if (bBoxSelection){
        UI->EnableSelectionRect( false );
        bBoxSelection = false;
        Scene->FrustumSelect(_Shift&ssAlt?0:1,LTools->CurrentClassID());
    }
    return true;
}

bool  TUI_CustomControl::DefaultMovingProcess(TShiftState Shift, Fvector& amount)
{
   
    return false;
}
