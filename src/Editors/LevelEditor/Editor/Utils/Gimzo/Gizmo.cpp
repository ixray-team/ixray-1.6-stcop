#include "stdafx.h"

Gizmo::Gizmo()
{
    m_bFixed = false;
    m_bVisible = false;
    m_CurrentStatus = EStatus::None;
    m_Type = EType::Move;
}

Gizmo::~Gizmo()
{
}

void Gizmo::Render()
{
    if (!m_bVisible||Scene->IsPlayInEditor())return;
    RCache.set_Z(false);
    if (m_Type == EType::Move)
    {
        Fvector Start = m_ScreenPosition;
        Fvector X; X.set(1, 0, 0); X.add(m_ScreenPosition);
        Fvector Y; Y.set(0, 1, 0); Y.add(m_ScreenPosition);
        Fvector Z; Z.set(0, 0, 1); Z.add(m_ScreenPosition);

        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
			DU_impl.DrawLine(Start, X, color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255));
        }
		DU_impl.DrawLine(Start, Y, color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255)); 
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            DU_impl.DrawLine(Start, Z, color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255));
        }
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            DU_impl.DrawCone(Fidentity, X, Fvector().set(-1, 0, 0), 0.1f, 0.03f, color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255), color_rgba(255, 0, 0, 255), TRUE, FALSE);
        }
        DU_impl.DrawCone(Fidentity, Y, Fvector().set(0, -1, 0), 0.1f, 0.03f, color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255), color_rgba(0, 255, 0, 255), TRUE, FALSE);
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            DU_impl.DrawCone(Fidentity, Z, Fvector().set(0, 0, -1), 0.1f, 0.03f, color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255), color_rgba(0, 0, 255, 255), TRUE, FALSE);
        }
    }
    else  if (m_Type == EType::Scale)
    {
        Fvector Start = m_ScreenPosition;
        Fvector X; X.set(1, 0, 0); X.add(m_ScreenPosition);
        Fvector Y; Y.set(0, 1, 0); Y.add(m_ScreenPosition);
        Fvector Z; Z.set(0, 0, 1); Z.add(m_ScreenPosition);

        DU_impl.DrawLine(Start, X, color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255));
        DU_impl.DrawLine(Start, Y, color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255));
        DU_impl.DrawLine(Start, Z, color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255));

        DU_impl.DrawBox(m_ScreenPosition, Fvector().set(0.06f, 0.06f, 0.06f), TRUE, FALSE, m_CurrentStatus == EStatus::SelectedXYZ?color_rgba(200, 200, 200, 255): color_rgba(127, 127, 127, 255), color_rgba(255, 0, 0, 255));

        DU_impl.DrawBox( X,Fvector().set(0.03f, 0.03f, 0.03f), TRUE, FALSE, color_rgba((m_CurrentStatus == EStatus::SelectedX || m_CurrentStatus == EStatus::SelectedXYZ) ? 255 : 127, 0, 0, 255), color_rgba(255, 0, 0, 255));
        DU_impl.DrawBox(Y, Fvector().set(0.03f, 0.03f, 0.03f), TRUE, FALSE, color_rgba(0,( m_CurrentStatus == EStatus::SelectedY || m_CurrentStatus == EStatus::SelectedXYZ) ? 255 : 127, 0, 255), color_rgba(0, 255, 0, 255));
        DU_impl.DrawBox(Z, Fvector().set(0.03f, 0.03f, 0.03f), TRUE, FALSE, color_rgba(0, 0,( m_CurrentStatus == EStatus::SelectedZ || m_CurrentStatus == EStatus::SelectedXYZ) ? 255 : 127, 255), color_rgba(0, 0, 255, 255));
    }
    else  if (m_Type == EType::Rotate)
    {
        Fvector Start = m_ScreenPosition;
        Fvector X; X.set(1, 0, 0); 
        Fvector Y; Y.set(0, 1, 0); 
        Fvector Z; Z.set(0, 0, 1);
		m_RotateMatrix.transform_dir(X);
		m_RotateMatrix.transform_dir(Y);
		m_RotateMatrix.transform_dir(Z);
		X.normalize_safe(); 
		Y.normalize_safe();
		Z.normalize_safe();
		X.mul(-1);
		Y.mul(-1);
		Z.mul(-1);
        Fvector XLine,YLine,ZLine;
        XLine.sub(m_ScreenPosition,X);
        YLine.sub(m_ScreenPosition,Y);
        ZLine.sub(m_ScreenPosition,Z);

        DU_impl.DrawLine(Start, ZLine, color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255));
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            DU_impl.DrawLine(Start, XLine, color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255));
        }
        DU_impl.DrawLine(Start, YLine, color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255));

        DU_impl.DrawCylinder(Fidentity, m_ScreenPosition, X, 0.001f, 1.f, color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255), color_rgba(m_CurrentStatus == EStatus::SelectedX ? 255 : 127, 0, 0, 255), FALSE, TRUE);
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            DU_impl.DrawCylinder(Fidentity, m_ScreenPosition, Y, 0.001f, 1.f, color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255), color_rgba(0, m_CurrentStatus == EStatus::SelectedY ? 255 : 127, 0, 255), FALSE, TRUE);
        }
        DU_impl.DrawCylinder(Fidentity, m_ScreenPosition, Z, 0.001f, 1.f, color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255), color_rgba(0, 0, m_CurrentStatus == EStatus::SelectedZ ? 255 : 127, 255), FALSE, TRUE);

    }
    
    RCache.set_Z(true); 
}

void Gizmo::OnFrame()
{
    m_RotateMatrix.identity();
    if (m_Type == EType::None)  
    {
        return;
    }
    Fbox Box; Box.invalidate(); 
    size_t SelectedCount = 0;
    bool bIsPointMode = LTools->GetSubTarget() == estWayModePoint;
    if (OBJCLASS_AIMAP == LTools->CurrentClassID())
    {
		if (m_Type == EType::Scale)
		{
            m_bFixed = false;
            m_bVisible = false;
            m_CurrentStatus = EStatus::None;
			return;
		}
        m_Position.set(0, 0, 0);
        SelectedCount = 0;
		AINodeVec& lst = ((ESceneAIMapTool*)Scene->GetTool(OBJCLASS_AIMAP))->Nodes();
		for (AINodeIt _F = lst.begin(); _F != lst.end(); _F++)
			if ((*_F)->flags.is(SAINode::flSelected))
			{
				SelectedCount++;
                m_Position.add((*_F)->Pos);
			}
        m_Position.div(SelectedCount);
        SelectedCount = 1;
    }
    else if (OBJCLASS_DUMMY == LTools->CurrentClassID())
    {
        SceneToolsMapPairIt _I = Scene->FirstTool();
        SceneToolsMapPairIt _E = Scene->LastTool();
        for (; _I != _E; _I++)
        {
            ESceneCustomOTool* ObjectTool = _I->second->CastObjectTool();
            if ((_I->first != OBJCLASS_DUMMY) && ObjectTool)
            {
                for (auto& Object : ObjectTool->GetObjects())
                {
                    if (!Object->Selected())continue;
                    m_ScreenPosition = Object->GetPosition();
                    Fbox ObjectBox;
                    if (Object->GetBox(ObjectBox))
                    {
                        Box.merge(ObjectBox);
                    }
                    SelectedCount++;
                }
            }
        }
    }
    else 
    {
        ESceneToolBase* mt = Scene->GetTool(LTools->CurrentClassID());
        if (mt)
        {
            ESceneCustomOTool* ObjectTool = mt->CastObjectTool();
            if (ObjectTool)
            {
                for (auto& Object : ObjectTool->GetObjects())
                {
                    if (!Object->Selected())continue;
                    if (bIsPointMode&& Object->FClassID == OBJCLASS_WAY)
                    {
                        CWayObject* Way = (CWayObject*)Object;
                        for (WPIt it = Way->m_WayPoints.begin(); it != Way->m_WayPoints.end(); it++)
                        {
                            if ((*it)->m_bSelected) 
                            {
                                m_Position = (*it)->m_vPosition;
                                m_RotateMatrix.identity();
								Fbox ObjectBox;
                                (*it)->GetBox(ObjectBox);
                                Box.merge(ObjectBox);
								SelectedCount++;
                            }
                        }
							
                    }
                    else
                    {
						m_Position = Object->GetPosition();
						m_RotateMatrix.setXYZ(Object->GetRotation());
						Fbox ObjectBox;
						if (Object->GetBox(ObjectBox))
						{
							Box.merge(ObjectBox);
						}
						SelectedCount++;
                    }
                 
                }
            }
        }
    }
    if (SelectedCount > 1)
	{
        m_RotateMatrix.identity();
        Box.getcenter(m_Position);
    }
    Fvector ViewNormal = EDevice->m_Camera.GetPosition();
    ViewNormal.sub(m_Position);
    ViewNormal.normalize_safe();
    Fvector CameraRight = EDevice->m_Camera.GetDirection();

    m_bVisible =  SelectedCount > 0;
    float DotProduct = CameraRight.dotproduct(ViewNormal);
    m_bVisible = m_bVisible && DotProduct <0;
    if(m_bVisible)
    {
        RCache.set_xform_world(Fidentity);

        Fvector3 ScreenPosition;
        RCache.xforms.m_wvp.transform(ScreenPosition, m_Position);



        Fvector start, dir;
        Ivector2 pt;

        static int _wh = 50;
        static float _kl = 5.0f;

        pt.x = UI->GetRenderWidth() * ((ScreenPosition.x + 1) / 2);
        pt.y = UI->GetRenderHeight() * (1.f - ((ScreenPosition.y + 1) / 2));

        EDevice->m_Camera.MouseRayFromPoint(m_ScreenPosition, dir, pt);
        m_ScreenPosition.mad(dir, _kl);
    }

    if (m_bFixed)return;
    m_StartPosition = m_Position;
    auto OldStatus = m_CurrentStatus;
    m_CurrentStatus = EStatus::None;

    if (!m_bVisible)return;

    if (m_Type == EType::Move)
    {
        Fbox XBox;
        Fbox YBox;
        Fbox ZBox;

        XBox.set(0, -0.03, -0.03, 1, 0.03, 0.03);
        XBox.offset(m_ScreenPosition);
        YBox.set(-0.03, 0, -0.03, 0.03, 1, 0.03);
        YBox.offset(m_ScreenPosition);
        ZBox.set(-0.03, -0.03, 0, 0.03, 0.03, 1);
        ZBox.offset(m_ScreenPosition);
        if (OBJCLASS_AIMAP != LTools->CurrentClassID()&&XBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedX;
        else  if (YBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedY;
        else  if (OBJCLASS_AIMAP != LTools->CurrentClassID()&&ZBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedZ;
    }
    else   if (m_Type == EType::Scale)
    {
        Fbox XYZBox;
        Fbox XBox;
        Fbox YBox;
        Fbox ZBox;
        XYZBox.set(-0.06, -0.06, -0.06, 0.06, 0.06, 0.06);
        XYZBox.offset(m_ScreenPosition);

        XBox.set(0, -0.03, -0.03, 1, 0.03, 0.03);
        XBox.offset(m_ScreenPosition);
        YBox.set(-0.03, 0, -0.03, 0.03, 1, 0.03);
        YBox.offset(m_ScreenPosition);
        ZBox.set(-0.03, -0.03, 0, 0.03, 0.03, 1);
        ZBox.offset(m_ScreenPosition);
        if (XYZBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedXYZ;
        else if (XBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedX;
        else  if (YBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedY;
        else  if (ZBox.Pick(UI->m_CurrentRStart, UI->m_CurrentRDir))
            m_CurrentStatus = EStatus::SelectedZ;
    }
    else   if (m_Type == EType::Rotate)
    {
        Fcylinder Xcylinder;
        Fcylinder Ycylinder;
        Fcylinder Zcylinder;


        Fsphere sphere;


		Fvector X; X.set(1, 0, 0);
		Fvector Y; Y.set(0, 1, 0);
		Fvector Z; Z.set(0, 0, 1);
		m_RotateMatrix.transform_dir(X);
		m_RotateMatrix.transform_dir(Y);
		m_RotateMatrix.transform_dir(Z);
		X.normalize_safe();
		Y.normalize_safe();
		Z.normalize_safe();
		X.mul(-1);
		Y.mul(-1);
		Z.mul(-1);

        Xcylinder.m_center = m_ScreenPosition;
        Xcylinder.m_direction = X;
        Xcylinder.m_height = 0.05f;
        Xcylinder.m_radius = 1.f;

        Ycylinder.m_center = m_ScreenPosition;
        Ycylinder.m_direction=Y;
        Ycylinder.m_height = 0.05f;
        Ycylinder.m_radius = 1.f;


        Zcylinder.m_center = m_ScreenPosition;
        Zcylinder.m_direction=Z;
        Zcylinder.m_height = 0.05f;
        Zcylinder.m_radius = 1.f;

        sphere.P = m_ScreenPosition;
        sphere.R = 0.8f;

        bool SphereIntersect = false;
        float SphereDist = FLT_MAX;
        SphereIntersect = sphere.intersect2(UI->m_CurrentRStart, UI->m_CurrentRDir, SphereDist);

       
		bool XcylinderIntersect = false;
		float XcylinderDist = FLT_MAX;
        XcylinderIntersect = Xcylinder.intersect(UI->m_CurrentRStart, UI->m_CurrentRDir, XcylinderDist);



		bool YcylinderIntersect = false;
		float YcylinderDist = FLT_MAX;
		YcylinderIntersect = Ycylinder.intersect(UI->m_CurrentRStart, UI->m_CurrentRDir, YcylinderDist);



		bool ZcylinderIntersect = false;
		float ZcylinderDist = FLT_MAX;
		ZcylinderIntersect = Zcylinder.intersect(UI->m_CurrentRStart, UI->m_CurrentRDir, ZcylinderDist);
        float Dist = FLT_MAX;
        if (SphereIntersect)
		{
            Dist = SphereDist;
        }
			
        if (XcylinderIntersect && Dist > XcylinderDist)
        {
            Dist = XcylinderDist;
            m_CurrentStatus = EStatus::SelectedX;
        }
        if (OBJCLASS_AIMAP != LTools->CurrentClassID())
        {
            if (YcylinderIntersect && Dist > YcylinderDist)
            {
                Dist = YcylinderDist;
                m_CurrentStatus = EStatus::SelectedY;
            }
        }
		if (ZcylinderIntersect && Dist > ZcylinderDist)
		{
			Dist = ZcylinderDist;
			m_CurrentStatus = EStatus::SelectedZ;
		}
    }
    if (OldStatus != m_CurrentStatus)
    {
        LUI->RedrawScene();
    }
}

void Gizmo::Clear()
{
    m_CurrentStatus = EStatus::None;
    m_bFixed = false;
}

void Gizmo::Fixed()
{
    m_bFixed = true;
}

void Gizmo::SetStep(EType Type, float Step)
{
    m_Steps[static_cast<int>(Type)] = Step;
}

float Gizmo::GetStep(EType Type) const
{
    return m_Steps[static_cast<int>(Type)];
}

void Gizmo::SwitchStep(EType Type, bool Enable)
{
    m_StepEnable[static_cast<int>(Type)] = Enable;
}

bool Gizmo::IsStepEnable(EType Type) const
{
    return  m_StepEnable[static_cast<int>(Type)];
}

