#include "Stdafx.h"

#include "UIMapWndActions.h"
#include "UIMap.h"
#include "UIMapWnd.h"

FRbmkMapActionPlanner::FRbmkMapActionPlanner(CUIMapWnd* InOwner): Owner(InOwner), EndMovingTime(0), TargetZoom(0), DesiredMapRect()
{
}

void FRbmkMapActionPlanner::Update()
{
	constexpr float MapResizeSpeed		= 350.f;	
	constexpr float	MapZoomTime			= 0.5f;		
	constexpr float	MinMoveTime			= 0.25f;	
			
	auto Initialize = [this]()
	{
		float Distance				= this->Owner->GlobalMap()->CalcOpenRect(this->Owner->m_tgtCenter,DesiredMapRect,TargetZoom);
		bool NeedMove				= !fis_zero(Distance,EPS_L);
		bool NeedZoom				= !fsimilar(TargetZoom, this->Owner->GlobalMap()->GetCurrentZoom().x, EPS_L);
		
		EndMovingTime				= Device.fTimeGlobal;
		
		if (NeedZoom&&NeedMove)
		{
			EndMovingTime += std::max(MapZoomTime,Distance/MapResizeSpeed);
		}
		else if (NeedZoom)
		{
			EndMovingTime += MapZoomTime;
		}
		else if (NeedMove)
		{
			EndMovingTime += std::max(Distance/MapResizeSpeed, MinMoveTime);
		}
	};

	auto Execute = [this,Initialize]()
	{
		{
			float CurMapZoom			= Owner->GetZoom();
			if(!fsimilar(CurMapZoom,TargetZoom))
			{
				TargetZoom			= CurMapZoom;
				Initialize			();
			}
		}
		
		CUIGlobalMap* UIGlobalMap		= Owner->GlobalMap();
		float GlobalTime				= Device.fTimeGlobal;
		float TimeTo					= EndMovingTime-GlobalTime;
		float DeltaTime					= _min(Device.fTimeDelta,TimeTo);
		if(EndMovingTime > Device.fTimeGlobal)
		{
			Frect CurrentRect = UIGlobalMap->GetWndRect();
			CurrentRect.x1 += ((DesiredMapRect.x1-CurrentRect.x1)/TimeTo)*DeltaTime;
			CurrentRect.y1 += ((DesiredMapRect.y1-CurrentRect.y1)/TimeTo)*DeltaTime;
			CurrentRect.x2 += ((DesiredMapRect.x2-CurrentRect.x2)/TimeTo)*DeltaTime;
			CurrentRect.y2 += ((DesiredMapRect.y2-CurrentRect.y2)/TimeTo)*DeltaTime;
			UIGlobalMap->SetWndRect	(CurrentRect);
		}
		else
		{
			UIGlobalMap->SetWndRect			(DesiredMapRect);
			CurrentState = 4;
		}	

		UIGlobalMap->Update					();
		Owner->UpdateScroll		();
	};

	auto TargetMapShown = [this] () 
	{
		Fvector2 Point = Owner->m_tgtCenter; 
		Point.mul (Owner->GlobalMap()->GetCurrentZoom());
		Fvector2 AbsolutePosition;
		Owner->GlobalMap()->GetAbsolutePos(AbsolutePosition);
		Point.add (AbsolutePosition);
		Frect Rect = Owner->ActiveMapRect();
		Rect.grow(Rect.width(),Rect.height());
		if (Rect.in(Point))
		{
			return true;
		}
		return false;
	};
	
	if(CurrentState == 0)
	{
		TargetZoom = Owner->GlobalMap()->GetMinZoom();
		Initialize();
		EndMovingTime 	= Device.fTimeGlobal+MapZoomTime;
		CurrentState++;
	}
	if(CurrentState == 1)
	{
		Execute();
		if(TargetMapShown())
		{
			CurrentState++;
			Owner->UpdateScroll();
		}
	}
	if(CurrentState == 2)
	{
		TargetZoom = Owner->GetZoom();
		Initialize();
		CurrentState++;
	}
	if(CurrentState == 3)
	{
		Execute();
	}
}

void FRbmkMapActionPlanner::Reset()
{
	CurrentState = 0;
}
