#include "stdafx.h"
#include "UIStaticItem.h"
#include "ui_base.h"

UI_API void CreateUIGeom()
{
	UIRender->CreateUIGeom();
}

UI_API void DestroyUIGeom()
{
	UIRender->DestroyUIGeom();
}

CUIStaticItem::CUIStaticItem()
{    
	uFlags.zero			();
	vSize.set			(0,0);
	TextureRect.set	(0,0,0,0);
	vHeadingPivot.set	(0,0); 
	vHeadingOffset.set	(0,0);
	dwColor				= 0xffffffff;
}

void CUIStaticItem::ResetHeadingPivot()
{
	uFlags.set(flValidHeadingPivot, false); 
	uFlags.set(flFixedLTWhileHeading,false);
}

void CUIStaticItem::SetHeadingPivot(const Fvector2& p, const Fvector2& offset, bool fixedLT)		
{
	vHeadingPivot=p; 
	vHeadingOffset=offset; 
	uFlags.set(flValidHeadingPivot, true); 
	if(fixedLT)
		uFlags.set(flFixedLTWhileHeading,true);
	else
		uFlags.set(flFixedLTWhileHeading,false);
}

void CUIStaticItem::RenderInternal(const Fvector2& in_pos)
{
	Fvector2					pos;
	UI().ClientToScreenScaled	(pos, in_pos.x, in_pos.y);
	UI().AlignPixel				(pos.x);
	UI().AlignPixel				(pos.y);

	Fvector2		ts;
	UIRender->GetActiveTextureResolution(ts);

	if(!uFlags.test(flValidSize))
		SetSize(ts);

	if(!uFlags.test(flValidTextureRect))
		SetTextureRect(Frect().set(0,0,ts.x,ts.y));

	Fvector2 LTp,RBp;
	Fvector2 LTt,RBt;
	//координаты на экране в пикселях
	LTp.set						(pos);

	UI().ClientToScreenScaled	(RBp, vSize.x, vSize.y);
	RBp.add						(pos);

	//текстурные координаты
	LTt.set			( TextureRect.x1/ts.x, TextureRect.y1/ts.y);
	RBt.set			( TextureRect.x2/ts.x, TextureRect.y2/ts.y);

	float offset	= -0.5f;
	if(UI().m_currentPointType==IUIRender::pttLIT)
		offset		= 0.0f;
	
	// clip poly
	sPoly2D			S; 
	S.resize		(4);

	LTp.x			+=offset;
	LTp.y			+=offset;
	RBp.x			+=offset;
	RBp.y			+=offset;

	S[0].set		(LTp.x,	LTp.y,	LTt.x,	LTt.y);	// LT
	S[1].set		(RBp.x,	LTp.y,	RBt.x,	LTt.y);	// RT
	S[2].set		(RBp.x,	RBp.y,	RBt.x,	RBt.y);	// RB
	S[3].set		(LTp.x,	RBp.y,	LTt.x,	RBt.y);	// LB
	
	sPoly2D D;
	sPoly2D* R		= nullptr;

	if(UI().m_currentPointType!=IUIRender::pttLIT)
		R			= UI().ScreenFrustum().ClipPoly(S,D);
	else
	{
		R			= UI().ScreenFrustumLIT().ClipPoly(S,D);
	}

	if (R && R->size())
	{
		for (u32 k=0; k<R->size()-2; ++k)
		{
			UIRender->PushPoint((*R)[0+0].pt.x, (*R)[0+0].pt.y,	0, dwColor, (*R)[0+0].uv.x, (*R)[0+0].uv.y);
			UIRender->PushPoint((*R)[k+1].pt.x, (*R)[k+1].pt.y,	0, dwColor, (*R)[k+1].uv.x, (*R)[k+1].uv.y);
			UIRender->PushPoint((*R)[k+2].pt.x, (*R)[k+2].pt.y,	0, dwColor, (*R)[k+2].uv.x, (*R)[k+2].uv.y);
		}
	}
}

void CUIStaticItem::RenderInternal(float angle, bool CorrectedAspect)
{
	Fvector2		ts;
	Fvector2		hp;

	UIRender->GetActiveTextureResolution(ts);
	hp.set			(0.5f/ts.x,0.5f/ts.y);

	if(!uFlags.test(flValidSize))
		SetSize(ts);

	if(!uFlags.test(flValidTextureRect))
		SetTextureRect(Frect().set(0,0,ts.x,ts.y));

	Fvector2 pivot,offset,SZ;
	SZ.set(vSize);


	float cosA = std::cos(angle);
	float sinA = std::sin(angle);

	// Rotation
	if( !uFlags.test(flValidHeadingPivot) )	
		pivot.set(vSize.x/2.f, vSize.y/2.f);
	else								
		pivot.set(vHeadingPivot.x, vHeadingPivot.y);

	offset.set(vPos);
	offset.add(vHeadingOffset);

	Fvector2 LTt,RBt;
	LTt.set(TextureRect.x1/ts.x+hp.x, TextureRect.y1/ts.y+hp.y);
	RBt.set(TextureRect.x2/ts.x+hp.x, TextureRect.y2/ts.y+hp.y);

	float kx = CorrectedAspect ? 1.0f : UI().get_current_kx();

	// clip poly
	sPoly2D								S; 
	S.resize							(4);
	// LT
	S[0].set		(0.f,0.f,LTt.x,LTt.y);
	S[0].rotate_pt	(pivot,cosA,sinA,kx);
	S[0].pt.add		(offset);

	// RT
	S[1].set		(SZ.x,0.f,RBt.x,LTt.y);
	S[1].rotate_pt	(pivot,cosA,sinA,kx);
	S[1].pt.add		(offset);
	// RB
	S[2].set		(SZ.x,SZ.y,RBt.x,RBt.y);
	S[2].rotate_pt	(pivot,cosA,sinA,kx);
	S[2].pt.add		(offset);
	// LB
	S[3].set		(0.f,SZ.y,LTt.x,RBt.y);
	S[3].rotate_pt	(pivot,cosA,sinA,kx);
	S[3].pt.add		(offset);

	for(int i=0; i<4;++i)
		UI().ClientToScreenScaled		(S[i].pt);

	sPoly2D D;
	sPoly2D* R		= UI().ScreenFrustum().ClipPoly(S,D);
	if (R&&R->size()){
		for (u32 k=0; k<R->size()-2; k++)
		{
			UIRender->PushPoint((*R)[0+0].pt.x, (*R)[0+0].pt.y,	0, dwColor, (*R)[0+0].uv.x, (*R)[0+0].uv.y);
			UIRender->PushPoint((*R)[k+1].pt.x, (*R)[k+1].pt.y,	0, dwColor, (*R)[k+1].uv.x, (*R)[k+1].uv.y);
			UIRender->PushPoint((*R)[k+2].pt.x, (*R)[k+2].pt.y,	0, dwColor, (*R)[k+2].uv.x, (*R)[k+2].uv.y);
		}
	}
}

//---from static-item

void CUIStaticItem::Render()
{
	VERIFY						(g_bRendering);
	UIRender->SetShader			(*hShader);
	UIRender->StartPrimitive	(8, IUIRender::ptTriList, UI().m_currentPointType);
	RenderInternal				(vPos);
	UIRender->FlushPrimitive	();
}

void CUIStaticItem::Render(float angle)
{
	Render(angle, false);
}

void CUIStaticItem::Render(float Angle, bool CorrectedAspect)
{
	VERIFY(g_bRendering);

	UIRender->SetShader(*hShader);
	UIRender->StartPrimitive(32, IUIRender::ptTriList, UI().m_currentPointType);
	RenderInternal(Angle, CorrectedAspect);
	UIRender->FlushPrimitive();
}


void CUIStaticItem::RenderBlurredShadow(const Fvector2& Light, float ProjectionScale, float LightHeight, float LightRadius,
	float Radius, float Sigma, float Falloff, float MinOpacityRatio, u32 Color, bool Rotated, float Angle, bool CorrectedAspect)
{
	VERIFY(g_bRendering);
	UIRender->SetShader(*hShader);
	Fvector2 TextureSize;
	UIRender->GetActiveTextureResolution(TextureSize);
	if (TextureSize.x <= 0.0f || TextureSize.y <= 0.0f)
	{
		return;
	}
	if (!uFlags.test(flValidSize))
	{
		SetSize(TextureSize);
	}
	if (!uFlags.test(flValidTextureRect))
	{
		SetTextureRect(Frect().set(0.0f, 0.0f, TextureSize.x, TextureSize.y));
	}
	if (vSize.x <= 0.0f || vSize.y <= 0.0f || TextureRect.width() <= 0.0f || TextureRect.height() <= 0.0f)
	{
		return;
	}
	const UITransform2D& Transform = UIRender->Transform2D;
	const float Determinant = Transform.XX * Transform.YY - Transform.XY * Transform.YX;
	if (!std::isfinite(ProjectionScale) || !std::isfinite(Determinant) || std::abs(Determinant) < 1e-8f)
	{
		return;
	}
	const UITransform2D Inverse = Transform.Inverted();
	const Frect UVRect = Frect().set(TextureRect.x1 / TextureSize.x, TextureRect.y1 / TextureSize.y,
		TextureRect.x2 / TextureSize.x, TextureRect.y2 / TextureSize.y);
	const Fvector2 Pivot = uFlags.test(flValidHeadingPivot) ? vHeadingPivot : Fvector2().set(vSize.x * 0.5f, vSize.y * 0.5f);
	Fvector2 Origin = vPos;
	if (Rotated)
	{
		Origin.add(vHeadingOffset);
	}
	const auto RootPoint = [&](float X, float Y)
	{
		S2DVert Point;
		Point.set(X, Y, 0.0f, 0.0f);
		if (Rotated)
		{
			Point.rotate_pt(Pivot, std::cos(Angle), std::sin(Angle), CorrectedAspect ? 1.0f : UI().get_current_kx());
		}
		Point.pt.add(Origin);
		Fvector2 Result;
		UI().ClientToScreenScaled(Result, Point.pt.x, Point.pt.y);
		Transform.Apply(Result.x, Result.y);
		return Result;
	};
	Fvector2 PixelScale, LightPosition;
	UI().ClientToScreenScaled(PixelScale, 1.0f, 1.0f);
	UI().ClientToScreenScaled(LightPosition, Light.x, Light.y);
	if (PixelScale.x <= 0.0f || PixelScale.y <= 0.0f)
	{
		return;
	}
	Fvector2 Padding = Fvector2().set(Sigma > 0.0f ? Radius : 0.0f, Sigma > 0.0f ? Radius : 0.0f);
	Fvector2 UVRadius = Fvector2().set(Padding.x * UVRect.width() / vSize.x, Padding.y * UVRect.height() / vSize.y);
	Fvector2 UVPadding = UVRadius;
	Fvector4 LightParams = Fvector4().set(0.0f, 0.0f, 1.0f, 0.0f);
	Fvector4 Basis = Fvector4().set(1.0f, 0.0f, 0.0f, 1.0f);
	const float Depth = LightHeight / ProjectionScale;
	if (LightRadius > 0.0f && LightHeight > 0.0f && Sigma > 0.0f && Radius > 0.0f && ProjectionScale >= 1.0f)
	{
		const auto UIPlanePoint = [&](float X, float Y)
		{
			Fvector2 Result = RootPoint(X, Y);
			Result.x /= PixelScale.x;
			Result.y /= PixelScale.y;
			return Result;
		};
		const Fvector2 Corner0 = UIPlanePoint(0.0f, 0.0f);
		const Fvector2 Corner1 = UIPlanePoint(vSize.x, 0.0f);
		const Fvector2 Corner3 = UIPlanePoint(0.0f, vSize.y);
		const Fvector2 AxisU = Fvector2().set((Corner1.x - Corner0.x) / UVRect.width(), (Corner1.y - Corner0.y) / UVRect.width());
		const Fvector2 AxisV = Fvector2().set((Corner3.x - Corner0.x) / UVRect.height(), (Corner3.y - Corner0.y) / UVRect.height());
		const float BasisDet = AxisU.x * AxisV.y - AxisU.y * AxisV.x;
		if (!std::isfinite(Depth) || Depth <= 1e-3f || std::abs(BasisDet) < 1e-8f)
		{
			return;
		}
		const Fvector2 InverseU = Fvector2().set(AxisV.y / BasisDet, -AxisV.x / BasisDet);
		const Fvector2 InverseV = Fvector2().set(-AxisU.y / BasisDet, AxisU.x / BasisDet);
		const Fvector2 ToLight = Fvector2().sub(Light, Corner0);
		const float LightU = UVRect.x1 + InverseU.x * ToLight.x + InverseU.y * ToLight.y;
		const float LightV = UVRect.y1 + InverseV.x * ToLight.x + InverseV.y * ToLight.y;
		float MaxSlopeSquared = 0.0f;
		for (int Corner = 0; Corner < 4; ++Corner)
		{
			const Fvector2 Point = UIPlanePoint((Corner & 1) ? vSize.x : 0.0f, (Corner & 2) ? vSize.y : 0.0f);
			const float DX = Point.x - Light.x, DY = Point.y - Light.y;
			MaxSlopeSquared = std::max(MaxSlopeSquared, (DX * DX + DY * DY) / (Depth * Depth));
		}
		const float MaxStretch = std::sqrt(1.0f + MaxSlopeSquared);
		// Finite spherical emitter: r*h/(H-h) on the receiving plane.
		const float BaseRadius = LightRadius * (ProjectionScale - 1.0f) / ProjectionScale;
		const float Bound = BaseRadius * MaxStretch * std::sqrt(2.0f);
		UVPadding.set(Bound * std::sqrt(InverseU.x * InverseU.x + InverseU.y * InverseU.y),
			Bound * std::sqrt(InverseV.x * InverseV.x + InverseV.y * InverseV.y));
		Padding.set(UVPadding.x * vSize.x / UVRect.width(), UVPadding.y * vSize.y / UVRect.height());
		UVRadius.set(BaseRadius, BaseRadius);
		LightParams.set(LightU, LightV, Depth, MaxStretch);
		Basis.set(AxisU.x, AxisV.x, AxisU.y, AxisV.y);
	}
	const bool ScreenSpace = UI().m_currentPointType != IUIRender::pttLIT;
	Fvector2 Positions[4];
	for (int Corner = 0; Corner < 4; ++Corner)
	{
		const float X = (Corner == 1 || Corner == 2) ? vSize.x + Padding.x : -Padding.x;
		const float Y = Corner >= 2 ? vSize.y + Padding.y : -Padding.y;
		Positions[Corner] = RootPoint(X, Y);
		Positions[Corner].x = LightPosition.x + (Positions[Corner].x - LightPosition.x) * ProjectionScale;
		Positions[Corner].y = LightPosition.y + (Positions[Corner].y - LightPosition.y) * ProjectionScale;
		Inverse.Apply(Positions[Corner].x, Positions[Corner].y);
		if (ScreenSpace)
		{
			Positions[Corner].sub(Fvector2().set(0.5f, 0.5f));
		}
	}
	UIRender->DrawBlurredTextureShadow(*hShader, Positions, UVRect, UVRadius, UVPadding, LightParams, Basis,
		Radius > 0.0f ? Sigma / Radius : 0.0f, Falloff, MinOpacityRatio, Color, ScreenSpace);
}

void CUIStaticItem::CreateShader(const char* tex, const char* sh)
{
	hShader->create(sh,tex);

#ifdef DEBUG
	dbg_tex_name = tex;
#endif
	uFlags.set(flValidSize, false);
	uFlags.set(flValidTextureRect, false);
}


void CUIStaticItem::SetShader(const ui_shader& sh)
{
	hShader = sh;
}

void CUIStaticItem::Init(const char* tex, const char* sh, float left, float top)
{
	uFlags.set		(flValidSize, false);
	CreateShader	(tex,sh);
	SetPos			(left,top);
}
