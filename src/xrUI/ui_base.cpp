#include "stdafx.h"
#include "ui_base.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/Render.h"
#include "UICursor.h"
#include "Widgets/UIWindow.h"

#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/XR_IOConsole.h"

UI_API ui_core* m_pUI_core = nullptr;

UI_API CUICursor& GetUICursor()	
{
	return UI().GetUICursor();
};

UI_API ui_core& UI()	
{
	return *m_pUI_core;
};

void S2DVert::rotate_pt(const Fvector2& pivot, const float cosA, const float sinA, const float kx)
{
	Fvector2 t		= pt;
	t.sub			(pivot);
	pt.x			= t.x*cosA+t.y*sinA;
	pt.y			= t.y*cosA-t.x*sinA;
	pt.x			*= kx;
	pt.add			(pivot);
}

void C2DFrustum::CreateFromRect	(const Frect& rect)
{
	m_rect.set(float(rect.x1), float(rect.y1), float(rect.x2), float(rect.y2) );
	planes.resize	(4);
	planes[0].build	(rect.lt, Fvector2().set(-1, 0));
	planes[1].build	(rect.lt, Fvector2().set( 0,-1));
	planes[2].build	(rect.rb, Fvector2().set(+1, 0));
	planes[3].build	(rect.rb, Fvector2().set( 0,+1));
}

sPoly2D* C2DFrustum::ClipPoly	(sPoly2D& S, sPoly2D& D) const
{
	bool bFullTest		= false;
	for (u32 j=0; j<S.size(); j++)
	{
		if( !m_rect.in(S[j].pt) ) {
			bFullTest	= true;
			break		;
		}
	}

	sPoly2D*	src		= &D;
	sPoly2D*	dest	= &S;
	if(!bFullTest)		return dest;

	for (u32 i=0; i<planes.size(); i++)
	{
		// cache plane and swap lists
		const Fplane2 &P	= planes[i]	;
		std::swap			(src,dest)	;
		dest->clear			()			;

		// classify all points relative to plane #i
		float cls[UI_FRUSTUM_SAFE]	;
		for (u32 j=0; j<src->size(); j++) cls[j]=P.classify((*src)[j].pt);

		// clip everything to this plane
		cls[src->size()] = cls[0]	;
		src->push_back((*src)[0])	;
		Fvector2 dir_pt,dir_uv;		float denum,t;
		for (u32 j=0; j<src->size()-1; j++)	{
			if ((*src)[j].pt.similar((*src)[j+1].pt,EPS_S)) continue;
			if (negative(cls[j]))	{
				dest->push_back((*src)[j])	;
				if (positive(cls[j+1]))	{
					// segment intersects plane
					dir_pt.sub((*src)[j+1].pt,(*src)[j].pt);
					dir_uv.sub((*src)[j+1].uv,(*src)[j].uv);
					denum = P.n.dotproduct(dir_pt);
					if (denum!=0) {
						t = -cls[j]/denum	; //VERIFY(t<=1.f && t>=0);
						dest->last().pt.mad	((*src)[j].pt,dir_pt,t);
						dest->last().uv.mad	((*src)[j].uv,dir_uv,t);
						dest->inc();
					}
				}
			} else {
				// J - outside
				if (negative(cls[j+1]))	{
					// J+1  - inside
					// segment intersects plane
					dir_pt.sub((*src)[j+1].pt,(*src)[j].pt);
					dir_uv.sub((*src)[j+1].uv,(*src)[j].uv);
					denum = P.n.dotproduct(dir_pt);
					if (denum!=0)	{
						t = -cls[j]/denum	; //VERIFY(t<=1.f && t>=0);
						dest->last().pt.mad	((*src)[j].pt,dir_pt,t);
						dest->last().uv.mad	((*src)[j].uv,dir_uv,t);
						dest->inc();
					}
				}
			}
		}

		// here we end up with complete polygon in 'dest' which is inside plane #i
		if (dest->size()<3) return 0;
	}
	return dest;
}

void ui_core::OnDeviceReset()
{
	m_scale_.set		( float(Device.TargetWidth)/UI_BASE_WIDTH, float(Device.TargetHeight)/UI_BASE_HEIGHT );

	m_2DFrustum.CreateFromRect	(Frect().set(	0.0f,
												0.0f,
												float(Device.TargetWidth),
												float(Device.TargetHeight)
												));
}

void ui_core::ClientToScreenScaled(Fvector2& dest, float left, float top)	const
{
	if(m_currentPointType!=IUIRender::pttLIT)
		dest.set(ClientToScreenScaledX(left),	ClientToScreenScaledY(top));
	else
		dest.set(left,top);
}

void ui_core::ClientToScreenScaled(Fvector2& src_and_dest)	const
{
	if(m_currentPointType!=IUIRender::pttLIT)
		src_and_dest.set(ClientToScreenScaledX(src_and_dest.x),	ClientToScreenScaledY(src_and_dest.y));
}

void ui_core::ClientToScreenScaledWidth(float& src_and_dest)	const
{
	if(m_currentPointType!=IUIRender::pttLIT)
		src_and_dest		/= m_current_scale->x;
}

void ui_core::ClientToScreenScaledHeight(float& src_and_dest)	const
{
	if(m_currentPointType!=IUIRender::pttLIT)
		src_and_dest		/= m_current_scale->y;
}

void ui_core::AlignPixel(float& src_and_dest)	const
{
	if(m_currentPointType!=IUIRender::pttLIT)
		src_and_dest		= (float)iFloor(src_and_dest);
}

void ui_core::PushScissor(const Frect& r_tgt, bool overlapped)
{
	if(UI().m_currentPointType==IUIRender::pttLIT)
		return;

	Frect r_top			= {0.0f, 0.0f, UI_BASE_WIDTH, UI_BASE_HEIGHT};
	Frect result		= r_tgt;
	if (!m_Scissors.empty()&&!overlapped){
		r_top			= m_Scissors.top();
	}
	if (!result.intersection(r_top,r_tgt))
			result.set	(0.0f,0.0f,0.0f,0.0f);

	if (!(result.x1>=0&&result.y1>=0&&result.x2<=UI_BASE_WIDTH&&result.y2<=UI_BASE_HEIGHT) )
	{
		Msg("! r_tgt [%.3f][%.3f][%.3f][%.3f]", r_tgt.x1, r_tgt.y1, r_tgt.x2, r_tgt.y2);
		Msg("! result [%.3f][%.3f][%.3f][%.3f]", result.x1, result.y1, result.x2, result.y2);
		VERIFY(result.x1>=0&&result.y1>=0&&result.x2<=UI_BASE_WIDTH&&result.y2<=UI_BASE_HEIGHT);
	}
	m_Scissors.push		(result);

	result.lt.x 		= ClientToScreenScaledX(result.lt.x);
	result.lt.y 		= ClientToScreenScaledY(result.lt.y);
	result.rb.x 		= ClientToScreenScaledX(result.rb.x);
	result.rb.y 		= ClientToScreenScaledY(result.rb.y);

	Irect				r;
	r.x1 				= iFloor(result.x1);
	r.x2 				= iFloor(result.x2+0.5f);
	r.y1 				= iFloor(result.y1);
	r.y2 				= iFloor(result.y2+0.5f);
	UIRender->SetScissor(&r);
}

void ui_core::PopScissor()
{
	if(UI().m_currentPointType==IUIRender::pttLIT)
		return;

	VERIFY(!m_Scissors.empty());
	m_Scissors.pop		();
	
	if(m_Scissors.empty())
		UIRender->SetScissor(nullptr);
	else{
		const Frect& top= m_Scissors.top();
		Irect tgt;
		tgt.lt.x 		= iFloor(ClientToScreenScaledX(top.lt.x));
		tgt.lt.y 		= iFloor(ClientToScreenScaledY(top.lt.y));
		tgt.rb.x 		= iFloor(ClientToScreenScaledX(top.rb.x));
		tgt.rb.y 		= iFloor(ClientToScreenScaledY(top.rb.y));

		UIRender->SetScissor(&tgt);
	}
}

#ifdef DEBUG_DRAW

#define ArrowMoveStep 0.5f

void ui_core::RenderUIDebugger()
{
	static CUIWindow* Selected = nullptr;
	static xr_vector<CUIWindow*> Roots;

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::UI_General)])
	{
		LastFrameWidgets.clear();
		return;
	}

	auto BuildTree = [&]()
	{
		Roots.clear();
		Roots.reserve(LastFrameWidgets.size());

		for (CUIWindow* Window : LastFrameWidgets)
		{
			CUIWindow* Parent = Window->GetParent();

			if (Parent == nullptr || LastFrameWidgets.count(Parent) == 0)
			{
				Roots.push_back(Window);
			}
		}
	};

	auto GetWndName = [](CUIWindow* WndPtr)->shared_str
	{
		if (WndPtr->WindowName().size() > 0)
		{
			return WndPtr->WindowName();
		}

		return WndPtr->WindowNodeName();
	};

	std::function<void(CUIWindow*)> DrawNode = [&](CUIWindow* Window)
	{
		if (!LastFrameWidgets.contains(Window))
		{
			return;
		}

		ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;

		if (Window == Selected)
		{
			Flags |= ImGuiTreeNodeFlags_Selected;
		}

		bool IsOpen = ImGui::TreeNodeEx(Window, Flags, "%s (%p)", GetWndName(Window).c_str(), Window);

		if (ImGui::IsItemClicked())
		{
			Selected = Window;
		}

		if (IsOpen)
		{
			for (CUIWindow* Child : Window->GetChildWndList())
			{
				DrawNode(Child);
			}

			ImGui::TreePop();
		}
	};

	if (!ImGui::Begin("UI Debugger", &Engine.External.EditorStates[static_cast<u8>(EditorUI::UI_General)]))
	{
		LastFrameWidgets.clear();
		ImGui::End();
		return;
	}

	if (ImGui::Button("Rebuild Tree"))
	{
		BuildTree();
	}
	ImGui::SameLine();

	if (ImGui::Button("Reload UI"))
	{
		Console->Execute("ui_reload");
	}

	ImGui::Separator();

	for (CUIWindow* Root : Roots)
	{
		DrawNode(Root);
	}

	ImGui::Separator();

	if (Selected != nullptr && LastFrameWidgets.contains(Selected))
	{
		ImGui::Text("Selected: %s", GetWndName(Selected).c_str());

		Fvector2 Position = Selected->GetWndPos();
		Fvector2 Size = Selected->GetWndSize();

		if (ImGui::DragFloat2("Position", reinterpret_cast<float*>(&Position), 1.0f))
		{
			Selected->SetWndPos(Position);
		}

		if (ImGui::DragFloat2("Size", reinterpret_cast<float*>(&Size), 1.0f))
		{
			Selected->SetWndSize(Size);
		}
	}
	else
	{
		ImGui::Text("No UI window selected.");
	}

	ImGui::End();

	if (Selected != nullptr && LastFrameWidgets.contains(Selected))
	{
		Fvector2 AbsPos;
		Selected->GetAbsolutePos(AbsPos);

		Fvector2 Size = Selected->GetWndSize();

		AbsPos.x	*= get_current_zx();
		Size.x		*= get_current_zx();
		AbsPos.y	*= get_current_zy();
		Size.y		*= get_current_zy();

		ImDrawList* Draw = ImGui::GetForegroundDrawList();

		bool a = 
			UIDebuggerMouseMove(Selected, 
			(ImGui::GetIO().MousePos.x >= (AbsPos.x) && ImGui::GetIO().MousePos.x <= (AbsPos.x + Size.x) &&
			ImGui::GetIO().MousePos.y >= (AbsPos.y) && ImGui::GetIO().MousePos.y <= (AbsPos.y + Size.y)));

		ImU32 col = a ?
			IM_COL32(255, 50, 50, 220) :
			IM_COL32(50, 255, 500, 220);

		auto ArrowMove = [](ImGuiKey key, auto operation, char axis)
		{
			if (ImGui::IsKeyPressed(key))
			{
				Fvector2 NPosition = Selected->GetWndPos();
				if constexpr (std::is_same_v<decltype(operation), std::minus<>>)
				{
					if (axis == 'x') NPosition.x -= ArrowMoveStep;
					else if (axis == 'y') NPosition.y -= ArrowMoveStep;
				}
				else if constexpr (std::is_same_v<decltype(operation), std::plus<>>)
				{
					if (axis == 'x') NPosition.x += ArrowMoveStep;
					else if (axis == 'y') NPosition.y += ArrowMoveStep;
				}
				Selected->SetWndPos(NPosition);
				return true;
			}
			return false;
		};

		if (ArrowMove(ImGuiKey_UpArrow, std::minus{}, 'y')) {}
		else if (ArrowMove(ImGuiKey_DownArrow, std::plus{}, 'y')) {}
		else if (ArrowMove(ImGuiKey_LeftArrow, std::minus{}, 'x')) {}
		else if (ArrowMove(ImGuiKey_RightArrow, std::plus{}, 'x')) {}

		Draw->AddRect
		(
			ImVec2(AbsPos.x, AbsPos.y),
			ImVec2(AbsPos.x + Size.x, AbsPos.y + Size.y),
			col, 0.0f, 0, 2.0f
		);
	}

	LastFrameWidgets.clear();
}

bool ui_core::UIDebuggerMouseMove(CUIWindow* Selected, bool inside)
{
	static bool lockDrag = false;

	ImVec2 mouse_delta = {};
	if ((ImGui::IsMouseDragging(ImGuiMouseButton_Left) && inside) || (lockDrag && !inside))
	{
		lockDrag = true;
		mouse_delta = ImGui::GetMouseDragDelta(ImGuiMouseButton_Left);

		mouse_delta.x /= get_current_zx();
		mouse_delta.y /= get_current_zy();


		if ((inside || lockDrag) && (mouse_delta.x != 0.0f || mouse_delta.y != 0.0f))
		{
			Fvector2 Position = Selected->GetWndPos();

			Position.x += mouse_delta.x;
			Position.y += mouse_delta.y;

			Selected->SetWndPos(Position);

			ImGui::ResetMouseDragDelta(ImGuiMouseButton_Left);
		}
	}
	else
	{
		lockDrag = false;
	}
#if 0
	ImGui::Begin("dbg_test");
	ImGui::Text("delta %f : %f", mouse_delta.x, mouse_delta.y);
	ImGui::Text("lockdrg %d", lockDrag);
	ImGui::Text("inside %d", inside);

	ImGui::End();
#endif
	return (inside || lockDrag);
}

#endif

ui_core::ui_core()
{
	if(!g_dedicated_server)
	{
		m_pUICursor					= new CUICursor();
	}else
	{
		m_pUICursor					= nullptr;
	}
	m_bPostprocess				= false;
	
	OnDeviceReset				();

	m_current_scale				= &m_scale_;
	m_currentPointType			= IUIRender::pttTL;

#ifdef DEBUG_DRAW
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("UIDEBUG", CImGuiManager::ERenderPriority::eMedium, xr_make_delegate(this, &ui_core::RenderUIDebugger));
	}
#endif
}

ui_core::~ui_core()
{
	xr_delete						(m_pUICursor);
}

void ui_core::pp_start()
{
	m_bPostprocess = true;

	m_pp_scale_.set(float(Device.TargetWidth) / float(UI_BASE_WIDTH), float(Device.TargetHeight) / float(UI_BASE_HEIGHT));
	m_2DFrustumPP.CreateFromRect(Frect().set(0.0f, 0.0f, float(Device.TargetWidth), float(Device.TargetHeight)));

	m_current_scale = &m_pp_scale_;
}

void ui_core::pp_stop()
{
	m_bPostprocess			= false;
	m_current_scale			= &m_scale_;
}

void ui_core::RenderFont()
{
	Font().Render();
}

bool ui_core::is_widescreen()
{
/*	if (is_ultrawide()) // St4lker0k765: if playing on ultrawide monitor, skip widescreen check 
		return true;
		*/
	return (Device.TargetWidth)/float(Device.TargetHeight) > (UI_BASE_WIDTH/UI_BASE_HEIGHT +0.01f);
}

bool ui_core::is_ultrawide()
{
	return (Device.TargetWidth) / float(Device.TargetHeight) > (1366.0f / UI_BASE_HEIGHT + 0.01f);
}

float ui_core::get_current_kx()
{
	float h		= float(Device.TargetHeight);
	float w		= float(Device.TargetWidth);

	float res = (h/w)/(UI_BASE_HEIGHT/UI_BASE_WIDTH);
	return res;
}
float ui_core::get_current_zx()
{
	float w = float(Device.TargetWidth);
	float res = w / UI_BASE_WIDTH;
	return res;
}
float ui_core::get_current_zy()
{
	float h = float(Device.TargetHeight);
	float res = h / UI_BASE_HEIGHT;
	return res;
}
shared_str	ui_core::get_xml_name(const char* fn)
{
	string_path				str;
	if(!is_widescreen())
	{
		xr_sprintf(str, "%s", fn);
		if ( !strext(fn) ) 
			xr_strcat(str, ".xml");
	}
	else if (is_ultrawide())
	{
		string_path			str_;
		if (strext(fn))
		{
			xr_strcpy(str, fn);
			*strext(str) = 0;
			xr_strcat(str, "_21.xml");
		}
		else
			xr_sprintf(str, "%s_21", fn);

		if (!FS.exist(str_, _game_config_, "ui\\", str))
		{
			if (strext(fn))
			{
				xr_strcpy(str, fn);
				*strext(str) = 0;
				xr_strcat(str, "_16.xml");
			}
			else
				xr_sprintf(str, "%s_16", fn);

			if (!FS.exist(str_, _game_config_, "ui\\", str))
			{
				xr_sprintf(str, "%s", fn);
				if (!strext(fn)) 
					xr_strcat(str, ".xml");
			}
		}
#ifdef _DEBUG
		Msg("[21-9] get_xml_name for[%s] returns [%s]", fn, str);
#endif // #ifdef DEBUG
	}
	else
	{

		string_path			str_;
		if ( strext(fn) )
		{
			xr_strcpy	(str, fn);
			*strext(str)	= 0;
			xr_strcat	(str, "_16.xml");
		}
		else
			xr_sprintf				(str, "%s_16", fn);

		if(!FS.exist(str_, _game_config_, "ui\\" , str) )
		{
			xr_sprintf(str, "%s", fn);
			if (!strext(fn) ) 
				xr_strcat(str, ".xml");
		}
#ifdef _DEBUG
		Msg("[16-9] get_xml_name for[%s] returns [%s]", fn, str);
#endif // #ifdef DEBUG
	}
	return str;
}

const ui_shader& ui_core::GetVectorShader(const std::string_view& subpath, float requested_width, float requested_height)
{
	return GetVectorShader(subpath, requested_width, requested_height, SVGTintRGBA{});
}

const ui_shader& ui_core::GetVectorShader(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	R_ASSERT(DevicePtr && "Render must be initialized otherwise early calling!");
	R_ASSERT(DevicePtr->m_pRender && "Resource manager");

	if (DevicePtr == nullptr || DevicePtr->m_pRender == nullptr)
		return m_empty_default;

	return DevicePtr->m_pRender->GetSVGShader(subpath, requested_width, requested_height, tint);
}

const ui_shader& ui_core::GetVectorShader(const char* pSubpath, float requested_width, float requested_height)
{
	R_ASSERT(pSubpath && "invalid string (nullptr)");

	return GetVectorShader(std::string_view(pSubpath), requested_width, requested_height);
}

const ui_shader& ui_core::GetVectorShader(const char* pSubpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	R_ASSERT(pSubpath && "invalid string (nullptr)");

	return GetVectorShader(std::string_view(pSubpath), requested_width, requested_height, tint);
}

Frect ui_core::GetVectorUV(const std::string_view& subpath, float requested_width, float requested_height)
{
	return GetVectorUV(subpath, requested_width, requested_height, SVGTintRGBA{});
}

Frect ui_core::GetVectorUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	if (DevicePtr == nullptr || DevicePtr->m_pRender == nullptr)
		return Frect{};

	return DevicePtr->m_pRender->GetSVGUV(subpath, requested_width, requested_height, tint);
}
