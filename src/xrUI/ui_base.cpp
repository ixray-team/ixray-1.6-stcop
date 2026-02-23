#include "stdafx.h"
#include "ui_base.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/Render.h"
#include "UICursor.h"
#include "Widgets/UIWindow.h"
#include "ui_defs.h"
#include "uiabstract.h"
#include "UILayout.h"

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

float ui_core::ClientToScreenScaledX(float left) const
{
	float result;
	if (m_currentScaleMode == UI_SCALE_MODE_NONE)
	{
		// Use Y scale for X too - preserve aspect ratio (keep squares square)
		result = left * m_current_scale->y;
	}
	else
	{
		result = left * m_current_scale->x;
		if (m_currentScaleMode == UI_SCALE_MODE_WIDESCREEN)
			result *= get_current_kx();
	}
	return result;
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
#ifdef DEBUG_DRAW
	m_ScissorsForDebug.push_back(result);
#endif

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
#define AnchorPointHitRadius 12.0f

static const char* GetWindowTypeName(CUIWindow* wnd)
{
	if (!wnd)
	{
		return "?";
	}
	if (wnd->ui_cast_list() != nullptr)
	{
		return "ListWnd";
	}
	if (wnd->ui_cast_scroll_view() != nullptr)
	{
		return "ScrollView";
	}
	if (wnd->ui_cast_static() != nullptr)
	{
		return "Static";
	}
	if (wnd->ui_cast_texture_owner() != nullptr)
	{
		return "Frame";
	}
	return "Window";
}

static CUIWindow* FindWindowAtPoint(const xr_vector<CUIWindow*>& roots, const xr_hash_set<CUIWindow*>& widgets, float clientX, float clientY)
{
	Fvector2 pt;
	pt.set(clientX, clientY);

	auto findHit = [&](CUIWindow* wnd, auto& findHitLambda) -> CUIWindow*
	{
		if (!widgets.count(wnd))
		{
			return nullptr;
		}
		Frect r;
		wnd->GetAbsoluteRect(r);
		if (!r.in(pt))
		{
			return nullptr;
		}
		auto& children = wnd->GetChildWndList();
		for (int i = (int)children.size() - 1; i >= 0; --i)
		{
			CUIWindow* hit = findHitLambda(children[i], findHitLambda);
			if (hit)
			{
				return hit;
			}
		}
		return wnd;
	};

	for (int r = (int)roots.size() - 1; r >= 0; --r)
	{
		CUIWindow* hit = findHit(roots[r], findHit);
		if (hit)
		{
			return hit;
		}
	}
	return nullptr;
}

void ui_core::RenderUIDebugger()
{
	static CUIWindow* Selected = nullptr;
	static xr_vector<CUIWindow*> Roots;
	static char filterBuf[256] = "";
	static bool showLayoutBounds = false;
	static bool showHidden = false;
	static bool showClipping = false;
	static bool showSafeArea = false;
	static int g_previewResIndex = 0;
	static int g_exportFormat = 0;

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::UI_General)])
	{
		LastFrameWidgets.clear();
		return;
	}

	auto BuildTree = [&](const xr_hash_set<CUIWindow*>& widgetSet)
	{
		Roots.clear();
		Roots.reserve(widgetSet.size());
		for (CUIWindow* window : widgetSet)
		{
			CUIWindow* parent = window->GetParent();
			if (parent == nullptr || widgetSet.count(parent) == 0)
			{
				Roots.push_back(window);
			}
		}
	};

	auto GetWndName = [](CUIWindow* wndPtr) -> shared_str
	{
		if (wndPtr->WindowName().size() > 0)
		{
			return wndPtr->WindowName();
		}
		return wndPtr->WindowNodeName();
	};

	static xr_hash_set<CUIWindow*> s_debuggerWidgets;
	static bool s_lastShowHidden = false;
	bool needRebuild = (Roots.empty() || (showHidden != s_lastShowHidden));
	s_debuggerWidgets = LastFrameWidgets;
	if (showHidden)
	{
		s_lastShowHidden = true;
		xr_hash_set<CUIWindow*> rootsSet;
		for (CUIWindow* w : LastFrameWidgets)
		{
			CUIWindow* p = w;
			while (p && p->GetParent())
			{
				p = p->GetParent();
			}
			if (p)
			{
				rootsSet.insert(p);
			}
		}
		std::function<void(CUIWindow*)> addRecursive = [&](CUIWindow* w)
		{
			if (!w || s_debuggerWidgets.count(w))
			{
				return;
			}
			s_debuggerWidgets.insert(w);
			for (CUIWindow* child : w->GetChildWndList())
			{
				addRecursive(child);
			}
		};
		for (CUIWindow* r : rootsSet)
		{
			addRecursive(r);
		}
	}
	else
	{
		s_lastShowHidden = false;
	}
	const xr_hash_set<CUIWindow*>& debuggerWidgets = s_debuggerWidgets;

	if (needRebuild)
	{
		BuildTree(debuggerWidgets);
	}

	float screenToClientX = UI_BASE_WIDTH / (float)Device.TargetWidth;
	float screenToClientY = UI_BASE_HEIGHT / (float)Device.TargetHeight;
	ImVec2 mousePos = ImGui::GetIO().MousePos;
	float clientX = mousePos.x * screenToClientX;
	float clientY = mousePos.y * screenToClientY;

	bool imguiWantsMouse = ImGui::GetIO().WantCaptureMouse;
	CUIWindow* hoveredWnd = imguiWantsMouse ? nullptr : FindWindowAtPoint(Roots, debuggerWidgets, clientX, clientY);
	if (!imguiWantsMouse && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && hoveredWnd != nullptr)
	{
		Selected = hoveredWnd;
	}

	std::function<bool(CUIWindow*, const xr_string&)> DrawNode = [&](CUIWindow* window, const xr_string& filterLower) -> bool
	{
		if (!debuggerWidgets.contains(window))
		{
			return false;
		}
		shared_str wndName = GetWndName(window);
		shared_str nodeName = window->WindowNodeName();
		const char* typeName = GetWindowTypeName(window);
		string512 displayBuf;
		xr_sprintf(displayBuf, "%s [%s] (%s)", wndName.c_str(), nodeName.c_str(), typeName);
		if (!filterLower.empty())
		{
			xr_string displayLower = displayBuf;
			for (size_t i = 0; i < displayLower.size(); ++i)
			{
				displayLower[i] = (char)tolower(displayLower[i]);
			}
			if (displayLower.find(filterLower) == xr_string::npos)
			{
				bool hasMatchingChild = false;
				for (CUIWindow* child : window->GetChildWndList())
				{
					if (DrawNode(child, filterLower))
					{
						hasMatchingChild = true;
					}
				}
				if (!hasMatchingChild)
				{
					return false;
				}
			}
		}

		ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
		if (window == Selected)
		{
			flags |= ImGuiTreeNodeFlags_Selected;
		}
		if (window == hoveredWnd)
		{
			ImGui::PushStyleColor(ImGuiCol_Header, IM_COL32(80, 120, 180, 255));
		}
		bool hasChildren = !window->GetChildWndList().empty();
		if (!hasChildren)
		{
			flags |= ImGuiTreeNodeFlags_Leaf;
		}
		bool isOpen = ImGui::TreeNodeEx(window, flags, "%s", displayBuf);
		if (window == hoveredWnd)
		{
			ImGui::PopStyleColor();
		}
		if (ImGui::IsItemClicked())
		{
			Selected = window;
		}
		if (isOpen)
		{
			for (CUIWindow* child : window->GetChildWndList())
			{
				DrawNode(child, filterLower);
			}
			ImGui::TreePop();
		}
		return true;
	};

	if (!ImGui::Begin("UI Debugger", &Engine.External.EditorStates[static_cast<u8>(EditorUI::UI_General)]))
	{
		LastFrameWidgets.clear();
		ImGui::End();
		return;
	}

	if (ImGui::Button("Rebuild Tree"))
	{
		BuildTree(debuggerWidgets);
	}
	ImGui::SameLine();
	if (ImGui::Button("Reload UI"))
	{
		Console->Execute("ui_reload");
	}
	struct SPreviewResolution
	{
		const char* label;
		int width;
		int height;
	};
	static const SPreviewResolution g_previewResolutions[] =
	{
		{"Current", 0, 0},
		{"640 x 480 (4:3)", 640, 480},
		{"800 x 600 (4:3)", 800, 600},
		{"1024 x 768 (4:3)", 1024, 768},
		{"1280 x 960 (4:3)", 1280, 960},
		{"1280 x 1024 (5:4)", 1280, 1024},
		{"1280 x 720 (16:9)", 1280, 720},
		{"1366 x 768 (16:9)", 1366, 768},
		{"1600 x 900 (16:9)", 1600, 900},
		{"1920 x 1080 (16:9)", 1920, 1080},
		{"2560 x 1440 (16:9)", 2560, 1440},
		{"3840 x 2160 (16:9)", 3840, 2160},
		{"1280 x 800 (16:10)", 1280, 800},
		{"1680 x 1050 (16:10)", 1680, 1050},
		{"1920 x 1200 (16:10)", 1920, 1200},
		{"2560 x 1080 (21:9)", 2560, 1080},
		{"3440 x 1440 (21:9)", 3440, 1440},
		{"3840 x 1080 (32:9)", 3840, 1080},
		{"5120 x 1440 (32:9)", 5120, 1440},
	};

	auto getDebuggerZx = [&]() -> float
	{
		if (g_previewResIndex <= 0 || g_previewResolutions[g_previewResIndex].width <= 0)
		{
			return get_current_zx();
		}
		return float(g_previewResolutions[g_previewResIndex].width) / UI_BASE_WIDTH;
	};
	auto getDebuggerZy = [&]() -> float
	{
		if (g_previewResIndex <= 0 || g_previewResolutions[g_previewResIndex].height <= 0)
		{
			return get_current_zy();
		}
		return float(g_previewResolutions[g_previewResIndex].height) / UI_BASE_HEIGHT;
	};

	int previewW = (g_previewResIndex > 0) ? g_previewResolutions[g_previewResIndex].width : Device.TargetWidth;
	int previewH = (g_previewResIndex > 0) ? g_previewResolutions[g_previewResIndex].height : Device.TargetHeight;
	ImGui::Text("Resolution: %d x %d | zx: %.2f zy: %.2f kx: %.2f",
		Device.TargetWidth, Device.TargetHeight,
		get_current_zx(), get_current_zy(), get_current_kx());
	ImGui::SetNextItemWidth(120.0f);
	ImGui::Combo("Preview resolution", &g_previewResIndex,
		[](void* data, int idx, const char** out)
		{
			*out = g_previewResolutions[idx].label;
			return true;
		},
		nullptr, sizeof(g_previewResolutions) / sizeof(g_previewResolutions[0]));

	ImGui::Checkbox("Show layout bounds", &showLayoutBounds);
	ImGui::SameLine();
	ImGui::Checkbox("Show hidden", &showHidden);
	ImGui::SameLine();
	ImGui::Checkbox("Show clipping", &showClipping);
	ImGui::SameLine();
	ImGui::Checkbox("Show safe area", &showSafeArea);

	if (ImGui::Button("Export hierarchy"))
	{
		xr_string output;
		std::function<void(CUIWindow*, int, bool)> appendWindow;
		appendWindow = [&output, &appendWindow, &GetWndName](CUIWindow* wnd, int depth, bool asJson) -> void
		{
			if (!wnd)
			{
				return;
			}
			shared_str name = GetWndName(wnd);
			shared_str nodeName = wnd->WindowNodeName();
			Fvector2 pos = wnd->GetWndPos();
			Fvector2 size = wnd->GetWndSize();
			bool visible = wnd->IsShown();
			for (int i = 0; i < depth; ++i)
			{
				output += asJson ? "  " : "  ";
			}
			if (asJson)
			{
				char buf[512];
				xr_sprintf(buf, "{\"name\":\"%s\",\"node\":\"%s\",\"x\":%.1f,\"y\":%.1f,\"w\":%.1f,\"h\":%.1f,\"visible\":%s",
					name.c_str(), nodeName.c_str(), pos.x, pos.y, size.x, size.y, visible ? "true" : "false");
				output += buf;
				auto& children = wnd->GetChildWndList();
				if (children.empty())
				{
					output += "}";
				}
				else
				{
					output += ",\"children\":[";
					for (size_t i = 0; i < children.size(); ++i)
					{
						if (i > 0)
						{
							output += ",";
						}
						output += "\n";
						appendWindow(children[i], depth + 1, true);
					}
					output += "\n";
					for (int i = 0; i < depth; ++i)
					{
						output += "  ";
					}
					output += "]}";
				}
			}
			else
			{
				char buf[256];
				xr_sprintf(buf, "%s [%s] pos(%.0f,%.0f) size(%.0f,%.0f) %s\n",
					name.c_str(), nodeName.c_str(), pos.x, pos.y, size.x, size.y, visible ? "visible" : "hidden");
				output += buf;
				for (CUIWindow* child : wnd->GetChildWndList())
				{
					appendWindow(child, depth + 1, false);
				}
			}
		};

		xr_hash_set<CUIWindow*> debuggerWidgets;
		xr_vector<CUIWindow*> exportRoots;
		{
			xr_hash_set<CUIWindow*> rootsSet;
			for (CUIWindow* w : LastFrameWidgets)
			{
				CUIWindow* p = w;
				while (p && p->GetParent())
				{
					p = p->GetParent();
				}
				if (p)
				{
					rootsSet.insert(p);
				}
			}
			std::function<void(CUIWindow*)> addRecursive = [&](CUIWindow* w)
			{
				if (!w || debuggerWidgets.count(w))
				{
					return;
				}
				debuggerWidgets.insert(w);
				for (CUIWindow* child : w->GetChildWndList())
				{
					addRecursive(child);
				}
			};
			for (CUIWindow* r : rootsSet)
			{
				exportRoots.push_back(r);
				addRecursive(r);
			}
		}

		bool asJson = (g_exportFormat != 0);
		if (asJson)
		{
			output += "[\n";
		}
		for (size_t i = 0; i < exportRoots.size(); ++i)
		{
			appendWindow(exportRoots[i], 1, asJson);
			if (asJson && i + 1 < exportRoots.size())
			{
				output += ",\n";
			}
		}
		if (asJson)
		{
			output += "\n]";
		}
		ImGui::SetClipboardText(output.c_str());
	}
	ImGui::SameLine();
	ImGui::SetNextItemWidth(80.0f);
	ImGui::Combo("Export format", &g_exportFormat, "Text\0JSON\0");

	ImGui::SetNextItemWidth(-80.0f);
	ImGui::InputTextWithHint("##filter", "Filter by name...", filterBuf, sizeof(filterBuf));
	xr_string filterLower = filterBuf;
	for (size_t i = 0; i < filterLower.size(); ++i)
	{
		filterLower[i] = (char)tolower(filterLower[i]);
	}

	ImGui::Separator();

	for (CUIWindow* root : Roots)
	{
		DrawNode(root, filterLower);
	}

	ImGui::Separator();

	if (Selected != nullptr && debuggerWidgets.contains(Selected))
	{
		ImGui::Text("Selected: %s", GetWndName(Selected).c_str());
		ImGui::Text("Mode: %s", Selected->GetUseAnchors() ? "Anchored" : "Legacy");

		Fvector2 position = Selected->GetWndPos();
		Fvector2 size = Selected->GetWndSize();
		if (ImGui::DragFloat2("Position (x, y)", reinterpret_cast<float*>(&position), 1.0f))
		{
			Selected->SetWndPos(position);
		}
		if (ImGui::DragFloat2("Size (width, height)", reinterpret_cast<float*>(&size), 1.0f))
		{
			Selected->SetWndSize(size);
		}
		Frect absRect;
		Selected->GetAbsoluteRect(absRect);
		ImGui::Text("Absolute: (%.1f, %.1f) - (%.1f, %.1f)",
			absRect.x1, absRect.y1, absRect.x2, absRect.y2);

		if (Selected->GetUseAnchors())
		{
			SAnchorData& ad = Selected->GetAnchorData();
			ImGui::Separator();
			ImGui::Text("Anchors (0-1 normalized)");
			if (ImGui::DragFloat2("anchor_min", reinterpret_cast<float*>(&ad.anchorMin), 0.01f, 0.0f, 1.0f, "%.2f"))
			{
				ad.anchorMin.x = (ad.anchorMin.x < 0.0f) ? 0.0f : (ad.anchorMin.x > 1.0f ? 1.0f : ad.anchorMin.x);
				ad.anchorMin.y = (ad.anchorMin.y < 0.0f) ? 0.0f : (ad.anchorMin.y > 1.0f ? 1.0f : ad.anchorMin.y);
			}
			if (ImGui::DragFloat2("anchor_max", reinterpret_cast<float*>(&ad.anchorMax), 0.01f, 0.0f, 1.0f, "%.2f"))
			{
				ad.anchorMax.x = (ad.anchorMax.x < 0.0f) ? 0.0f : (ad.anchorMax.x > 1.0f ? 1.0f : ad.anchorMax.x);
				ad.anchorMax.y = (ad.anchorMax.y < 0.0f) ? 0.0f : (ad.anchorMax.y > 1.0f ? 1.0f : ad.anchorMax.y);
			}
			if (ImGui::DragFloat2("offset_min", reinterpret_cast<float*>(&ad.offsetMin), 1.0f))
			{
			}
			if (ImGui::DragFloat2("offset_max", reinterpret_cast<float*>(&ad.offsetMax), 1.0f))
			{
			}
			if (ImGui::Button("Copy coordinates"))
			{
				string512 buf;
				xr_sprintf(buf, "anchor_min=\"%.2f,%.2f\" anchor_max=\"%.2f,%.2f\" offset_x=\"%.0f\" offset_y=\"%.0f\" width=\"%.0f\" height=\"%.0f\"",
					ad.anchorMin.x, ad.anchorMin.y, ad.anchorMax.x, ad.anchorMax.y,
					ad.offsetMin.x, ad.offsetMin.y, Selected->GetWidth(), Selected->GetHeight());
				ImGui::SetClipboardText(buf);
			}
			ImGui::SameLine();
			if (ImGui::Button("Copy legacy (x,y,w,h)"))
			{
				string512 buf;
				xr_sprintf(buf, "x=\"%.0f\" y=\"%.0f\" width=\"%.0f\" height=\"%.0f\"",
					position.x, position.y, Selected->GetWidth(), Selected->GetHeight());
				ImGui::SetClipboardText(buf);
			}
		}
		else
		{
			if (ImGui::Button("Copy coordinates"))
			{
				string512 buf;
				xr_sprintf(buf, "x=\"%.0f\" y=\"%.0f\" width=\"%.0f\" height=\"%.0f\"",
					position.x, position.y, Selected->GetWidth(), Selected->GetHeight());
				ImGui::SetClipboardText(buf);
			}
		}
	}
	else
	{
		ImGui::Text("No UI window selected. Click on element or use tree.");
	}

	ImGui::End();

	imguiWantsMouse = ImGui::GetIO().WantCaptureMouse;
	if (Selected != nullptr && debuggerWidgets.contains(Selected))
	{
		Fvector2 absPos;
		Selected->GetAbsolutePos(absPos);
		Fvector2 size = Selected->GetWndSize();
		absPos.x *= getDebuggerZx();
		size.x *= getDebuggerZx();
		absPos.y *= getDebuggerZy();
		size.y *= getDebuggerZy();

		bool inside = !imguiWantsMouse && (mousePos.x >= absPos.x && mousePos.x <= (absPos.x + size.x) &&
			mousePos.y >= absPos.y && mousePos.y <= (absPos.y + size.y));

		static int draggingAnchor = 0;
		if (imguiWantsMouse)
		{
			draggingAnchor = 0;
		}
		else if (Selected->GetUseAnchors())
		{
			Frect parentRectForDrag;
			if (Selected->GetParent() != nullptr)
			{
				Selected->GetParent()->GetAbsoluteRect(parentRectForDrag);
			}
			else
			{
				UI().GetSafeAreaRootRect(parentRectForDrag);
			}
			const float pw = parentRectForDrag.width();
			const float ph = parentRectForDrag.height();
			const float zx = getDebuggerZx();
			const float zy = getDebuggerZy();
			float aMinX = (parentRectForDrag.x1 + Selected->GetAnchorData().anchorMin.x * pw) * zx;
			float aMinY = (parentRectForDrag.y1 + Selected->GetAnchorData().anchorMin.y * ph) * zy;
			float aMaxX = (parentRectForDrag.x1 + Selected->GetAnchorData().anchorMax.x * pw) * zx;
			float aMaxY = (parentRectForDrag.y1 + Selected->GetAnchorData().anchorMax.y * ph) * zy;
			float distMin = (mousePos.x - aMinX) * (mousePos.x - aMinX) + (mousePos.y - aMinY) * (mousePos.y - aMinY);
			float distMax = (mousePos.x - aMaxX) * (mousePos.x - aMaxX) + (mousePos.y - aMaxY) * (mousePos.y - aMaxY);
			float hitR2 = AnchorPointHitRadius * AnchorPointHitRadius;
			bool overMin = (distMin <= hitR2);
			bool overMax = (distMax <= hitR2);

			if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
			{
				draggingAnchor = overMin ? 1 : (overMax ? 2 : 0);
			}
			if (ImGui::IsMouseReleased(ImGuiMouseButton_Left))
			{
				draggingAnchor = 0;
			}
			if (draggingAnchor != 0 && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
			{
				ImVec2 delta = ImGui::GetMouseDragDelta(ImGuiMouseButton_Left);
				if (delta.x != 0.0f || delta.y != 0.0f)
				{
					SAnchorData& ad = Selected->GetAnchorData();
					float dNormX = (delta.x / zx) / pw;
					float dNormY = (delta.y / zy) / ph;
					if (draggingAnchor == 1)
					{
						ad.anchorMin.x += dNormX;
						ad.anchorMin.y += dNormY;
						ad.anchorMin.x = (ad.anchorMin.x < 0.0f) ? 0.0f : (ad.anchorMin.x > 1.0f ? 1.0f : ad.anchorMin.x);
						ad.anchorMin.y = (ad.anchorMin.y < 0.0f) ? 0.0f : (ad.anchorMin.y > 1.0f ? 1.0f : ad.anchorMin.y);
					}
					else
					{
						ad.anchorMax.x += dNormX;
						ad.anchorMax.y += dNormY;
						ad.anchorMax.x = (ad.anchorMax.x < 0.0f) ? 0.0f : (ad.anchorMax.x > 1.0f ? 1.0f : ad.anchorMax.x);
						ad.anchorMax.y = (ad.anchorMax.y < 0.0f) ? 0.0f : (ad.anchorMax.y > 1.0f ? 1.0f : ad.anchorMax.y);
					}
					ImGui::ResetMouseDragDelta(ImGuiMouseButton_Left);
				}
			}
			else
			{
				bool rectInside = inside && !overMin && !overMax;
				UIDebuggerMouseMove(Selected, rectInside);
			}
		}
		else
		{
			draggingAnchor = 0;
			UIDebuggerMouseMove(Selected, inside);
		}

		ImDrawList* draw = ImGui::GetForegroundDrawList();
		ImU32 selectedCol = inside ? IM_COL32(255, 50, 50, 220) : IM_COL32(50, 255, 50, 220);
		draw->AddRect(
			ImVec2(absPos.x, absPos.y),
			ImVec2(absPos.x + size.x, absPos.y + size.y),
			selectedCol, 0.0f, 0, 2.0f);

		CUIWindow* parent = Selected->GetParent();
		float parentAlpha = 0.6f;
		while (parent != nullptr && debuggerWidgets.contains(parent))
		{
			Frect parentRect;
			parent->GetAbsoluteRect(parentRect);
			Fvector2 parentSize = parent->GetWndSize();
			parentRect.lt.x *= getDebuggerZx();
			parentRect.rb.x = parentRect.lt.x + parentSize.x * getDebuggerZx();
			parentRect.lt.y *= getDebuggerZy();
			parentRect.rb.y = parentRect.lt.y + parentSize.y * getDebuggerZy();
			draw->AddRect(
				ImVec2(parentRect.lt.x, parentRect.lt.y),
				ImVec2(parentRect.rb.x, parentRect.rb.y),
				IM_COL32(100, 100, 255, (u32)(255 * parentAlpha)), 0.0f, 0, 1.0f);
			parentAlpha *= 0.7f;
			parent = parent->GetParent();
		}

		if (Selected->GetUseAnchors())
		{
			Frect parentRect;
			if (Selected->GetParent() != nullptr)
			{
				Selected->GetParent()->GetAbsoluteRect(parentRect);
			}
			else
			{
				parentRect.set(0.0f, 0.0f, UI_BASE_WIDTH, UI_BASE_HEIGHT);
			}
			const SAnchorData& ad = Selected->GetAnchorData();
			const float pw = parentRect.width();
			const float ph = parentRect.height();
			const float zx = getDebuggerZx();
			const float zy = getDebuggerZy();

			float anchorMinScreenX = (parentRect.x1 + ad.anchorMin.x * pw) * zx;
			float anchorMinScreenY = (parentRect.y1 + ad.anchorMin.y * ph) * zy;
			float anchorMaxScreenX = (parentRect.x1 + ad.anchorMax.x * pw) * zx;
			float anchorMaxScreenY = (parentRect.y1 + ad.anchorMax.y * ph) * zy;
			float pivotScreenX = (absPos.x + size.x * 0.5f);
			float pivotScreenY = (absPos.y + size.y * 0.5f);

			const ImU32 colMin = IM_COL32(255, 80, 80, 255);
			const ImU32 colMax = IM_COL32(80, 80, 255, 255);
			const ImU32 colPivot = IM_COL32(255, 255, 80, 255);
			const float pointRadius = 5.0f;

			draw->AddLine(ImVec2(anchorMinScreenX, anchorMinScreenY), ImVec2(absPos.x, absPos.y), colMin, 2.0f);
			draw->AddLine(ImVec2(anchorMaxScreenX, anchorMaxScreenY), ImVec2(absPos.x + size.x, absPos.y + size.y), colMax, 2.0f);
			draw->AddCircleFilled(ImVec2(anchorMinScreenX, anchorMinScreenY), pointRadius, colMin);
			draw->AddCircleFilled(ImVec2(anchorMaxScreenX, anchorMaxScreenY), pointRadius, colMax);
			draw->AddCircleFilled(ImVec2(pivotScreenX, pivotScreenY), pointRadius, colPivot);
		}

		auto arrowMove = [&](ImGuiKey key, float dx, float dy) -> bool
		{
			if (ImGui::IsKeyPressed(key))
			{
				Fvector2 newPos = Selected->GetWndPos();
				newPos.x += dx;
				newPos.y += dy;
				Selected->SetWndPos(newPos);
				return true;
			}
			return false;
		};
		arrowMove(ImGuiKey_UpArrow, 0.0f, -ArrowMoveStep);
		arrowMove(ImGuiKey_DownArrow, 0.0f, ArrowMoveStep);
		arrowMove(ImGuiKey_LeftArrow, -ArrowMoveStep, 0.0f);
		arrowMove(ImGuiKey_RightArrow, ArrowMoveStep, 0.0f);
	}

	if (showLayoutBounds)
	{
		ImDrawList* layoutDraw = ImGui::GetForegroundDrawList();
		const float zx = getDebuggerZx();
		const float zy = getDebuggerZy();
		const ImU32 colStackBounds = IM_COL32(100, 200, 100, 180);
		const ImU32 colSpacing = IM_COL32(255, 200, 100, 200);
		const ImU32 colGrid = IM_COL32(150, 150, 255, 150);
		const ImU32 colOverflow = IM_COL32(255, 0, 0, 180);

		for (CUIWindow* wnd : debuggerWidgets)
		{
			if (!wnd)
			{
				continue;
			}
			ILayoutProvider* layout = wnd->GetLayout();
			if (!layout)
			{
				continue;
			}
			Frect parentAbs;
			wnd->GetAbsoluteRect(parentAbs);
			const float pLtX = parentAbs.x1 * zx;
			const float pLtY = parentAbs.y1 * zy;
			const float pW = wnd->GetWidth() * zx;
			const float pH = wnd->GetHeight() * zy;

			if (layout->GetLayoutType() == EUILayoutType::Stack)
			{
				CUIStackLayout* stackLayout = static_cast<CUIStackLayout*>(layout);
				const float pl = stackLayout->GetPaddingLeft() * zx;
				const float pt = stackLayout->GetPaddingTop() * zy;
				const float pr = stackLayout->GetPaddingRight() * zx;
				const float pb = stackLayout->GetPaddingBottom() * zy;
				const float contentL = pLtX + pl;
				const float contentT = pLtY + pt;
				const float contentR = pLtX + pW - pr;
				const float contentB = pLtY + pH - pb;
				layoutDraw->AddRect(ImVec2(contentL, contentT), ImVec2(contentR, contentB), colStackBounds, 0.0f, 0, 1.5f);

				const float spacing = stackLayout->GetSpacing() * (stackLayout->GetDirection() == EUIStackLayoutDir::Horizontal ? zx : zy);
				auto& children = wnd->GetChildWndList();
				float cursor = (stackLayout->GetDirection() == EUIStackLayoutDir::Horizontal) ? contentL : contentT;
				for (size_t i = 0; i < children.size(); ++i)
				{
					CUIWindow* child = children[i];
					if (!child || !child->IsShown() || child->GetCustomDraw())
					{
						continue;
					}
					Frect childRect;
					child->GetAbsoluteRect(childRect);
					float cL = childRect.x1 * zx;
					float cT = childRect.y1 * zy;
					float cW = child->GetWidth() * zx;
					float cH = child->GetHeight() * zy;

					if (stackLayout->GetDirection() == EUIStackLayoutDir::Horizontal)
					{
						cursor += cW;
						if (i + 1 < children.size() && spacing > 0.0f)
						{
							layoutDraw->AddLine(ImVec2(cursor, contentT), ImVec2(cursor, contentB), colSpacing, 2.0f);
							cursor += spacing;
						}
					}
					else
					{
						cursor += cH;
						if (i + 1 < children.size() && spacing > 0.0f)
						{
							layoutDraw->AddLine(ImVec2(contentL, cursor), ImVec2(contentR, cursor), colSpacing, 2.0f);
							cursor += spacing;
						}
					}
				}
			}
			else if (layout->GetLayoutType() == EUILayoutType::Grid)
			{
				CUIGridLayout* gridLayout = static_cast<CUIGridLayout*>(layout);
				const int cols = gridLayout->GetCols();
				const int rows = gridLayout->GetRows();
				if (cols <= 0)
				{
					continue;
				}
				const float pl = gridLayout->GetPaddingLeft() * zx;
				const float pt = gridLayout->GetPaddingTop() * zy;
				const float csx = gridLayout->GetCellSpacingX() * zx;
				const float csy = gridLayout->GetCellSpacingY() * zy;
				float cellW = gridLayout->GetCellWidth() * zx;
				float cellH = gridLayout->GetCellHeight() * zy;
				if (cellW <= 0.0f || cellH <= 0.0f)
				{
					for (CUIWindow* ch : wnd->GetChildWndList())
					{
						if (ch && ch->IsShown() && !ch->GetCustomDraw())
						{
							if (cellW <= 0.0f)
							{
								cellW = ch->GetWidth() * zx;
							}
							if (cellH <= 0.0f)
							{
								cellH = ch->GetHeight() * zy;
							}
							if (cellW > 0.0f && cellH > 0.0f)
							{
								break;
							}
						}
					}
				}
				int visibleChildren = 0;
				for (CUIWindow* ch : wnd->GetChildWndList())
				{
					if (ch && ch->IsShown() && !ch->GetCustomDraw())
					{
						++visibleChildren;
					}
				}
				const int rowCount = (rows > 0) ? rows : (visibleChildren + cols - 1) / cols;
				for (int row = 0; row <= rowCount; ++row)
				{
					float y = pLtY + pt + row * (cellH + csy);
					layoutDraw->AddLine(ImVec2(pLtX + pl, y), ImVec2(pLtX + pW, y), colGrid, 1.0f);
				}
				for (int col = 0; col <= cols; ++col)
				{
					float x = pLtX + pl + col * (cellW + csx);
					layoutDraw->AddLine(ImVec2(x, pLtY + pt), ImVec2(x, pLtY + pH), colGrid, 1.0f);
				}
			}
		}

		for (CUIWindow* wnd : LastFrameWidgets)
		{
			CUIWindow* parent = wnd->GetParent();
			if (!parent || !debuggerWidgets.contains(parent))
			{
				continue;
			}
			Frect parentAbs;
			parent->GetAbsoluteRect(parentAbs);
			Frect childAbs;
			wnd->GetAbsoluteRect(childAbs);
			float cL = childAbs.x1;
			float cT = childAbs.y1;
			float cR = childAbs.x2;
			float cB = childAbs.y2;
			float pL = parentAbs.x1;
			float pT = parentAbs.y1;
			float pR = parentAbs.x2;
			float pB = parentAbs.y2;
			bool overflows = (cL < pL || cT < pT || cR > pR || cB > pB);
			if (overflows)
			{
				float scL = cL * zx;
				float scT = cT * zy;
				float scR = cR * zx;
				float scB = cB * zy;
				layoutDraw->AddRect(ImVec2(scL, scT), ImVec2(scR, scB), colOverflow, 0.0f, 0, 2.0f);
			}
		}
	}

	if (showHidden)
	{
		ImDrawList* hiddenDraw = ImGui::GetForegroundDrawList();
		const float zx = getDebuggerZx();
		const float zy = getDebuggerZy();
		const ImU32 colHidden = IM_COL32(128, 128, 255, 100);
		for (CUIWindow* wnd : debuggerWidgets)
		{
			if (!wnd || wnd->IsShown())
			{
				continue;
			}
			Frect r;
			wnd->GetAbsoluteRect(r);
			float x1 = r.x1 * zx;
			float y1 = r.y1 * zy;
			float x2 = r.x2 * zx;
			float y2 = r.y2 * zy;
			hiddenDraw->AddRect(ImVec2(x1, y1), ImVec2(x2, y2), colHidden, 0.0f, 0, 1.5f);
		}
	}

	if (showClipping)
	{
		ImDrawList* clipDraw = ImGui::GetForegroundDrawList();
		const float zx = getDebuggerZx();
		const float zy = getDebuggerZy();
		const ImU32 colScissor = IM_COL32(255, 200, 0, 150);
		for (u32 i = 0; i < m_ScissorsForDebug.size(); ++i)
		{
			const Frect& r = m_ScissorsForDebug[i];
			float x1 = r.x1 * zx;
			float y1 = r.y1 * zy;
			float x2 = r.x2 * zx;
			float y2 = r.y2 * zy;
			clipDraw->AddRect(ImVec2(x1, y1), ImVec2(x2, y2), colScissor, 0.0f, 0, 2.0f);
		}
	}

	if (showSafeArea)
	{
		Frect safeRect;
		GetSafeAreaRootRect(safeRect);
		ImDrawList* safeDraw = ImGui::GetForegroundDrawList();
		const float zx = getDebuggerZx();
		const float zy = getDebuggerZy();
		const ImU32 colSafeArea = IM_COL32(0, 200, 255, 120);
		float x1 = safeRect.x1 * zx;
		float y1 = safeRect.y1 * zy;
		float x2 = safeRect.x2 * zx;
		float y2 = safeRect.y2 * zy;
		safeDraw->AddRect(ImVec2(x1, y1), ImVec2(x2, y2), colSafeArea, 0.0f, 0, 2.0f);
	}

	m_ScissorsForDebug.clear();
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
	m_currentScaleMode			= UI_SCALE_MODE_DEFAULT;

	_safeAreaInsetLeft			= 0.0f;
	_safeAreaInsetTop			= 0.0f;
	_safeAreaInsetRight			= 0.0f;
	_safeAreaInsetBottom		= 0.0f;

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

UIScaleModeScope::UIScaleModeScope(ui_core* ui, u8 mode)
	: _ui(ui)
	, _prevMode(ui != nullptr ? ui->GetCurrentScaleMode() : UI_SCALE_MODE_DEFAULT)
{
	if (_ui != nullptr && mode != UI_SCALE_MODE_DEFAULT)
	{
		_ui->SetCurrentScaleMode(mode);
	}
}

UIScaleModeScope::~UIScaleModeScope()
{
	if (_ui != nullptr)
	{
		_ui->SetCurrentScaleMode(_prevMode);
	}
}

void ui_core::SetSafeAreaInset(float left, float top, float right, float bottom)
{
	_safeAreaInsetLeft	= (left >= 0.0f) ? left : 0.0f;
	_safeAreaInsetTop	= (top >= 0.0f) ? top : 0.0f;
	_safeAreaInsetRight	= (right >= 0.0f) ? right : 0.0f;
	_safeAreaInsetBottom= (bottom >= 0.0f) ? bottom : 0.0f;
}

void ui_core::GetSafeAreaRootRect(Frect& outRect) const
{
	outRect.set(
		_safeAreaInsetLeft,
		_safeAreaInsetTop,
		UI_BASE_WIDTH - _safeAreaInsetRight,
		UI_BASE_HEIGHT - _safeAreaInsetBottom
	);
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
shared_str	ui_core::get_xml_name(LPCSTR fn)
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
	R_ASSERT(DevicePtr && "Render must be initialized otherwise early calling!");
	R_ASSERT(DevicePtr->m_pRender && "Resource manager");

	if (DevicePtr == nullptr || DevicePtr->m_pRender == nullptr)
		return m_empty_default;

	return DevicePtr->m_pRender->GetSVGShader(subpath, requested_width, requested_height);
}

const ui_shader& ui_core::GetVectorShader(const char* pSubpath, float requested_width, float requested_height)
{
	R_ASSERT(pSubpath && "invalid string (nullptr)");

	return GetVectorShader(std::string_view(pSubpath), requested_width, requested_height);
}

Frect ui_core::GetVectorUV(const std::string_view& subpath, float requested_width, float requested_height)
{
	if (DevicePtr == nullptr || DevicePtr->m_pRender == nullptr)
		return Frect();


	return DevicePtr->m_pRender->GetSVGUV(subpath, requested_width, requested_height);
}
