
ImVec2 originalFramePadding{ 0,0 };
bool IXBeginMainMenuBar()
{
	float UIMainMenuSize = UI->GetMenuBarHeight();
	ImGuiViewport* viewport = ImGui::GetMainViewport();
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, UIMainMenuSize));
	ImGuiStyle& style = ImGui::GetStyle();
	originalFramePadding = style.FramePadding;

	style.FramePadding.y = 9.0f;
	if (!ImGui::BeginMainMenuBar())
	{
		return false;
	}
	ImVec2 LogoButtonSize = ImVec2(UIMainMenuSize, UIMainMenuSize);
	ImVec2 LogoSize = ImVec2(24, 24);

	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2((LogoButtonSize.x - LogoSize.x) / 2, (LogoButtonSize.y - LogoSize.y) / 2));
	ImGui::ImageButton("##IXBeginMainMenuBar00", UI->m_HeaderLogo->pSurface, LogoSize);
	ImGui::PopStyleVar();

	return true;
}

void IXEndMainMenuBar()
{
	float UIMainMenuSize = UI->GetMenuBarHeight();
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(1, 1));
	bool MaxBut = false;
	bool MoveWin = false;
	SDL_Event Event;
	ImVec2 dragZoneSize = ImVec2(ImGui::GetContentRegionAvail().x - (3 * UIMainMenuSize * 1.1f), UIMainMenuSize);

	ImGui::InvisibleButton("##DragZone", dragZoneSize);


	if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		MaxBut = true;

	if (EDevice->isZoomed && ImGui::IsItemHovered() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		MaxBut = true;
		MoveWin = true;
	}
	else if (!EDevice->isZoomed && ImGui::IsItemHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		MoveWin = true;

	ImVec2 ControlButtonSize = ImVec2(UIMainMenuSize * 1.1f, UIMainMenuSize);
	ImVec2 ImageSize = ImVec2(10, 10);

	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2((ControlButtonSize.x - ImageSize.x) / 2, (ControlButtonSize.y - ImageSize.y) / 2));


	if (ImGui::ImageButton("##IXEndMainMenuBar01",UI->m_WinMin->pSurface, ImageSize))
		SendMessageW(EDevice->GetHWND(), WM_SYSCOMMAND, SC_MINIMIZE, 0);

	if (ImGui::ImageButton("##IXEndMainMenuBar02", (EDevice->isZoomed ? UI->m_WinRes->pSurface : UI->m_WinMax->pSurface), ImageSize))
		MaxBut = true;

	if (ImGui::ImageButton("##IXEndMainMenuBar03", UI->m_WinClose->pSurface, ImageSize))
		SendMessageW(EDevice->GetHWND(), WM_CLOSE, 0, 0);


	if (MaxBut)
	{
		if (EDevice->isZoomed)
		{
			EDevice->ResoreWindow(MoveWin);
		}
		else
		{
			Event.type = SDL_EVENT_WINDOW_MAXIMIZED;
			SDL_PushEvent(&Event);
		}
	}

	if (MoveWin)
	{
		ReleaseCapture();
		SendMessageW(EDevice->GetHWND(), 0xA1, 0x2, 0);
	}

	ImGui::EndMainMenuBar();
	ImGui::PopStyleVar(2);
	ImGuiStyle& style = ImGui::GetStyle();
	style.FramePadding = originalFramePadding;
}