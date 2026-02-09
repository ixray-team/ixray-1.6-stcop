
//ImVec2 originalFramePadding{ 0,0 };
//ImVec4 old_ImGuiCol_ChildBg{};

bool IXBeginMainMenuBar()
{
	float UIMainMenuSize = UI->GetMenuBarHeight();
	ImGuiViewport* viewport = ImGui::GetMainViewport();

	ImGuiStyle& style = ImGui::GetStyle();
	//originalFramePadding = style.FramePadding;

	//style.FramePadding.y = 9.0f;

	ImVec2 LogoButtonSize = ImVec2(UIMainMenuSize, UIMainMenuSize);
	ImVec2 LogoSize = ImVec2(43, 43);


	//ImGui::SetCursorPos({ 0, 0 });

	ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y));
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, LogoButtonSize.y));
	//ImGui::SetNextWindowViewport(viewport->ID);

	ImGuiWindowFlags window_flags = 0
		| ImGuiWindowFlags_NoDocking
		| ImGuiWindowFlags_NoTitleBar
		| ImGuiWindowFlags_NoResize
		| ImGuiWindowFlags_NoMove
		| ImGuiWindowFlags_NoScrollbar
		| ImGuiWindowFlags_NoScrollWithMouse
		| ImGuiWindowFlags_NoBringToFrontOnFocus
		;

	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);


	ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.f, 0.f, 0.f, 0.f));
	ImGui::PushStyleColor(ImGuiCol_Border, { 0.f,0.f,0.f,0.f });
	ImGui::PushStyleColor(ImGuiCol_BorderShadow, { 0.f,0.f,0.f,0.f });

	if (!ImGui::Begin("##ChezzeTopMenu", NULL, window_flags))
	{
		ImGui::PopStyleVar(4);
		ImGui::PopStyleColor(4);
		return false;
	}


	auto WindowPadding = style.WindowPadding;

	if (ImGui::BeginChild("##IXBeginMainMenuBarGROUP00", 
		{ LogoButtonSize.x - WindowPadding.x, LogoButtonSize.y - WindowPadding.y }))
	{
		auto t_size = ImGui::GetContentRegionAvail();
		ImVec2 t_pose = { (t_size.x - LogoSize.x) / 2 - (WindowPadding.x/2), (t_size.y - LogoSize.y) / 2 - (WindowPadding.y/2)};
		ImGui::SetCursorPos(t_pose);
		ImGui::Image(UI->m_HeaderLogo->get_SRView()->GetRawSRV(), LogoSize);

		ImGui::EndChild();
		ImGui::SameLine();
	}
	return true;
}

void IXEndMainMenuBar()
{

	ImGuiStyle& style = ImGui::GetStyle();

	float button_w = 46.f;
	float button_h = UI->GetMenuBarButtonHeight();

	float UIMainMenuSize = UI->GetMenuBarHeight();
	bool MaxBut = false;
	bool MoveWin = false;
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	SDL_Event Event;
	ImGui::SameLine();


	ImVec2 dragZoneSize = ImVec2(ImGui::GetContentRegionAvail().x+ style.WindowPadding.x /*- button_w*3*/, UIMainMenuSize);
	ImGui::SetCursorPosY(0.f);
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

	{
		ImVec2 ControlButtonSize = ImVec2(button_w, button_h);
		ImVec2 ImageSize = ImVec2(10.f, 10.f);

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2((ControlButtonSize.x - ImageSize.x) / 2, (ControlButtonSize.y - ImageSize.y) / 2));

		ImGui::SetCursorPos({ ImGui::GetContentRegionMax().x - button_w * 3 + style.WindowPadding.x, 0 });
		ImGui::BeginChild("##ControlButtons", { button_w * 3,button_h } );

		if (ImGui::ImageButton("##IXEndMainMenuBar01", UI->m_WinMin->get_SRView()->GetRawSRV(), ImageSize))
			SendMessageW(EDevice->GetHWND(), WM_SYSCOMMAND, SC_MINIMIZE, 0);

		ImGui::SameLine();

		if (ImGui::ImageButton("##IXEndMainMenuBar02", (EDevice->isZoomed ? UI->m_WinRes->get_SRView()->GetRawSRV() : UI->m_WinMax->get_SRView()->GetRawSRV()), ImageSize))
			MaxBut = true;


		ImGui::SameLine();
		if (ImGui::ImageButton("##IXEndMainMenuBar03", UI->m_WinClose->get_SRView()->GetRawSRV(), ImageSize))
			SendMessageW(EDevice->GetHWND(), WM_CLOSE, 0, 0);

		ImGui::EndChild();


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

		ImGui::PopStyleVar(2);

	}
	ImGui::PopStyleVar(3);
	ImGui::PopStyleColor(4);
	ImGui::End();
}