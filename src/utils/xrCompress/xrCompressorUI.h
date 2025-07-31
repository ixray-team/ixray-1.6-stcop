


static xrCriticalSection csLog;

xr_vector<xr_string> myLogVector; //<-- kak placeholder


//Ex: 25, 200, 50, 255 -> 0.0980392, 0.784314, 0.196078, 1
#define RGBAColor(r,g,b,a) r/(float)255, g/(float)255, b/(float)255, a/(float)255
const ImVec4 getLogColor(char* text) //<- copypaste func
{
	if (text == nullptr || xr_strlen(text) == 0)
		return ImVec4(RGBAColor(230, 230, 230, 255));

	xr_string TextEx = text;
	TextEx = TextEx.RemoveWhitespaces();
	size_t Pos = TextEx.find('|');

	while (Pos != xr_string::npos)
	{
		TextEx.erase(Pos, 1);
		Pos = TextEx.find('|');
	}

	char Word = TextEx[0];

	switch (Word)
	{
	case '~': return ImVec4(RGBAColor(248, 248, 49, 255));
	case '!': return ImVec4(RGBAColor(204, 102, 102, 255));
	case '@': return ImVec4(RGBAColor(125, 125, 241, 255));
	case '#': return ImVec4(RGBAColor(0, 222, 205, 155));
	case '%': return ImVec4(RGBAColor(202, 85, 219, 155));
	case '$': return ImVec4(RGBAColor(172, 172, 255, 255));
	case '*': return ImVec4(RGBAColor(248, 248, 49, 255));
	case '^': return ImVec4(RGBAColor(100, 246, 121, 255));
	case '&': return ImVec4(RGBAColor(255, 255, 0, 255));
	case '-': return ImVec4(RGBAColor(0, 255, 0, 255));
	case '+': return ImVec4(RGBAColor(84, 255, 255, 255));
	case '=': return ImVec4(RGBAColor(205, 205, 105, 255));
	case '/': return ImVec4(RGBAColor(146, 146, 252, 255));
	}

	return ImVec4(RGBAColor(230, 230, 230, 255));
}

enum ECompressorType
{
	Pack = 0,
	Diff,
	Unpack
};

#define MenuButton(_type) ImGui::BeginDisabled(SelectedType == ECompressorType::_type);\
							if (ImGui::Button(#_type)) {SelectedType = _type;}\
							  ImGui::EndDisabled();

#define IXCase(_type) \
    case _type: \
        ImGui::Text("Options [%s]", #_type); \
        ImGui::Separator(); \
        Render##_type##Options(); \
        break;


void RenderPackOptions();
void RenderDiffOptions();
void RenderUnpackOptions();

void RenderMainUI()
{
	int Size[2] = {};
	SDL_GetWindowSize(g_AppInfo.Window, &Size[0], &Size[1]);
	ImGui::SetNextWindowPos({ 0, 0 });
	ImGui::SetNextWindowSize({ (float)Size[0], (float)Size[1] });

	static ECompressorType SelectedType = Pack;
	static bool Started = false;
	static float Progress = 0.f;

	if (!ImGui::Begin("MainForm", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus))
	{
		return;
	}

	ImGui::BeginDisabled(Started);
	{
		ImGui::Text("Selected Mode:"); ImGui::SameLine();
		MenuButton(Pack); ImGui::SameLine();
		MenuButton(Diff); ImGui::SameLine();
		MenuButton(Unpack); ImGui::SameLine();

		auto AvaRegX = ImGui::GetContentRegionAvail().x;
		auto ProgHeight = AvaRegX *0.7f;
		ImGui::SetCursorPosX( ImGui::GetCursorPosX() + AvaRegX-ProgHeight);
		ImGui::ProgressBar(Progress, { ProgHeight,0});

	}
	ImGui::EndDisabled();

	auto PanesHeight = ImGui::GetContentRegionAvail().y;
	float buttonPadding = 45.f;
	ImGui::BeginChild("LeftPane", ImVec2(Size[0]*0.6, PanesHeight- buttonPadding), true);
	{
		ImGui::Text("Log");
		ImGui::Separator();

		ImGui::BeginChild("scroll_area");
			ImGuiListClipper clipper;

			xrCriticalSectionGuard LogGuard(&csLog);

			clipper.Begin(myLogVector.size());

			while (clipper.Step())
			{
				for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
				{
					auto& line = myLogVector[i];
					ImGui::TextColored(getLogColor((char*)line.c_str()), "%s", line.c_str());
				}
			}


			//if (autoScroll) //<- nie znau, nuzen li
			//	ImGui::SetScrollY(ImGui::GetScrollMaxY());

		ImGui::EndChild();
	}
	ImGui::EndChild();

	//////
	ImGui::SameLine();
	ImGui::BeginDisabled(Started);
	ImGui::BeginChild("RightPane", ImVec2(0, PanesHeight - buttonPadding), true);
	{
		switch (SelectedType)
		{
			IXCase(Pack);
			IXCase(Diff);
			IXCase(Unpack);
		default:
			break;
		}
	}
	ImGui::EndChild();

	if (ImGui::Button("K/\\ATCHu 4TO6 3AnyCTuTb", { -1,-1 }))
	{
		Started = true;
		Progress = 0.5f;
		for (int i = 0; i <= 50; ++i)
		{
			xr_string karabas = std::to_string(i).c_str();
			karabas += "\tPlaceHolder";
			myLogVector.push_back(karabas);
		}

		//
	}
	ImGui::EndDisabled();

	ImGui::End();
}

void RenderPackOptions()
{
	static bool PlaceHolderOption = false;

	ImGui::Checkbox("Pack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Pack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Pack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Pack Options Example", &PlaceHolderOption);
}

void RenderDiffOptions()
{
	static bool PlaceHolderOption = false;

	ImGui::Checkbox("Diff Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Diff Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Diff Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Diff Options Example", &PlaceHolderOption);
}

void RenderUnpackOptions()
{
	static bool PlaceHolderOption = false;

	ImGui::Checkbox("Unpack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Unpack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Unpack Options Example", &PlaceHolderOption);
	ImGui::Checkbox("Unpack Options Example", &PlaceHolderOption);
}

