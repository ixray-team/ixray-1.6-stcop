#include "api.h"
#include <imgui.h>
#include "../../editors/xrEUI/imgui_impl_sdl3.h"
#include "../xrForms/imgui_impl_sdlrenderer3.h"

#include "../../Editors/xrEUI/spectrum.h"
#include "ImGuiSpinner.h"

#include "../../xrCore/FormatParsers/json/JsonSerialize.h"

enum UIState
{
	LoginForm = 0,
	Connecting,
	ConFail,
	WorkForm
};

static xr_vector<shared_str> LogInfo;

SLoginInfo GLoginInfo;
UIState GUIState;
sql::Driver* GSQLDriver = nullptr;
sql::Connection* GSQLConnector = nullptr;

void RenderLoginUI(ImGuiViewport* viewport, bool connecting = false)
{
	if (connecting)
	{
		float r = 32;
		ImGui::SetCursorPos({ (viewport->Size.x - r * 2) / 2, (viewport->Size.y - r * 2) / 2 });
		const ImU32 col = ImGui::GetColorU32(ImGuiCol_TextDisabled);
		ImGui::Spinner("##spinner", r, 6, col);
		//ImGui::LoadingIndicatorCircle("##spin", r, { 150,150,150,255 }, { 50,50,50,255 }, 12, 2.f);
	}

	// Центрируем по вертикали (располагаем примерно по центру экрана)
	float center_y = viewport->Size.y * 0.3f;
	ImGui::SetCursorPosY(center_y);

	// Центрируем каждый элемент по горизонтали
	const float ItemWidth = 250.0f; // Ширина элементов ввода
	float WndWidth = viewport->Size.x;

	auto x = ImGui::GetContentRegionAvail().x;
	auto cur_x = ImGui::GetCursorPosX();

	ImGui::BeginDisabled(connecting);

	ImGui::SetCursorPosX((x - ItemWidth) / 2 + cur_x);
	if (ImGui::BeginChild("##loginForm", { ItemWidth,-1 }, 0, 0))
	{
		x = ImGui::GetContentRegionAvail().x;
		cur_x = ImGui::GetCursorPosX();

#define CenterText(text) ImGui::SetCursorPosX( (x-ImGui::CalcTextSize(text).x)/2 +cur_x);\
						 ImGui::Text(text);

		// Login
		ImGui::PushItemWidth(-1);
		CenterText("Login");
		ImGui::InputText("##login", GLoginInfo.Login, 32);

		// Server
		CenterText("Host");
		ImGui::InputText("##server", GLoginInfo.Host, 128);

		// Password
		CenterText("Password");
		ImGui::InputText("##pass", GLoginInfo.Pass, 32, ImGuiInputTextFlags_Password);

		ImGui::Dummy({ 0,5 });
		ImGui::Separator();
		ImGui::Dummy({ 0,5 });

		if (ImGui::Button("Login", { -1,35 }))
		{
			GUIState = UIState::Connecting;

			std::thread([&]()
			{
				try
				{
					GSQLDriver = get_driver_instance();
					GSQLConnector = GSQLDriver->connect
					(
						GLoginInfo.Host,
						GLoginInfo.Login,
						GLoginInfo.Pass
					);

					// Проверка состояния подключения
					GUIState = (GSQLConnector && GSQLConnector->isValid() ? UIState::WorkForm : UIState::ConFail);

					if (GUIState == UIState::WorkForm)
					{
						sql::Statement* stmt = GSQLConnector->createStatement();
						sql::ResultSet* res = stmt->executeQuery("SHOW DATABASES;");

						// Выводим список схем
						while (res->next())
						{
							GLoginInfo.SubDB.push_back(res->getString(1).c_str());
						}
					}
				}
				catch (const sql::SQLException& e)
				{
					GUIState = UIState::ConFail;
				}
			}).detach();
		}

		ImGui::PopItemWidth();

		ImGui::EndChild();
	}
	ImGui::EndDisabled();
}

void RenderErrorPage(ImGuiViewport* viewport)
{
	ImGui::OpenPopup("Connection Error");
	if (ImGui::BeginPopupModal("Connection Error", NULL, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::TextColored(ImVec4(1, 0.3f, 0.3f, 1), "Failed to connect to server!");

		ImGui::Spacing();
		ImGui::Separator();
		ImGui::Spacing();

		ImGuiStyle& style = ImGui::GetStyle();
		float button_width = 120.0f;
		float avail = ImGui::GetContentRegionAvail().x;
		ImGui::SetCursorPosX((avail - button_width) * 0.5f);

		if (ImGui::Button("OK", ImVec2(button_width, 0)))
		{
			GUIState = UIState::LoginForm;
		}
		ImGui::EndPopup();
	}
}

void RenderWorkForm(ImGuiViewport* viewport)
{
	static bool syncQuest = false;
	static bool syncItems = false;
	static int selectedDBIndex = 0;  // Индекс выбранной БД

	// Верхняя часть — чекбоксы и кнопка
	ImGui::BeginChild("Top", ImVec2(0, 250), true);
	ImGui::Text("Active Database:");
	ImGui::SameLine();
	if (!GLoginInfo.SubDB.empty() && ImGui::BeginCombo("##Database", GLoginInfo.SubDB[selectedDBIndex].c_str()))
	{
		for (int i = 0; i < GLoginInfo.SubDB.size(); i++)
		{
			bool isSelected = (selectedDBIndex == i);
			if (ImGui::Selectable(GLoginInfo.SubDB[i].c_str(), isSelected))
			{
				selectedDBIndex = i;  // Обновляем выбранный индекс
			}
			if (isSelected) {
				ImGui::SetItemDefaultFocus();  // Подсветка выбранного элемента
			}
		}
		ImGui::EndCombo();
	}

	ImGui::SameLine();
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 4);
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 43);
	if (ImGui::Button("Refresh", {65, 30}))
	{
		GLoginInfo.SubDB.clear();
		sql::Statement* stmt = GSQLConnector->createStatement();
		sql::ResultSet* res = stmt->executeQuery("SHOW DATABASES;");

		// Выводим список схем
		while (res->next())
		{
			GLoginInfo.SubDB.push_back(res->getString(1).c_str());
		}
	}

	ImGui::Separator();

	ImGui::Checkbox("Sync Quest", &syncQuest);
	ImGui::Checkbox("Sync Items", &syncItems);

	ImGui::EndChild();

	ImGui::ProgressBar(GLoginInfo.ProgressStatus, { 550, 20 });
	ImGui::ProgressBar(GLoginInfo.SubProgressStatus, { 550 , 20});
	ImGui::SameLine();

	ImGui::SetCursorPosX(viewport->Size.x - 90);
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 20);
	if (ImGui::Button("Sync", ImVec2(70, 30)))
	{
		GLoginInfo.ProgressStatus = 0;
		GLoginInfo.SubProgressStatus = 0;
		LogInfo.clear();
		std::thread SQLThread
		(
			[]()
			{
				RunSQLRequest(syncQuest, syncItems, *GLoginInfo.SubDB[selectedDBIndex]);
				Msg("Done!");
			}
		);

		SQLThread.detach();
	}
	// Нижняя часть — лог
	ImGui::Separator();
	ImGui::Text("Log:");
	ImGui::Separator();

	if (ImGui::BeginChild("Log", ImVec2(0, 0), true))
	{
		static bool shouldScrollToBottom = false;
		static size_t lastLogCount = 0;

		// Выводим все строки лога
		for (shared_str& Text : LogInfo)
		{
			ImGui::TextUnformatted(*Text);
		}

		if (LogInfo.size() != lastLogCount)
		{
			shouldScrollToBottom = true;
			lastLogCount = LogInfo.size();
		}

		if (shouldScrollToBottom)
		{
			ImGui::SetScrollY(ImGui::GetScrollMaxY());
			shouldScrollToBottom = false;
		}

		ImGui::EndChild();
	}
}

void UIHub()
{
	// основной viewport
	ImGuiViewport* viewport = ImGui::GetMainViewport();

	ImGui::SetNextWindowPos(viewport->Pos);
	ImGui::SetNextWindowSize(viewport->Size);

	ImGuiWindowFlags window_flags = 
		ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize |
		ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus;

	if (ImGui::Begin("##MainWnd", nullptr, window_flags))
	{
		switch (GUIState)
		{
		case UIState::LoginForm:
			RenderLoginUI(viewport);
			break;
		case UIState::Connecting:
			RenderLoginUI(viewport,true);
			break;
		case UIState::ConFail:
			RenderErrorPage(viewport);
			RenderLoginUI(viewport);
			break;
		case UIState::WorkForm:
			RenderWorkForm(viewport);
			break;
		default:
			break;
		}
		ImGui::End();
	}
}

void SDL_Application()
{
	if (!SDL_Init(SDL_INIT_EVENTS) != 0)
	{
		printf("Error: SDL_Init(): %s\n", SDL_GetError());
		return;
	}

	GUIState = UIState::LoginForm;

	SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
	g_AppInfo.Window = SDL_CreateWindow("IX-Ray MySQL DB Generator", 700, 560, window_flags);
	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL);

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);

	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO(); (void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

	XRay::ImGui::MakeEditorTheme();

	//setup cool font!
	ImGui::GetIO().Fonts->Clear();
	ImGui::Spectrum::LoadFont(18.f);

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
	ImFont* defaultFont = io.Fonts->AddFontDefault();

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	bool done = false;
	while (!done)
	{
		// Poll and handle events (inputs, window resize, etc.)
		// You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
		// - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
		// - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
		// Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			ImGui_ImplSDL3_ProcessEvent(&event);
			if (event.type == SDL_EVENT_QUIT)
				done = true;
			if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED && event.window.windowID == SDL_GetWindowID(g_AppInfo.Window))
				done = true;
		}

		// Start the Dear ImGui frame
		ImGui_ImplSDLRenderer3_NewFrame();
		ImGui_ImplSDL3_NewFrame();
		ImGui::NewFrame();

		{
			UIHub();
		}

		// Rendering
		ImGui::Render();

		SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
		SDL_RenderClear(renderer);
		ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());
		SDL_RenderPresent(renderer);
	}

	// Cleanup
	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(g_AppInfo.Window);
	SDL_Quit();

}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	Debug._initialize(false);
	Core._initialize("IXRay SQL Gen", nullptr, TRUE, "fsgame.ltx");
	
	auto LogCallback = [](const char* Msg)
	{
		LogInfo.push_back(Msg);
	};
	xrLogger::AddLogCallback(LogCallback);

	CJsonSerializer Serializer("sql_login.json");

	Msg("IX-Ray SQL Connector init...\r\nParse data from json...");
	Serializer.Read("Host", GLoginInfo.Host);
	Serializer.Read("Login", GLoginInfo.Login);
	Serializer.Read("Password", GLoginInfo.Pass);

	string_path Config;

	SDL_Application();
	xr_delete(GSQLConnector);

	Serializer.Write("Host", GLoginInfo.Host);
	Serializer.Write("Login", GLoginInfo.Login);
	Serializer.Write("Password", GLoginInfo.Pass);

	return 0;
}
