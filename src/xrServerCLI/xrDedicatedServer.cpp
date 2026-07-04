#include "stdafx.h"
#include "resource.h"
#include "../xrEngine/XR_IOConsole.h"
#include "DedicatedConsoleInput.h"

#include <cctype>
#include <cstdio>
#include <string>
#include <vector>
#ifndef IXR_WINDOWS
#include <sstream>
#endif

ENGINE_API void EngineLoadStage1(char* lpCmdLine);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();
ENGINE_API void EngineLoopAndDestroy();

namespace
{
std::vector<char> BuildCommandLineBuffer(int argc, char** argv)
{
#if defined(IXR_WINDOWS)
	LPSTR rawCommandLine = GetCommandLineA();
	char* commandStart = rawCommandLine;
	if (commandStart && *commandStart)
	{
		if (*commandStart == '"')
		{
			++commandStart;
			while (*commandStart && *commandStart != '"')
			{
				++commandStart;
			}
			if (*commandStart == '"')
			{
				++commandStart;
			}
		}
		else
		{
			while (*commandStart && !std::isspace(static_cast<unsigned char>(*commandStart)))
			{
				++commandStart;
			}
		}

		while (*commandStart && std::isspace(static_cast<unsigned char>(*commandStart)))
		{
			++commandStart;
		}
	}

	std::string commandTail = commandStart ? commandStart : "";
#else
	std::ostringstream oss;
	for (int index = 1; index < argc; ++index)
	{
		if (index > 1)
		{
			oss << ' ';
		}
		oss << argv[index];
	}
	std::string commandTail = oss.str();
#endif

	std::vector<char> buffer(commandTail.begin(), commandTail.end());
	buffer.push_back('\0');
	return buffer;
}

const char* SkipConsoleMark(const char* line)
{
	if (!line || !*line)
		return line;

	static constexpr char markers[] = "~!@#$%^&*-+/";

	if (std::strchr(markers, *line))
	{
		if (line[1] != '\0')
			return line + 2;
		return line + 1;
	}

	return line;
}

void StdoutLogCallback(LPCSTR line)
{
	const char* text = SkipConsoleMark(line);
	if (text == nullptr || *text == '\0')
		return;

	const u32 length = xr_strlen(text);
	const xr_string utf8Text = Platform::ANSI_TO_UTF8(xr_string(text));
	DedicatedConsoleInput::HandleLogLine(utf8Text, length);
}

void CreateGameWindow()
{
	if (g_AppInfo.Window == NULL)
	{
		SDL_WindowFlags window_flags = SDL_WINDOW_HIDDEN;
		u32 screen_width = GetSystemMetrics(SM_CXSCREEN);
		u32 screen_height = GetSystemMetrics(SM_CYSCREEN);
		g_AppInfo.Window = SDL_CreateWindow("IX-Ray Dedicated Server", screen_width, screen_height, window_flags);
	}
}
} // namespace

int main(int argc, char** argv)
{
	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS))
	{
		return -1;
	}

	Debug._initialize(true);

#if defined(IXR_WINDOWS)
	SetConsoleOutputCP(CP_UTF8);
	SetConsoleCP(CP_UTF8);
#endif
	std::ios::sync_with_stdio(false);
	std::cin.tie(nullptr);

	g_dedicated_server = true;

	CreateGameWindow();

	std::vector<char> commandLineBuffer = BuildCommandLineBuffer(argc, argv);
	EngineLoadStage1(commandLineBuffer.data());

	EngineLoadStage2();

	Console = new CConsole();
	xrLogger::AddLogCallback(StdoutLogCallback);
	EngineLoadStage3();

	Engine.External.CreateRendererList();
	Console->Execute("renderer renderer_ds0");
	Engine.External.Initialize();

	DedicatedConsoleInput::Start();

	EngineLoadStage4();
	Console->Execute("rs_fullscreen 0");
	Console->Execute("vid_mode 800x600");
	Console->Execute("vid_restart");

	EngineLoadStage5();
	EngineLoopAndDestroy();

	DedicatedConsoleInput::Stop();

	xrLogger::RemoveLogCallback(StdoutLogCallback);
	Core._destroy();
	SDL_Quit();

	return 0;
}

