#include "UIEditorMain.h"
#include "../xrScripts/stdafx.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_process.h"
#include "../xrScripts/lua_ext.h"

static xr_string CodeText;

namespace Platform
{
	XRCORE_API xr_string TCHAR_TO_ANSI_U8(const wchar_t* C);
}

void EditorLuaCodespace()
{
	if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::LuaCodespace)])
		return;

	if (!ImGui::Begin("Lua Coder", &Engine.External.EditorStates[static_cast<u8>(EditorUI::LuaCodespace)]))
	{
		ImGui::End();
		return;
	}

	float WndSizeX = ImGui::GetWindowSize().x;
	float WndSizeY = ImGui::GetWindowSize().y;

	ImGui::InputTextMultiline(" ", CodeText.data(), 4096, { WndSizeX - 15, WndSizeY - 80}, ImGuiInputTextFlags_AllowTabInput);
	if (ImGui::Button("Run", { 70.f, 25.f }))
	{
		xr_string AnsiStr = Platform::UTF8_to_CP1251(CodeText.data());

		g_pScriptEngine->script_process(ScriptEngine::eScriptProcessorHelper)->add_script(AnsiStr.data(), true, true);
		g_pScriptEngine->script_process(ScriptEngine::eScriptProcessorHelper)->update();
	}

	ImGui::End();
}

void EditorLuaInit()
{
	CodeText.resize(4096);
	CImGuiManager::Instance().Subscribe("LuaCoder", CImGuiManager::ERenderPriority::eMedium, EditorLuaCodespace);

	CImGuiManager::Instance().Subscribe("LuaDebug", CImGuiManager::ERenderPriority::eLow, []()
	{
		static bool Attach = false;

		if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::LuaDebug)])
			return;

		if (!Attach)
		{
			DebbugerAttach();
			Attach = true;
		}
	});

}