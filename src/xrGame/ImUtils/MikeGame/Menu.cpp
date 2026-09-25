#include "StdAfx.h"
#include "Menu.h"
#include <cmath>

#include "res/bg_embedded.h"
#include "res/logo_embedded.h"
#include "res/mm_but00_embedded.h"
#include "res/mm_but01_embedded.h"

#include "res/mm_tablo_embedded.h"

namespace v_obj
{
	MenuAction Menu::Draw()
	{
		const auto appsize = ImGui::GetContentRegionAvail();

		if (m_Background.IsValid())
		{
			ImGui::Image(
				m_Background.srv->GetRawSRV(),
				ImVec2(
					static_cast<float>(m_Background.width),
					static_cast<float>(m_Background.height)));
		}

		ImGui::SetCursorPos({ (appsize.x - m_Logo.width) / 2.f,190 });

		const float time = static_cast<float>(ImGui::GetTime());

		constexpr float amplitude = 0.04f;
		constexpr float speed = 0.7f;

		const float angle =
			std::sin(time * speed) * amplitude;

		const ImVec2 size(m_Logo.width, m_Logo.height);

		const ImVec2 pos = ImGui::GetCursorScreenPos();

		ImVec2 center{
			pos.x + size.x * 0.5f,
			pos.y + size.y * 0.5f
		};

		ImageRotated(
			m_Logo.srv->GetRawSRV(),
			center,
			size,
			angle);

		//ImVec2 butSize = { 250,70 };
#if tablo
		ImGui::SetCursorPos({ 0, ImGui::GetCursorPosY()+ m_Logo.height });

		ImGui::Image(
			m_mmTablo.srv->GetRawSRV(),
			ImVec2(
				static_cast<float>(m_mmTablo.width),
				static_cast<float>(m_mmTablo.height)));
#endif

		if (m_mmBut00.IsValid() && m_mmBut01.IsValid())
		{
			const ImVec2 size{
				static_cast<float>(m_mmBut00.width),
				static_cast<float>(m_mmBut00.height)
			};

			const ImVec2 pos{
				(appsize.x - size.x) / 2.0f,
				475.0f
			};

			ImGui::SetCursorPos(pos);

			ImGui::InvisibleButton("main_menu_button", size);

			const bool hovered = ImGui::IsItemHovered();
			const bool clicked = ImGui::IsItemClicked();

			ImGui::SetCursorPos(pos);

			ImGui::Image(
				hovered
				? m_mmBut01.srv->GetRawSRV()
				: m_mmBut00.srv->GetRawSRV(),
				size);

			if (clicked)
			{
				return MenuAction::eStartGame;
			}
		}

		{
			ImGui::SetCursorPos({ 0, 30 });
			if (ImGui::Button("CLICK ME TO UNLOAD THE GAME"))
			{
				return MenuAction::eExit;
			}
		}

		return MenuAction::eNone;
	}

	Menu::Menu()
	{
		m_Background = LoadTexture(
			Assets::bg,
			Assets::bg_size);

		//
		m_Logo = LoadTexture(
			Assets::logo,
			Assets::logo_size);

		m_Logo.width *= 2;
		m_Logo.height *= 2;
		//
		m_mmBut00 = LoadTexture(
			Assets::mm_but00,
			Assets::mm_but00_size);
		m_mmBut01 = LoadTexture(
			Assets::mm_but01,
			Assets::mm_but01_size);
		//
#if tablo
		m_mmTablo = LoadTexture(
			Assets::mm_tablo,
			Assets::mm_tablo_size);
#endif
	}
}