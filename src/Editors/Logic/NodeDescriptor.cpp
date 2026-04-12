#include "../../xrCore/stdafx.h"
#include "NodeEditor.h"

FNodeRenderDesc GetStateRenderDesc(FState& State)
{
	FNodeRenderDesc Desc;

	Desc.Title = State.StateName;

	switch (State.StateType)
	{
	case EStateType::Walker:
	{
		auto& P = std::get<FWalkerParams>(State.Params);

		Desc.Color = { 80, 160, 255, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Next" };

		Desc.DrawBody = [&P](const FState&)
			{
				ImGui::Text("Path: %s", P.PathWalk.c_str());
				ImGui::SetNextItemWidth(150.f);
				ImGui::SliderFloat("Speed", &P.WalkSpeed, 0.0f, 3.0f);
				ImGui::Checkbox("Combat Ignore", &P.bCombatIgnore);

				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
	} break;

	case EStateType::Combat:
	{
		auto& P = std::get<FCombatParams>(State.Params);

		Desc.Color = { 255, 80, 80, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Attack", "Retreat" };

		Desc.DrawBody = [&P](const FState&)
			{
				ImGui::Text("Style: %d", (int)P.Style);
				ImGui::Checkbox("Use Cover", &P.bUseCover);
				ImGui::SetNextItemWidth(150.f);
				ImGui::SliderFloat("Aggression", &P.AggressionRadius, 0.0f, 100.0f);

				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
	} break;

	case EStateType::Trader:
	{
		auto& P = std::get<FTraderParams>(State.Params);

		Desc.Color = { 120, 255, 120, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Trade", "Idle" };

		Desc.DrawBody = [&P](const FState&)
			{
				ImGui::Text("Config: %s", P.TradeConfig.c_str());
				ImGui::Checkbox("Buy", &P.bBuyItems);
				ImGui::Checkbox("Sell", &P.bSellItems);

				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
	} break;

	case EStateType::Anim:
	{
		auto& P = std::get<FAnimParams>(State.Params);

		Desc.Color = { 200, 200, 80, 255 };

		Desc.Inputs = { "Play" };
		Desc.Outputs = { "Done" };

		Desc.DrawBody = [&P](const FState&)
			{
				ImGui::Text("Anim: %s", P.AnimationName.c_str());
				ImGui::Checkbox("Loop", &P.bLoopAnimation);
				ImGui::SetNextItemWidth(150.f);
				ImGui::SliderFloat("Blend In", &P.BlendInTime, 0.0f, 1.0f);

				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
	} break;

	case EStateType::Idle:
	{
		auto& P = std::get<FIdleParams>(State.Params);
		Desc.Color = { 180, 180, 80, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Out" };

		Desc.DrawBody = [&P](const FState&)
			{
				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
		break;
	}
	case EStateType::Panic:
	{
		auto& P = std::get<FPanicParams>(State.Params);
		Desc.Color = { 80, 180, 180, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Out" };

		Desc.DrawBody = [&P](const FState&)
			{
				ImGui::TextUnformatted("Run Speed");
				ImGui::SetNextItemWidth(258);
				ImGui::InputFloat("##RunSpeed", &P.RunSpeed);

				ImGui::TextUnformatted("Ignore Distance");
				ImGui::SetNextItemWidth(258);
				ImGui::InputFloat("##IgnoreDistance", &P.IgnoreDistance);

				ImGui::TextUnformatted("Panic Timeout");
				ImGui::SetNextItemWidth(258);
				ImGui::InputInt("##PanicTimeoutMs", &P.PanicTimeoutMs);

				ImGui::TextUnformatted("Run Away");
				ImGui::Checkbox("##bRunAway", &P.bRunAway);

				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
		break;
	}
	default:
	{
		auto& P = std::get<FBaseParams>(State.Params);
		Desc.Color = { 180, 180, 180, 255 };

		Desc.Inputs = { "In" };
		Desc.Outputs = { "Out" };

		Desc.DrawBody = [&P](const FState&)
			{
				for (auto& Var : P.CustomVariables)
				{
					ImGui::PushID(Var.first.c_str());
					ImGui::TextUnformatted(Var.first.c_str());

					string256 Str;
					strcpy(Str, Var.second.c_str());
					ImGui::SetNextItemWidth(180);
					if (ImGui::InputText("##customvar", Str, std::size(Str)))
					{
						Var.second = Str;
					}
					ImGui::PopID();
				}
			};
	} break;
	}

	return Desc;
}