#include "../../xrCore/stdafx.h"
#include "NodeEditor.h"

constexpr float NodeItemWidth = 258.f;

string2048 CustomVarStr;

void DrawCustomVars(FBaseParams* Params)
{
	for (auto& Var : Params->CustomVariables)
	{
		ImGui::PushID(Var.first.c_str());
		ImGui::TextUnformatted(Var.first.c_str());

		strcpy(CustomVarStr, Var.second.c_str());
		ImGui::SetNextItemWidth(NodeItemWidth);
		if (ImGui::InputText("##customvar", CustomVarStr, std::size(CustomVarStr)))
		{
			Var.second = CustomVarStr;
		}
		ImGui::PopID();
	}
}

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
				ImGui::TextUnformatted("Path:");
				string256 Str;
				strcpy(Str, P.PathWalk.c_str());
				ImGui::SetNextItemWidth(NodeItemWidth);
				if (ImGui::InputText("##customvar", Str, std::size(Str)))
				{
					P.PathWalk = Str;
				}

				ImGui::TextUnformatted("Speed:");
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::SliderFloat("##Speed", &P.WalkSpeed, 0.0f, 3.0f);
				ImGui::Checkbox("Combat Ignore", &P.bCombatIgnore);

				DrawCustomVars(&P);
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
				ImGui::TextUnformatted("Aggression:");
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::SliderFloat("##Aggression", &P.AggressionRadius, 0.0f, 100.0f);

				DrawCustomVars(&P);
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

				DrawCustomVars(&P);
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
				ImGui::TextUnformatted("Blend In:");
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::SliderFloat("##Blend In", &P.BlendInTime, 0.0f, 1.0f);

				DrawCustomVars(&P);
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
				DrawCustomVars(&P);
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
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::InputFloat("##RunSpeed", &P.RunSpeed);

				ImGui::TextUnformatted("Ignore Distance");
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::InputFloat("##IgnoreDistance", &P.IgnoreDistance);

				ImGui::TextUnformatted("Panic Timeout");
				ImGui::SetNextItemWidth(NodeItemWidth);
				ImGui::InputInt("##PanicTimeoutMs", &P.PanicTimeoutMs);

				ImGui::TextUnformatted("Run Away");
				ImGui::Checkbox("##bRunAway", &P.bRunAway);

				DrawCustomVars(&P);
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
				DrawCustomVars(&P);
			};
	} break;
	}

	return Desc;
}