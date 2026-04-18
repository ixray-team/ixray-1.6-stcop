#include "LogicLoader.h"
#include <filesystem>
#include <iostream>
#include "../../xrCore/xrCore.h"
#include <algorithm>
#include <cctype>

namespace fs = std::filesystem;

static EStateType ParseStateType(const xr_string& v)
{
	if (v.StartWith("walker"))return EStateType::Walker;
	if (v.StartWith("combat"))return EStateType::Combat;
	if (v.StartWith("camper"))return EStateType::Camper;
	if (v.StartWith("trader"))return EStateType::Trader;
	if (v.StartWith("anim")) return EStateType::Anim;
	if (v.StartWith("sound")) return EStateType::Sound;
	if (v.StartWith("panic")) return EStateType::Panic;
	if (v.StartWith("reactor")) return EStateType::Reactor;
	if (v.StartWith("guard")) return EStateType::Guard;
	if (v.StartWith("follow")) return EStateType::Follow;
	if (v.StartWith("idle")) return EStateType::Idle;
	if (v.StartWith("trigger")) return EStateType::Trigger;
	if (v.StartWith("action")) return EStateType::Action;
	return EStateType::Custom;
}

static xr_string Trim(const xr_string& s) 
{
	size_t start = s.find_first_not_of(" \t\n\r");
	if (start == xr_string::npos) return "";
	size_t end = s.find_last_not_of(" \t\n\r");
	return s.substr(start, end - start + 1);
}

xr_string ReadUntilOperator(const xr_string& str, size_t& pos)
{
	size_t start = pos;
	while (pos < str.size() && str[pos] != '+' && str[pos] != '-' && str[pos] != '=' && str[pos] != '!' && str[pos] != '~' && str[pos] != ',')
		pos++;
	return Trim(str.substr(start, pos - start));
}

void ParseFunction(const xr_string& str, size_t& pos, FParsedCondition& cond)
{
	size_t name_start = pos;
	while (pos < str.size() && str[pos] != '(') pos++;
	cond.FuncName = Trim(str.substr(name_start, pos - name_start));

	if (pos < str.size() && str[pos] == '(')
	{
		pos++; // Пропускаем '('
		size_t param_start = pos;
		int paren_depth = 1;
		while (pos < str.size() && paren_depth > 0)
		{
			if (str[pos] == '(') paren_depth++;
			else if (str[pos] == ')') paren_depth--;
			pos++;
		}
		// pos указывает на символ после ')'
		xr_string params_str = str.substr(param_start, pos - param_start - 1);

		// Разделяем параметры по запятой
		size_t comma = 0;
		size_t start = 0;
		while ((comma = params_str.find(',', start)) != xr_string::npos)
		{
			cond.Params.push_back(Trim(params_str.substr(start, comma - start)));
			start = comma + 1;
		}
		if (start < params_str.size())
			cond.Params.push_back(Trim(params_str.substr(start)));
	}
}

void ParseConditionBlock(const xr_string& block, xr_vector<FParsedCondition>& out)
{
	xr_string content = Trim(block);

	size_t pos = 0;
	while (pos < content.size())
	{
		char ch = content[pos];
		FParsedCondition cond;

		if (ch == '+') {
			cond.Op = FParsedCondition::AddInfo;
			pos++;
			cond.InfoName = ReadUntilOperator(content, pos);
		}
		else if (ch == '-') {
			cond.Op = FParsedCondition::RemoveInfo;
			pos++;
			cond.InfoName = ReadUntilOperator(content, pos);
		}
		else if (ch == '=') {
			cond.Op = FParsedCondition::FuncTrue;
			cond.bExpectedTrue = true;
			pos++;
			ParseFunction(content, pos, cond);
		}
		else if (ch == '!') {
			cond.Op = FParsedCondition::FuncFalse;
			cond.bExpectedTrue = false;
			pos++;
			ParseFunction(content, pos, cond);
		}
		else if (ch == '~') {
			cond.Op = FParsedCondition::Probability;
			pos++;
			cond.ProbabilityValue = atoi(content.c_str() + pos);
			// Пропускаем число
			while (pos < content.size() && isdigit(content[pos])) pos++;
		}
		else {
			pos++; // Пропускаем неизвестное
			continue;
		}

		out.push_back(cond);

		// Пропускаем возможные разделители (запятые, пробелы)
		while (pos < content.size() && (content[pos] == ',' || isspace(content[pos]))) pos++;
	}
}

// Чтение токена (имени инфопоршня) до пробела или конца строки
xr_string ReadEffectToken(const xr_string& str, size_t& pos)
{
	size_t start = pos;
	while (pos < str.size() && !isspace(str[pos]) && str[pos] != '+' && str[pos] != '-' && str[pos] != '=')
		pos++;
	return Trim(str.substr(start, pos - start));
}

// Парсинг параметров функции эффекта (разделитель ':')
void ParseEffectParams(const xr_string& params_str, xr_vector<xr_string>& out)
{
	if (params_str.empty()) return;

	size_t start = 0;
	size_t colon = 0;

	while ((colon = params_str.find(':', start)) != xr_string::npos)
	{
		out.push_back(Trim(params_str.substr(start, colon - start)));
		start = colon + 1;
	}

	// Последний параметр
	if (start < params_str.size())
	{
		out.push_back(Trim(params_str.substr(start)));
	}
}

void ParseEffectsBlock(const xr_string& block, xr_vector<FParsedEffect>& out)
{
	xr_string content = Trim(block);
	size_t pos = 0;

	while (pos < content.size())
	{
		while (pos < content.size() && isspace(content[pos])) pos++;
		if (pos >= content.size()) break;

		char ch = content[pos];
		FParsedEffect effect;

		switch (ch)
		{
		case '+':
		{
			effect.Type = FParsedEffect::GiveInfo;
			pos++; // пропускаем '+'
			effect.InfoName = ReadEffectToken(content, pos);
			out.push_back(effect);
			break;
		}

		case '-':
		{
			effect.Type = FParsedEffect::RemoveInfo;
			pos++; // пропускаем '-'
			effect.InfoName = ReadEffectToken(content, pos);
			out.push_back(effect);
			break;
		}

		case '=':
		{
			effect.Type = FParsedEffect::CallFunction;
			pos++; // пропускаем '='

			size_t name_start = pos;
			while (pos < content.size() && content[pos] != '(' && !isspace(content[pos])) pos++;
			effect.FuncName = Trim(content.substr(name_start, pos - name_start));

			if (pos < content.size() && content[pos] == '(')
			{
				pos++; // пропускаем '('
				size_t param_start = pos;
				int paren_depth = 1;

				while (pos < content.size() && paren_depth > 0)
				{
					if (content[pos] == '(') paren_depth++;
					else if (content[pos] == ')') paren_depth--;
					pos++;
				}

				// pos указывает на символ после ')'
				xr_string params_str = content.substr(param_start, pos - param_start - 1);

				// Парсим параметры (разделитель ':')
				ParseEffectParams(params_str, effect.Params);
			}

			out.push_back(effect);
			break;
		}

		case '!':
		{
			effect.Type = FParsedEffect::CallFunction;
			pos++; // пропускаем '!'

			size_t name_start = pos;
			while (pos < content.size() && content[pos] != '(' && !isspace(content[pos])) pos++;
			effect.FuncName = Trim(content.substr(name_start, pos - name_start));

			if (pos < content.size() && content[pos] == '(')
			{
				pos++;
				size_t param_start = pos;
				int paren_depth = 1;

				while (pos < content.size() && paren_depth > 0)
				{
					if (content[pos] == '(') paren_depth++;
					else if (content[pos] == ')') paren_depth--;
					pos++;
				}

				xr_string params_str = content.substr(param_start, pos - param_start - 1);
				ParseEffectParams(params_str, effect.Params);
			}

			out.push_back(effect);
			break;
		}

		default:
		{
			// Неизвестный токен - сохраняем как сырую команду
			effect.Type = FParsedEffect::CustomCommand;
			size_t start = pos;
			while (pos < content.size() && !isspace(content[pos])) pos++;
			effect.RawCommand = content.substr(start, pos - start);
			out.push_back(effect);
			break;
		}
		}

		while (pos < content.size() && isspace(content[pos]))
		{
			pos++;
		}
	}
}

xr_vector<FState> LogicLoader::LoadAsStates(const xr_string& folder)
{
	xr_vector<FState> out;
	return out;
}

xr_vector<FState> LogicLoader::LoadFromFile(const xr_string& filename)
{
	xr_vector<FState> out;
	xr_vector<FEventInfo> events;

	CInifile* ini = CInifile::Create(filename.c_str(), TRUE);
	if (!ini)
	{
		std::cerr << "LogicLoader: failed open " << filename << std::endl;
		return out;
	}

	// В LoadFromFile:
	for (auto& sect : ini->sections())
	{
		FState s;
		s.StateName = sect.Name.c_str();
		const char* sec = sect.Name.c_str();
		s.StateType = ParseStateType(s.StateName);

		FBaseParams base;
		for (auto& [Key, Value] : sect.Data)
		{
			if (!Key || !Value)
			{
				continue;
			}

			xr_string KeyStr = Key.c_str();
			xr_strlwr(KeyStr);

			if (!KeyStr.StartWith("on_") && KeyStr != "meet" && KeyStr != "wounded" && KeyStr != "danger" && KeyStr != "active")
			{
				base.CustomVariables[KeyStr] = Value.c_str();
			}
		}

		// Parse per-type friendly fields
		switch (s.StateType)
		{
		case EStateType::Walker:
		{
			FWalkerParams wp;
			if (ini->line_exist(sec, "path_walk")) wp.PathWalk = ini->r_string_wb(sec, "path_walk").c_str();
			else if (ini->line_exist(sec, "path")) wp.PathWalk = ini->r_string_wb(sec, "path").c_str();
			if (ini->line_exist(sec, "walk_speed")) wp.WalkSpeed = ini->r_float(sec, "walk_speed");
			if (ini->line_exist(sec, "combat_ignore")) wp.bCombatIgnore = ini->r_bool(sec, "combat_ignore");
			if (ini->line_exist(sec, "keep_safe_alife")) wp.bKeepSafeAlife = ini->r_bool(sec, "keep_safe_alife");
			if (ini->line_exist(sec, "animation")) wp.AnimationOverride = ini->r_string_wb(sec, "animation").c_str();

			wp.CustomVariables = base.CustomVariables;

			wp.CustomVariables.erase("path_walk");
			wp.CustomVariables.erase("path");
			wp.CustomVariables.erase("walk_speed");
			wp.CustomVariables.erase("combat_ignore");
			wp.CustomVariables.erase("keep_safe_alife");
			wp.CustomVariables.erase("animation");

			s.Params = wp;
		}
		break;

		case EStateType::Combat:
		{
			FCombatParams cp;
			if (ini->line_exist(sec, "style"))
			{
				shared_str sv = ini->r_string_wb(sec, "style");
				xr_string vs = sv.c_str();
				std::transform(vs.begin(), vs.end(), vs.begin(), ::tolower);
				if (vs == "aggressive") cp.Style = ECombatStyle::Aggressive;
				else if (vs == "sniper") cp.Style = ECombatStyle::Sniper;
				else if (vs == "coward") cp.Style = ECombatStyle::Coward;
				else cp.Style = ECombatStyle::Balanced;
			}
			if (ini->line_exist(sec, "use_cover")) cp.bUseCover = ini->r_bool(sec, "use_cover");
			if (ini->line_exist(sec, "fire_rate")) cp.FireRate = ini->r_float(sec, "fire_rate");
			if (ini->line_exist(sec, "grenade_chance")) cp.GrenadeChance = ini->r_float(sec, "grenade_chance");
			if (ini->line_exist(sec, "accuracy")) cp.AccuracyModifier = ini->r_float(sec, "accuracy");
			if (ini->line_exist(sec, "aggression_radius")) cp.AggressionRadius = ini->r_float(sec, "aggression_radius");

			cp.CustomVariables = base.CustomVariables;

			cp.CustomVariables.erase("style");
			cp.CustomVariables.erase("use_cover");
			cp.CustomVariables.erase("fire_rate");
			cp.CustomVariables.erase("grenade_chance");
			cp.CustomVariables.erase("accuracy");
			cp.CustomVariables.erase("aggression_radius");

			s.Params = cp;
		}
		break;

		case EStateType::Trader:
		{
			FTraderParams tp;
			if (ini->line_exist(sec, "trade_config")) tp.TradeConfig = ini->r_string_wb(sec, "trade_config").c_str();
			if (ini->line_exist(sec, "sections")) tp.TraderSections = ini->r_string_wb(sec, "sections").c_str();
			if (ini->line_exist(sec, "buy")) tp.bBuyItems = ini->r_bool(sec, "buy");
			if (ini->line_exist(sec, "sell")) tp.bSellItems = ini->r_bool(sec, "sell");

			tp.CustomVariables = base.CustomVariables;

			tp.CustomVariables.erase("trade_config");
			tp.CustomVariables.erase("sections");
			tp.CustomVariables.erase("buy");
			tp.CustomVariables.erase("sell");

			s.Params = tp;
		}
		break;

		case EStateType::Anim:
		{
			FAnimParams ap;
			if (ini->line_exist(sec, "animation")) ap.AnimationName = ini->r_string_wb(sec, "animation").c_str();
			if (ini->line_exist(sec, "loop")) ap.bLoopAnimation = ini->r_bool(sec, "loop");
			if (ini->line_exist(sec, "blend_in")) ap.BlendInTime = ini->r_float(sec, "blend_in");
			if (ini->line_exist(sec, "blend_out")) ap.BlendOutTime = ini->r_float(sec, "blend_out");
			if (ini->line_exist(sec, "single_hand")) ap.bUseSingleHand = ini->r_bool(sec, "single_hand");

			ap.CustomVariables = base.CustomVariables;

			ap.CustomVariables.erase("animation");
			ap.CustomVariables.erase("loop");
			ap.CustomVariables.erase("blend_in");
			ap.CustomVariables.erase("blend_out");
			ap.CustomVariables.erase("single_hand");

			s.Params = ap;
		}
		break;

		case EStateType::Panic:
		{
			FPanicParams pp(base);
			if (ini->line_exist(sec, "run_speed")) pp.RunSpeed = ini->r_float(sec, "run_speed");
			if (ini->line_exist(sec, "run_away")) pp.bRunAway = ini->r_bool(sec, "run_away");
			if (ini->line_exist(sec, "panic_timeout_ms")) pp.PanicTimeoutMs = ini->r_s32(sec, "panic_timeout_ms");
			else if (ini->line_exist(sec, "panic_timeout")) pp.PanicTimeoutMs = (int)(ini->r_float(sec, "panic_timeout") * 1000.0f);

			pp.CustomVariables = base.CustomVariables;

			pp.CustomVariables.erase("run_speed");
			pp.CustomVariables.erase("run_away");
			pp.CustomVariables.erase("panic_timeout_ms");
			pp.CustomVariables.erase("panic_timeout");

			s.Params = pp;
		}
		break;

		case EStateType::Idle:
		{
			FIdleParams ip(base);
			if (ini->line_exist(sec, "min_idle")) ip.MinIdleTime = ini->r_float(sec, "min_idle");
			if (ini->line_exist(sec, "max_idle")) ip.MaxIdleTime = ini->r_float(sec, "max_idle");
			if (ini->line_exist(sec, "idle_animations"))
			{
				shared_str sv = ini->r_string_wb(sec, "idle_animations");
				xr_string list = sv.c_str();

				// split by comma
				size_t start = 0;
				while (start < list.size())
				{
					size_t pos = list.find(',', start);
					xr_string token = (pos == xr_string::npos) ? list.substr(start) : list.substr(start, pos - start);
					// trim
					size_t a = token.find_first_not_of(" \t\r\n");
					size_t b = token.find_last_not_of(" \t\r\n");
					if (a != xr_string::npos && b != xr_string::npos)
					{
						ip.IdleAnimations.push_back(token.substr(a, b - a + 1));
					}

					if (pos == xr_string::npos)
						break;

					start = pos + 1;
				}
			}

			ip.CustomVariables = base.CustomVariables;

			ip.CustomVariables.erase("min_idle");
			ip.CustomVariables.erase("max_idle");
			ip.CustomVariables.erase("idle_animations");

			s.Params = ip;
		}
		break;

		default:
			s.Params = base;
			break;
		}

		// Вектор для событий этого состояния
		xr_vector<FEventTransition> stateEvents;

		// Парсим все линии секции
		int idx = 0;
		int lines = ini->line_count(sec);
		for (int li = 0; li < lines; ++li)
		{
			LPCSTR N = nullptr;
			LPCSTR V = nullptr;
			if (!ini->r_line(sec, li, &N, &V))
				continue;
			if (!N) continue;

			xr_string key = N;

			// Обработка on_* событий
			if (key.rfind("on_", 0) == 0)
			{
				FEventTransition eventTrans;
				eventTrans.EventKey = key;
				eventTrans.SourceState = s.StateName;
				eventTrans.Transition.DebugName = key;
				eventTrans.EventIndex = idx++;

				// Определяем тип события
				if (key == "on_timer" || key == "on_game_timer")
				{
					eventTrans.EventType = "on_timer";
					eventTrans.Transition.Condition.Type = EConditionType::OnTimer;
				}
				else if (key == "on_info")
				{
					eventTrans.EventType = "on_info";
					eventTrans.Transition.Condition.Type = EConditionType::OnInfo;
				}
				else if (key == "on_death")
				{
					eventTrans.EventType = "on_death";
					eventTrans.Transition.Condition.Type = EConditionType::OnDeath;
				}
				else if (key == "on_hit")
				{
					eventTrans.EventType = "on_hit";
					eventTrans.Transition.Condition.Type = EConditionType::OnHit;
				}
				else if (key == "on_combat")
				{
					eventTrans.EventType = "on_combat";
					eventTrans.Transition.Condition.Type = EConditionType::OnCombat;
				}
				else
				{
					eventTrans.EventType = "on_custom";
					eventTrans.Transition.Condition.Type = EConditionType::OnInfo;
				}

				// Парсим значение
				if (V && *V)
				{
					xr_string vs = Trim(xr_string(V));

					// Парсим таймер (часть до |)
					size_t pipe_pos = vs.find('|');
					if (pipe_pos != xr_string::npos)
					{
						xr_string timer_part = Trim(vs.substr(0, pipe_pos));
						try
						{
							float timerValue = std::stof(timer_part.c_str());
							eventTrans.Transition.Condition.Value = timerValue;
							eventTrans.EventType = "on_timer";
							eventTrans.Transition.Condition.Type = EConditionType::OnTimer;
						}
						catch (...) {}

						vs = Trim(vs.substr(pipe_pos + 1));
					}

					// Парсим эффекты (часть между %...%)
					size_t percent_start = vs.find('%');
					size_t percent_end = vs.rfind('%');
					xr_string effects_block;
					if (percent_start != xr_string::npos && percent_end != xr_string::npos && percent_start != percent_end)
					{
						effects_block = vs.substr(percent_start + 1, percent_end - percent_start - 1);
						vs = Trim(vs.substr(0, percent_start));
					}

					// Парсим условия (часть между {...})
					size_t cond_start = vs.find('{');
					size_t cond_end = vs.find('}');
					xr_string condition_block;
					if (cond_start != xr_string::npos && cond_end != xr_string::npos)
					{
						condition_block = vs.substr(cond_start + 1, cond_end - cond_start - 1);
						vs = Trim(vs.erase(cond_start, cond_end - cond_start + 1).data());
					}

					// Целевое состояние
					eventTrans.Transition.TargetState = vs;
					if (!eventTrans.Transition.TargetState.empty() && (eventTrans.Transition.TargetState.back() == ',' || eventTrans.Transition.TargetState.back() == ';'))
					{
						eventTrans.Transition.TargetState.pop_back();
					}

					// Парсим условия и эффекты
					if (!condition_block.empty())
					{
						ParseConditionBlock(condition_block, eventTrans.Transition.ParsedConditions);
						eventTrans.Transition.RawCondition = condition_block;
					}

					if (!effects_block.empty())
					{
						ParseEffectsBlock(effects_block, eventTrans.Transition.Effects);
						eventTrans.Transition.RawEffects = effects_block;
					}

					// Для не-таймеров пытаемся распарсить значение
					if (eventTrans.EventType != "on_timer")
					{
						try
						{
							eventTrans.Transition.Condition.Value = std::stof(vs.c_str());
						}
						catch (...)
						{
							if (!condition_block.empty())
							{
								eventTrans.Transition.Condition.InfoName = condition_block;
							}
							else
							{
								eventTrans.Transition.Condition.InfoName = vs;
							}
						}
					}
				}

				stateEvents.push_back(eventTrans);
			}
		}

		// Обработка специальных переходов (active, meet, wounded, danger)
		auto addSpecialEvent = [&](const char* eventName, const char* targetState)
		{
			if (targetState && *targetState)
			{
				FEventTransition eventTrans;
				eventTrans.EventKey = eventName;
				eventTrans.EventType = eventName;
				eventTrans.SourceState = s.StateName;
				eventTrans.Transition.DebugName = eventName;
				eventTrans.Transition.TargetState = targetState;

				if (xr_string(eventName) == "meet" && eventTrans.Transition.TargetState == "no_meet")
					return;

				stateEvents.push_back(eventTrans);
			}
		};

		if (ini->line_exist(sec, "meet"))
			addSpecialEvent("meet", ini->r_string_wb(sec, "meet").c_str());
		else if (ini->line_exist(sec, "wounded"))
			addSpecialEvent("wounded", ini->r_string_wb(sec, "wounded").c_str());
		else if (ini->line_exist(sec, "danger"))
			addSpecialEvent("danger", ini->r_string_wb(sec, "danger").c_str());

		if (ini->line_exist(sec, "active"))
		{
			FTransition t;
			t.DebugName = "active";
			t.TargetState = ini->r_string_wb(sec, "active").c_str();
			s.Transitions.push_back(t);
		}
		else
		{
			s.Events = stateEvents;
		}

		out.push_back(std::move(s));
	}

	CInifile::Destroy(ini);
	return out;
}
