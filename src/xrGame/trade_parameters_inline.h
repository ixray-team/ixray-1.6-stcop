////////////////////////////////////////////////////////////////////////////
//	Module 		: trade_parameters_inline.h
//	Created 	: 13.01.2006
//  Modified 	: 13.01.2006
//	Author		: Dmitriy Iassenev
//	Description : trade parameters class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	CTradeParameters::CTradeParameters						(const shared_str &section) :
	m_buy	(
		CTradeFactors(
			pSettings->r_float(section,"buy_price_factor_hostile"),
			pSettings->r_float(section,"buy_price_factor_friendly")
		)
	),
	m_sell	(
		CTradeFactors(
			pSettings->r_float(section,"sell_price_factor_hostile"),
			pSettings->r_float(section,"sell_price_factor_friendly")
		)
	)
{
}

IC	void CTradeParameters::clear							()
{
	m_buy.clear				();
	m_sell.clear			();
}

IC	CTradeParameters &CTradeParameters::instance			()
{
	if (m_instance)
		return				(*m_instance);

	m_instance				= new CTradeParameters();
	return					(*m_instance);
}

IC	void CTradeParameters::clean							()
{
	xr_delete				(m_instance);
}

IC	CTradeParameters &default_trade_parameters				()
{
	return					(CTradeParameters::instance());
}

IC	const CTradeActionParameters &CTradeParameters::action	(action_buy) const
{
	return					(m_buy);
}

IC	const CTradeActionParameters &CTradeParameters::action	(action_sell) const
{
	return					(m_sell);
}

IC	const CTradeBoolParameters &CTradeParameters::action	(action_show) const
{
	return					(m_show);
}

IC	CTradeActionParameters &CTradeParameters::action	(action_buy)
{
	return					(m_buy);
}

IC	CTradeActionParameters &CTradeParameters::action	(action_sell)
{
	return					(m_sell);
}

IC	CTradeBoolParameters &CTradeParameters::action		(action_show)
{
	return					(m_show);
}

template <typename _action_type>
IC	bool CTradeParameters::enabled							(_action_type type, const shared_str &section) const
{
	if (action(type).disabled(section))
		return				(false);

	if (default_trade_parameters().action(type).disabled(section))
		return				(false);

	return					(true);
}

template <typename _action_type>
IC	const CTradeFactors &CTradeParameters::factors			(_action_type type, const shared_str &section) const
{
	VERIFY					(enabled(type,section));

	if (action(type).enabled(section))
		return				(action(type).factors(section));

	if (default_trade_parameters().action(type).enabled(section))
		return				(default_trade_parameters().action(type).factors(section));

	return					(action(type).default_factors());
}

template <typename _action_type>
IC	void CTradeParameters::process							(_action_type type, CInifile &ini_file, const shared_str &section)
{
	R_ASSERT2(ini_file.section_exist(section), make_string<const char*>("cannot find section %s", *section));

	CTradeActionParameters	&_action = action(type);
	_action.clear			();

	auto ParseTradeParametersFunc = [&](shared_str args, u8& param_num, float& param1, float& param2)
	{
		if (!args.size()) {
			param_num = 0;
			return;
		}

		param_num = _GetItemCount(*args);
		string256 temp;
		if (param_num < 2)
		{
			param1 = atof(_GetItem(*args, 0, temp));
			param2 = param1;
		} else
		{
			param1 = atof(_GetItem(*args, 0, temp));
			param2 = atof(_GetItem(*args, 1, temp));
		}
	};

	auto ProcessSingleTradeItemSettingFunc = [&](shared_str loc_section, u8 param_num, float param1, float param2)
	{
		
		if (!param_num) {
			_action.disable(loc_section);
			return;
		}

		if(param_num < 2)
		{
			param2 = param1;
		}

		_action.enable		(
			loc_section,
			CTradeFactors	(
				param1,
				param2
			)
		);
	};

	auto ProcessMultipleTradeItemsSettingsFunc = [&](this auto self, shared_str loc_section, u8 param_num, float param1, float param2)
	{
		if(!I_ASSERT(pSettings->section_exist(loc_section)))
		{
			return;
		}

		for(auto& Item : pSettings->r_section(loc_section).Data)
		{
			if (!pSettings->section_exist(Item.first))
			{
				if(Item.first.c_str()[0] == '$')
				{
					LPCSTR section_name = Item.first.c_str()+1;
					self(section_name,  param_num, param1, param2);
					continue;
				}
				Msg("! Section [%s] (parsing trade list [%s]) doesn't exist!", Item.first.c_str(), loc_section.c_str());
				continue;
			}
			ProcessSingleTradeItemSettingFunc(Item.first, param_num, param1, param2);
		}
	};

	CInifile::Sect			&S = ini_file.r_section(section);
	for(auto& Sect : S.Data)
	{
		u8 param_num = 0;
		float param1, param2;
		if (!pSettings->section_exist(Sect.first))
		{
			if(Sect.first.c_str()[0] == '$')
			{
				LPCSTR section_name = Sect.first.c_str()+1;
				ParseTradeParametersFunc(Sect.second, param_num,param1,param2);
				ProcessMultipleTradeItemsSettingsFunc(section_name,  param_num, param1, param2);
				continue;
			}
			Msg("! Section [%s] (parse trade config file [%s]) doesn't exist!", Sect.first.c_str(), ini_file.fname());
			continue;
		}
		ParseTradeParametersFunc(Sect.second,param_num,param1,param2);
		ProcessSingleTradeItemSettingFunc(Sect.first, param_num, param1, param2);
	}
}

template <typename _action_type>
IC	void CTradeParameters::default_factors(_action_type type, const CTradeFactors &trade_factors)
{
	action(type).default_factors(trade_factors);
}
