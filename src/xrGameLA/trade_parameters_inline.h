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
			pSettings->r_float(section,"buy_price_factor_friendly"),
			pSettings->r_float(section,"buy_price_factor_hostile")
		),
		!pSettings->r_bool(section,"buy_allowed")
	),
	m_sell	(
		CTradeFactors(
			pSettings->r_float(section,"sell_price_factor_friendly"),
			pSettings->r_float(section,"sell_price_factor_hostile")
		),
		!pSettings->r_bool(section,"sell_allowed")
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

	if (action(type).enabled(section))
		return				(true);

	if (default_trade_parameters().action(type).disabled(section))
		return				(false);

	if (default_trade_parameters().action(type).enabled(section))
		return				(true);

	return					(!action(type).default_disabled());
}

IC	bool CTradeParameters::enabled							(action_show type, const shared_str &section) const
{
	if (action(type).disabled(section))
		return				(false);

	if (default_trade_parameters().action(type).disabled(section))
		return				(false);

	return					(!action(type).default_disabled());
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

template <typename _action_type, typename Container>
IC	void CTradeParameters::process							(_action_type type, Container cnt)
{
	CTradeActionParameters	&_action = action(type);
	_action.clear			();
	for (auto I = cnt.begin(); I != cnt.end(); ++I) {
		//R_ASSERT2(pSettings->section_exist((*I).first), make_string("%s has invalid section %s", *section, (*I).first.c_str())); 
		if (!pSettings->section_exist((*I).first))
		{
			Msg("! Invalid section %s", (*I).first.c_str()); 
			continue;
		}
		if (!(*I).second.size()) {
			_action.disable	((*I).first);
			continue;
		}

		string256			temp0, temp1;
		THROW3				(_GetItemCount(*(*I).second) == 2, "Invalid parameters for item",*(*I).first);
		_action.enable		(
			(*I).first,
			CTradeFactors	(
				(float)atof(_GetItem(*(*I).second,0,temp0)),
				(float)atof(_GetItem(*(*I).second,1,temp1))
			)
		);
	}
}

template <typename Container>
IC void CTradeParameters::process	(action_show, Container cnt)
{
	for (auto I = cnt.begin() ; I != cnt.end(); ++I)
		if (!(*I).second.size())
			m_show.disable	((*I).first);
}

template <typename _action_type>
IC	void CTradeParameters::process(_action_type type, CInifile &ini_file, const shared_str &section)
{
	R_ASSERT2(ini_file.section_exist(section),make_string<const char*>("cannot find section %s",*section));
	auto &S = ini_file.r_section(section);
	process(type, S.Data);
}

template <typename _action_type>
IC	void CTradeParameters::default_factors(_action_type type, const CTradeFactors &trade_factors)
{
	action(type).default_factors(trade_factors);
}

template <typename _action_type>
IC	void CTradeParameters::default_disabled(_action_type type, bool flag)
{
	action(type).default_disabled(flag);
}

