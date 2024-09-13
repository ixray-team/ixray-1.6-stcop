#include "stdafx.h"
#include "ui_af_params.h"
#include "UIStatic.h"

#include "../Actor.h"
#include "../ActorCondition.h"
#include "object_broker.h"
#include "UIXmlInit.h"
#include "UIHelper.h"
#include "../../xrEngine/string_table.h"

u32 const red_clr   = color_argb(255,210,50,50);
u32 const green_clr = color_argb(255,170,170,170);

CUIArtefactParams::CUIArtefactParams(const CParamType& type)
{
	for ( u32 i = 0; i < ALife::infl_max_count; ++i )
	{
		m_immunity_item[i] = nullptr;
	}
	for ( u32 i = 0; i < ALife::eRestoreTypeMax; ++i )
	{
		m_restore_item[i] = nullptr;
	}

	m_additional_weight = nullptr;
	m_af_slots = nullptr;

	object_type = type;
}

CUIArtefactParams::~CUIArtefactParams()
{
	delete_data	( m_immunity_item );
	delete_data	( m_restore_item );
	xr_delete	( m_additional_weight );
	xr_delete	(m_af_slots);
	xr_delete	( m_Prop_line );
}

LPCSTR af_immunity_section_names[] = // ALife::EInfluenceType
{
	"radiation_immunity",		// infl_rad=0
	"burn_immunity",			// infl_fire=1
	"chemical_burn_immunity",	// infl_acid=2
	"telepatic_immunity",		// infl_psi=3
	"shock_immunity",			// infl_electra=4
};

LPCSTR af_restore_section_names[] = // ALife::EConditionRestoreType
{
	"health_restore_speed",			// eHealthRestoreSpeed=0
	"satiety_restore_speed",		// eSatietyRestoreSpeed=1
	"power_restore_speed",			// ePowerRestoreSpeed=2
	"bleeding_restore_speed",		// eBleedingRestoreSpeed=3
	"radiation_restore_speed",		// eRadiationRestoreSpeed=4
};

LPCSTR af_immunity_caption[] =  // ALife::EInfluenceType
{
	"ui_inv_outfit_radiation_protection",		// "(radiation_imm)",
	"ui_inv_outfit_burn_protection",			// "(burn_imm)",
	"ui_inv_outfit_chemical_burn_protection",	// "(chemical_burn_imm)",
	"ui_inv_outfit_telepatic_protection",		// "(telepatic_imm)",
	"ui_inv_outfit_shock_protection",			// "(shock_imm)",
};

LPCSTR af_restore_caption[] =  // ALife::EConditionRestoreType
{
	"ui_inv_health",
	"ui_inv_satiety",
	"ui_inv_power",
	"ui_inv_bleeding",
	"ui_inv_radiation",
};

void CUIArtefactParams::InitFromXml( CUIXml& xml )
{
	LPCSTR base	= "af_params";

	XML_NODE* stored_root = xml.GetLocalRoot();
	XML_NODE* base_node   = xml.NavigateToNode( base, 0 );
	if ( !base_node )
	{
		return;
	}
	CUIXmlInit::InitWindow( xml, base, 0, this );
	xml.SetLocalRoot( base_node );
	
	m_Prop_line = new CUIStatic();
	AttachChild( m_Prop_line );
	m_Prop_line->SetAutoDelete( false );	
	CUIXmlInit::InitStatic( xml, "prop_line", 0, m_Prop_line );

	for ( u32 i = 0; i < ALife::infl_max_count; ++i )
	{
		m_immunity_item[i] = new UIArtefactParamItem();
		m_immunity_item[i]->Init( xml, af_immunity_section_names[i] );
		m_immunity_item[i]->SetAutoDelete(false);

		LPCSTR name = g_pStringTable->translate(af_immunity_caption[i]).c_str();
		m_immunity_item[i]->SetCaption( name );

		xml.SetLocalRoot( base_node );
	}

	for ( u32 i = 0; i < ALife::eRestoreTypeMax; ++i )
	{
		m_restore_item[i] = new UIArtefactParamItem();
		m_restore_item[i]->Init( xml, af_restore_section_names[i] );
		m_restore_item[i]->SetAutoDelete(false);

		LPCSTR name = g_pStringTable->translate(af_restore_caption[i]).c_str();
		m_restore_item[i]->SetCaption( name );

		xml.SetLocalRoot( base_node );
	}
	
	{
		m_af_slots = new UIArtefactParamItem();
		m_af_slots->Init(xml, "af_slots");
		m_af_slots->SetAutoDelete(false);

		LPCSTR name = g_pStringTable->translate("st_prop_artefact").c_str();
		m_af_slots->SetCaption(name);
		xml.SetLocalRoot(base_node);
	}

	{
		m_additional_weight = new UIArtefactParamItem();
		m_additional_weight->Init( xml, "additional_weight" );
		m_additional_weight->SetAutoDelete(false);

		LPCSTR name = g_pStringTable->translate( "ui_inv_weight" ).c_str();
		m_additional_weight->SetCaption( name );
	}

	xml.SetLocalRoot( stored_root );
}

bool CUIArtefactParams::Check(const shared_str& af_section)
{
	return !!pSettings->line_exist(af_section, "af_actor_properties");
}

void CUIArtefactParams::SetInfo( shared_str const& af_section )
{
	DetachAll();
	AttachChild( m_Prop_line );

	CActor* actor = smart_cast<CActor*>( Level().CurrentViewEntity() );
	if ( !actor )
	{
		return;
	}

	float val = 0.0f, max_val = 1.0f;
	Fvector2 pos {0,0};
	float h = m_Prop_line->GetWndPos().y+m_Prop_line->GetWndSize().y;

	if (is_artefact())
	{
		for (u32 i = 0; i < ALife::infl_max_count; ++i)
		{
			shared_str const& sect = pSettings->r_string(af_section, "hit_absorbation_sect");
			val = pSettings->r_float(sect, af_immunity_section_names[i]);
			if (fis_zero(val))
			{
				continue;
			}
			max_val = actor->conditions().GetZoneMaxPower((ALife::EInfluenceType)i);
			val /= max_val;
			m_immunity_item[i]->SetValue(val);

			pos.set(m_immunity_item[i]->GetWndPos());
			pos.y = h;
			m_immunity_item[i]->SetWndPos(pos);

			h += m_immunity_item[i]->GetWndSize().y;
			AttachChild(m_immunity_item[i]);
		}

		for (u32 i = 0; i < ALife::eRestoreTypeMax; ++i)
		{
			val = pSettings->r_float(af_section, af_restore_section_names[i]);
			if (fis_zero(val))
			{
				continue;
			}
			m_restore_item[i]->SetValue(val);

			pos.set(m_restore_item[i]->GetWndPos());
			pos.y = h;
			m_restore_item[i]->SetWndPos(pos);

			h += m_restore_item[i]->GetWndSize().y;
			AttachChild(m_restore_item[i]);
		}
	}
	else
	{
		u32 count = READ_IF_EXISTS(pSettings, r_u32, af_section, "artefact_count", 0);
		if (count > 0)
		{
			m_af_slots->SetValue(count);

			pos.set(m_af_slots->GetWndPos());
			pos.y = h;
			m_af_slots->SetWndPos(pos);

			h += m_af_slots->GetWndSize().y;
			AttachChild(m_af_slots);
		}
	}

	{
		val	= READ_IF_EXISTS(pSettings, r_float, af_section, "additional_inventory_weight", 0.0f);
		if ( !fis_zero(val) )
		{
			m_additional_weight->SetValue( val );

			pos.set( m_additional_weight->GetWndPos() );
			pos.y = h;
			m_additional_weight->SetWndPos( pos );

			h += m_additional_weight->GetWndSize().y;
			AttachChild( m_additional_weight );
		}
	}

	SetHeight( h );
}

/// ----------------------------------------------------------------

UIArtefactParamItem::UIArtefactParamItem()
{
	m_caption   = nullptr;
	m_value     = nullptr;
	m_magnitude = 1.0f;
	m_sign_inverse = false;
	
	m_unit_str._set( "" );
	m_texture_minus._set( "" );
	m_texture_plus._set( "" );
}

UIArtefactParamItem::~UIArtefactParamItem()
{
}

void UIArtefactParamItem::Init( CUIXml& xml, LPCSTR section )
{
	CUIXmlInit::InitWindow( xml, section, 0, this );
	xml.SetLocalRoot( xml.NavigateToNode( section ) );

	m_caption   = UIHelper::CreateStatic( xml, "caption", this );
	m_value     = UIHelper::CreateTextWnd( xml, "value",   this );
	m_magnitude = xml.ReadAttribFlt( "value", 0, "magnitude", 1.0f );
	m_sign_inverse = (xml.ReadAttribInt( "value", 0, "sign_inverse", 0 ) == 1);
	
	LPCSTR unit_str = xml.ReadAttrib( "value", 0, "unit_str", "" );
	m_unit_str._set( g_pStringTable->translate( unit_str ) );
	
	LPCSTR texture_minus = xml.Read( "texture_minus", 0, "" );
	if ( texture_minus && xr_strlen(texture_minus) )
	{
		m_texture_minus._set( texture_minus );
		
		LPCSTR texture_plus = xml.Read( "caption:texture", 0, "" );
		m_texture_plus._set( texture_plus );
		VERIFY( m_texture_plus.size() );
	}
}

void UIArtefactParamItem::SetCaption( LPCSTR name )
{
	m_caption->TextItemControl()->SetText( name );
}

void UIArtefactParamItem::SetValue( float value )
{
	value *= m_magnitude;
	string32	buf;
	xr_sprintf( buf, "%+.0f", value );
	
	string256 str;
	if ( m_unit_str.size() )
	{
		xr_strconcat( str, buf, " ", m_unit_str.c_str() );
	}
	else
	{
		xr_strconcat( str, buf );
	}
	m_value->SetText( str );

	bool positive = (value >= 0.0f);
	positive      = (m_sign_inverse)? !positive : positive;
	u32 color     = (positive      )? green_clr : red_clr;
	m_value->SetTextColor( color );

	if ( m_texture_minus.size() )
	{
		if ( positive )
		{
			m_caption->InitTexture( m_texture_plus.c_str() );
		}
		else
		{
			m_caption->InitTexture( m_texture_minus.c_str() );
		}
	}

}
