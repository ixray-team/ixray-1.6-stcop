//Weather configuration module
//1.1
//Updated 24/11/17

#include "stdafx.h"
#include "../pch_script.h"
#include "UIWeatherEditor.h"

#include "xrUIXmlParser.h"
#include "UIXmlInit.h"
#include "../../IGame_Persistent.h"
#include "../../Environment.h"
#include "../Level.h"
#include "../../XR_IOConsole.h"

#include "UI3tButton.h"

CUIWeatherEditor::CUIWeatherEditor()
{
	Init();
	UserSavedParams.is_empty = true;
	DefaultParams.is_empty = true;
	sun_pos.x = 0.0;
	sun_pos.y = 0.0;
	sky_rotation = 0.0;
}


CUIWeatherEditor::~CUIWeatherEditor()
{

}

void CUIWeatherEditor::Init()
{
	CUIXml								uiXml;

	string128		WeatherEditor_XML;
	xr_sprintf(WeatherEditor_XML, "CUIWeatherEditor_16_9.xml");

	bool xml_result = uiXml.Load(CONFIG_PATH, UI_PATH, WeatherEditor_XML);
	R_ASSERT3(xml_result, "file parsing error ", uiXml.m_xml_file_name);

	CUIXmlInit							xml_init;

	xml_init.InitWindow(uiXml, "main", 0, this);

	ScrollView = new CUIScrollView(); ScrollView->SetAutoDelete(true);
	xml_init.InitScrollView(uiXml, "scrollview", 0, ScrollView);
	AttachChild(ScrollView);

	ScrollView2 = new CUIScrollView(); ScrollView2->SetAutoDelete(true);
	xml_init.InitScrollView(uiXml, "scrollview2", 0, ScrollView2);
	AttachChild(ScrollView2);

	ScrollView3 = new CUIScrollView(); ScrollView3->SetAutoDelete(true);
	xml_init.InitScrollView(uiXml, "scrollview3", 0, ScrollView3);
	AttachChild(ScrollView3);

	ScrollView4 = new CUIScrollView(); ScrollView4->SetAutoDelete(true);
	xml_init.InitScrollView(uiXml, "scrollview4", 0, ScrollView4);
	AttachChild(ScrollView4);

	UILoadDefaultParams = new CUI3tButton(); UILoadDefaultParams->SetAutoDelete(true);
	AttachChild(UILoadDefaultParams);
	xml_init.Init3tButton(uiXml, "button_LoadDefaultParams", 0, UILoadDefaultParams);

	UILoadUserParams = new CUI3tButton(); UILoadUserParams->SetAutoDelete(true);
	AttachChild(UILoadUserParams);
	xml_init.Init3tButton(uiXml, "button_LoadUserParams", 0, UILoadUserParams);

	UISaveUserParams = new CUI3tButton(); UISaveUserParams->SetAutoDelete(true);
	AttachChild(UISaveUserParams);
	xml_init.Init3tButton(uiXml, "button_SaveUserParams", 0, UISaveUserParams);

	UIPrinToLogBTN = new CUI3tButton(); UIPrinToLogBTN->SetAutoDelete(true);
	AttachChild(UIPrinToLogBTN);
	xml_init.Init3tButton(uiXml, "button_PrintToLog", 0, UIPrinToLogBTN);

	AttachChild(&UITitle);
	xml_init.InitStatic(uiXml, "title", 0, &UITitle);

	AttachChild(&UIBack);
	xml_init.InitStatic(uiXml, "back", 0, &UIBack);


	//param ambient
	xml_init.InitStatic(uiXml, "ambient_r_title", 0, &UI_ambient_rTitle);
	UI_ambient_rTitle.AttachChild(&UI_ambient_rValue);
	xml_init.InitStatic(uiXml, "ambient_r_value", 0, &UI_ambient_rValue);
	UI_ambient_rTitle.AttachChild(&UI_f_ambient_r);
	xml_init.InitTrackBarVariable(uiXml, "track_ambient_r", 0, &UI_f_ambient_r);


	xml_init.InitStatic(uiXml, "ambient_g_title", 0, &UI_ambient_gTitle);
	UI_ambient_gTitle.AttachChild(&UI_ambient_gValue);
	xml_init.InitStatic(uiXml, "ambient_g_value", 0, &UI_ambient_gValue);
	UI_ambient_gTitle.AttachChild(&UI_f_ambient_g);
	xml_init.InitTrackBarVariable(uiXml, "track_ambient_g", 0, &UI_f_ambient_g);


	xml_init.InitStatic(uiXml, "ambient_b_title", 0, &UI_ambient_bTitle);
	UI_ambient_bTitle.AttachChild(&UI_ambient_bValue);
	xml_init.InitStatic(uiXml, "ambient_b_value", 0, &UI_ambient_bValue);
	UI_ambient_bTitle.AttachChild(&UI_f_ambient_b);
	xml_init.InitTrackBarVariable(uiXml, "track_ambient_b", 0, &UI_f_ambient_b);



	//sun_color
	xml_init.InitStatic(uiXml, "sun_color_r_title", 0, &UI_sun_rTitle);
	UI_sun_rTitle.AttachChild(&UI_sun_rValue);
	xml_init.InitStatic(uiXml, "sun_color_r_value", 0, &UI_sun_rValue);
	UI_sun_rTitle.AttachChild(&UI_f_sun_r);
	xml_init.InitTrackBarVariable(uiXml, "track_sun_color_r", 0, &UI_f_sun_r);


	xml_init.InitStatic(uiXml, "sun_color_g_title", 0, &UI_sun_gTitle);
	UI_sun_gTitle.AttachChild(&UI_sun_gValue);
	xml_init.InitStatic(uiXml, "sun_color_g_value", 0, &UI_sun_gValue);
	UI_sun_gTitle.AttachChild(&UI_f_sun_g);
	xml_init.InitTrackBarVariable(uiXml, "track_sun_color_g", 0, &UI_f_sun_g);


	xml_init.InitStatic(uiXml, "sun_color_b_title", 0, &UI_sun_bTitle);
	UI_sun_bTitle.AttachChild(&UI_sun_bValue);
	xml_init.InitStatic(uiXml, "sun_color_b_value", 0, &UI_sun_bValue);
	UI_sun_bTitle.AttachChild(&UI_f_sun_b);
	xml_init.InitTrackBarVariable(uiXml, "track_sun_color_b", 0, &UI_f_sun_b);



	//hemi_color
	xml_init.InitStatic(uiXml, "hemi_color_r_title", 0, &UI_hemi_color_rTitle);
	UI_hemi_color_rTitle.AttachChild(&UI_hemi_color_rValue);
	xml_init.InitStatic(uiXml, "hemi_color_r_value", 0, &UI_hemi_color_rValue);
	UI_hemi_color_rTitle.AttachChild(&UI_f_hemi_color_r);
	xml_init.InitTrackBarVariable(uiXml, "track_hemi_color_r", 0, &UI_f_hemi_color_r);


	xml_init.InitStatic(uiXml, "hemi_color_g_title", 0, &UI_hemi_color_gTitle);
	UI_hemi_color_gTitle.AttachChild(&UI_hemi_color_gValue);
	xml_init.InitStatic(uiXml, "hemi_color_g_value", 0, &UI_hemi_color_gValue);
	UI_hemi_color_gTitle.AttachChild(&UI_f_hemi_color_g);
	xml_init.InitTrackBarVariable(uiXml, "track_hemi_color_g", 0, &UI_f_hemi_color_g);


	xml_init.InitStatic(uiXml, "hemi_color_b_title", 0, &UI_hemi_color_bTitle);
	UI_hemi_color_bTitle.AttachChild(&UI_hemi_color_bValue);
	xml_init.InitStatic(uiXml, "hemi_color_b_value", 0, &UI_hemi_color_bValue);
	UI_hemi_color_bTitle.AttachChild(&UI_f_hemi_color_b);
	xml_init.InitTrackBarVariable(uiXml, "track_hemi_color_b", 0, &UI_f_hemi_color_b);


	//xml_init.InitStatic(uiXml, "hemi_color_a_title", 0, &UI_hemi_color_aTitle);
	//UI_hemi_color_aTitle.AttachChild(&UI_hemi_color_aValue);
	//xml_init.InitStatic(uiXml, "hemi_color_a_value", 0, &UI_hemi_color_aValue);
	//UI_hemi_color_aTitle.AttachChild(&UI_f_hemi_color_a);
	//xml_init.InitTrackBarVariable(uiXml, "track_hemi_color_a", 0, &UI_f_hemi_color_a);


	//rain_color
	xml_init.InitStatic(uiXml, "rain_color_r_title", 0, &UI_rain_color_rTitle);
	UI_rain_color_rTitle.AttachChild(&UI_rain_color_rValue);
	xml_init.InitStatic(uiXml, "rain_color_r_value", 0, &UI_rain_color_rValue);
	UI_rain_color_rTitle.AttachChild(&UI_f_rain_color_r);
	xml_init.InitTrackBarVariable(uiXml, "track_rain_color_r", 0, &UI_f_rain_color_r);


	xml_init.InitStatic(uiXml, "rain_color_g_title", 0, &UI_rain_color_gTitle);
	UI_rain_color_gTitle.AttachChild(&UI_rain_color_gValue);
	xml_init.InitStatic(uiXml, "rain_color_g_value", 0, &UI_rain_color_gValue);
	UI_rain_color_gTitle.AttachChild(&UI_f_rain_color_g);
	xml_init.InitTrackBarVariable(uiXml, "track_rain_color_g", 0, &UI_f_rain_color_g);


	xml_init.InitStatic(uiXml, "rain_color_b_title", 0, &UI_rain_color_bTitle);
	UI_rain_color_bTitle.AttachChild(&UI_rain_color_bValue);
	xml_init.InitStatic(uiXml, "rain_color_b_value", 0, &UI_rain_color_bValue);
	UI_rain_color_bTitle.AttachChild(&UI_f_rain_color_b);
	xml_init.InitTrackBarVariable(uiXml, "track_rain_color_b", 0, &UI_f_rain_color_b);



	//fog_color
	xml_init.InitStatic(uiXml, "fog_color_r_title", 0, &UI_fog_color_rTitle);
	UI_fog_color_rTitle.AttachChild(&UI_fog_color_rValue);
	xml_init.InitStatic(uiXml, "fog_color_r_value", 0, &UI_fog_color_rValue);
	UI_fog_color_rTitle.AttachChild(&UI_f_fog_color_r);
	xml_init.InitTrackBarVariable(uiXml, "track_fog_color_r", 0, &UI_f_fog_color_r);


	xml_init.InitStatic(uiXml, "fog_color_g_title", 0, &UI_fog_color_gTitle);
	UI_fog_color_gTitle.AttachChild(&UI_fog_color_gValue);
	xml_init.InitStatic(uiXml, "fog_color_g_value", 0, &UI_fog_color_gValue);
	UI_fog_color_gTitle.AttachChild(&UI_f_fog_color_g);
	xml_init.InitTrackBarVariable(uiXml, "track_fog_color_g", 0, &UI_f_fog_color_g);


	xml_init.InitStatic(uiXml, "fog_color_b_title", 0, &UI_fog_color_bTitle);
	UI_fog_color_bTitle.AttachChild(&UI_fog_color_bValue);
	xml_init.InitStatic(uiXml, "fog_color_b_value", 0, &UI_fog_color_bValue);
	UI_fog_color_bTitle.AttachChild(&UI_f_fog_color_b);
	xml_init.InitTrackBarVariable(uiXml, "track_fog_color_b", 0, &UI_f_fog_color_b);



	//clouds_color
	xml_init.InitStatic(uiXml, "clouds_color_r_title", 0, &UI_clouds_color_rTitle);
	UI_clouds_color_rTitle.AttachChild(&UI_clouds_color_rValue);
	xml_init.InitStatic(uiXml, "clouds_color_r_value", 0, &UI_clouds_color_rValue);
	UI_clouds_color_rTitle.AttachChild(&UI_f_clouds_color_r);
	xml_init.InitTrackBarVariable(uiXml, "track_clouds_color_r", 0, &UI_f_clouds_color_r);


	xml_init.InitStatic(uiXml, "clouds_color_g_title", 0, &UI_clouds_color_gTitle);
	UI_clouds_color_gTitle.AttachChild(&UI_clouds_color_gValue);
	xml_init.InitStatic(uiXml, "clouds_color_g_value", 0, &UI_clouds_color_gValue);
	UI_clouds_color_gTitle.AttachChild(&UI_f_clouds_color_g);
	xml_init.InitTrackBarVariable(uiXml, "track_clouds_color_g", 0, &UI_f_clouds_color_g);


	xml_init.InitStatic(uiXml, "clouds_color_b_title", 0, &UI_clouds_color_bTitle);
	UI_clouds_color_bTitle.AttachChild(&UI_clouds_color_bValue);
	xml_init.InitStatic(uiXml, "clouds_color_b_value", 0, &UI_clouds_color_bValue);
	UI_clouds_color_bTitle.AttachChild(&UI_f_clouds_color_b);
	xml_init.InitTrackBarVariable(uiXml, "track_clouds_color_b", 0, &UI_f_clouds_color_b);


	xml_init.InitStatic(uiXml, "clouds_color_x_title", 0, &UI_clouds_color_xTitle);
	UI_clouds_color_xTitle.AttachChild(&UI_clouds_color_xValue);
	xml_init.InitStatic(uiXml, "clouds_color_x_value", 0, &UI_clouds_color_xValue);
	UI_clouds_color_xTitle.AttachChild(&UI_f_clouds_color_x);
	xml_init.InitTrackBarVariable(uiXml, "track_clouds_color_x", 0, &UI_f_clouds_color_x);


	//sky_color
	xml_init.InitStatic(uiXml, "sky_color_r_title", 0, &UI_sky_color_rTitle);
	UI_sky_color_rTitle.AttachChild(&UI_sky_color_rValue);
	xml_init.InitStatic(uiXml, "sky_color_r_value", 0, &UI_sky_color_rValue);
	UI_sky_color_rTitle.AttachChild(&UI_f_sky_color_r);
	xml_init.InitTrackBarVariable(uiXml, "track_sky_color_r", 0, &UI_f_sky_color_r);


	xml_init.InitStatic(uiXml, "sky_color_g_title", 0, &UI_sky_color_gTitle);
	UI_sky_color_gTitle.AttachChild(&UI_sky_color_gValue);
	xml_init.InitStatic(uiXml, "sky_color_g_value", 0, &UI_sky_color_gValue);
	UI_sky_color_gTitle.AttachChild(&UI_f_sky_color_g);
	xml_init.InitTrackBarVariable(uiXml, "track_sky_color_g", 0, &UI_f_sky_color_g);


	xml_init.InitStatic(uiXml, "sky_color_b_title", 0, &UI_sky_color_bTitle);
	UI_sky_color_bTitle.AttachChild(&UI_sky_color_bValue);
	xml_init.InitStatic(uiXml, "sky_color_b_value", 0, &UI_sky_color_bValue);
	UI_sky_color_bTitle.AttachChild(&UI_f_sky_color_b);
	xml_init.InitTrackBarVariable(uiXml, "track_sky_color_b", 0, &UI_f_sky_color_b);



	//param dof
	xml_init.InitStatic(uiXml, "dof_x_title", 0, &UI_dof_xTitle);
	UI_dof_xTitle.AttachChild(&UI_dof_xValue);
	xml_init.InitStatic(uiXml, "dof_x_value", 0, &UI_dof_xValue);
	UI_dof_xTitle.AttachChild(&UI_f_dof_x);
	xml_init.InitTrackBarVariable(uiXml, "track_dof_x", 0, &UI_f_dof_x);


	xml_init.InitStatic(uiXml, "dof_y_title", 0, &UI_dof_yTitle);
	UI_dof_yTitle.AttachChild(&UI_dof_yValue);
	xml_init.InitStatic(uiXml, "dof_y_value", 0, &UI_dof_yValue);
	UI_dof_yTitle.AttachChild(&UI_f_dof_y);
	xml_init.InitTrackBarVariable(uiXml, "track_dof_y", 0, &UI_f_dof_y);


	xml_init.InitStatic(uiXml, "dof_z_title", 0, &UI_dof_zTitle);
	UI_dof_zTitle.AttachChild(&UI_dof_zValue);
	xml_init.InitStatic(uiXml, "dof_z_value", 0, &UI_dof_zValue);
	UI_dof_zTitle.AttachChild(&UI_f_dof_z);
	xml_init.InitTrackBarVariable(uiXml, "track_dof_z", 0, &UI_f_dof_z);


	//param dof kernel
	xml_init.InitStatic(uiXml, "dof_kernel_title", 0, &UI_dof_kernelTitle);
	UI_dof_kernelTitle.AttachChild(&UI_dof_kernelValue);
	xml_init.InitStatic(uiXml, "dof_kernel_value", 0, &UI_dof_kernelValue);
	UI_dof_kernelTitle.AttachChild(&UI_f_dof_kernel);
	xml_init.InitTrackBarVariable(uiXml, "track_dof_kernel", 0, &UI_f_dof_kernel);


	//param dof sky
	xml_init.InitStatic(uiXml, "dof_sky_title", 0, &UI_dof_skyTitle);
	UI_dof_skyTitle.AttachChild(&UI_dof_skyValue);
	xml_init.InitStatic(uiXml, "dof_sky_value", 0, &UI_dof_skyValue);
	UI_dof_skyTitle.AttachChild(&UI_f_dof_sky);
	xml_init.InitTrackBarVariable(uiXml, "track_dof_sky", 0, &UI_f_dof_sky);


	//sun_dir_a
	xml_init.InitStatic(uiXml, "sun_dir_a_title", 0, &UI_sun_dir_a_Title);
	UI_sun_dir_a_Title.AttachChild(&UI_sun_dir_a_Value);
	xml_init.InitStatic(uiXml, "sun_dir_a_value", 0, &UI_sun_dir_a_Value);
	UI_sun_dir_a_Title.AttachChild(&UI_f_sun_dir_a);
	xml_init.InitTrackBarVariable(uiXml, "sun_dir_a_track", 0, &UI_f_sun_dir_a);


	//sun_dir_b
	xml_init.InitStatic(uiXml, "sun_dir_b_title", 0, &UI_sun_dir_b_Title);
	UI_sun_dir_b_Title.AttachChild(&UI_sun_dir_b_Value);
	xml_init.InitStatic(uiXml, "sun_dir_b_value", 0, &UI_sun_dir_b_Value);
	UI_sun_dir_b_Title.AttachChild(&UI_f_sun_dir_b);
	xml_init.InitTrackBarVariable(uiXml, "sun_dir_b_track", 0, &UI_f_sun_dir_b);


	//sun_shafts_intensity
	xml_init.InitStatic(uiXml, "sun_shafts_intensity_title", 0, &UI_sun_shafts_intensity_Title);
	UI_sun_shafts_intensity_Title.AttachChild(&UI_sun_shafts_intensity_Value);
	xml_init.InitStatic(uiXml, "sun_shafts_intensity_value", 0, &UI_sun_shafts_intensity_Value);
	UI_sun_shafts_intensity_Title.AttachChild(&UI_f_sun_shafts_intensity);
	xml_init.InitTrackBarVariable(uiXml, "sun_shafts_intensity_track", 0, &UI_f_sun_shafts_intensity);


	//sun_lumscale
	xml_init.InitStatic(uiXml, "sun_lumscale_title", 0, &UI_sun_lumscale_Title);
	UI_sun_lumscale_Title.AttachChild(&UI_sun_lumscale_Value);
	xml_init.InitStatic(uiXml, "sun_lumscale_value", 0, &UI_sun_lumscale_Value);
	UI_sun_lumscale_Title.AttachChild(&UI_f_sun_lumscale);
	xml_init.InitTrackBarVariable(uiXml, "sun_lumscale_track", 0, &UI_f_sun_lumscale);


	//bolt_period
	xml_init.InitStatic(uiXml, "bolt_period_title", 0, &UI_bolt_period_Title);
	UI_bolt_period_Title.AttachChild(&UI_bolt_period_Value);
	xml_init.InitStatic(uiXml, "bolt_period_value", 0, &UI_bolt_period_Value);
	UI_bolt_period_Title.AttachChild(&UI_f_bolt_period);
	xml_init.InitTrackBarVariable(uiXml, "bolt_period_track", 0, &UI_f_bolt_period);


	//bolt_duration
	xml_init.InitStatic(uiXml, "bolt_duration_title", 0, &UI_bolt_duration_Title);
	UI_bolt_duration_Title.AttachChild(&UI_bolt_duration_Value);
	xml_init.InitStatic(uiXml, "bolt_duration_value", 0, &UI_bolt_duration_Value);
	UI_bolt_duration_Title.AttachChild(&UI_f_bolt_duration);
	xml_init.InitTrackBarVariable(uiXml, "bolt_duration_track", 0, &UI_f_bolt_duration);


	//rain_density
	xml_init.InitStatic(uiXml, "rain_density_title", 0, &UI_rain_density_Title);
	UI_rain_density_Title.AttachChild(&UI_rain_density_Value);
	xml_init.InitStatic(uiXml, "rain_density_value", 0, &UI_rain_density_Value);
	UI_rain_density_Title.AttachChild(&UI_f_rain_density);
	xml_init.InitTrackBarVariable(uiXml, "rain_density_track", 0, &UI_f_rain_density);


	//fog_density
	xml_init.InitStatic(uiXml, "fog_density_title", 0, &UI_fog_density_Title);
	UI_fog_density_Title.AttachChild(&UI_fog_density_Value);
	xml_init.InitStatic(uiXml, "fog_density_value", 0, &UI_fog_density_Value);
	UI_fog_density_Title.AttachChild(&UI_f_fog_density);
	xml_init.InitTrackBarVariable(uiXml, "fog_density_track", 0, &UI_f_fog_density);


	//fog_distance
	xml_init.InitStatic(uiXml, "fog_distance_title", 0, &UI_fog_distance_Title);
	UI_fog_distance_Title.AttachChild(&UI_fog_distance_Value);
	xml_init.InitStatic(uiXml, "fog_distance_value", 0, &UI_fog_distance_Value);
	UI_fog_distance_Title.AttachChild(&UI_f_fog_distance);
	xml_init.InitTrackBarVariable(uiXml, "fog_distance_track", 0, &UI_f_fog_distance);


	//far_plane
	xml_init.InitStatic(uiXml, "far_plane_title", 0, &UI_far_plane_Title);
	UI_far_plane_Title.AttachChild(&UI_far_plane_Value);
	xml_init.InitStatic(uiXml, "far_plane_value", 0, &UI_far_plane_Value);
	UI_far_plane_Title.AttachChild(&UI_f_far_plane);
	xml_init.InitTrackBarVariable(uiXml, "far_plane_track", 0, &UI_f_far_plane);


	//sky_rotation
	xml_init.InitStatic(uiXml, "sky_rotation_title", 0, &UI_sky_rotation_Title);
	UI_sky_rotation_Title.AttachChild(&UI_sky_rotation_Value);
	xml_init.InitStatic(uiXml, "sky_rotation_value", 0, &UI_sky_rotation_Value);
	UI_sky_rotation_Title.AttachChild(&UI_f_sky_rotation);
	xml_init.InitTrackBarVariable(uiXml, "sky_rotation_track", 0, &UI_f_sky_rotation);


	//clouds_velocity_0
	xml_init.InitStatic(uiXml, "clouds_velocity_0_title", 0, &UI_clouds_velocity_0_Title);
	UI_clouds_velocity_0_Title.AttachChild(&UI_clouds_velocity_0_Value);
	xml_init.InitStatic(uiXml, "clouds_velocity_0_value", 0, &UI_clouds_velocity_0_Value);
	UI_clouds_velocity_0_Title.AttachChild(&UI_f_clouds_velocity_0);
	xml_init.InitTrackBarVariable(uiXml, "clouds_velocity_0_track", 0, &UI_f_clouds_velocity_0);


	//clouds_velocity_1
	xml_init.InitStatic(uiXml, "clouds_velocity_1_title", 0, &UI_clouds_velocity_1_Title);
	UI_clouds_velocity_1_Title.AttachChild(&UI_clouds_velocity_1_Value);
	xml_init.InitStatic(uiXml, "clouds_velocity_1_value", 0, &UI_clouds_velocity_1_Value);
	UI_clouds_velocity_1_Title.AttachChild(&UI_f_clouds_velocity_1);
	xml_init.InitTrackBarVariable(uiXml, "clouds_velocity_1_track", 0, &UI_f_clouds_velocity_1);


	//water_intensity
	xml_init.InitStatic(uiXml, "water_intensity_title", 0, &UI_water_intensity_Title);
	UI_water_intensity_Title.AttachChild(&UI_water_intensity_Value);
	xml_init.InitStatic(uiXml, "water_intensity_value", 0, &UI_water_intensity_Value);
	UI_water_intensity_Title.AttachChild(&UI_f_water_intensity);
	xml_init.InitTrackBarVariable(uiXml, "water_intensity_track", 0, &UI_f_water_intensity);


	//trees_amplitude
	xml_init.InitStatic(uiXml, "trees_amplitude_title", 0, &UI_trees_amplitude_Title);
	UI_trees_amplitude_Title.AttachChild(&UI_trees_amplitude_Value);
	xml_init.InitStatic(uiXml, "trees_amplitude_value", 0, &UI_trees_amplitude_Value);
	UI_trees_amplitude_Title.AttachChild(&UI_f_trees_amplitude);
	xml_init.InitTrackBarVariable(uiXml, "trees_amplitude_track", 0, &UI_f_trees_amplitude);


	//swing_fast_speed
	xml_init.InitStatic(uiXml, "swing_fast_speed_title", 0, &UI_swing_fast_speed_Title);
	UI_swing_fast_speed_Title.AttachChild(&UI_swing_fast_speed_Value);
	xml_init.InitStatic(uiXml, "swing_fast_speed_value", 0, &UI_swing_fast_speed_Value);
	UI_swing_fast_speed_Title.AttachChild(&UI_f_swing_fast_speed);
	xml_init.InitTrackBarVariable(uiXml, "swing_fast_speed_track", 0, &UI_f_swing_fast_speed);


	//swing_normal_speed
	xml_init.InitStatic(uiXml, "swing_normal_speed_title", 0, &UI_swing_normal_speed_Title);
	UI_swing_normal_speed_Title.AttachChild(&UI_swing_normal_speed_Value);
	xml_init.InitStatic(uiXml, "swing_normal_speed_value", 0, &UI_swing_normal_speed_Value);
	UI_swing_normal_speed_Title.AttachChild(&UI_f_swing_normal_speed);
	xml_init.InitTrackBarVariable(uiXml, "swing_normal_speed_track", 0, &UI_f_swing_normal_speed);


	//swing_fast_rot1
	xml_init.InitStatic(uiXml, "swing_fast_rot1_title", 0, &UI_swing_fast_rot1_Title);
	UI_swing_fast_rot1_Title.AttachChild(&UI_swing_fast_rot1_Value);
	xml_init.InitStatic(uiXml, "swing_fast_rot1_value", 0, &UI_swing_fast_rot1_Value);
	UI_swing_fast_rot1_Title.AttachChild(&UI_f_swing_fast_rot1);
	xml_init.InitTrackBarVariable(uiXml, "swing_fast_rot1_track", 0, &UI_f_swing_fast_rot1);


	//swing_fast_rot2
	xml_init.InitStatic(uiXml, "swing_fast_rot2_title", 0, &UI_swing_fast_rot2_Title);
	UI_swing_fast_rot2_Title.AttachChild(&UI_swing_fast_rot2_Value);
	xml_init.InitStatic(uiXml, "swing_fast_rot2_value", 0, &UI_swing_fast_rot2_Value);
	UI_swing_fast_rot2_Title.AttachChild(&UI_f_swing_fast_rot2);
	xml_init.InitTrackBarVariable(uiXml, "swing_fast_rot2_track", 0, &UI_f_swing_fast_rot2);


	//swing_fast_amp1
	xml_init.InitStatic(uiXml, "swing_fast_amp1_title", 0, &UI_swing_fast_amp1_Title);
	UI_swing_fast_amp1_Title.AttachChild(&UI_swing_fast_amp1_Value);
	xml_init.InitStatic(uiXml, "swing_fast_amp1_value", 0, &UI_swing_fast_amp1_Value);
	UI_swing_fast_amp1_Title.AttachChild(&UI_f_swing_fast_amp1);
	xml_init.InitTrackBarVariable(uiXml, "swing_fast_amp1_track", 0, &UI_f_swing_fast_amp1);


	//swing_fast_amp2
	xml_init.InitStatic(uiXml, "swing_fast_amp2_title", 0, &UI_swing_fast_amp2_Title);
	UI_swing_fast_amp2_Title.AttachChild(&UI_swing_fast_amp2_Value);
	xml_init.InitStatic(uiXml, "swing_fast_amp2_value", 0, &UI_swing_fast_amp2_Value);
	UI_swing_fast_amp2_Title.AttachChild(&UI_f_swing_fast_amp2);
	xml_init.InitTrackBarVariable(uiXml, "swing_fast_amp2_track", 0, &UI_f_swing_fast_amp2);


	//swing_normal_rot1
	xml_init.InitStatic(uiXml, "swing_normal_rot1_title", 0, &UI_swing_normal_rot1_Title);
	UI_swing_normal_rot1_Title.AttachChild(&UI_swing_normal_rot1_Value);
	xml_init.InitStatic(uiXml, "swing_normal_rot1_value", 0, &UI_swing_normal_rot1_Value);
	UI_swing_normal_rot1_Title.AttachChild(&UI_f_swing_normal_rot1);
	xml_init.InitTrackBarVariable(uiXml, "swing_normal_rot1_track", 0, &UI_f_swing_normal_rot1);


	//swing_normal_rot2
	xml_init.InitStatic(uiXml, "swing_normal_rot2_title", 0, &UI_swing_normal_rot2_Title);
	UI_swing_normal_rot2_Title.AttachChild(&UI_swing_normal_rot2_Value);
	xml_init.InitStatic(uiXml, "swing_normal_rot2_value", 0, &UI_swing_normal_rot2_Value);
	UI_swing_normal_rot2_Title.AttachChild(&UI_f_swing_normal_rot2);
	xml_init.InitTrackBarVariable(uiXml, "swing_normal_rot2_track", 0, &UI_f_swing_normal_rot2);


	//swing_normal_amp1
	xml_init.InitStatic(uiXml, "swing_normal_amp1_title", 0, &UI_swing_normal_amp1_Title);
	UI_swing_normal_amp1_Title.AttachChild(&UI_swing_normal_amp1_Value);
	xml_init.InitStatic(uiXml, "swing_normal_amp1_value", 0, &UI_swing_normal_amp1_Value);
	UI_swing_normal_amp1_Title.AttachChild(&UI_f_swing_normal_amp1);
	xml_init.InitTrackBarVariable(uiXml, "swing_normal_amp1_track", 0, &UI_f_swing_normal_amp1);


	//swing_normal_amp2
	xml_init.InitStatic(uiXml, "swing_normal_amp2_title", 0, &UI_swing_normal_amp2_Title);
	UI_swing_normal_amp2_Title.AttachChild(&UI_swing_normal_amp2_Value);
	xml_init.InitStatic(uiXml, "swing_normal_amp2_value", 0, &UI_swing_normal_amp2_Value);
	UI_swing_normal_amp2_Title.AttachChild(&UI_f_swing_normal_amp2);
	xml_init.InitTrackBarVariable(uiXml, "swing_normal_amp2_track", 0, &UI_f_swing_normal_amp2);


	//wind_velocity
	xml_init.InitStatic(uiXml, "wind_velocity_title", 0, &UI_wind_velocity_Title);
	UI_wind_velocity_Title.AttachChild(&UI_wind_velocity_Value);
	xml_init.InitStatic(uiXml, "wind_velocity_value", 0, &UI_wind_velocity_Value);
	UI_wind_velocity_Title.AttachChild(&UI_f_wind_velocity);
	xml_init.InitTrackBarVariable(uiXml, "wind_velocity_track", 0, &UI_f_wind_velocity);


	//wind_direction
	xml_init.InitStatic(uiXml, "wind_direction_title", 0, &UI_wind_direction_Title);
	UI_wind_direction_Title.AttachChild(&UI_wind_direction_Value);
	xml_init.InitStatic(uiXml, "wind_direction_value", 0, &UI_wind_direction_Value);
	UI_wind_direction_Title.AttachChild(&UI_f_wind_direction);
	xml_init.InitTrackBarVariable(uiXml, "wind_direction_track", 0, &UI_f_wind_direction);


	//wind_sound_volume
	xml_init.InitStatic(uiXml, "wind_sound_volume_title", 0, &UI_wind_sound_volume_Title);
	UI_wind_sound_volume_Title.AttachChild(&UI_wind_sound_volume_Value);
	xml_init.InitStatic(uiXml, "wind_sound_volume_value", 0, &UI_wind_sound_volume_Value);
	UI_wind_sound_volume_Title.AttachChild(&UI_f_wind_sound_volume);
	xml_init.InitTrackBarVariable(uiXml, "wind_sound_volume_track", 0, &UI_f_wind_sound_volume);

	//Подразделение 1
	ScrollView->AddWindow(&UI_ambient_rTitle, false);
	ScrollView->AddWindow(&UI_ambient_gTitle, false);
	ScrollView->AddWindow(&UI_ambient_bTitle, false);

	ScrollView->AddWindow(&UI_sun_rTitle, false);
	ScrollView->AddWindow(&UI_sun_gTitle, false);
	ScrollView->AddWindow(&UI_sun_bTitle, false);

	ScrollView->AddWindow(&UI_hemi_color_rTitle, false);
	ScrollView->AddWindow(&UI_hemi_color_gTitle, false);
	ScrollView->AddWindow(&UI_hemi_color_bTitle, false);
	//ScrollView->AddWindow(&UI_hemi_color_aTitle, false);

	ScrollView->AddWindow(&UI_rain_color_rTitle, false);
	ScrollView->AddWindow(&UI_rain_color_gTitle, false);
	ScrollView->AddWindow(&UI_rain_color_bTitle, false);

	ScrollView->AddWindow(&UI_fog_color_rTitle, false);
	ScrollView->AddWindow(&UI_fog_color_gTitle, false);
	ScrollView->AddWindow(&UI_fog_color_bTitle, false);

	ScrollView->AddWindow(&UI_clouds_color_rTitle, false);
	ScrollView->AddWindow(&UI_clouds_color_gTitle, false);
	ScrollView->AddWindow(&UI_clouds_color_bTitle, false);
	ScrollView->AddWindow(&UI_clouds_color_xTitle, false);

	ScrollView->AddWindow(&UI_sky_color_rTitle, false);
	ScrollView->AddWindow(&UI_sky_color_gTitle, false);
	ScrollView->AddWindow(&UI_sky_color_bTitle, false);


	//Подразделение 2
	ScrollView2->AddWindow(&UI_dof_xTitle, false);
	ScrollView2->AddWindow(&UI_dof_yTitle, false);
	ScrollView2->AddWindow(&UI_dof_zTitle, false);

	ScrollView2->AddWindow(&UI_dof_kernelTitle, false);
	ScrollView2->AddWindow(&UI_dof_skyTitle, false);


	//Подразделение 3
	ScrollView3->AddWindow(&UI_sun_dir_a_Title, false);
	ScrollView3->AddWindow(&UI_sun_dir_b_Title, false);
	ScrollView3->AddWindow(&UI_sky_rotation_Title, false);
	ScrollView3->AddWindow(&UI_sun_shafts_intensity_Title, false);
	ScrollView3->AddWindow(&UI_sun_lumscale_Title, false);
	ScrollView3->AddWindow(&UI_bolt_period_Title, false);
	ScrollView3->AddWindow(&UI_bolt_duration_Title, false);
	ScrollView3->AddWindow(&UI_rain_density_Title, false);
	ScrollView3->AddWindow(&UI_fog_density_Title, false);
	ScrollView3->AddWindow(&UI_fog_distance_Title, false);
	ScrollView3->AddWindow(&UI_far_plane_Title, false);


	//Подразделение 4
	ScrollView4->AddWindow(&UI_clouds_velocity_0_Title, false);
	ScrollView4->AddWindow(&UI_clouds_velocity_1_Title, false);
	ScrollView4->AddWindow(&UI_water_intensity_Title, false);
	ScrollView4->AddWindow(&UI_trees_amplitude_Title, false);
	ScrollView4->AddWindow(&UI_swing_fast_speed_Title, false);
	ScrollView4->AddWindow(&UI_swing_normal_speed_Title, false);
	ScrollView4->AddWindow(&UI_swing_fast_rot1_Title, false);
	ScrollView4->AddWindow(&UI_swing_fast_rot2_Title, false);
	ScrollView4->AddWindow(&UI_swing_fast_amp1_Title, false);
	ScrollView4->AddWindow(&UI_swing_fast_amp2_Title, false);
	ScrollView4->AddWindow(&UI_swing_normal_rot1_Title, false);
	ScrollView4->AddWindow(&UI_swing_normal_rot2_Title, false);
	ScrollView4->AddWindow(&UI_swing_normal_amp1_Title, false);
	ScrollView4->AddWindow(&UI_swing_normal_amp2_Title, false);
	ScrollView4->AddWindow(&UI_wind_velocity_Title, false);
	ScrollView4->AddWindow(&UI_wind_direction_Title, false);
	ScrollView4->AddWindow(&UI_wind_sound_volume_Title, false);

}

void CUIWeatherEditor::Draw()
{
	CUIWindow::Draw();
}

void CUIWeatherEditor::InitVars()
{
	CEnvDescriptor* TargetWeather = g_pGamePersistent->Environment().Current[0];

	UI_f_ambient_r.f_controlledfloat = &TargetWeather->ambient.x;
	UI_f_ambient_g.f_controlledfloat = &TargetWeather->ambient.y;
	UI_f_ambient_b.f_controlledfloat = &TargetWeather->ambient.z;

	UI_f_sun_r.f_controlledfloat = &TargetWeather->sun_color.x;
	UI_f_sun_g.f_controlledfloat = &TargetWeather->sun_color.y;
	UI_f_sun_b.f_controlledfloat = &TargetWeather->sun_color.z;

	UI_f_hemi_color_r.f_controlledfloat = &TargetWeather->hemi_color.x;
	UI_f_hemi_color_g.f_controlledfloat = &TargetWeather->hemi_color.y;
	UI_f_hemi_color_b.f_controlledfloat = &TargetWeather->hemi_color.z;
	//UI_f_hemi_color_a.f_controlledfloat = &TargetWeather->hemi_color.w;

	UI_f_rain_color_r.f_controlledfloat = &TargetWeather->rain_color.x;
	UI_f_rain_color_g.f_controlledfloat = &TargetWeather->rain_color.y;
	UI_f_rain_color_b.f_controlledfloat = &TargetWeather->rain_color.z;

	UI_f_fog_color_r.f_controlledfloat = &TargetWeather->fog_color.x;
	UI_f_fog_color_g.f_controlledfloat = &TargetWeather->fog_color.y;
	UI_f_fog_color_b.f_controlledfloat = &TargetWeather->fog_color.z;

	//Значения цвета облаков считываются задом наперед b/g/r/a
	UI_f_clouds_color_r.f_controlledfloat = &TargetWeather->clouds_color.z;
	UI_f_clouds_color_g.f_controlledfloat = &TargetWeather->clouds_color.y;
	UI_f_clouds_color_b.f_controlledfloat = &TargetWeather->clouds_color.x;
	UI_f_clouds_color_x.f_controlledfloat = &TargetWeather->clouds_color.w;

	//Значения цвета неба считываются задом наперед b/g/r/a
	UI_f_sky_color_r.f_controlledfloat = &TargetWeather->sky_color.z;
	UI_f_sky_color_g.f_controlledfloat = &TargetWeather->sky_color.y;
	UI_f_sky_color_b.f_controlledfloat = &TargetWeather->sky_color.x;

	UI_f_dof_x.f_controlledfloat = &TargetWeather->dof_value.x;
	UI_f_dof_y.f_controlledfloat = &TargetWeather->dof_value.y;
	UI_f_dof_z.f_controlledfloat = &TargetWeather->dof_value.z;

	UI_f_dof_kernel.f_controlledfloat = &TargetWeather->dof_kernel;

	UI_f_dof_sky.f_controlledfloat = &TargetWeather->dof_sky;

	UI_f_sun_shafts_intensity.f_controlledfloat = &TargetWeather->m_fSunShaftsIntensity;

	UI_f_sun_lumscale.f_controlledfloat = &TargetWeather->sun_lumscale;

	if (pSettings->line_exist(TargetWeather->m_identifier.c_str(), "sun_dir")) sun_pos = pSettings->r_fvector2(TargetWeather->m_identifier.c_str(), "sun_dir");
	UI_f_sun_dir_a.f_controlledfloat = &sun_pos.x;
	UI_f_sun_dir_b.f_controlledfloat = &sun_pos.y;
	TargetWeather->sun_dir.setHP(deg2rad(sun_pos.y), deg2rad(sun_pos.x));

	if (pSettings->line_exist(TargetWeather->m_identifier.c_str(), "sky_rotation")) sky_rotation = pSettings->r_float(TargetWeather->m_identifier.c_str(), "sky_rotation");
	UI_f_sky_rotation.f_controlledfloat = &sky_rotation;
	TargetWeather->sky_rotation = deg2rad(sky_rotation);

	UI_f_bolt_period.f_controlledfloat = &TargetWeather->bolt_period;

	UI_f_bolt_duration.f_controlledfloat = &TargetWeather->bolt_duration;

	UI_f_rain_density.f_controlledfloat = &TargetWeather->rain_density;

	UI_f_fog_density.f_controlledfloat = &TargetWeather->fog_density;

	UI_f_fog_distance.f_controlledfloat = &TargetWeather->fog_distance;

	UI_f_far_plane.f_controlledfloat = &TargetWeather->far_plane;

	UI_f_clouds_velocity_0.f_controlledfloat = &TargetWeather->clouds_velocity_0;

	UI_f_clouds_velocity_1.f_controlledfloat = &TargetWeather->clouds_velocity_1;

	UI_f_water_intensity.f_controlledfloat = &TargetWeather->m_fWaterIntensity;

	UI_f_trees_amplitude.f_controlledfloat = &TargetWeather->m_fTreeAmplitude;

	UI_f_swing_fast_speed.f_controlledfloat = &TargetWeather->m_cSwingDesc[1].speed;

	UI_f_swing_normal_speed.f_controlledfloat = &TargetWeather->m_cSwingDesc[0].speed;

	UI_f_swing_fast_rot1.f_controlledfloat = &TargetWeather->m_cSwingDesc[1].rot1;

	UI_f_swing_fast_rot2.f_controlledfloat = &TargetWeather->m_cSwingDesc[1].rot2;

	UI_f_swing_fast_amp1.f_controlledfloat = &TargetWeather->m_cSwingDesc[1].amp1;

	UI_f_swing_fast_amp2.f_controlledfloat = &TargetWeather->m_cSwingDesc[1].amp2;

	UI_f_swing_normal_rot1.f_controlledfloat = &TargetWeather->m_cSwingDesc[0].rot1;

	UI_f_swing_normal_rot2.f_controlledfloat = &TargetWeather->m_cSwingDesc[0].rot2;

	UI_f_swing_normal_amp1.f_controlledfloat = &TargetWeather->m_cSwingDesc[0].amp1;

	UI_f_swing_normal_amp2.f_controlledfloat = &TargetWeather->m_cSwingDesc[0].amp2;

	UI_f_wind_velocity.f_controlledfloat = &TargetWeather->wind_velocity;

	UI_f_wind_direction.f_controlledfloat = &TargetWeather->wind_direction;

	UI_f_wind_sound_volume.f_controlledfloat = &TargetWeather->wind_volume;

	UpdateTracksPositions();
}

void CUIWeatherEditor::Update()
{
	CUIWindow::Update();
	CEnvDescriptor* TargetWeather = g_pGamePersistent->Environment().Current[0];
	string64						value;
	xr_sprintf(value, "%f", TargetWeather->ambient.x);
	UI_ambient_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->ambient.y);
	UI_ambient_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->ambient.z);
	UI_ambient_bValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->sun_color.x);
	UI_sun_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->sun_color.y);
	UI_sun_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->sun_color.z);
	UI_sun_bValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->hemi_color.x);
	UI_hemi_color_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->hemi_color.y);
	UI_hemi_color_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->hemi_color.z);
	UI_hemi_color_bValue.SetText(value);
	//xr_sprintf(value, "%f", TargetWeather->hemi_color.w);
	//UI_hemi_color_aValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->rain_color.x);
	UI_rain_color_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->rain_color.y);
	UI_rain_color_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->rain_color.z);
	UI_rain_color_bValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->fog_color.x);
	UI_fog_color_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->fog_color.y);
	UI_fog_color_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->fog_color.z);
	UI_fog_color_bValue.SetText(value);

	//Values of clouds color are flipped in render or in shaders
	xr_sprintf(value, "%f", TargetWeather->clouds_color.z);
	UI_clouds_color_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->clouds_color.y);
	UI_clouds_color_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->clouds_color.x);
	UI_clouds_color_bValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->clouds_color.w);
	UI_clouds_color_xValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->sky_color.x);
	UI_sky_color_rValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->sky_color.y);
	UI_sky_color_gValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->sky_color.z);
	UI_sky_color_bValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->dof_value.x);
	UI_dof_xValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->dof_value.y);
	UI_dof_yValue.SetText(value);
	xr_sprintf(value, "%f", TargetWeather->dof_value.z);
	UI_dof_zValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->dof_kernel);
	UI_dof_kernelValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->dof_sky);
	UI_dof_skyValue.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_fSunShaftsIntensity);
	UI_sun_shafts_intensity_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->sun_lumscale);
	UI_sun_lumscale_Value.SetText(value);

	xr_sprintf(value, "%f", sun_pos.x);
	UI_sun_dir_a_Value.SetText(value);
	xr_sprintf(value, "%f", sun_pos.y);
	UI_sun_dir_b_Value.SetText(value);
	TargetWeather->sun_dir.setHP(deg2rad(sun_pos.y), deg2rad(sun_pos.x));

	xr_sprintf(value, "%f", sky_rotation);
	UI_sky_rotation_Value.SetText(value);
	TargetWeather->sky_rotation = deg2rad(sky_rotation);

	xr_sprintf(value, "%f", TargetWeather->bolt_period);
	UI_bolt_period_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->bolt_duration);
	UI_bolt_duration_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->rain_density);
	UI_rain_density_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->fog_density);
	UI_fog_density_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->fog_distance);
	UI_fog_distance_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->far_plane);
	UI_far_plane_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->clouds_velocity_0);
	UI_clouds_velocity_0_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->clouds_velocity_1);
	UI_clouds_velocity_1_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_fWaterIntensity);
	UI_water_intensity_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_fTreeAmplitude);
	UI_trees_amplitude_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[1].speed);
	UI_swing_fast_speed_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[0].speed);
	UI_swing_normal_speed_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[1].rot1);
	UI_swing_fast_rot1_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[1].rot2);
	UI_swing_fast_rot2_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[1].amp1);
	UI_swing_fast_amp1_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[1].amp2);
	UI_swing_fast_amp2_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[0].rot1);
	UI_swing_normal_rot1_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[0].rot2);
	UI_swing_normal_rot2_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[0].amp1);
	UI_swing_normal_amp1_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->m_cSwingDesc[0].amp2);
	UI_swing_normal_amp2_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->wind_velocity);
	UI_wind_velocity_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->wind_direction);
	UI_wind_direction_Value.SetText(value);

	xr_sprintf(value, "%f", TargetWeather->wind_volume);
	UI_wind_sound_volume_Value.SetText(value);


}

void CUIWeatherEditor::KeepTimeAsWasAtStart()
{
	Level().SetGameTime(HoursAtStartUp, MinsAtStartUp);
}

void CUIWeatherEditor::ShowDialog(bool bDoHideIndicators)
{

	inherited::ShowDialog(bDoHideIndicators);

	Update();
	Msg("Target weather section - [%s]", g_pGamePersistent->Environment().Current[0]->m_identifier.c_str());

}

void CUIWeatherEditor::HideDialog()
{
	inherited::HideDialog();
}

void CUIWeatherEditor::PrintToLog()
{
	CEnvDescriptor* TargetWeather = g_pGamePersistent->Environment().Current[0];
	Msg("---WeatherEditor Output:");

	Msg(";Generated in WeatherEditor:");
	Msg("[%s]", TargetWeather->m_identifier.c_str());

	Msg("		sky_rotation			= %f", sky_rotation);
	Msg("		sky_color				= %f, %f, %f", TargetWeather->sky_color.x, TargetWeather->sky_color.y, TargetWeather->sky_color.z);
	Msg("		clouds_color			= %f, %f, %f, %f", TargetWeather->clouds_color.x, TargetWeather->clouds_color.y, TargetWeather->clouds_color.z, TargetWeather->clouds_color.w);
	Msg("		clouds_velocity_0		= %f", TargetWeather->clouds_velocity_0);
	Msg("		clouds_velocity_1		= %f", TargetWeather->clouds_velocity_1);
	Msg("		far_plane				= %f", TargetWeather->far_plane);
	Msg("		fog_distance			= %f", TargetWeather->fog_distance);
	Msg("		fog_color				= %f, %f, %f", TargetWeather->fog_color.x, TargetWeather->fog_color.y, TargetWeather->fog_color.z);
	Msg("		fog_density				= %f", TargetWeather->fog_density);
	Msg("		rain_color				= %f, %f, %f", TargetWeather->rain_color.x, TargetWeather->rain_color.y, TargetWeather->rain_color.z);
	Msg("		rain_density			= %f", TargetWeather->rain_density);
	Msg("		bolt_duration			= %f", TargetWeather->bolt_duration);
	Msg("		bolt_period				= %f", TargetWeather->bolt_period);
	Msg("		wind_direction			= %f", TargetWeather->wind_direction);
	Msg("		wind_velocity			= %f", TargetWeather->wind_velocity);
	Msg("		wind_sound_volume		= %f", TargetWeather->wind_volume);
	Msg("		trees_amplitude			= %f", TargetWeather->m_fTreeAmplitude);
	Msg("		swing_normal_amp1		= %f", TargetWeather->m_cSwingDesc[0].amp1);
	Msg("		swing_normal_amp2		= %f", TargetWeather->m_cSwingDesc[0].amp2);
	Msg("		swing_normal_rot1		= %f", TargetWeather->m_cSwingDesc[0].rot1);
	Msg("		swing_normal_rot2		= %f", TargetWeather->m_cSwingDesc[0].rot2);
	Msg("		swing_normal_speed		= %f", TargetWeather->m_cSwingDesc[0].speed);
	Msg("		swing_fast_amp1			= %f", TargetWeather->m_cSwingDesc[1].amp1);
	Msg("		swing_fast_amp2			= %f", TargetWeather->m_cSwingDesc[1].amp2);
	Msg("		swing_fast_rot1			= %f", TargetWeather->m_cSwingDesc[1].rot1);
	Msg("		swing_fast_rot2			= %f", TargetWeather->m_cSwingDesc[1].rot2);
	Msg("		swing_fast_speed		= %f", TargetWeather->m_cSwingDesc[1].speed);
	Msg("		hemi_color				= %f, %f, %f, %f", TargetWeather->hemi_color.x, TargetWeather->hemi_color.y, TargetWeather->hemi_color.z, TargetWeather->hemi_color.w);
	Msg("		sun_color				= %f, %f, %f", TargetWeather->sun_color.x, TargetWeather->sun_color.y, TargetWeather->sun_color.z);
	Msg("		sun_dir					= %f, %f", sun_pos.x ,sun_pos.y);
	Msg("		sun_shafts_intensity	= %f", TargetWeather->m_fSunShaftsIntensity);
	Msg("		sun_lumscale			= %f", TargetWeather->sun_lumscale);
	Msg("		water_intensity			= %f", TargetWeather->m_fWaterIntensity);
	Msg("		ambient					= %f, %f, %f", TargetWeather->ambient.x, TargetWeather->ambient.y, TargetWeather->ambient.z);
	Msg("		dof						= %f, %f, %f", TargetWeather->dof_value.x, TargetWeather->dof_value.y, TargetWeather->dof_value.z);
	Msg("		dof_kernel				= %f", TargetWeather->dof_kernel);
	Msg("		dof_sky					= %f", TargetWeather->dof_sky);
	Msg("		env_ambient				= %s", pSettings->r_string(TargetWeather->m_identifier.c_str(), "env_ambient"));
	Msg("		sky_texture				= %s", pSettings->r_string(TargetWeather->m_identifier.c_str(), "sky_texture"));
	Msg("		clouds_texture			= %s", pSettings->r_string(TargetWeather->m_identifier.c_str(), "clouds_texture"));
	Msg("		flares					= %s", pSettings->r_string(TargetWeather->m_identifier.c_str(), "flares"));
	Msg("		thunderbolt				= %s", pSettings->r_string(TargetWeather->m_identifier.c_str(), "thunderbolt"));

	Msg("---WeatherEditor Output End");
	Console->Execute("flush");
}

void CUIWeatherEditor::SaveParams(SavedData* saveid)
{
	Msg("---WeatherEditor SaveParams:");
	CEnvDescriptor* TargetWeather = g_pGamePersistent->Environment().Current[0];

	saveid->Saved_ambient_color = TargetWeather->ambient;
	saveid->Saved_sun_color = TargetWeather->sun_color;
	saveid->Saved_hemi_color = TargetWeather->hemi_color;
	saveid->Saved_rain_color = TargetWeather->rain_color;
	saveid->Saved_fog_color = TargetWeather->fog_color;
	saveid->Saved_clouds_color = TargetWeather->clouds_color;
	saveid->Saved_sky_color = TargetWeather->sky_color;
	saveid->Saved_dof = TargetWeather->dof_value;
	saveid->Saved_sun_shafts_intensity = TargetWeather->m_fSunShaftsIntensity;
	saveid->Saved_sun_lumscale = TargetWeather->sun_lumscale;
	saveid->Saved_sun_pos = sun_pos;
	saveid->Saved_sky_rotation = sky_rotation;
	saveid->Saved_bolt_period = TargetWeather->bolt_period;
	saveid->Saved_bolt_duration = TargetWeather->bolt_duration;
	saveid->Saved_rain_density = TargetWeather->rain_density;
	saveid->Saved_fog_density = TargetWeather->fog_density;
	saveid->Saved_fog_distance = TargetWeather->fog_distance;
	saveid->Saved_far_plane = TargetWeather->far_plane;
	saveid->Saved_clouds_velocity_0 = TargetWeather->clouds_velocity_0;
	saveid->Saved_clouds_velocity_1 = TargetWeather->clouds_velocity_1;
	saveid->Saved_water_intensity = TargetWeather->m_fWaterIntensity;
	saveid->Saved_trees_amplitude = TargetWeather->m_fTreeAmplitude;
	saveid->Saved_swing_fast_speed = TargetWeather->m_cSwingDesc[1].speed;
	saveid->Saved_swing_normal_speed = TargetWeather->m_cSwingDesc[0].speed;
	saveid->Saved_swing_fast_rot1 = TargetWeather->m_cSwingDesc[1].rot1;
	saveid->Saved_swing_fast_rot2 = TargetWeather->m_cSwingDesc[1].rot2;
	saveid->Saved_swing_fast_amp1 = TargetWeather->m_cSwingDesc[1].amp1;
	saveid->Saved_swing_fast_amp2 = TargetWeather->m_cSwingDesc[1].amp2;
	saveid->Saved_swing_normal_rot1 = TargetWeather->m_cSwingDesc[0].rot1;
	saveid->Saved_swing_normal_rot2 = TargetWeather->m_cSwingDesc[0].rot2;
	saveid->Saved_swing_normal_amp1 = TargetWeather->m_cSwingDesc[0].amp1;
	saveid->Saved_swing_normal_amp2 = TargetWeather->m_cSwingDesc[0].amp2;
	saveid->Saved_wind_velocity = TargetWeather->wind_velocity;
	saveid->Saved_wind_direction = TargetWeather->wind_direction;
	saveid->Saved_wind_sound_volume = TargetWeather->wind_volume;
	saveid->Saved_dof_kernel = TargetWeather->dof_kernel;
	saveid->Saved_dof_sky = TargetWeather->dof_sky;

	saveid->is_empty = false;
}

void CUIWeatherEditor::LoadParams(SavedData* saveid)
{
	Msg("---WeatherEditor LoadParams:");
	if (!saveid->is_empty){
		CEnvDescriptor* TargetWeather = g_pGamePersistent->Environment().Current[0];

		TargetWeather->ambient = saveid->Saved_ambient_color;
		TargetWeather->sun_color = saveid->Saved_sun_color;
		TargetWeather->hemi_color = saveid->Saved_hemi_color;
		TargetWeather->rain_color = saveid->Saved_rain_color;
		TargetWeather->fog_color = saveid->Saved_fog_color;
		TargetWeather->clouds_color = saveid->Saved_clouds_color;
		TargetWeather->sky_color = saveid->Saved_sky_color;
		TargetWeather->dof_value = saveid->Saved_dof;
		TargetWeather->m_fSunShaftsIntensity = saveid->Saved_sun_shafts_intensity;
		TargetWeather->sun_lumscale = saveid->Saved_sun_lumscale;

		sun_pos = saveid->Saved_sun_pos;
		TargetWeather->sun_dir.setHP(deg2rad(sun_pos.y), deg2rad(sun_pos.x));

		sky_rotation = saveid->Saved_sky_rotation;
		TargetWeather->sky_rotation = deg2rad(sky_rotation);

		TargetWeather->bolt_period = saveid->Saved_bolt_period;
		TargetWeather->bolt_duration = saveid->Saved_bolt_duration;
		TargetWeather->rain_density = saveid->Saved_rain_density;
		TargetWeather->fog_density = saveid->Saved_fog_density;
		TargetWeather->fog_distance = saveid->Saved_fog_distance;
		TargetWeather->far_plane = saveid->Saved_far_plane;
		TargetWeather->clouds_velocity_0 = saveid->Saved_clouds_velocity_0;
		TargetWeather->clouds_velocity_1 = saveid->Saved_clouds_velocity_1;
		TargetWeather->m_fWaterIntensity = saveid->Saved_water_intensity;
		TargetWeather->m_fTreeAmplitude = saveid->Saved_trees_amplitude;
		TargetWeather->m_cSwingDesc[1].speed = saveid->Saved_swing_fast_speed;
		TargetWeather->m_cSwingDesc[0].speed = saveid->Saved_swing_normal_speed;
		TargetWeather->m_cSwingDesc[1].rot1 = saveid->Saved_swing_fast_rot1;
		TargetWeather->m_cSwingDesc[1].rot2 = saveid->Saved_swing_fast_rot2;
		TargetWeather->m_cSwingDesc[1].amp1 = saveid->Saved_swing_fast_amp1;
		TargetWeather->m_cSwingDesc[1].amp2 = saveid->Saved_swing_fast_amp2;
		TargetWeather->m_cSwingDesc[0].rot1 = saveid->Saved_swing_normal_rot1;
		TargetWeather->m_cSwingDesc[0].rot2 = saveid->Saved_swing_normal_rot2;
		TargetWeather->m_cSwingDesc[0].amp1 = saveid->Saved_swing_normal_amp1;
		TargetWeather->m_cSwingDesc[0].amp2 = saveid->Saved_swing_normal_amp2;
		TargetWeather->wind_velocity = saveid->Saved_wind_velocity;
		TargetWeather->wind_direction = saveid->Saved_wind_direction;
		TargetWeather->wind_volume = saveid->Saved_wind_sound_volume;
		TargetWeather->dof_kernel = saveid->Saved_dof_kernel;
		TargetWeather->dof_sky = saveid->Saved_dof_sky;

		UpdateTracksPositions();
	}
}

void CUIWeatherEditor::UpdateTracksPositions()
{
	//Msg("---WeatherEditor UpdateTracksPositions:");


	UI_f_ambient_r.SetCurenntValue();
	UI_f_ambient_g.SetCurenntValue();
	UI_f_ambient_b.SetCurenntValue();

	UI_f_sun_r.SetCurenntValue();
	UI_f_sun_g.SetCurenntValue();
	UI_f_sun_b.SetCurenntValue();

	UI_f_hemi_color_r.SetCurenntValue();
	UI_f_hemi_color_g.SetCurenntValue();
	UI_f_hemi_color_b.SetCurenntValue();
	//UI_f_hemi_color_a.SetCurenntValue();

	UI_f_rain_color_r.SetCurenntValue();
	UI_f_rain_color_g.SetCurenntValue();
	UI_f_rain_color_b.SetCurenntValue();

	UI_f_fog_color_r.SetCurenntValue();
	UI_f_fog_color_g.SetCurenntValue();
	UI_f_fog_color_b.SetCurenntValue();

	UI_f_clouds_color_r.SetCurenntValue();
	UI_f_clouds_color_g.SetCurenntValue();
	UI_f_clouds_color_b.SetCurenntValue();
	UI_f_clouds_color_x.SetCurenntValue();


	UI_f_sky_color_r.SetCurenntValue();
	UI_f_sky_color_g.SetCurenntValue();
	UI_f_sky_color_b.SetCurenntValue();

	UI_f_dof_x.SetCurenntValue();
	UI_f_dof_y.SetCurenntValue();
	UI_f_dof_z.SetCurenntValue();

	UI_f_dof_kernel.SetCurenntValue();

	UI_f_dof_sky.SetCurenntValue();

	UI_f_sun_shafts_intensity.SetCurenntValue();

	UI_f_sun_lumscale.SetCurenntValue();

	UI_f_sun_dir_a.SetCurenntValue();

	UI_f_sun_dir_b.SetCurenntValue();

	UI_f_sky_rotation.SetCurenntValue();

	UI_f_bolt_period.SetCurenntValue();

	UI_f_bolt_duration.SetCurenntValue();

	UI_f_rain_density.SetCurenntValue();

	UI_f_fog_density.SetCurenntValue();

	UI_f_fog_distance.SetCurenntValue();

	UI_f_far_plane.SetCurenntValue();

	UI_f_clouds_velocity_0.SetCurenntValue();

	UI_f_clouds_velocity_1.SetCurenntValue();

	UI_f_water_intensity.SetCurenntValue();

	UI_f_trees_amplitude.SetCurenntValue();

	UI_f_swing_fast_speed.SetCurenntValue();

	UI_f_swing_normal_speed.SetCurenntValue();

	UI_f_swing_fast_rot1.SetCurenntValue();

	UI_f_swing_fast_rot2.SetCurenntValue();

	UI_f_swing_fast_amp1.SetCurenntValue();

	UI_f_swing_fast_amp2.SetCurenntValue();

	UI_f_swing_normal_rot1.SetCurenntValue();

	UI_f_swing_normal_rot2.SetCurenntValue();

	UI_f_swing_normal_amp1.SetCurenntValue();

	UI_f_swing_normal_amp2.SetCurenntValue();

	UI_f_wind_velocity.SetCurenntValue();

	UI_f_wind_direction.SetCurenntValue();

	UI_f_wind_sound_volume.SetCurenntValue();
}

void CUIWeatherEditor::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if (UILoadDefaultParams == pWnd && BUTTON_CLICKED == msg)
	{
		LoadParams(&DefaultParams);
	}
	if (UILoadUserParams == pWnd && BUTTON_CLICKED == msg)
	{
		LoadParams(&UserSavedParams);
	}
	if (UISaveUserParams == pWnd && BUTTON_CLICKED == msg)
	{
		SaveParams(&UserSavedParams);
	}
	if (UIPrinToLogBTN == pWnd && BUTTON_CLICKED == msg)
	{
		PrintToLog();
	}

	CUIWindow::SendMessage(pWnd, msg, pData);
}

bool CUIWeatherEditor::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	CUIWindow::OnMouseAction(x, y, mouse_action);

	switch (mouse_action)
	{

	case WINDOW_LBUTTON_DOWN:
	case WINDOW_LBUTTON_UP:
	case WINDOW_CBUTTON_DOWN:
	case WINDOW_MOUSE_WHEEL_UP:
	case WINDOW_RBUTTON_DOWN:
	case WINDOW_MOUSE_WHEEL_DOWN:
	{
		Update();
		KeepTimeAsWasAtStart();
	}
	break;
	};
	return true;
}