#pragma once

#include "UIDialogWnd.h"
#include "UIStatic.h"
#include "UITrackBarVariable.h"
#include "UIScrollView.h"

class CUI3tButton;
class CUIWeatherEditor : public CUIDialogWnd
{
private:
	typedef CUIDialogWnd	inherited;
public:
	CUIWeatherEditor();
	virtual					~CUIWeatherEditor();
	//Функции УИ окна
	virtual void			Init();
	virtual void			Update();
	virtual void			Draw();
	virtual void			ShowDialog(bool bDoHideIndicators);
	virtual void			HideDialog();
	virtual void			SendMessage(CUIWindow *pWnd, s16 msg, void *pData);
	virtual bool			OnMouseAction(float x, float y, EUIMessages mouse_action);

	//Внутренняя механика
	u32						HoursAtStartUp;
	u32						MinsAtStartUp;
	virtual void			KeepTimeAsWasAtStart();
	//Вывести в лог значения параметров
	virtual void			PrintToLog();

	virtual void			InitVars();
	//Обновить позиции полосок
	virtual void			UpdateTracksPositions();

	//позиция солнца
	Fvector2 sun_pos;
	//поворот неба
	float sky_rotation;

	//Возможность временного сохранения параметров
	struct SavedData{
		bool is_empty;
		Fvector3 Saved_ambient_color;
		Fvector3 Saved_sun_color;
		Fvector4 Saved_hemi_color;
		Fvector3 Saved_rain_color;
		Fvector3 Saved_fog_color;
		Fvector4 Saved_clouds_color;
		Fvector3 Saved_sky_color;
		Fvector3 Saved_dof;
		float Saved_sun_shafts_intensity;
		float Saved_sun_lumscale;
		Fvector2 Saved_sun_pos;
		float Saved_sky_rotation;
		float Saved_bolt_period;
		float Saved_bolt_duration;
		float Saved_rain_density;
		float Saved_fog_density;
		float Saved_fog_distance;
		float Saved_far_plane;
		float Saved_clouds_velocity_0;
		float Saved_clouds_velocity_1;
		float Saved_water_intensity;
		float Saved_trees_amplitude;
		float Saved_swing_fast_speed;
		float Saved_swing_normal_speed;
		float Saved_swing_fast_rot1;
		float Saved_swing_fast_rot2;
		float Saved_swing_fast_amp1;
		float Saved_swing_fast_amp2;
		float Saved_swing_normal_rot1;
		float Saved_swing_normal_rot2;
		float Saved_swing_normal_amp1;
		float Saved_swing_normal_amp2;
		float Saved_wind_velocity;
		float Saved_wind_direction;
		float Saved_wind_sound_volume;
		float Saved_dof_kernel;
		float Saved_dof_sky;
	};

	//Дефолтные значения при запуске
	SavedData DefaultParams;
	//Сохраненые значения разработчиком
	SavedData UserSavedParams;

	//Сохраненить значения
	virtual void			SaveParams(SavedData* saveid);
	//Загрузить значения
	virtual void			LoadParams(SavedData* saveid);

	CUI3tButton*				UILoadDefaultParams;
	CUI3tButton*				UILoadUserParams;
	CUI3tButton*				UISaveUserParams;
	CUI3tButton*				UIPrinToLogBTN;

	//Основыне окна
	CUIStatic					UIBack;
	CUIStatic					UITitle;

	//Подразделения
	CUIScrollView*				ScrollView;
	CUIScrollView*				ScrollView2;
	CUIScrollView*				ScrollView3;
	CUIScrollView*				ScrollView4;

	//Дочки подразделений

	// ambient
	CUITrackBarVariable			UI_f_ambient_r;
	CUIStatic					UI_ambient_rTitle;
	CUIStatic					UI_ambient_rValue;
	CUITrackBarVariable			UI_f_ambient_g;
	CUIStatic					UI_ambient_gTitle;
	CUIStatic					UI_ambient_gValue;
	CUITrackBarVariable			UI_f_ambient_b;
	CUIStatic					UI_ambient_bTitle;
	CUIStatic					UI_ambient_bValue;

	// sun_color
	CUITrackBarVariable			UI_f_sun_r;
	CUIStatic					UI_sun_rTitle;
	CUIStatic					UI_sun_rValue;
	CUITrackBarVariable			UI_f_sun_g;
	CUIStatic					UI_sun_gTitle;
	CUIStatic					UI_sun_gValue;
	CUITrackBarVariable			UI_f_sun_b;
	CUIStatic					UI_sun_bTitle;
	CUIStatic					UI_sun_bValue;

	//hemi color
	CUITrackBarVariable			UI_f_hemi_color_r;
	CUIStatic					UI_hemi_color_rTitle;
	CUIStatic					UI_hemi_color_rValue;
	CUITrackBarVariable			UI_f_hemi_color_g;
	CUIStatic					UI_hemi_color_gTitle;
	CUIStatic					UI_hemi_color_gValue;
	CUITrackBarVariable			UI_f_hemi_color_b;
	CUIStatic					UI_hemi_color_bTitle;
	CUIStatic					UI_hemi_color_bValue;
	//CUITrackBarVariable			UI_f_hemi_color_a;
	//CUIStatic					UI_hemi_color_aTitle;
	//CUIStatic					UI_hemi_color_aValue;

	//rain color
	CUITrackBarVariable			UI_f_rain_color_r;
	CUIStatic					UI_rain_color_rTitle;
	CUIStatic					UI_rain_color_rValue;
	CUITrackBarVariable			UI_f_rain_color_g;
	CUIStatic					UI_rain_color_gTitle;
	CUIStatic					UI_rain_color_gValue;
	CUITrackBarVariable			UI_f_rain_color_b;
	CUIStatic					UI_rain_color_bTitle;
	CUIStatic					UI_rain_color_bValue;

	//fog color
	CUITrackBarVariable			UI_f_fog_color_r;
	CUIStatic					UI_fog_color_rTitle;
	CUIStatic					UI_fog_color_rValue;
	CUITrackBarVariable			UI_f_fog_color_g;
	CUIStatic					UI_fog_color_gTitle;
	CUIStatic					UI_fog_color_gValue;
	CUITrackBarVariable			UI_f_fog_color_b;
	CUIStatic					UI_fog_color_bTitle;
	CUIStatic					UI_fog_color_bValue;

	//clouds color
	CUITrackBarVariable			UI_f_clouds_color_r;
	CUIStatic					UI_clouds_color_rTitle;
	CUIStatic					UI_clouds_color_rValue;
	CUITrackBarVariable			UI_f_clouds_color_g;
	CUIStatic					UI_clouds_color_gTitle;
	CUIStatic					UI_clouds_color_gValue;
	CUITrackBarVariable			UI_f_clouds_color_b;
	CUIStatic					UI_clouds_color_bTitle;
	CUIStatic					UI_clouds_color_bValue;
	CUITrackBarVariable			UI_f_clouds_color_x;
	CUIStatic					UI_clouds_color_xTitle;
	CUIStatic					UI_clouds_color_xValue;

	//sky color
	CUITrackBarVariable			UI_f_sky_color_r;
	CUIStatic					UI_sky_color_rTitle;
	CUIStatic					UI_sky_color_rValue;
	CUITrackBarVariable			UI_f_sky_color_g;
	CUIStatic					UI_sky_color_gTitle;
	CUIStatic					UI_sky_color_gValue;
	CUITrackBarVariable			UI_f_sky_color_b;
	CUIStatic					UI_sky_color_bTitle;
	CUIStatic					UI_sky_color_bValue;

	//sun_dir_a
	CUIStatic					UI_sun_dir_a_Title;
	CUIStatic					UI_sun_dir_a_Value;
	CUITrackBarVariable			UI_f_sun_dir_a;

	//sun_dir_b
	CUIStatic					UI_sun_dir_b_Title;
	CUIStatic					UI_sun_dir_b_Value;
	CUITrackBarVariable			UI_f_sun_dir_b;

	//sun_shafts_intensity
	CUIStatic					UI_sun_shafts_intensity_Title;
	CUIStatic					UI_sun_shafts_intensity_Value;
	CUITrackBarVariable			UI_f_sun_shafts_intensity;

	//sun_lumscale
	CUIStatic					UI_sun_lumscale_Title;
	CUIStatic					UI_sun_lumscale_Value;
	CUITrackBarVariable			UI_f_sun_lumscale;

	//bolt_period
	CUIStatic					UI_bolt_period_Title;
	CUIStatic					UI_bolt_period_Value;
	CUITrackBarVariable			UI_f_bolt_period;

	//bolt_duration
	CUIStatic					UI_bolt_duration_Title;
	CUIStatic					UI_bolt_duration_Value;
	CUITrackBarVariable			UI_f_bolt_duration;

	//rain_density
	CUIStatic					UI_rain_density_Title;
	CUIStatic					UI_rain_density_Value;
	CUITrackBarVariable			UI_f_rain_density;

	//fog_density
	CUIStatic					UI_fog_density_Title;
	CUIStatic					UI_fog_density_Value;
	CUITrackBarVariable			UI_f_fog_density;

	//fog_distance
	CUIStatic					UI_fog_distance_Title;
	CUIStatic					UI_fog_distance_Value;
	CUITrackBarVariable			UI_f_fog_distance;

	//far_plane
	CUIStatic					UI_far_plane_Title;
	CUIStatic					UI_far_plane_Value;
	CUITrackBarVariable			UI_f_far_plane;

	//sky_rotation
	CUIStatic					UI_sky_rotation_Title;
	CUIStatic					UI_sky_rotation_Value;
	CUITrackBarVariable			UI_f_sky_rotation;

	//clouds_velocity_0
	CUIStatic					UI_clouds_velocity_0_Title;
	CUIStatic					UI_clouds_velocity_0_Value;
	CUITrackBarVariable			UI_f_clouds_velocity_0;

	//clouds_velocity_1
	CUIStatic					UI_clouds_velocity_1_Title;
	CUIStatic					UI_clouds_velocity_1_Value;
	CUITrackBarVariable			UI_f_clouds_velocity_1;

	//water_intensity
	CUIStatic					UI_water_intensity_Title;
	CUIStatic					UI_water_intensity_Value;
	CUITrackBarVariable			UI_f_water_intensity;

	//trees_amplitude
	CUIStatic					UI_trees_amplitude_Title;
	CUIStatic					UI_trees_amplitude_Value;
	CUITrackBarVariable			UI_f_trees_amplitude;

	//swing_fast_speed
	CUIStatic					UI_swing_fast_speed_Title;
	CUIStatic					UI_swing_fast_speed_Value;
	CUITrackBarVariable			UI_f_swing_fast_speed;

	//swing_normal_speed
	CUIStatic					UI_swing_normal_speed_Title;
	CUIStatic					UI_swing_normal_speed_Value;
	CUITrackBarVariable			UI_f_swing_normal_speed;

	//swing_fast_rot1
	CUIStatic					UI_swing_fast_rot1_Title;
	CUIStatic					UI_swing_fast_rot1_Value;
	CUITrackBarVariable			UI_f_swing_fast_rot1;

	//swing_fast_rot2
	CUIStatic					UI_swing_fast_rot2_Title;
	CUIStatic					UI_swing_fast_rot2_Value;
	CUITrackBarVariable			UI_f_swing_fast_rot2;

	//swing_fast_amp1
	CUIStatic					UI_swing_fast_amp1_Title;
	CUIStatic					UI_swing_fast_amp1_Value;
	CUITrackBarVariable			UI_f_swing_fast_amp1;

	//swing_fast_amp2
	CUIStatic					UI_swing_fast_amp2_Title;
	CUIStatic					UI_swing_fast_amp2_Value;
	CUITrackBarVariable			UI_f_swing_fast_amp2;

	//swing_normal_rot1
	CUIStatic					UI_swing_normal_rot1_Title;
	CUIStatic					UI_swing_normal_rot1_Value;
	CUITrackBarVariable			UI_f_swing_normal_rot1;

	//swing_normal_rot2
	CUIStatic					UI_swing_normal_rot2_Title;
	CUIStatic					UI_swing_normal_rot2_Value;
	CUITrackBarVariable			UI_f_swing_normal_rot2;

	//swing_normal_amp1
	CUIStatic					UI_swing_normal_amp1_Title;
	CUIStatic					UI_swing_normal_amp1_Value;
	CUITrackBarVariable			UI_f_swing_normal_amp1;

	//swing_normal_amp2
	CUIStatic					UI_swing_normal_amp2_Title;
	CUIStatic					UI_swing_normal_amp2_Value;
	CUITrackBarVariable			UI_f_swing_normal_amp2;

	//wind_velocity
	CUIStatic					UI_wind_velocity_Title;
	CUIStatic					UI_wind_velocity_Value;
	CUITrackBarVariable			UI_f_wind_velocity;

	//wind_direction
	CUIStatic					UI_wind_direction_Title;
	CUIStatic					UI_wind_direction_Value;
	CUITrackBarVariable			UI_f_wind_direction;

	//wind_sound_volume
	CUIStatic					UI_wind_sound_volume_Title;
	CUIStatic					UI_wind_sound_volume_Value;
	CUITrackBarVariable			UI_f_wind_sound_volume;

	// dof
	CUITrackBarVariable			UI_f_dof_x;
	CUIStatic					UI_dof_xTitle;
	CUIStatic					UI_dof_xValue;
	CUITrackBarVariable			UI_f_dof_y;
	CUIStatic					UI_dof_yTitle;
	CUIStatic					UI_dof_yValue;
	CUITrackBarVariable			UI_f_dof_z;
	CUIStatic					UI_dof_zTitle;
	CUIStatic					UI_dof_zValue;

	// dof kernel
	CUITrackBarVariable			UI_f_dof_kernel;
	CUIStatic					UI_dof_kernelTitle;
	CUIStatic					UI_dof_kernelValue;

	// dof sky
	CUITrackBarVariable			UI_f_dof_sky;
	CUIStatic					UI_dof_skyTitle;
	CUIStatic					UI_dof_skyValue;
};