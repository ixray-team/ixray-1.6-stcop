#pragma once

class UIWeatherPropForm: public IEditorWnd
{
public:
    UIWeatherPropForm();
    virtual ~UIWeatherPropForm();
    virtual void Draw();

    static void  Update();
    static void  Show();

    float m_speed_time;
    bool m_sun_visible;
    bool m_use_sun_dir;
    bool m_use_hemi;
    bool m_raindrop_collision;
    bool m_snd_on_roof;
    void IsShowSunChanged() const;
    void IsUseSunDirChanged() const;
    void IsUseHemiChanged() const;
    void IsRaindropCollisionChanged() const;
    void IsSndOnRoofChanged() const;

private:
    static UIWeatherPropForm* Form;
    ref_texture m_weather_properties;
};
