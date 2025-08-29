#pragma once
class UIWorldPropertiesFrom :
	public IEditorWnd
{
public:
	UIWorldPropertiesFrom();
	virtual ~UIWorldPropertiesFrom();
	virtual void Draw();
	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }

private:
    ref_texture m_dds_texture;

    bool m_show_r;
    bool m_show_g;
    bool m_show_b;
    bool m_show_a;

    u32 m_texture_width;
    u32 m_texture_height;
};