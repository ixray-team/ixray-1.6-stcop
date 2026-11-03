
class CDS0_RenderLight :
	public IRender_Light
{
	vis_data visimask;
	bool LightActive = false;
public:
	virtual void set_type(LT type) override {};
	virtual void set_active(bool value) override { LightActive = value; };
	virtual bool get_active() override { return LightActive; }
	virtual void set_shadow(bool) override {};
	virtual void set_volumetric(bool) override {};
	virtual void set_volumetric_quality(float) override {};
	virtual void set_volumetric_intensity(float) override {}
	virtual void set_volumetric_distance(float) override {}
	virtual void set_indirect(bool) override {};
	virtual void set_position(const Fvector& P) override {}
	virtual void set_rotation(const Fvector& D, const Fvector& R) override {}
	virtual void set_cone(float angle) override {}
	virtual void set_range(float R) override {}
	virtual void set_virtual_size(float R) override {}
	virtual void set_texture(LPCSTR name) override {}
	virtual void set_color(const Fcolor& C) override {}
	virtual void set_color(float r, float g, float b) override {}
	virtual void set_hud_mode(bool b) override {}
	virtual bool get_hud_mode() override { return false; }
	virtual vis_data& get_homdata() override { return visimask; }

	virtual void set_occq_mode(bool b) override {}
	virtual bool get_occq_mode() override { return false; }

	virtual void set_ignore_object(CObject* O) override {}
	virtual CObject* get_ignore_object() override { return nullptr; }

	virtual void set_decor_object(CObject* O, int index = 0) override {}
	virtual CObject* get_decor_object(int index = 0) override { return nullptr; }

	virtual void destroy(bool deffered = true) override {}
};