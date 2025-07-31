
class XRayRenderLight :
	public IRender_Light
{
	vis_data visimask;

public:
	virtual void set_type(LT type) override {};
	virtual void set_active(bool) {};
	virtual bool get_active() { return false; }
	virtual void set_shadow(bool) {};
	virtual void set_volumetric(bool) {};
	virtual void set_volumetric_quality(float) {};
	virtual void set_volumetric_intensity(float) {}
	virtual void set_volumetric_distance(float) {}
	virtual void set_indirect(bool) {};
	virtual void set_position(const Fvector& P) {}
	virtual void set_rotation(const Fvector& D, const Fvector& R) {}
	virtual void set_cone(float angle) {}
	virtual void set_range(float R) {}
	virtual void set_virtual_size(float R) {}
	virtual void set_texture(LPCSTR name) {}
	virtual void set_color(const Fcolor& C) {}
	virtual void set_color(float r, float g, float b) {}
	virtual void set_hud_mode(bool b) {}
	virtual bool get_hud_mode() { return false; }
	virtual vis_data& get_homdata() { return visimask; }

	virtual void set_occq_mode(bool b) {}
	virtual bool get_occq_mode() { return false; }

	virtual void set_ignore_object(CObject* O) {}
	virtual CObject* get_ignore_object() { return nullptr; }

	virtual void set_decor_object(CObject* O, int index = 0) {}
	virtual CObject* get_decor_object(int index = 0) { return nullptr; }

	virtual void destroy(bool deffered = true) {}
};