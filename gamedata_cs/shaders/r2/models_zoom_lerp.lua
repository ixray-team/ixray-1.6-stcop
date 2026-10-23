function normal(shader, t_base, t_second, t_detail)
	shader:begin("model_scope_lense", "model_scope_lense_lerp")
		:fog(true)
		:zb(true, false)
		:blend(true, blend.srcalpha, blend.invsrcalpha)
		:aref(true, 0)
		:sorting(2, true)
		:distort(true)
	shader:sampler("s_base"):texture(t_base)
	shader:sampler("s_vp2"):texture("$user$scope_scene"):clamp():f_linear()
end

function l_special(shader, t_base, t_second, t_detail)
	shader:begin("model_scope_lense", "scope_lense_mask_lerp")
		:zb(true, false)
		:scope(true)
		:fog(false)
	shader:sampler("s_base"):texture(t_base)
end
