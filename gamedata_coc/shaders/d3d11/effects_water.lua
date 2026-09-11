local tex_base = "water\\water_water"

function normal(shader, t_base, t_second, t_detail)
    effects_water.normal_impl(shader, "water", tex_base)
end

function l_special(shader, t_base, t_second, t_detail)
    effects_water.l_special_impl(shader, "water", tex_base)
end

function normal_impl(shader, vs, t_base)

	local wboit = GetShaderOption("ALLOW_WBOIT_TRANSPARENCY")
	
    shader:begin(vs, "water")

	:sorting(2, false)
	
	:zb(true, false)
	:distort(true)
	:fog(true)
	
	if wboit then
		shader:blend(true, blend.one, blend.one)
		: iblend(2, true, blend.destcolor, blend.zero)
		: iblend(1, true, blend.srcalpha, blend.invsrcalpha)
	else
		shader:blend(true, blend.srcalpha, blend.invsrcalpha)
	end

    shader:dx10texture("s_base", t_base)

    shader:dx10texture("s_nmap", "water\\water_normal")

    shader:dx10texture("s_env0", "$user$sky0")
    shader:dx10texture("s_env1", "$user$sky1")
	
    shader:dx10texture("s_env", "$user$env")
    shader:dx10texture("s_env_dist", "$user$env_temp")

    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")

    shader:dx10texture("s_accumulator", "$user$accum")
    shader:dx10texture("s_position", "$user$position")
    shader:dx10texture("s_velocity", "$user$velocity")
    shader:dx10texture("s_image", "$user$generic")

    shader:dx10texture("s_material", "$user$material")

    shader:dx10texture("s_leaves", "water\\water_foam")
    shader:dx10texture("s_caustic", "water\\water_caustic")
	
    shader:dx10texture("s_smap_sun", "$user$smap_depth_sun")

    shader:dx10sampler("smp_smap")
    shader:dx10sampler("smp_base")
	
    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_nofilter")
    shader:dx10sampler("smp_linear")
end

function l_special_impl(shader, vs, t_base)
    shader:begin(vs, "waterd")
	
	:blend(true, blend.srcalpha, blend.invsrcalpha)
	:zb(true, false)
	:fog(false)
	:distort(true)

    shader:dx10color_write_enable(true, true, true, false)

    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_distort", "water\\water_dudv")
	
    shader:dx10texture("s_position", "$user$position")

    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_nofilter")
end
