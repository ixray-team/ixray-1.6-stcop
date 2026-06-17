function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_fullscreen_triangle", "accum_volumetric_sun")

	:fog(false)
	:zb(false, false)
	
	:blend(true, blend.one, blend.one)

    shader:dx10texture("s_blue_noise", "shaders\\blue_noise_3x3")
    shader:dx10texture("s_smap_sun", "$user$smap_depth_sun")
    shader:dx10texture("s_position", "$user$position")

    shader:dx10sampler("smp_nofilter")
    shader:dx10sampler("smp_jitter")
    shader:dx10sampler("smp_smap")
end
