function normal(shader, t_base, t_second, t_detail)
    shader:begin("effects_rain", "effects_rain")

    :zb(true, false)
	:blend(true, blend.srcalpha, blend.invsrcalpha)

    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")

    shader:dx10texture("s_base", "fx\\hud_rain")
    shader:dx10texture("s_image", "$user$generic")
    shader:dx10texture("s_bloom", "$user$bloomA2")

    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_rtlinear")
end
