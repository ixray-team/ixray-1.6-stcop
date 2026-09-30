function normal(shader, t_base, t_second, t_detail)
    shader:begin("effects_rain", "effects_rain")

    :zb(true, false)
	:blend(true, blend.srcalpha, blend.invsrcalpha)

    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")

    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end