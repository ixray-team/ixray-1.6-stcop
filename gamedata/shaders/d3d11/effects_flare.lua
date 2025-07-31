function normal(shader, t_base, t_second, t_detail)
    shader:begin("effects_sun", "stub_default")
	:blend(true, blend.srcalpha, blend.one)
	:zb(false, false)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end
