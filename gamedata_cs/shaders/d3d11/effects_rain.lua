function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_default", "stub_srgb")
	
	:zb(true, false)
	:blend(true, blend.srcalpha, blend.invsrcalpha)
	:aref(true, 0)

    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end
