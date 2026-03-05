function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_default", "rain_test")

    :zb(true, false)
    :blend(true, blend.one, blend.invsrcalpha)
    :aref(false, 0)

    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")

    shader:dx10texture("s_depth", "$user$depth")
    shader:dx10sampler("smp_depth")
end