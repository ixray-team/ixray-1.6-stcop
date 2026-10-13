function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_notransform_t", "scope_detector")
        :fog(false)
        :zb(false, false)
        :blend(true, blend.one, blend.invsrcalpha)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end
