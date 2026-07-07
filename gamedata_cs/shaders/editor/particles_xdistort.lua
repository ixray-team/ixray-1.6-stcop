function normal(shader, t_base, t_second, t_detail)
    shader:begin("particle_distort", "particle_distort")
        :sorting(3, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :zb(true, false)
        :fog(false)
    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_distort",t_base)
    shader:dx10texture("s_image","$user$rt_color_copy")
    
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_linear")
    shader:dx10sampler("smp_nofilter")
end
