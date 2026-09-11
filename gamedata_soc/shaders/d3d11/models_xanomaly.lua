function normal(shader, t_base, t_second, t_detail)
    shader:begin("model_distort4glass", "particle_hard")
        :fog(true)
        :zb(true, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(true, 0)
        :sorting(3, true)

    shader:dx10texture("s_base", t_base)

    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_linear")
    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_material")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("model_distort4glass", "particle_distort_hard")
        :sorting(2, true)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :zb(true, true)
        :fog(false)
        :distort(true)
    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_distort", "pfx\\pfx_dist_glass3")
    shader:dx10sampler("smp_linear")
end
