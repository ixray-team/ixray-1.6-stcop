function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "forward_base")
        :fog(true)
        :zb(true, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(true, 0)
        :sorting(3, true)

    shader:dx10texture("s_base", t_base)

    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")

    shader:dx10texture("sky_s0", "$user$sky0")
    shader:dx10texture("sky_s1", "$user$sky1")

    shader:dx10texture("s_material", "$user$material")

    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_material")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("model_distort4glass", "particle_distort_hard")
        :sorting(3, true)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :zb(true, false)
        :fog(false)
        :distort(true)

    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_distort", "pfx\\pfx_dist_glass")
    shader:dx10sampler("smp_linear")
end
