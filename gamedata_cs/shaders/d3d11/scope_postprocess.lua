-- normal pp
t_rt    = "$user$albedo"
t_noise = "fx\\fx_noise2"

function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_notransform_postpr", "scope_postprocess")
        :fog(false)
        :zb(false, false)

    shader:dx10texture("s_base0", "$user$generic0")
    shader:dx10texture("s_base1", "$user$generic0")
    shader:dx10texture("s_noise", "fx\\fx_noise2")
    shader:dx10texture("s_scope_tonemap", "$user$tonemap")

    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_linear")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("stub_notransform_postpr", "scope_postprocess_cm")
        :fog(false)
        :zb(false, false)

    shader:dx10texture("s_base0", "$user$generic0")
    shader:dx10texture("s_base1", "$user$generic0")
    shader:dx10texture("s_noise", "fx\\fx_noise2")
    shader:dx10texture("s_scope_tonemap", "$user$tonemap")

    shader:dx10texture("s_grad0", "$user$scope_cmap0")
    shader:dx10texture("s_grad1", "$user$scope_cmap1")

    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_linear")
end
