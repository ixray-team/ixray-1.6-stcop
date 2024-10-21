function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "deffer_base")
        :fog(false)
        :emissive(true)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
    shader:dx10stencil(true, cmp_func.always, 255, 127, stencil_op.keep, stencil_op.replace, stencil_op.keep)
    shader:dx10stencil_ref(1)
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "accum_emissivel")
        :zb(true, false)
        :fog(false)
        :emissive(true)
    shader:dx10texture("s_base", "$user$ui_pda")
    shader:dx10sampler("smp_base")
end
