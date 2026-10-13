function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_base", "deffer_base")
        :fog(false)
        :emissive(true)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
    shader:dx10stencil(true, cmp_func.always, 255, 127, stencil_op.keep, stencil_op.replace, stencil_op.keep)
    shader:dx10stencil_ref(1)
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("deffer_base", "accum_emissivel")
        :zb(true, false)
        :fog(false)
        :emissive(true)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        r1.base(shader, t_base, "r1_selflight", "r1_selflight", true)
    end

    function l_special(shader, t_base, t_second, t_detail)
        r1.base(shader, t_base, "r1_selflight", "r1_selflight", false)
    end

    function l_spot(shader, t_base, t_second, t_detail)
        r1.lspot(shader, t_base, "r1_selflight_spot")
    end

    function l_point(shader, t_base, t_second, t_detail)
        r1.lpoint(shader, t_base, "r1_selflight_point")
    end
end
