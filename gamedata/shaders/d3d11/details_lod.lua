function pass_setup_common(shader, t_base, t_second, t_detail)
    shader:blend(false, blend.one, blend.zero)
        :zb(true, true)
        :fog(false)

        :dx10stencil(true, cmp_func.always,
            255, 127,
            stencil_op.keep, stencil_op.replace, stencil_op.keep)
        :dx10stencil_ref(1)

    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_base0", t_base)
    shader:dx10texture("s_base1", t_base)
    shader:dx10texture("s_hemi0", t_base .. "_nm")
    shader:dx10texture("s_hemi1", t_base .. "_nm")
    shader:dx10texture("s_hemi", t_base .. "_nm")

    shader:dx10sampler("smp_base");
    shader:dx10sampler("smp_linear");
end

function pass_setup_r1(shader, t_base)
    shader:dx10texture("s_base0", t_base)
    shader:dx10texture("s_base1", t_base)
    shader:dx10texture("s_hemi0", t_base .. "_nm")
    shader:dx10texture("s_hemi1", t_base .. "_nm")
    shader:dx10sampler("smp_base")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("lod", "lod")
    if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

        shader:blend(false, blend.one, blend.zero)
            :aref(true, 200)
            :zb(true, true)
            :fog(false)
        details_lod.pass_setup_r1(shader, t_base)
        return
    end
    details_lod.pass_setup_common(shader, t_base, t_second, t_detail)
end

function normal(shader, t_base, t_second, t_detail)
    shader:begin("lod", "lod")
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(true, 8)
        :zb(true, false)
        :fog(false)
    details_lod.pass_setup_r1(shader, t_base)
end
