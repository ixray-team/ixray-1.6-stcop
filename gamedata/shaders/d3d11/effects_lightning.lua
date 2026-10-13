function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_default", "stub_srgb")
	
	:zb(true, false)
	:blend(true, blend.srcalpha, blend.one)
	:aref(true, 2)

    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        shader:begin("stub_default", "stub_default")
            :zb(true, false)
            :blend(true, blend.srcalpha, blend.one)
            :aref(true, 2)
        r1.textures(shader, {"s_base", t_base})
    end
end
