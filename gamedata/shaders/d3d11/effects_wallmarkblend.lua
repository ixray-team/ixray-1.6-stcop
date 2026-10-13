function normal(shader, t_base, t_second, t_detail)
    shader:begin("wmark", "simple")
        :sorting(1, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(true, 0)
        :zb(true, false)
        :fog(false)
        :wmark(true)
    --	shader:sampler	("s_base")      :texture	(t_base)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_rtlinear")
    shader:dx10color_write_enable(true, true, true, false)
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        r1.wmark(shader, t_base, "r1_wmark", 2, true, blend.srcalpha, blend.invsrcalpha, false)
    end

    function l_spot(shader, t_base, t_second, t_detail)
        r1.lspot(shader, t_base, "r1_wmark_spot")
    end

    function l_point(shader, t_base, t_second, t_detail)
        r1.lpoint(shader, t_base, "r1_wmark_point")
    end
end
