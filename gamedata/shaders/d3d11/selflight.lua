function normal(shader, t_base, t_second, t_detail)
    shader:begin("dumb", "dumb")
        :fog(false)
        :zb(false, false)
        :blend(true, blend.zero, blend.one)
        :aref(false, 0)
        :sorting(2, false)
    --	shader:sampler	("s_base")      :texture	(t_base)

    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        r1.base(shader, t_base, "r1_selflight", "r1_selflight", true)
    end

    function l_spot(shader, t_base, t_second, t_detail)
        r1.lspot(shader, t_base, "r1_selflight_spot")
    end

    function l_point(shader, t_base, t_second, t_detail)
        r1.lpoint(shader, t_base, "r1_selflight_point")
    end

    function l_special(shader, t_base, t_second, t_detail)
        r1.base(shader, t_base, "r1_selflight", "r1_selflight", false)
    end
end
