local tex_base = "water\\water_studen"

function normal(shader, t_base, t_second, t_detail)
    effects_water.normal_impl(shader, "water", tex_base)
end

function l_special(shader, t_base, t_second, t_detail)
    effects_water.l_special_impl(shader, "water", tex_base)
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        r1.water(shader, tex_base, true, true)
    end

    function l_special(shader, t_base, t_second, t_detail)
        r1.waterd(shader, tex_base)
    end
end
