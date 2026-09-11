local tex_base = "water\\water_ryaska1"

function normal(shader, t_base, t_second, t_detail)
    effects_water.normal_impl(shader, "water", tex_base)
end

function l_special(shader, t_base, t_second, t_detail)
    effects_water.l_special_impl(shader, "water", tex_base)
end