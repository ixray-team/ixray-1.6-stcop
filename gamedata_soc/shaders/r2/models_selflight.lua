function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "deffer_base")
        :fog(false)
        :emissive(true)
    shader:sampler("s_base"):texture(t_base)
end

function normal_ui(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "ui_base")
        :fog(false)
    shader:sampler("s_base"):texture(t_base)
    shader:sampler("s_material"):texture("$user$material"):clamp()
    shader:sampler("env_s0"):texture("shaders\\newsky_viewport#small"):clamp()
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "accum_emissive")
        :zb(true, false)
        :fog(false)
        :emissive(true)
end
