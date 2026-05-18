function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "deffer_base")
        :fog(false)
        :emissive(true)
    shader:sampler("s_base"):texture(t_base)
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("deffer_model", "accum_emissive")
        :zb(true, false)
        :fog(false)
        :emissive(true)
end
