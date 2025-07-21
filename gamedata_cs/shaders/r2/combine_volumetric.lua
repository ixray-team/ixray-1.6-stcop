function normal(shader, t_base, t_second, t_detail)
    shader:begin("combine_1", "combine_volumetric")
        :fog(false)
        :zb(false, false)
        :blend(true, blend.one, blend.one)
        :sorting(2, false)
    shader:sampler("s_vollight"):texture("$user$generic2")
end
