function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_fullscreen_triangle", "combine_volumetric")
        :fog(false)
        :zb(false, false)
        :blend(true, blend.one, blend.one)
        :sorting(2, false)

    shader:dx10texture("s_vollight", "$user$generic2")
    shader:dx10sampler("smp_nofilter")
end
