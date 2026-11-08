function normal(shader, t_base, t_second, t_detail)
    shader:begin("ui_rounded", "ui_rounded")
        :fog(false)
        :zb(true, false)
        :blend(false, blend.one, blend.zero)
    shader:dx10texture("s_round_content", t_base)
    shader:dx10texture("s_round_background", t_second)
end
