function normal(shader, t_base, t_second, t_detail)
    shader:begin("ui_color_lit", "simple_color")
        :fog(false)
        :zb(true, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
end
