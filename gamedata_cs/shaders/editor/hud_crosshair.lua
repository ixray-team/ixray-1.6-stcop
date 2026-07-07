function normal(shader, t_base, t_second, t_detail)
    shader:begin("stub_notransform_t", "hud_crosshair")
        :fog(false)
        :zb(false, false)
        :blend(false, blend.srcalpha, blend.invsrcalpha)
end
