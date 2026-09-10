-- Detail Layers Editor brush overlay (world-space projected disk, no depth)
function l_special(shader, t_base, t_second, t_detail)
    shader:begin("detail_brush", "detail_brush")
        :zb(false, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
end