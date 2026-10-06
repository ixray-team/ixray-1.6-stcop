-- Detail Layers Editor painted imprint (fading stroke stamps on the terrain, no depth)
function l_special(shader, t_base, t_second, t_detail)
    shader:begin("detail_brush_trail", "detail_brush_trail")
        :zb(false, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
end