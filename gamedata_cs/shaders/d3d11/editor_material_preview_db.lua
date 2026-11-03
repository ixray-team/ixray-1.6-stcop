-- editor_material_preview with the bump of the detail texture
function l_special(shader, t_base, t_second, t_detail)
    shader:begin("editor_material_preview", "editor_material_preview")
        :zb(true, true)
    shader:dx10texture("s_base", t_base)
    if t_second ~= "$null" then
        shader:dx10texture("s_bump", t_second)
        shader:dx10texture("s_bumpX", t_second .. "#")
    end
    shader:dx10texture("s_detail", t_detail)
    shader:dx10texture("s_detailBump", t_detail .. "_bump")
    shader:dx10texture("s_detailBumpX", t_detail .. "_bump#")
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_linear")
end
