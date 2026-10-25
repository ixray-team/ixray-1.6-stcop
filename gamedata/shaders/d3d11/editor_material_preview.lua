-- Material preview of the Thumbnail View: t_base = diffuse, t_second = bump, t_detail = detail.
-- "$null" skips the slot. The bump# and detail bump names follow the game shaders.
function l_special(shader, t_base, t_second, t_detail)
    shader:begin("editor_material_preview", "editor_material_preview")
        :zb(true, true)
    shader:dx10texture("s_base", t_base)
    if t_second ~= "$null" then
        shader:dx10texture("s_bump", t_second)
        shader:dx10texture("s_bumpX", t_second .. "#")
    end
    if t_detail ~= "$null" then
        shader:dx10texture("s_detail", t_detail)
    end
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_linear")
end
