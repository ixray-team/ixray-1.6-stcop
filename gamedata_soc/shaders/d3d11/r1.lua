function lpoint(shader, t_base, vs, aref)
    shader:begin(vs, "add_point")
        :fog(false)
        :zb(true, false)
        :blend(true, blend.one, blend.one)
        :aref(true, aref or 0)
    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_lmap", "internal\\internal_light_attpoint")
    shader:dx10texture("s_att", "internal\\internal_light_attpoint")
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_rtlinear")
end

function lspot(shader, t_base, vs, aref)
    shader:begin(vs, "add_spot")
        :fog(false)
        :zb(true, false)
        :blend(true, blend.one, blend.one)
        :aref(true, aref or 0)
    shader:dx10texture("s_base", t_base)
    shader:dx10texture("s_lmap", "internal\\internal_light_att")
    shader:dx10texture("s_att", "internal\\internal_light_attclip")
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_rtlinear")
end

function base(shader, t_base, vs, ps, fog)
    shader:begin(vs, ps)
        :fog(fog)
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end

function textures(shader, list)
    for i = 1, #list, 2 do
        shader:dx10texture(list[i], list[i + 1])
    end
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_rtlinear")
end

function wmark(shader, t_base, ps, sort, blended, src, dst, zwrite)
    shader:begin("r1_wmark", ps)
        :sorting(sort, false)
        :blend(blended, src, dst)
        :aref(blended, 0)
        :zb(true, zwrite)
        :fog(false)
        :wmark(true)
    r1.textures(shader, {"s_base", t_base})
end

function water(shader, t_base, sort, aref)
    shader:begin("r1_water", "r1_water")
        :sorting(2, sort)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(aref, 0)
        :zb(true, false)
        :distort(true)
        :fog(false)
    r1.textures(shader, {"s_base", t_base, "s_nmap", "water\\water_normal", "s_env0", "$user$sky0", "s_env1", "$user$sky1"})
end

function waterd(shader, t_base)
    shader:begin("r1_waterd", "r1_waterd")
        :sorting(2, true)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :zb(true, false)
        :fog(false)
        :distort(true)
    r1.textures(shader, {"s_base", t_base, "s_distort0", "water\\water_dudv", "s_distort1", "water\\water_dudv"})
end

function env(shader, t_base, hq, sort)
    shader:begin(hq and "model_env_hq" or "model_env_lq", hq and "model_env_hq" or "model_env_lq")
        :fog(true)
        :zb(true, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :aref(true, 0)
        :sorting(sort or 3, true)
    r1.textures(shader, hq and {"s_base", t_base, "s_env", "sky\\sky_5_cube", "s_lmap", "$user$projector"} or {"s_base", t_base, "s_env", "sky\\sky_5_cube"})
end

function lplanes(shader, t_base, ps, extra)
    shader:begin("r1_model_lplanes", ps)
        :fog(false)
        :zb(true, false)
        :blend(true, blend.srcalpha, blend.one)
        :aref(true, 0)
        :sorting(2, true)
    local list = {"s_base", t_base}
    if extra then list[3] = "s_lmap"; list[4] = extra end
    r1.textures(shader, list)
end
