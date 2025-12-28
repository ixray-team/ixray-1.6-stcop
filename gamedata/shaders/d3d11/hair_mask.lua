function normal(shader, t_base, t_second, t_detail)
    shader:begin("deffer_hair", "deffer_base")
        :fog(false)
        
    -- ОСНОВНЫЕ ТЕКСТУРЫ
    shader:dx10texture("s_base", t_base)      -- Основная текстура (R=маска анимации)
    
    -- ОКРУЖЕНИЕ И ОТРАЖЕНИЯ
    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")
    shader:dx10texture("sky_s0", "$user$sky0")
    shader:dx10texture("sky_s1", "$user$sky1")
    shader:dx10texture("t_hair_mask", t_base.."_hairmask")
    
    -- МАТЕРИАЛЫ
    shader:dx10texture("s_material", "$user$material")
    
    -- ДЛЯ BUMP МАППИНГА (если нужно)
    if shader:dx10sampler("s_bump") then
        shader:dx10texture("s_bump", t_second)
    end
    
    if shader:dx10sampler("s_bumpX") then
        shader:dx10texture("s_bumpX", t_detail)
    end
    
    -- СЭМПЛЕРЫ
    shader:dx10sampler("smp_nofilter")     -- для окружения
    shader:dx10sampler("smp_rtlinear")     -- для окружения
    shader:dx10sampler("smp_material")     -- для материала
    shader:dx10stencil(true, cmp_func.always, 255, 127, stencil_op.keep, stencil_op.replace, stencil_op.keep)
    shader:dx10stencil_ref(1)
end
