function normal		(shader, t_base, t_second, t_detail)
  shader:begin  	("model_distort","model_exoscreen")
      : fog    		(false)
      : zb     		(true,false)
      : blend   	(true,blend.srcalpha,blend.one)
      : aref    	(true,0)
      : sorting		(2, true)
  --shader:sampler	("s_base")      :texture  (t_base)
	shader:dx10texture	("s_base",	t_base)
	shader:dx10sampler	("smp_base")
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    function normal(shader, t_base, t_second, t_detail)
        r1.lplanes(shader, t_base, "r1_exoscreen", "ui\\ui_mono_noise")
    end
end
