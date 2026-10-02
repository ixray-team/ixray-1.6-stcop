function normal   (shader, t_base, t_second, t_detail)
	  shader:begin  ("deffer_model","model_scope_lense")
      : fog       	(true)
      : zb        	(true,false)
      : blend     	(true,blend.srcalpha,blend.invsrcalpha)
      : aref      	(true,0)
      : sorting	  	(2,true)
      : distort   	(true)
	shader:dx10texture	("s_base",	t_base)
	shader:dx10texture 	("s_vp2",	"$user$viewport2")	
	shader:dx10sampler	("smp_base")	
end

if GetShaderOption("USE_R1_STATIC_LIGHTING") then
    r1_static = true

    l_special = nil

    function normal(shader, t_base, t_second, t_detail)
        r1.env(shader, t_base, false, 2)
    end
end
