local tex_env0	= "$user$sky0"
local tex_env1	= "$user$sky1"

function normal   (shader, t_base, t_second, t_detail)
	  shader:begin	("model_distort","model_scope_gauss")
      : fog			(true)
      : zb			(true,false)
      : blend		(true,blend.srcalpha,blend.invsrcalpha)
      : aref		(true,0)
      : sorting		(2,true)
      : distort		(true)
	shader:dx10texture	("s_base",	t_base)
	shader:dx10texture 	("s_image",	"$user$accum")
	shader:dx10sampler	("smp_base")	
	shader:dx10sampler	("smp_rtlinear")	
	shader:dx10sampler	("smp_nofilter")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("scope_lense_mask", "scope_lense_mask")
	: zb(false, true) : scope(true) : fog(false)
	
	shader : dx10texture("s_position", "$user$position")
    shader : dx10sampler("smp_base")
end
