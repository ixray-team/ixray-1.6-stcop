-- normal pp
t_rt 		= "$user$albedo"
t_noise		= "fx\\fx_noise2"

function normal		(shader, t_base, t_second, t_detail)
	shader:begin	("stub_notransform_postpr","postprocess")
			: fog	(false)
			: zb 	(false,false)
--	shader:sampler	("s_base0")	:texture("$user$albedo")	: clamp() : f_linear ()
--	shader:sampler	("s_base1")    	:texture("$user$albedo")	: clamp() : f_linear ()
--	shader:sampler	("s_noise")    	:texture("fx\\fx_noise2")	: f_linear ()

	shader:dx10texture	("s_base0", "$user$albedo")
	shader:dx10texture	("s_base1", "$user$albedo")
	shader:dx10texture	("s_noise", "fx\\fx_noise2")

	shader:dx10sampler	("smp_rtlinear")
	shader:dx10sampler	("smp_linear")
end
