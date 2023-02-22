function normal		(shader, t_base, t_second, t_detail)
	shader:begin	("particle",	"particle_add")
			: sorting	(3, false)
			: blend		(true,blend.one,blend.one)
			: aref 		(false,0)
			: zb 		(true,false)
			: fog		(false)

	shader:dx10texture	("s_base"		,t_base)
	shader:dx10texture  ("s_position"	,"$user$position")

	shader:dx10sampler	("smp_base")
	shader:dx10sampler	("smp_nofilter")
end