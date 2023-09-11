function normal		(shader, t_base, t_second, t_detail)
	shader:begin	("stub_notransform_t","distort")
			: fog	(false)
			: zb 	(false,false)

	shader:dx10texture	("s_base", "$user$output")
	shader:dx10texture	("s_distort", "$user$distort")
	shader:dx10sampler	("smp_rtlinear");
end
