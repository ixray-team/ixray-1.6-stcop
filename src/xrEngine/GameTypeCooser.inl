
enum ERPGameType {		// [0..255]
	rpgtGameAny = u8(0),
	rpgtGameDeathmatch,
	rpgtGameTeamDeathmatch,
	rpgtGameArtefactHunt,
	rpgtGameCaptureTheArtefact,
	rpgtGameCount,
};

xr_token rpoint_game_type[] = {
	{ "Any game",			rpgtGameAny					},
	{ "Deathmatch",			rpgtGameDeathmatch			},
	{ "TeamDeathmatch",		rpgtGameTeamDeathmatch		},
	{ "ArtefactHunt",		rpgtGameArtefactHunt		},
	{ "CaptureTheArtefact",	rpgtGameCaptureTheArtefact	},
	{ 0,					0	}
};
