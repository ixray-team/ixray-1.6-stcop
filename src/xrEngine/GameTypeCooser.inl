
enum ERPGameType {		// [0..255]
	rpgtGameAny = u8(0),
	rpgtGameDeathmatch,
	rpgtGameTeamDeathmatch,
	rpgtGameArtefactHunt,
	rpgtGameCaptureTheArtefact, 
	rpgtFreeMP,
	rpgtGameCount,
};

xr_token rpoint_game_type[] = {
	{ "Any game",			rpgtGameAny					},
	{ "Deathmatch",			rpgtGameDeathmatch			},
	{ "TeamDeathmatch",		rpgtGameTeamDeathmatch		},
	{ "ArtefactHunt",		rpgtGameArtefactHunt		},
	{ "CaptureTheArtefact",	rpgtGameCaptureTheArtefact	},
	{ "FreeMp",				rpgtFreeMP					},
	{ 0,					0	}
};
