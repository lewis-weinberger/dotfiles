require('vis')

vis.events.subscribe(vis.events.INIT, function()
	vis:command('set theme plain')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	vis:command('set number')
	vis:command('set autoindent')
	vis:command('set colorcolumn 80')
	vis:command('set show-tabs')
	vis:command('set tabwidth 4')
	vis:command('set expandtab')
end)
