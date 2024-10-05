return {
	"RRethy/nvim-base16",
	lazy = false,
	priority = 1000,
	config = function() 
		vim.cmd([[set termguicolors]])
		vim.cmd([[colorscheme base16-monokai]])
	end,
}
