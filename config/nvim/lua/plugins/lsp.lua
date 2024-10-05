return {
	'neovim/nvim-lspconfig',
	cmd = 'LspStart',
	init = function() 
		vim.keymap.set('n', '[MODE]l', ':LspStart<CR>', { noremap = true} )
	end,
	config = function()
		require('lspconfig').clangd.setup({ autostart = false })
		require('lspconfig').rust_analyzer.setup({ autostart = false })
	end,
	keys = {
		{ 'K', function() vim.lsp.buf.hover() end, expr = true, mode = 'n' }
	},
}
