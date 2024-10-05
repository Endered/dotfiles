vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local nmap = function(lhs, rhs) 
	vim.keymap.set('n', lhs, rhs, { remap = true })
end

local nnoremap = function(lhs, rhs) 
	vim.keymap.set('n', lhs, rhs, { noremap = true })
end

local tmap = function(lhs, rhs) 
	vim.keymap.set('t', lhs, rhs, { remap = true })
end

local tnoremap = function(lhs, rhs) 
	vim.keymap.set('t', lhs, rhs, { noremap = true })
end

local imap = function(lhs, rhs) 
	vim.keymap.set('i', lhs, rhs, { remap = true })
end

local inoremap = function(lhs, rhs) 
	vim.keymap.set('i', lhs, rhs, { noremap = true })
end

local make_state = function(state_prefix,state_name) 
	nmap('<LEADER>' .. state_prefix, state_name)
	nnoremap(state_name, '<NOP>')
end

vim.opt.clipboard = 'unnamedplus'

inoremap('fd', '<ESC>')
tnoremap('fd', '<C-\\><C-n>')

nnoremap(';', ':')
nnoremap(':', ';')

nnoremap('<LEADER><LEADER>', ':')
nnoremap([[<LEADER>']], ':terminal<CR>')

nmap('<LEADER>w', '[WINDOW]')
nnoremap('[WINDOW]', '<C-w>')
nnoremap('[WINDOW]d', ':q!<CR>')

make_state('t','[TAB]')
nnoremap('[TAB]l', 'gt')
nnoremap('[TAB]L', ':+tabmove<CR>')
nnoremap('[TAB]h', 'gT')
nnoremap('[TAB]H', ':-tabmove<CR>')
nnoremap('[TAB]d', ':tabclose<CR>')
nnoremap('[TAB]n', ':tabnew<CR>')

make_state('f', '[FILE]')

make_state('m', '[MODE]')

require('config.lazy')
