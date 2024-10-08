local opt = vim.opt
vim.g.mapleader = " "

opt.cursorline = true
opt.number = true
opt.relativenumber = false
opt.termguicolors = true
opt.conceallevel = 1

vim.cmd([[
"set clipboard+=unnamedplus
" " Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy
" " Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

]])
