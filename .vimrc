"------------- syntax and global vimrc enable -------------------------
"show syntax for c, cpp, scripting files. 
"It will enable colour codeing and fonts. 
if has("syntax")
	syntax on
endif
" Source a global configuration file if available.
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif
"----------------------------------------------------------------------

"-----------------------Generic parameters-----------------------------
colorscheme desert
"----------------------------------------------------------------------


"------------------------ cscope enable -------------------------------
" casope is a easy to nafivigate in a project between 
" symbols,definations, function calling, file, indlude files, 
" grep test and egrep symbols 

"cscope add project tag files.
map cap :cscope add /home/vbonagiri/host/cscope_tag_files/tags/<C-R>=expand("<cword>")<CR>/cscope.out
"cscope show projects, which are already added. 
map csp :cscope show
"cscope disconnect project, which is already added. 
map cdp :cscope kill

"Find symbol
map fs :cscope find s <C-R>=expand("<cword>")<CR>
"Find definition
map fg :cscope find g <C-R>=expand("<cword>")<CR>
"Find function calling this functiona
map fc :cscope find c <C-R>=expand("<cword>")<CR>

"Find file
map ff :cscope find f <C-R>=expand("<cfile>")<CR>
"Find file #including this file
map fi :cscope find i <C-R>=expand("<cfile>")<CR>

"finding text string 
map ft :cscope find t <C-R>=expand("<cword>")<CR>
"Find egrep pattern
map fe :cscope find e <C-R>=expand("<cword>")<CR>

"----------------------------------------------------------------------


"---------- Tag list and NERD Tree plugins enable ---------------------
"Tag list is a easy to navigate in one file.
"It Show/Close all the opened file definiftions in al list and Hilite the function name under the curcse. 
map tl :TlistToggle<CR>

"NERD Tree is a easy to navigate between files. 
"Show/Close directory and file list.
map nt :NERDTreeToggle<CR>

"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p
"----------------------------------------------------------------------


"-------------- window spliting and moving enable ---------------------
"Easy moving between windows 

"move left window
nmap mlw :wincmd h<CR> 
"move right window
nmap mrw :wincmd l<CR>
"move top window
nmap mtw :wincmd k<CR>
"move bottom window 
nmap mbw :wincmd j<CR>


"Easy split of windows. 
"It will be help fille in tasks like comparing files 

"split horizantal 
nmap sh :sp<CR>
"split vertiacal 
nmap sv :vsp<CR>
"----------------------------------------------------------------------
:inoremap <Esc>Oq 1
:inoremap <Esc>Or 2
:inoremap <Esc>Os 3
:inoremap <Esc>Ot 4
:inoremap <Esc>Ou 5
:inoremap <Esc>Ov 6
:inoremap <Esc>Ow 7
:inoremap <Esc>Ox 8
:inoremap <Esc>Oy 9
:inoremap <Esc>Op 0
:inoremap <Esc>On .
:inoremap <Esc>OQ /
:inoremap <Esc>OR *
:inoremap <Esc>Ol +
:inoremap <Esc>OS -
:inoremap <Esc>OM <Enter>
:nnoremap <Esc>Oq 1
:nnoremap <Esc>Or 2
:nnoremap <Esc>Os 3
:nnoremap <Esc>Ot 4
:nnoremap <Esc>Ou 5
:nnoremap <Esc>Ov 6
:nnoremap <Esc>Ow 7
:nnoremap <Esc>Ox 8
:nnoremap <Esc>Oy 9
:nnoremap <Esc>Op 0
:nnoremap <Esc>On .

"------------------------------------------------------------------------
:set tabstop=2 
:retab
