" This file is an expurgated version of:
"
"    http://algorithm.com.au/code/vimacs/download/
"

" What doesn't work:
" 
"    C-/            [but C-_ and C-x C-u do]
"    C-x -C-s
"    M-<num>
"    C-u <num> 
"    C-x (, C-x ), C-x e  [macros]
"    C-q            [mapped to C-v but doesn't seem to work]
"    M-r

if version < 600
  echoerr 'Emacs key bindings require Vim 6 to run :('
  finish
endif

" We want to be able to remap <> key codes and do line continuation
"
let s:saved_cpoptions = &cpoptions
set cpoptions-=<,C

" Set a default value for a variable only if it doesn't exist
" (like "$foo |= 'bar'" in Perl)
"
function! <SID>LetDefault(var_name, value)
  if !exists(a:var_name)
    execute 'let ' . a:var_name . '=' . a:value
  endif
endfunction

command! -nargs=+ LetDefault call s:LetDefault(<f-args>)

" Function to mark a cursor position and restore it afterward -- used in a few
" functions, like FillParagraph().
"
function! <SID>Mark(...)
  if a:0 == 0
    let mark = line(".") . "G" . virtcol(".") . "|"
    normal! H
    let mark = "normal!" . line(".") . "Gzt" . mark
    execute mark
    return mark
  elseif a:0 == 1
    return "normal!" . a:1 . "G1|"
  else
    return "normal!" . a:1 . "G" . a:2 . "|"
  endif
endfun

LetDefault g:VM_CmdHeightAdj 1

if &cmdheight == 1 && &showmode == 1 && g:VM_CmdHeightAdj
  set cmdheight=2
endif

" Vim options essential for emulating Emacs-ish behaviour
"
set winaltkeys=no
set whichwrap=b,s,<,>,h,l,[,],~
set hidden
set backspace=indent,eol,start
set wildcharm=<Tab>
set esckeys

LetDefault g:VM_UnixConsoleMetaSendsEsc 1

LetDefault g:VM_SingleEscToNormal 1

" <Esc>x maps to <M-x>
"
if has("unix") && !has("gui_running") && g:VM_UnixConsoleMetaSendsEsc
 set <M-1>=1
 set <M-2>=2
 set <M-3>=3
 set <M-4>=4
 set <M-5>=5
 set <M-6>=6
 set <M-7>=7
 set <M-8>=8
 set <M-9>=9
 set <M-0>=0
 set <M-a>=a
 set <M-b>=b
 set <M-c>=c
 set <M-d>=d
 set <M-e>=e
 set <M-f>=f
 set <M-g>=g
 set <M-h>=h
 set <M-i>=i
 set <M-j>=j
 set <M-k>=k
 set <M-l>=l
 set <M-m>=m
 set <M-n>=n
 set <M-o>=o
 set <M-p>=p
 set <M-q>=q
 set <M-r>=r
 set <M-s>=s
 set <M-t>=t
 set <M-u>=u
 set <M-v>=v
 set <M-w>=w
 set <M-x>=x
 set <M-y>=y
 set <M-z>=z
 set <M->=
 set <M-/>=/
 " Doing "set <M->>=^[>" throws up an error, so we be dodgey and use Char-190
 " instead, which is ASCII 62 ('>' + 128).
 set <Char-190>=>
 " Probably don't need both of these ;)
 set <Char-188>=<
 set <M-<>=<
 set <M-0>=0

 set <M-%>=%
 set <M-*>=*
 set <M-.>=.
 set <M-^>=^
endif

" One or two <Esc>s to get back to Normal mode?
"
if g:VM_SingleEscToNormal == 1
  if &insertmode
    if has('unix') && !has('gui_running') && g:VM_UnixConsoleMetaSendsEsc
      inoremap <Esc><Esc> <C-o>:UseF1ForNormal<CR>
    else
      inoremap <Esc> <C-l>
    endif
  endif

  set notimeout
  set ttimeout
  set timeoutlen=50
else
  inoremap <Esc><Esc> <C-l>
  vnoremap <Esc><Esc> <Esc>
  set notimeout
  set nottimeout
endif

command! UseF1ForNormal echoerr "Use F1 or <C-z> to return to Normal mode.  :help vimacs-unix-esc-key"

" Insert mode <-> Normal mode <-> Command mode
" 
inoremap <M-x> <C-o>:
inoremap <M-:> <C-o>:
inoremap <silent> <C-z> <C-l>:echo "Returning to Normal mode; press <C-z> again to suspend Vimacs"<CR>
nnoremap <C-z> :call <SID>Suspend()<CR>

LetDefault g:VM_NormalMetaXRemap 1

if g:VM_NormalMetaXRemap == 1
  nnoremap <M-x> :
endif

function! <SID>Suspend()
  suspend!
  if &insertmode
    startinsert
  endif
endfunction

" Leaving Vim
"
inoremap <C-x><C-c> <C-o>:confirm qall<CR>

" Files & Buffers
"
inoremap <C-x><C-f> <C-o>:hide edit<Space>
inoremap <C-x><C-s> <C-o>:update<CR>
inoremap <C-x>s <C-o>:wall<CR>
inoremap <C-x>i <C-o>:read<Space>
"what does C-x C-v do?
inoremap <C-x><C-w> <C-o>:write<Space>
inoremap <C-x><C-q> <C-o>:set invreadonly<CR>
inoremap <C-x><C-r> <C-o>:hide view<Space>

" Help
"
inoremap <C-h> <C-o>:help

" Undo
" 
inoremap <C-_> <C-o>u
inoremap <C-x><C-u> <C-o>u
inoremap <C-/> <C-o>u

" Incremental Searching and Query Replace
"
inoremap <C-s> <C-o>:call <SID>StartSearch('/')<CR><C-o>/
inoremap <C-r> <C-o>:call <SID>StartSearch('?')<CR><C-o>?
inoremap <M-n> <C-o>:cnext<CR>
inoremap <M-p> <C-o>:cprevious<CR>
inoremap <C-M-s> <C-o>:call <SID>StartSearch('/')<CR><C-o>/
inoremap <C-M-r> <C-o>:call <SID>StartSearch('?')<CR><C-o>?
inoremap <M-s> <C-o>:set invhls<CR>
inoremap <M-%> <C-o>:call <SID>QueryReplace()<CR>
inoremap <C-M-%> <C-o>:call <SID>QueryReplaceRegexp()<CR>
cnoremap <C-r> <CR><C-o>?<Up>

command! QueryReplace :call <SID>QueryReplace()<CR>
command! QueryReplaceRegexp :call <SID>QueryReplaceRegexp()<CR>

" Searching is a bit tricky because we have to emulate Emacs's behaviour of
" searching again when <C-s> or <C-r> is pressed _inside_ the search
" commandline.  Vim has no equivalent to this, so we must use a bit of
" on-the-fly remap trickery (popular in Quake-style games) to provide
" different functionality for <C-s>, depending on whether you're in 'search
" mode' or not.
"
" We must map <C-g> and <CR> because we have to undo the map trickery that we
" set up when we abort/finish the search.  All in all, it's not too complex
" when you actually look at what the code does.
"
" Note that <C-c> in Emacs is functionally the same as <CR>.

LetDefault g:VM_SearchRepeatHighlight 0

function! <SID>StartSearch(search_dir)
  let s:incsearch_status = &incsearch
  let s:lazyredraw_status = &lazyredraw
  set incsearch
  set lazyredraw
  cmap <C-c> <CR>
  cnoremap <C-s> <C-c><C-o>:call <SID>SearchAgain()<CR><C-o>/<Up>
  cnoremap <C-r> <C-c><C-o>:call <SID>SearchAgain()<CR><C-o>?<Up>
  cnoremap <silent> <CR> <CR><C-o>:call <SID>StopSearch()<CR>
  cnoremap <silent> <C-g> <C-c><C-o>:call <SID>AbortSearch()<CR>
  cnoremap <silent> <Esc> <C-c><C-o>:call <SID>AbortSearch()<CR>
  if a:search_dir == '/'
    cnoremap <M-s> <CR><C-o>:set invhls<CR><Left><C-o>/<Up>
  else
    cnoremap <M-s> <CR><C-o>:set invhls<CR><Left><C-o>?<Up>
  endif
  let s:before_search_mark = <SID>Mark()
endfunction

function! <SID>StopSearch()
  cunmap <C-c>
  cunmap <C-s>
  cunmap <C-r>
  cunmap <CR>
  cunmap <C-g>
  cnoremap <C-g> <C-c>
  if exists("s:incsearch_status")
    let &incsearch = s:incsearch_status
    unlet s:incsearch_status
  endif
  if g:VM_SearchRepeatHighlight == 1
    if exists("s:hls_status")
      let &hls = s:hls_status
      unlet s:hls_status
    endif
  endif
endfunction

function! <SID>AbortSearch()
  call <SID>StopSearch()
  if exists("s:before_search_mark")
    execute s:before_search_mark
    unlet s:before_search_mark
  endif
endfunction

function! <SID>SearchAgain()
  if (winline() <= 2)
    normal zb
  elseif (( winheight(0) - winline() ) <= 2)
    normal zt
  endif
  cnoremap <C-s> <CR><C-o>:call <SID>SearchAgain()<CR><C-o>/<Up>
  cnoremap <C-r> <CR><C-o>:call <SID>SearchAgain()<CR><C-o>?<Up>
  if g:VM_SearchRepeatHighlight == 1
    if !exists("s:hls_status")
      let s:hls_status = &hls
    endif
    set hls
  endif
endfunction

" `query-replace' functions
"
function! <SID>QueryReplace()
  let magic_status = &magic
  set nomagic
  let searchtext = input("Query replace: ")
  if searchtext == ""
    echo "(no text entered): exiting to Insert mode"
    return
  endif
  let replacetext = input("Query replace " . searchtext . " with: ")
  let searchtext_esc = escape(searchtext,'/\^$')
  let replacetext_esc = escape(replacetext,'/\')
  execute ".,$s/" . searchtext_esc . "/" . replacetext_esc . "/cg"
  let &magic = magic_status
endfunction

function! <SID>QueryReplaceRegexp()
  let searchtext = input("Query replace regexp: ")
  if searchtext == ""
    echo "(no text entered): exiting to Insert mode"
    return
  endif
  let replacetext = input("Query replace regexp " . searchtext . " with: ")
  let searchtext_esc = escape(searchtext,'/')
  let replacetext_esc = escape(replacetext,'/')
  execute ".,$s/" . searchtext_esc . "/" . replacetext_esc . "/cg"
endfunction

" Navigation
"
cmap <C-b> <Left>
cmap <C-f> <Right>
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>
cmap <C-a> <Home>
cmap <C-e> <End>

" Editing
"
cmap <M-p> <Up>
cmap <M-n> <Down>
cmap <C-d> <Del>
cnoremap <C-y> <C-r><C-o>"
cnoremap <M-w> <C-y>
cnoremap <M-BS> <C-w>
cnoremap <C-k> <C-f>d$<C-c><End>

" Insert/Visual/Operator mode maps
"
imap <C-b> <Left>
vmap <C-b> <Left>
omap <C-b> <Left>
imap <C-f> <Right>
vmap <C-f> <Right>
omap <C-f> <Right>
imap <C-p> <Up>
vmap <C-p> <Up>
omap <C-p> <Up>
imap <C-n> <Down>
vmap <C-n> <Down>
omap <C-n> <Down>
inoremap <M-f> <C-o>e<Right>
vnoremap <M-f> e<Right>
onoremap <M-f> e<Right>
inoremap <M-b> <C-Left>
vnoremap <M-b> <C-Left>
onoremap <M-b> <C-Left>
imap <C-a> <Home>
vmap <C-a> <Home>
omap <C-a> <Home>
imap <C-e> <End>
vmap <C-e> <End>
omap <C-e> <End>
inoremap <M-a> <C-o>(
vnoremap <M-a> (
onoremap <M-a> (
inoremap <M-e> <C-o>)
vnoremap <M-e> )
onoremap <M-e> )
inoremap <C-d> <Del>
vnoremap <C-d> <Del>
onoremap <C-d> <Del>
inoremap <M-<> <C-o>1G<C-o>0
vnoremap <M-<> 1G0
onoremap <M-<> 1G0
inoremap <M->> <C-o>G<C-o>$
vnoremap <M->> G$
onoremap <M->> G$
inoremap <C-v> <PageDown>
vnoremap <C-v> <PageDown>
onoremap <C-v> <PageDown>
inoremap <M-v> <PageUp>
vnoremap <M-v> <PageUp>
onoremap <M-v> <PageUp>
inoremap <M-m> <C-o>^
vnoremap <M-m> ^
onoremap <M-m> ^
inoremap <C-x>= <C-g>
vnoremap <C-x>= <C-g>
onoremap <C-x>= <C-g>
inoremap <silent> <M-g> <C-o>:call <SID>GotoLine()<CR>
vnoremap <silent> <M-g> :<C-u>call <SID>GotoLine()<CR>
onoremap <silent> <M-g> :call <SID>GotoLine()<CR>
" Phear, <M-g> works properly even in Visual/Operator-Pending
" modes :)  (It's rather dangerous with the latter, though ...)
inoremap <M-Left> <S-Left>
vnoremap <M-Left> <S-Left>
onoremap <M-Left> <S-Left>
inoremap <M-Right> <S-Right>
vnoremap <M-Right> <S-Right>
onoremap <M-Right> <S-Right>
inoremap <C-Up> <C-o>{
vnoremap <C-Up> {
onoremap <C-Up> {
inoremap <C-Down> <C-o>}
vnoremap <C-Down> }
onoremap <C-Down> }

command! GotoLine :call <SID>GotoLine()

function! <SID>GotoLine()
  let targetline = input("Goto line: ")
  if targetline =~ "^\\d\\+$"
    execute "normal! " . targetline . "G0"
  elseif targetline =~ "^\\d\\+%$"
    execute "normal! " . targetline . "%"
  elseif targetline == ""
    echo "(cancelled)"
  else
    echo " <- Not a Number"
  endif
endfunction

command! GotoLine :call <SID>GotoLine()

" General Editing
"
inoremap <C-c> <Space><Left>
inoremap <C-u> <C-o>d0
inoremap <C-q> <C-v>
inoremap <C-^> <C-y>
inoremap <M-r> <C-r>=

" Aborting
"
cnoremap <C-g> <C-c>
onoremap <C-g> <C-c>

" Killing and Deleting
"
inoremap <C-d> <Del>
inoremap <silent> <M-d> <C-r>=<SID>KillWord()<CR>
inoremap <M-> <C-w>
inoremap <M-BS> <C-w>
inoremap <C-BS> <C-w>
inoremap <silent> <C-k> <C-r>=<SID>KillLine()<CR>
inoremap <M-0><C-k> <C-o>d0
inoremap <M-k> <C-o>d)
inoremap <C-x><BS> <C-o>d(
inoremap <M-z> <C-o>dt
inoremap <M-\> <Esc>beldwi

function! <SID>KillWord()
  if col('.') > strlen(getline('.'))
    return "\<Del>\<C-o>dw"
  else
    return "\<C-o>dw"
  endif
endfunction

function! <SID>KillLine()
  if col('.') > strlen(getline('.'))
    return "\<Del>"
  else
    return "\<C-o>d$"
  endif
endfunction

" Abbreviations
" 
inoremap <M-/> <C-p>
inoremap <C-M-/> <C-x>
inoremap <C-M-x> <C-x>
inoremap <C-]> <C-x>

" Visual stuff (aka 'marking' aka 'region' aka 'block' etc etc)
"
set sel=exclusive
inoremap <silent> <C-Space> <C-r>=<SID>StartVisualMode()<CR>
imap <C-@> <C-Space>
vnoremap <C-x><C-Space> <Esc>
vnoremap <C-g> <Esc>
vnoremap <C-x><C-@> <Esc>
vnoremap <M-w> "1y
vnoremap <C-Ins> "*y
vnoremap <S-Del> "*d

" Marking blocks
"
inoremap <M-Space> <C-o>:call <SID>StartMarkSel()<CR><C-o>viw
inoremap <M-h> <C-o>:call <SID>StartMarkSel()<CR><C-o>vap
inoremap <C-<> <C-o>:call <SID>StartMarkSel()<CR><C-o>v1G0o
inoremap <C->> <C-o>:call <SID>StartMarkSel()<CR><C-o>vG$o
inoremap <C-x>h <C-o>:call <SID>StartMarkSel()<CR><Esc>1G0vGo

" Block operations
"
vnoremap <C-w> "1d
vnoremap <S-Del> "_d
vnoremap <C-x><C-x> o
vnoremap <C-x><C-u> U
vnoremap <M-x> :

" Pasting
"
inoremap <silent> <C-y> <C-o>:call <SID>ResetKillRing()<CR><C-r><C-o>"
inoremap <S-Ins> <C-r><C-o>*
inoremap <M-y> <C-o>:call <SID>YankPop()<CR>

function! <SID>YankPop()
  undo
  if !exists("s:kill_ring_position")
    call <SID>ResetKillRing()
  endif
  execute "normal! i\<C-r>\<C-o>" . s:kill_ring_position . "\<Esc>"
  call <SID>IncrKillRing()
endfunction

function! <SID>ResetKillRing()
  let s:kill_ring_position = 3
endfunction

function! <SID>IncrKillRing()
  if s:kill_ring_position >= 9
    let s:kill_ring_position = 2
  else
    let s:kill_ring_position = s:kill_ring_position + 1
  endif
endfunction

function! <SID>StartMarkSel()
  if &selectmode =~ 'key'
    set keymodel-=stopsel
  endif
endfunction

function! <SID>StartVisualMode()
  call <SID>StartMarkSel()
  if col('.') > strlen(getline('.'))
    " At EOL
    return "\<Right>\<C-o>v\<Left>"
  else
    return "\<C-o>v"
  endif
endfunction

" Window Operations
"
inoremap <C-x>2 <C-o><C-w>s
inoremap <C-x>3 <C-o><C-w>v
inoremap <C-x>0 <C-o><C-w>c
inoremap <C-x>1 <C-o><C-w>o
inoremap <C-x>o <C-o><C-w>w
" <C-x>O is not defined in Emacs ...
inoremap <C-x>O <C-o><C-w>W
inoremap <C-Tab> <C-o><C-w>w
inoremap <C-S-Tab> <C-o><C-w>W
inoremap <C-x>+ <C-o><C-w>=
inoremap <silent> <C-M-v> <C-o>:ScrollOtherWindow<CR>

inoremap <C-x>4<C-f> <C-o>:FindFileOtherWindow<Space>
inoremap <C-x>4f <C-o>:FindFileOtherWindow<Space>

function! <SID>number_of_windows()
  let i = 1
  while winbufnr(i) != -1
    let i = i + 1
  endwhile
  return i - 1
endfunction

function! <SID>FindFileOtherWindow(filename)
  let num_windows = <SID>number_of_windows()
  if num_windows <= 1
    wincmd s
  endif
  wincmd w
  execute "edit " . a:filename
  wincmd W
endfunction

command! -nargs=1 -complete=file FindFileOtherWindow :call <SID>FindFileOtherWindow(<f-args>)

command! ScrollOtherWindow silent! execute "normal! \<C-w>w\<PageDown>\<C-w>W"

" Formatting
"
inoremap <silent> <M-q> <C-o>:call <SID>FillParagraph()<CR>
inoremap <script> <C-o> <CR><Left>
inoremap <C-M-o> <C-o>:echoerr "<C-M-o> not supported yet; sorry!"<CR>
inoremap <C-x><C-o> <C-o>:call <SID>DeleteBlankLines()<CR>
inoremap <M-^> <Up><End><C-o>J
vnoremap <C-M-\> =
vnoremap <C-x><Tab> =

command! FillParagraph :call <SID>FillParagraph()

function! <SID>FillParagraph()
  let old_cursor_pos = <SID>Mark()
  normal! gqip
  execute old_cursor_pos
endfunction

function! <SID>DeleteBlankLines()
  if getline(".") == "" || getline(". + 1") == "" || getline(". - 1") == ""
    ?^.\+$?+1,/^.\+$/-2d"_"
  endif
  normal j
endfunction

" Case Change
" 
inoremap <M-l> <C-o>gul<C-o>w
inoremap <M-u> <C-o>gUe<C-o>w
inoremap <M-c> <C-o>gUl<C-o>w

" Buffers
"
inoremap <C-x>b <C-r>=<SID>BufExplorerOrBufferList()<CR>
inoremap <C-x><C-b> <C-o>:buffers<CR>
inoremap <C-x>k <C-o>:bdelete<Space>

" Integration with the BufExplorer plugin.
"
function! <SID>BufExplorerOrBufferList()
  if exists(":BufExplorer")
    if !exists("g:bufExplorerSortBy")
      let g:bufExplorerSortBy = "mru"
    endif
    return "\<C-o>:BufExplorer\<CR>"
  else
    return "\<C-o>:buffer \<Tab>"
  endif
endfunction

" Marks (a.k.a. "Registers" in Emacs)
"
inoremap <C-x>/ <C-o>:call <SID>PointToRegister()<CR>
inoremap <C-x>r<Space> <C-o>:call <SID>PointToRegister()<CR>
inoremap <C-x>r<C-Space> <C-o>:call <SID>PointToRegister()<CR>
inoremap <C-x>r<C-@> <C-o>:call <SID>PointToRegister()<CR>

inoremap <C-x>rj <C-o>:call <SID>JumpToRegister()<CR>
inoremap <C-x>p <C-o><C-o>

command! PointToRegister :call PointToRegister()
command! JumpToRegister :call JumpToRegister()

function! <SID>PointToRegister()
  echo "Point to mark: "
  let c = nr2char(getchar())
  execute "normal! m" . c
endfunction

function! <SID>JumpToRegister()
  echo "Jump to mark: "
  let c = nr2char(getchar())
  execute "normal! `" . c
endfunction

" Transposing, Tags, Shells, Rectanges, Redraw
" 
inoremap <C-t> <Left><C-o>x<C-o>p
inoremap <M-t> <Esc>dawbhpi
inoremap <C-x><C-t> <Up><C-o>dd<End><C-o>p<Down>
inoremap <C-M-t> <C-o>:echoerr "C-M-t is not implemented yet; sorry!"<CR>
inoremap <M-.> <C-o><C-]>
inoremap <M-*> <C-o><C-t>
inoremap <C-x>4. <C-o><C-w>}
vnoremap <M-!> !
inoremap <M-!> <C-o>:!
vnoremap <C-x>r <C-v>
inoremap <C-l> <C-o>zz<C-o><C-l>

" We're done :)
"
delcommand LetDefault
delfunction s:LetDefault

" restore options
"
let &cpoptions = s:saved_cpoptions