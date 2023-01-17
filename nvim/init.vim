" First thing I do is install the plugins (just in case)

call plug#begin()
" Installs telescope and it's dependencies
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} "Treesitter

" LSP Support
Plug 'neovim/nvim-lspconfig'

" Autocomplete
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'https://github.com/hrsh7th/nvim-cmp.git' 
Plug 'L3MON4D3/LuaSnip'
Plug 'saadparwaiz1/cmp_luasnip'

"Gruvbox theme
Plug 'https://github.com/morhetz/gruvbox'

"Airline Plug
Plug 'https://github.com/vim-airline/vim-airline.git'

"NERD Tree
Plug 'https://github.com/preservim/nerdtree.git'

"Tagbar: Indexes your file
Plug 'https://github.com/preservim/tagbar.git'

"Allows commenting lines with gcc and multiple lines in visual mode with gc
Plug 'https://github.com/tpope/vim-commentary.git'

"Git wrapper
Plug 'https://github.com/tpope/vim-fugitive.git'

"Rainbow colored {}[]()
Plug 'https://github.com/kien/rainbow_parentheses.vim.git'

call plug#end()


" ------------------ I set my preferences 
set number "Displays the line number to the left
set relativenumber "Displays the relative number of the line, relative to line you are on
set autoread "Reload file when edited outside of vim
set noshowcmd "Dont show pressed key
set hlsearch "Highlights all /search matches across the document
set mouse=a "Activates mouse support
set clipboard+=unnamedplus "Copies directly to system clipboard. Maybe change to "y (https://www.reddit.com/r/neovim/comments/3fricd/comment/ctrru3b/?utm_source=share&utm_medium=web2x&context=3)
set laststatus=3 "Only 1 status line per window
set termguicolors "Enables TRUE color set breakindent "Indented lines retain tabulization set showbreak=⤹ "Sets wrap indicator as that character set pastetoggle=<F3> "Maps F3 to toggle pastemode
set breakindent "Indented lines retain tabulization
set showbreak=⤹ "Sets wrap indicator as that character
set completeopt=menu,menuone,noselect
set colorcolumn=80

" ------------GRUVBOX CONFIGURARTION--
set background=dark "Sets dark mode
"Sets the Gruvbox color theme for everything(?)
autocmd vimenter * ++nested colorscheme gruvbox 
colorscheme gruvbox

" ------------------ I set my REBINDS
let mapleader = " " "Remaps leader key
nnoremap <Leader>f :NERDTreeFocus<CR>

"" Makes Leader key equal Ctrl W
:nnoremap <Leader> <C-w>

"" Ctrl + Alt + Up/Down moves text up or down
noremap <c-a-up> :m -2<CR>
noremap <c-a-down> :m +1<CR>

" Forces you no to use the arrow keys (great behaviour changer)
map <up> <Nop>
map <down> <Nop>
map <left> <Nop>
map <right> <Nop>
map <PageUp> <Nop>
map <PageDown> <Nop>
