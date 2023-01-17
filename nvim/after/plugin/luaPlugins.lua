-- LSP SETUP


local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)

	-- Mappings.
	-- See `:help vim.lsp.*` for documentation on any of the below functions
	local bufopts = { noremap=true, silent=true, buffer=bufnr }
	vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
	vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
	vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
	vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
	vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
	vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
	vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
	vim.keymap.set('n', '<leader>wl', function()
	print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, bufopts)
	vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
	vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
	vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
	vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
	vim.keymap.set('n', '<leader>r', function() vim.lsp.buf.format { async = true } end, bufopts)
	end

local lsp_flags = {
-- This is the default in Nvim 0.7+
debounce_text_changes = 150,
}

-- AUTOCOMPLETE SETUP


-- -- Python language server
require'lspconfig'.jedi_language_server.setup{
    on_attach = on_attach,
    flags = lsp_flags,
}

require'lspconfig'.sumneko_lua.setup{
    on_attach = on_attach,
    flags = lsp_flags,
}



-- vim.opt.cot={"menu", "menuone", "noselec"}
local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- Set up nvim-cmp.
local luasnip = require("luasnip")
local cmp = require'cmp'

cmp.setup({
	snippet = {
		expand = function(args)
		require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
		end,
		},

	window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
		},
	 mapping = {
		 ["<Tab>"] = cmp.mapping(function(fallback)
		 if cmp.visible() then
			 cmp.select_next_item()
	         elseif luasnip.expand_or_jumpable() then
			 luasnip.expand_or_jump()
		 elseif has_words_before() then
			cmp.complete()
		 else
			fallback()
		 end
	 end, { "i", "s" }),

	["<S-Tab>"] = cmp.mapping(function(fallback)
		if cmp.visible() then
			cmp.select_prev_item()
	        elseif luasnip.jumpable(-1) then
			luasnip.jump(-1)
	        else
			fallback()
	        end
	end, { "i", "s" }),

		  },

	sources = cmp.config.sources({
	      { name = 'nvim_lsp' },
	      { name = 'luasnip' }, -- For luasnip users.
	      -- { name = 'ultisnips' }, -- For ultisnips users.
	      -- { name = 'snippy' }, -- For snippy users.
	      }, {
	      { name = 'buffer' },
	    })
	  })

--   -- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
	sources = cmp.config.sources({
	      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
	      }, {
	      { name = 'buffer' },
	    })
	  })

--   -- Set up lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()
--   -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  -- require('lspconfig')['jedi_language_server'].setup { --This breaks the K window
  --   capabilities = capabilities
  -- }

-- TELESCOPE SETUP

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>gs', builtin.find_files, {})
vim.keymap.set('n', '<leader>gg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>gb', builtin.buffers, {})
vim.keymap.set('n', '<leader>gh', builtin.help_tags, {})

-- TREESITTER SETUP


require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "help", "c", "lua", "python", "go" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = true,


  ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
  -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}
