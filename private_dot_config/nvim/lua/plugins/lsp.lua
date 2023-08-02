return {
	{
		"VonHeikemen/lsp-zero.nvim",
		version = false,
		lazy = true,
		config = function()
			require("lsp-zero.settings").preset({})
		end,
	},

	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"nvim-lua/plenary.nvim",
			"hrsh7th/cmp-nvim-lsp",
			{
				"williamboman/mason.nvim",
				build = ":MasonUpdate",
			},
			"williamboman/mason-lspconfig.nvim",
			"jose-elias-alvarez/null-ls.nvim",
			"jay-babu/mason-null-ls.nvim",
			{
				"https://git.sr.ht/~whynothugo/lsp_lines.nvim",
				config = function()
					vim.diagnostic.config({
						virtual_text = false,
					})
					require("lsp_lines").setup()
				end,
				keys = {
					{
						"<leader>l",
						function()
							require("lsp_lines").toggle()
						end,
					},
				},
			},
		},
		config = function()
			local lsp = require("lsp-zero")

			lsp.on_attach(function(_, bufnr)
				lsp.default_keymaps({ buffer = bufnr })
			end)

			lsp.ensure_installed({
				"elixirls",
				"lua_ls",
				"pylsp",
				"rust_analyzer",
				"tsserver",
				"marksman",
			})

			lsp.format_mapping("<leader>f", {
				format_opts = {
					async = false,
					timeout_ms = 10000,
				},
				servers = {
					["null-ls"] = { "javascript", "typescript", "lua", "rust", "python" },
				},
			})

			lsp.format_on_save({
				format_opts = {
					async = false,
					timeout_ms = 10000,
				},
				servers = {
					["null-ls"] = { "javascript", "typescript", "lua", "rust", "python" },
				},
			})

			lsp.setup()

			require("lspconfig").lua_ls.setup({
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT",
						},
						diagnostics = {
							globals = { "vim" },
						},
						workspace = {
							library = vim.api.nvim_get_runtime_file("", true),
							checkThirdParty = false,
						},
						telemetry = {
							enable = false,
						},
					},
				},
			})
			require("lspconfig").pylsp.setup({})
			require("lspconfig").tsserver.setup({})
			require("lspconfig").marksman.setup({})

			local null_ls = require("null-ls")
			null_ls.setup({
				sources = {
					null_ls.builtins.formatting.astyle,
					null_ls.builtins.formatting.black,
					null_ls.builtins.formatting.mix,
					null_ls.builtins.formatting.prettier,
					null_ls.builtins.formatting.stylua,
				},
			})

			require("mason-null-ls").setup({
				ensure_installed = {},
				automatic_installation = true,
			})
		end,
	},
}
