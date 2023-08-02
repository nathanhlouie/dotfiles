return {
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		opts = {},
		keys = {
			{
				"s",
				mode = { "n", "x", "o" },
				function()
					require("flash").jump()
				end,
			},
			{
				"S",
				mode = { "n", "o", "x" },
				function()
					require("flash").treesitter()
				end,
			},
			{
				"r",
				mode = "o",
				function()
					require("flash").remote()
				end,
			},
			{
				"R",
				mode = { "o", "x" },
				function()
					require("flash").treesitter_search()
				end,
			},
			{
				"<c-s>",
				mode = { "c" },
				function()
					require("flash").toggle()
				end,
			},
		},
	},

	{
		"rmagatti/goto-preview",
		event = "VeryLazy",
		opts = {
			default_mappings = true,
		},
	},

	{
		"theprimeagen/harpoon",
		event = "VeryLazy",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			global_settings = {
				tabline = true,
			},
			menu = {
				width = vim.api.nvim_win_get_width(0) - 4,
			},
		},
		keys = function()
			local custom = {
				{
					"<leader>m",
					function()
						require("harpoon.mark").add_file()
					end,
				},
				{
					"<leader>h",
					function()
						require("harpoon.ui").toggle_quick_menu()
					end,
				},
				{
					"<leader>n",
					function()
						require("harpoon.ui").nav_next()
					end,
				},
				{
					"<leader>p",
					function()
						require("harpoon.ui").nav_prev()
					end,
				},
			}

			for i = 1, 9 do
				table.insert(custom, {
					string.format("<leader>%s", i),
					function()
						require("harpoon.ui").nav_file(i)
					end,
				})
			end

			return custom
		end,
		config = function(_, opts)
			require("harpoon").setup(opts)
			vim.api.nvim_set_hl(0, "HarpoonInactive", { bg = "NONE", fg = "#6e738d" })
			vim.api.nvim_set_hl(0, "HarpoonActive", { bg = "NONE", fg = "#cad3f5" })
			vim.api.nvim_set_hl(0, "HarpoonNumberActive", { bg = "NONE", fg = "#8bd5ca" })
			vim.api.nvim_set_hl(0, "HarpoonNumberInactive", { bg = "NONE", fg = "#c6a0f6" })
		end,
	},

	{
		"lewis6991/hover.nvim",
		event = "VeryLazy",
		opts = {
			init = function()
				require("hover.providers.lsp")
			end,
			preview_opts = {
				border = nil,
			},
			preview_window = false,
			title = true,
		},
		keys = {
			{
				"K",
				function()
					require("hover").hover()
				end,
			},
			{
				"gK",
				function()
					require("hover").hover_select()
				end,
			},
		},
	},

	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {},
	},

	{
		"nvim-pack/nvim-spectre",
		opts = { open_cmd = "noswapfile vnew" },
		keys = {
			{
				"<leader>s",
				function()
					require("spectre").open()
				end,
			},
		},
	},

	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		opts = {},
	},

	{
		"stevearc/oil.nvim",
		opts = {
			view_options = {
				show_hidden = true,
			},
		},
		dependencies = { "nvim-tree/nvim-web-devicons" },
		keys = {
			{
				"<leader>-",
				function()
					require("oil").open()
				end,
			},
		},
	},

	{
		"RRethy/vim-illuminate",
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			delay = 200,
			large_file_cutoff = 2000,
			large_file_overrides = {
				providers = { "lsp" },
			},
		},
		keys = {
			{
				"]]",
				function()
					require("illuminate").goto_next_reference()
				end,
			},
			{
				"[[",
				function()
					require("illuminate").goto_prev_reference()
				end,
			},
		},
		config = function(opts)
			require("illuminate").configure(opts)
		end,
	},
}
