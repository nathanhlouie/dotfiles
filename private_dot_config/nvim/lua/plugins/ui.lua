return {
	{
		"stevearc/dressing.nvim",
		event = "VeryLazy",
		opts = {},
	},

	{
		"lewis6991/gitsigns.nvim",
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			signs = {
				add = { text = "▎" },
				change = { text = "▎" },
				delete = { text = "" },
				topdelete = { text = "" },
				changedelete = { text = "▎" },
				untracked = { text = "▎" },
			},
			on_attach = function(buffer)
				local gs = package.loaded.gitsigns

				local function map(mode, l, r)
					vim.keymap.set(mode, l, r, { buffer = buffer })
				end

				map("n", "]h", gs.next_hunk)
				map("n", "[h", gs.prev_hunk)
				map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>")
				map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>")
				map("n", "<leader>ghS", gs.stage_buffer)
				map("n", "<leader>ghu", gs.undo_stage_hunk)
				map("n", "<leader>ghR", gs.reset_buffer)
				map("n", "<leader>ghp", gs.preview_hunk)
				map("n", "<leader>ghb", function()
					gs.blame_line({ full = true })
				end)
				map("n", "<leader>ghd", gs.diffthis)
				map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
			end,
		},
	},

	{
		"lukas-reineke/indent-blankline.nvim",
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			char = "│",
			filetype_exclude = {
				"help",
				"alpha",
				"Trouble",
				"lazy",
				"mason",
				"norg",
				"notify",
				"toggleterm",
				"lazyterm",
			},
			show_trailing_blankline_indent = false,
			show_current_context = false,
		},
	},

	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			cmdline = {
				format = {
					cmdline = { pattern = "^:", icon = "★", lang = "vim" },
				},
			},
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
				hover = {
					enabled = false,
				},
				signature = {
					enabled = false,
				},
			},
		},
		dependencies = {
			"MunifTanjim/nui.nvim",
			{
				"rcarriga/nvim-notify",
				keys = {
					{
						"<leader>u",
						function()
							require("notify").dismiss({ silent = true, pending = true })
						end,
					},
				},
				opts = {
					background_colour = "#000000",
					stages = "fade",
					timeout = 3000,
					max_height = function()
						return math.floor(vim.o.lines * 0.75)
					end,
					max_width = function()
						return math.floor(vim.o.columns * 0.75)
					end,
				},
			},
		},
	},

	{
		"echasnovski/mini.indentscope",
		version = false,
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			symbol = "│",
			options = { try_as_border = true },
		},
		init = function()
			vim.api.nvim_create_autocmd("FileType", {
				pattern = {
					"help",
					"alpha",
					"Trouble",
					"lazy",
					"mason",
					"norg",
					"notify",
					"toggleterm",
					"lazyterm",
				},
				callback = function()
					vim.b.miniindentscope_disable = true
				end,
			})
		end,
	},
}
