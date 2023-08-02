return {
	{
		"freddiehaddad/feline.nvim",
		lazy = false,
		opts = function()
			local clrs = require("catppuccin.palettes").get_palette()
			local lsp = require("feline.providers.lsp")

			local assets = {
				mode_icon = "",
				file = "󰈙",
				lsp = {
					server = "󰅡",
					error = "",
					warning = "",
					info = "",
					hint = "",
				},
				git = {
					branch = "",
					added = "",
					changed = "",
					removed = "",
				},
			}

			local sett = {
				text = clrs.mantle,
				bkg = "NONE",
				diffs = clrs.mauve,
				extras = clrs.overlay1,
				curr_file = clrs.maroon,
				curr_dir = clrs.flamingo,
				show_modified = true,
			}

			local mode_colors = {
				["n"] = { "NORMAL", clrs.lavender },
				["no"] = { "N-PENDING", clrs.lavender },
				["i"] = { "INSERT", clrs.green },
				["ic"] = { "INSERT", clrs.green },
				["t"] = { "TERMINAL", clrs.green },
				["v"] = { "VISUAL", clrs.flamingo },
				["V"] = { "V-LINE", clrs.flamingo },
				["�"] = { "V-BLOCK", clrs.flamingo },
				["R"] = { "REPLACE", clrs.maroon },
				["Rv"] = { "V-REPLACE", clrs.maroon },
				["s"] = { "SELECT", clrs.maroon },
				["S"] = { "S-LINE", clrs.maroon },
				["�"] = { "S-BLOCK", clrs.maroon },
				["c"] = { "COMMAND", clrs.peach },
				["cv"] = { "COMMAND", clrs.peach },
				["ce"] = { "COMMAND", clrs.peach },
				["r"] = { "PROMPT", clrs.teal },
				["rm"] = { "MORE", clrs.teal },
				["r?"] = { "CONFIRM", clrs.mauve },
				["!"] = { "SHELL", clrs.green },
			}

			local shortline = false

			local components = {
				active = { {}, {}, {} },
				inactive = {},
			}

			local function is_enabled(min_width)
				if shortline then
					return true
				end

				return vim.api.nvim_win_get_width(0) > min_width
			end

			local invi_sep = {
				str = " ",
				hl = {
					fg = sett.bkg,
					bg = sett.bkg,
				},
			}

			local function any_git_changes()
				local gst = vim.b.gitsigns_status_dict
				if gst then
					if
						gst["added"] and gst["added"] > 0
						or gst["removed"] and gst["removed"] > 0
						or gst["changed"] and gst["changed"] > 0
					then
						return true
					end
				end
				return false
			end

			components.active[1][1] = {
				provider = " " .. assets.mode_icon .. " ",
				hl = function()
					return {
						fg = mode_colors[vim.fn.mode()][2],
						bg = sett.bkg,
					}
				end,
			}

			components.active[1][2] = {
				provider = function()
					return mode_colors[vim.fn.mode()][1]
				end,
				hl = function()
					return {
						fg = mode_colors[vim.fn.mode()][2],
						bg = sett.bkg,
						style = "bold",
					}
				end,
				right_sep = function()
					if any_git_changes() then
						return {
							str = "  ",
							hl = {
								fg = sett.bkg,
								bg = sett.bkg,
							},
						}
					else
						return ""
					end
				end,
			}

			components.active[1][3] = {
				provider = "git_diff_added",
				hl = {
					fg = clrs.green,
					bg = sett.bkg,
				},
				icon = " " .. assets.git.added .. " ",
			}

			components.active[1][4] = {
				provider = "git_diff_changed",
				hl = {
					fg = clrs.yellow,
					bg = sett.bkg,
				},
				icon = " " .. assets.git.changed .. " ",
			}

			components.active[1][5] = {
				provider = "git_diff_removed",
				hl = {
					fg = clrs.red,
					bg = sett.bkg,
				},
				icon = " " .. assets.git.removed .. " ",
			}

			components.active[1][6] = {
				provider = function()
					local current_line = vim.fn.line(".")
					local total_line = vim.fn.line("$")

					if current_line == 1 then
						return "Top"
					elseif current_line == vim.fn.line("$") then
						return "Bot"
					end
					local result, _ = math.modf((current_line / total_line) * 100)
					return result .. "%%"
				end,
				hl = {
					fg = clrs.rosewater,
					bg = sett.bkg,
				},
				left_sep = {
					str = "   ",
					hl = {
						fg = sett.bkg,
						bg = sett.bkg,
					},
				},
			}

			components.active[1][7] = {
				provider = "position",
				hl = {
					fg = clrs.rosewater,
					bg = sett.bkg,
				},
				left_sep = invi_sep,
			}

			components.active[1][8] = {
				provider = "macro",
				enabled = function()
					return vim.api.nvim_get_option("cmdheight") == 0
				end,
				hl = {
					fg = sett.extras,
					bg = sett.bkg,
				},
				left_sep = invi_sep,
			}

			components.active[1][9] = {
				provider = "search_count",
				enabled = function()
					return vim.api.nvim_get_option("cmdheight") == 0
				end,
				hl = {
					fg = sett.extras,
					bg = sett.bkg,
				},
				left_sep = invi_sep,
			}

			components.active[2][1] = {
				provider = "diagnostic_errors",
				enabled = function()
					return lsp.diagnostics_exist(vim.diagnostic.severity.ERROR)
				end,

				hl = {
					fg = clrs.red,
					bg = sett.bkg,
				},
				icon = " " .. assets.lsp.error .. " ",
			}

			components.active[2][2] = {
				provider = "diagnostic_warnings",
				enabled = function()
					return lsp.diagnostics_exist(vim.diagnostic.severity.WARN)
				end,
				hl = {
					fg = clrs.yellow,
					bg = sett.bkg,
				},
				icon = " " .. assets.lsp.warning .. " ",
			}

			components.active[2][3] = {
				provider = "diagnostic_info",
				enabled = function()
					return lsp.diagnostics_exist(vim.diagnostic.severity.INFO)
				end,
				hl = {
					fg = clrs.sky,
					bg = sett.bkg,
				},
				icon = " " .. assets.lsp.info .. " ",
			}

			components.active[2][4] = {
				provider = "diagnostic_hints",
				enabled = function()
					return lsp.diagnostics_exist(vim.diagnostic.severity.HINT)
				end,
				hl = {
					fg = clrs.rosewater,
					bg = sett.bkg,
				},
				icon = " " .. assets.lsp.hint .. " ",
			}

			components.active[3][1] = {
				provider = "lsp_client_names",
				hl = {
					fg = sett.extras,
					bg = sett.bkg,
				},
				right_sep = invi_sep,
			}

			components.active[3][2] = {
				provider = "git_branch",
				enabled = is_enabled(70),
				hl = {
					fg = clrs.sapphire,
					bg = sett.bkg,
					style = "bold",
				},
				icon = "  " .. assets.git.branch .. " ",
				right_sep = invi_sep,
			}

			components.active[3][3] = {
				provider = function()
					local filename = vim.fn.expand("%:t")
					local extension = vim.fn.expand("%:e")
					local present, icons = pcall(require, "nvim-web-devicons")
					local icon = present and icons.get_icon(filename, extension) or assets.file
					return (sett.show_modified and " %m" or " ") .. " " .. icon .. " " .. filename .. " "
				end,
				enabled = is_enabled(70),
				hl = {
					fg = sett.curr_file,
					bg = sett.bkg,
					style = "bold",
				},
			}

			components.active[3][4] = {
				provider = function()
					local dir_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
					return "  " .. "" .. " " .. dir_name .. " "
				end,
				enabled = is_enabled(80),
				hl = {
					fg = sett.curr_dir,
					bg = sett.bkg,
					style = "bold",
				},
			}

			return {
				components = components,
				disable = {
					filetypes = {
						"alpha",
					},
				},
			}
		end,
	},
}
