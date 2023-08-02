return {
	"goolord/alpha-nvim",
	event = "VimEnter",
	opts = function()
		require("alpha")
		require("alpha.term")
		local dashboard = require("alpha.themes.dashboard")

		dashboard.section.terminal.command = os.getenv("HOME") .. "/.config/nvim/lua/render.sh"
		dashboard.section.terminal.width = 68
		dashboard.section.terminal.height = 34
		dashboard.section.terminal.opts.redraw = true

		dashboard.section.header.val = '"Life is here for us to become kinder."'
		dashboard.section.header.opts.hl = "AlphaFooter"

		dashboard.section.buttons.val = {
			dashboard.button("f", " " .. "Find File", ":Telescope find_files<CR>"),
			dashboard.button("g", " " .. "Daily Note", ":ObsidianToday<CR>"),
			dashboard.button("c", " " .. "Edit Config", ":e $MYVIMRC<CR>"),
			dashboard.button("q", " " .. "Quit", ":qa<CR>"),
		}
		for _, button in ipairs(dashboard.section.buttons.val) do
			button.opts.hl = "AlphaButtons"
		end

		dashboard.section.footer.opts.hl = "AlphaHeader"

		dashboard.config.layout = {
			dashboard.section.terminal,
			{ type = "padding", val = 8 },
			dashboard.section.header,
			{ type = "padding", val = 2 },
			dashboard.section.buttons,
			{ type = "padding", val = 1 },
			dashboard.section.footer,
		}

		return dashboard
	end,
	config = function(_, dashboard)
		if vim.o.filetype == "lazy" then
			vim.cmd.close()
			vim.api.nvim_create_autocmd("User", {
				pattern = "AlphaReady",
				callback = function()
					require("lazy").show()
				end,
			})
		end

		require("alpha").setup(dashboard.opts)

		vim.api.nvim_create_autocmd("User", {
			pattern = "LazyVimStarted",
			callback = function()
				local stats = require("lazy").stats()
				local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
				dashboard.section.footer.val = "Loaded " .. stats.count .. " plugins in " .. ms .. "ms"
				pcall(vim.cmd.AlphaRedraw)
			end,
		})
	end,
}
