return {
	"epwalsh/obsidian.nvim",
	event = "VeryLazy",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"dkarter/bullets.vim",
		{
			"lukas-reineke/headlines.nvim",
			dependencies = "nvim-treesitter/nvim-treesitter",
			config = true,
		},
	},
	keys = {
		{
			"<leader>oo",
			":ObsidianOpen<CR>",
		},
		{
			"<leader>on",
			":ObsidianNew<CR>",
		},
		{
			"<leader>or",
			":ObsidianNew ",
		},
		{
			"<leader>oq",
			":ObsidianQuickSwitch<CR>",
		},
		{
			"<leader>of",
			":ObsidianFollowLink<CR>",
		},
		{
			"<leader>ob",
			":ObsidianBackLinks<CR>",
		},
		{
			"<leader>ot",
			":ObsidianToday<CR>",
		},
		{
			"<leader>oy",
			":ObsidianYesterday<CR>",
		},
		{
			"<leader>oe",
			":ObsidianTemplate<CR>",
		},
		{
			"<leader>os",
			":ObsidianSearch<CR>",
		},
		{
			"<leader>ol",
			":ObsidianLink<CR>",
		},
		{
			"<leader>oi",
			":ObsidianLinkNew<CR>",
		},
	},
	config = function()
		require("obsidian").setup({
			dir = "~/Documents/obsidian",

			notes_subdir = "notes",

			daily_notes = {
				folder = "daily",
				date_format = "%Y-%m-%d",
			},

			completion = {
				nvim_cmp = true,
				min_chars = 2,
				new_notes_location = "current_dir",
				prepend_note_id = true,
			},

			mappings = {
				["gf"] = require("obsidian.mapping").gf_passthrough(),
			},

			note_id_func = function(title)
				local suffix = ""
				if title ~= nil then
					suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
				else
					for _ = 1, 4 do
						suffix = suffix .. string.char(math.random(65, 90))
					end
				end
				return "(" .. tostring(os.time()) .. ") " .. suffix
			end,

			disable_frontmatter = false,

			note_frontmatter_func = function(note)
				local out = { id = note.id, aliases = note.aliases, tags = note.tags }
				if note.metadata ~= nil and require("obsidian").util.table_length(note.metadata) > 0 then
					for k, v in pairs(note.metadata) do
						out[k] = v
					end
				end
				return out
			end,

			templates = {
				subdir = "templates",
				date_format = "%Y-%m-%d",
				time_format = "%H:%M",
			},

			follow_url_func = function(url)
				vim.fn.jobstart({ "open", url })
			end,

			use_advanced_uri = true,

			open_app_foreground = true,

			finder = "telescope.nvim",
		})
	end,
}
