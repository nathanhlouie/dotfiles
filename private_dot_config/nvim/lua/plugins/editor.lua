return {
  {
    "ggandor/flit.nvim",
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
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end)
        map("n", "<leader>ghd", gs.diffthis)
        map("n", "<leader>ghD", function() gs.diffthis("~") end)
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
      end,
    },
  },

  {
    "ThePrimeagen/harpoon",
    lazy = false,
    opts = {
      tabline = true,
    },
    keys = {
      { "<leader>m", function() require("harpoon.mark").add_file() end },
      { "<leader>o", function() require("harpoon.ui").toggle_quick_menu() end },
      { "<leader>1", function() require("harpoon.ui").nav_file(1) end },
      { "<leader>2", function() require("harpoon.ui").nav_file(2) end },
      { "<leader>3", function() require("harpoon.ui").nav_file(3) end },
      { "<leader>4", function() require("harpoon.ui").nav_file(4) end },
      { "<leader>5", function() require("harpoon.ui").nav_file(5) end },
      { "<leader>n", function() require("harpoon.ui").nav_next() end },
      { "<leader>p", function() require("harpoon.ui").nav_prev() end },
    },
    init = function()
      vim.api.nvim_set_hl(0, "HarpoonInactive", { bg = "NONE", fg = "#6e738d" })
      vim.api.nvim_set_hl(0, "HarpoonActive", { bg = "NONE", fg = "#cad3f5" })
      vim.api.nvim_set_hl(0, "HarpoonNumberActive", { bg = "NONE", fg = "#8bd5ca" })
      vim.api.nvim_set_hl(0, "HarpoonNumberInactive", { bg = "NONE", fg = "#c6a0f6" })
    end,
  },

  {
    "ggandor/leap.nvim",
    event = "VeryLazy",
    config = function()
      require("leap").add_default_mappings(true)
    end
  },

  {
    "stevearc/oil.nvim",
    lazy = false,
    opts = {},
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader>-", function() require("oil").open() end },
    },
  },

  {
    "nvim-pack/nvim-spectre",
    cmd = "Spectre",
    opts = { open_cmd = "noswapfile vnew" },
    keys = {
      { "<leader>sr", function() require("spectre").open() end },
    },
  },

  {
    "RRethy/vim-illuminate",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      large_file_cutoff = 2000,
      large_file_overrides = {
        providers = { "lsp" },
      },
    },
    keys = {
      { "]]", function() require("illuminate").goto_next_reference(false) end },
      { "[[", function() require("illuminate").goto_prev_reference(false) end },
    },
    config = function(_, opts)
      require("illuminate").configure(opts)
    end,
  },
}
