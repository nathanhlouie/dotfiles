return {
  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = "InsertEnter",
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        build = "make install_jsregexp",
        dependencies = {
          "rafamadriz/friendly-snippets",
          config = function()
            require("luasnip.loaders.from_vscode").lazy_load()
          end,
        },
      },
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      require("lsp-zero.cmp").extend()

      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
      vim.api.nvim_set_hl(0, "CmpNormal", { bg = "#24273a", fg = "#cad3f5" })

      local cmp = require("cmp")
      local cmp_action = require("lsp-zero.cmp").action()

      cmp.setup({
        preselect = "item",
        view = {
          entries = { name = "custom", selection_order = "near_cursor" }
        },
        completion = {
          completeopt = "menu,menuone,noinsert"
        },
        window = {
          completion = {
            border = "rounded",
            winhighlight = "Normal:CmpNormal",
          },
          documentation = {
            border = "rounded",
            winhighlight = "Normal:CmpNormal",
          },
        },
        mapping = {
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp_action.tab_complete(),
          ["<S-Tab>"] = cmp_action.select_prev_or_fallback(),
          ["<C-f>"] = cmp_action.luasnip_jump_forward(),
          ["<C-b>"] = cmp_action.luasnip_jump_backward(),
          ["<C-n>"] = cmp_action.luasnip_supertab(),
          ["<C-p>"] = cmp_action.luasnip_shift_supertab(),
        },
        sources = {
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "buffer" },
          { name = "path" },
          { name = "nvim_lsp_signature_help" },
        },
        formatting = {
          fields = { "abbr", "kind", "menu" },
          format = function(entry, vim_item)
            vim_item.menu = ({
              buffer = "[Buffer]",
              nvim_lsp = "[LSP]",
              luasnip = "[LuaSnip]",
              nvim_lua = "[Lua]",
              latex_symbols = "[LaTeX]",
            })[entry.source.name]
            return vim_item
          end,
        },
        experimental = {
          ghost_text = {
            hl_group = "CmpGhostText",
          },
        },
      })
    end,
  },
}
