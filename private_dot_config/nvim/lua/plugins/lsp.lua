return {
  {
    "VonHeikemen/lsp-zero.nvim",
    version = false,
    lazy = true,
    config = function()
      require("lsp-zero.settings").preset({})
    end
  },

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "williamboman/mason-lspconfig.nvim",
      {
        "williamboman/mason.nvim",
        build = ":MasonUpdate"
      },
      "lukas-reineke/lsp-format.nvim",
    },
    config = function()
      local lsp = require("lsp-zero")

      lsp.on_attach(function(client, bufnr)
        lsp.default_keymaps({ buffer = bufnr })

        if client.supports_method("textDocument/formatting") then
          require("lsp-format").on_attach(client)
        end

        vim.keymap.set({ "n", "x" }, "<leader>f", function()
          vim.lsp.buf.format({ async = false, timeout_ms = 10000 })
        end, opts)
      end)

      lsp.ensure_installed({
        "tsserver",
        "eslint",
        "rust_analyzer",
        "lua_ls",
      })

      lsp.setup()
    end
  },
}
