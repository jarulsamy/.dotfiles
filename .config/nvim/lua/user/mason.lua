local M = {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
        "williamboman/mason.nvim",
    },
}

function M.config()
    local servers = {
        "bashls",
        "clangd",
        "cssls",
        "docker_compose_language_service",
        -- "docker_language_service",
        "graphql",
        "html",
        "jinja_lsp",
        "jsonls",
        "lua_ls",
        "pylsp",
        "pyright",
        "ruff",
        "rust_analyzer",
        -- "terraform_lsp",
        "vimls",
        "yamlls",
    }

    require("mason").setup {
        ui = {
            border = "rounded",
        },
    }

    require("mason-lspconfig").setup {
        ensure_installed = servers,
    }
end

return M
