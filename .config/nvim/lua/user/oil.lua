local M = {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
}

function M.config()
    require("oil").setup {
        default_file_explorer = true,
        columns = {
            "icon",
            "permissions",
            "size",
            "mtime",
        },
        win_options = {
            wrap = false,
            signcolumn = "no",
            cursorcolumn = false,
            foldcolumn = "0",
            spell = false,
            list = false,
            conceallevel = 3,
            concealcursor = "nvic",
        },
        skip_confirm_for_simple_edits = true,
        watch_for_changes = false,
        float = {
            padding = 2,
            max_height = 0,
            max_width = 0,
            border = "rounded",
        },
    }
    vim.keymap.set("n", "<leader>o-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
end

return M
