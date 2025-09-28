local M = {
    "nvim-lualine/lualine.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
}

function M.config()
    require("lualine").setup {
        options = {
            component_separators = { left = "", right = "" },
            section_separators = { left = "", right = "" },
            ignore_focus = { "NvimTree" },
        },
        sections = {
            lualine_a = { "mode" },
            lualine_b = { "branch", "diff", "diagnostics" },
            lualine_c = { "filename" },
            lualine_x = { "copilot", "filetype" },
            lualine_y = { "progress" },
            lualine_z = {},
        },
        extensions = { "lazy", "oil", "quickfix", "man", "fugitive" },
    }
end

return M
