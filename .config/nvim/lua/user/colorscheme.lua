local M = {
    "navarasu/onedark.nvim",
    lazy = false,
    priority = 1000,
}

function M.config()
    vim.cmd.colorscheme "onedark"
end

return M
