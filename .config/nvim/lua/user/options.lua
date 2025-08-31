-- vim.opt.backspace = indent,eol,start
vim.opt.history = 1000
vim.opt.showcmd = true
vim.opt.showmode = false

vim.opt.cursorline = true -- highlight the current line
vim.opt.number = true
vim.opt.numberwidth = 4
vim.opt.relativenumber = true
vim.opt.ruler = true

vim.opt.visualbell = false

vim.opt.clipboard = "unnamedplus"
vim.opt.mouse = "a"
vim.opt.cmdheight = 1

-- Swap Files
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false

-- Default Indent
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.wrap = false

-- Scrolling
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 15
vim.opt.sidescroll = 1

-- Search
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.completeopt = { "menuone", "noselect" }
vim.opt.conceallevel = 0

vim.opt.laststatus = 2
vim.opt.pumblend = 10
vim.opt.pumheight = 10
vim.opt.showtabline = 1
vim.opt.signcolumn = "yes"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.opt.timeoutlen = 1000
vim.opt.title = false
vim.opt.undofile = true
vim.opt.updatetime = 100
vim.opt.fillchars = vim.opt.fillchars + "eob: "
vim.opt.fillchars:append {
    stl = " ",
}
vim.opt.list = true
vim.opt.listchars = {
    tab = "  ", -- two spaces
    trail = "·",
}

vim.opt.shortmess:append "c"

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]

vim.g.netrw_banner = 0
vim.g.netrw_mouse = 2
