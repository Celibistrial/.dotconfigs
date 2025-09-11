require('tardis-nvim').setup {
    keymap = {
        ["next"] = '<C-j>',             -- next entry in log (older)
        ["prev"] = '<C-k>',             -- previous entry in log (newer)
        ["quit"] = 'q',                 -- quit all
        ["revision_message"] = '<C-m>', -- show revision message for current revision
        ["commit"] = '<C-g>',           -- replace contents of origin buffer with contents of tardis buffer
    },
    settings = {
        initial_revisions = 10,         -- initial revisions to create buffers for
        max_revisions = 256,            -- max number of revisions to load
        show_commit_index = false,      -- append [index|total] to buffer names when browsing revisions
    },
}
