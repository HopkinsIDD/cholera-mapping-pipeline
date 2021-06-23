all_subdirs <- list.dirs(".", recursive = TRUE, full.names = TRUE)

all_files <- unlist(lapply(all_subdirs, list.files, recursive = TRUE, full.names = TRUE)[-1])

r_files <- all_files[grepl("\\.R", all_files)]

test_files <- r_files[grepl("^test_", basename(r_files))]
setup_files <- r_files[grepl("^setup_", basename(r_files))]
helper_files <- r_files[grepl("^helper_", basename(r_files))]


for (file in c(setup_files, helper_files, test_files)) {
    source(file)
}
