options(covr.exclude_end="(?://|#)[[:space:]]*nocov[[:space:]]*end")
options(covr.exclude_start="(?://|#)[[:space:]]*nocov[[:space:]]*start")
options(covr.exclude_pattern="(?://|#)[[:space:]]*nocov")

covr::codecov()

