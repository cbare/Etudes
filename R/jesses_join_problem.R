setup <- function(n = 1e6) {

    library(data.table)

    hex_val <- function(n) {
        sprintf("%08X", sample(1e9,n))
    }

    shuffled_vals <- function(n, ids, suffix) {
        message(suffix)
        x <- data.table(var1=sample(n,n), var2=sample(n,n), var3=sample(n,n), var4=sample(n,n), var5=sample(n,n))
        setnames(x, paste(colnames(x),suffix,sep=""))
        cbind(ids, x)[sample(n,.9*n), ]
    }

    message("Setting up..")
    i <- hex_val(n)
    ids <- data.table(a=sample(i), b=sample(i), c=sample(i), d=sample(i), e=sample(i))
    L <- lapply(letters[1:5], function(s) shuffled_vals(n, ids, s))
    names(L) <- paste("x",1:5,sep="")

    message("Join x1, x2, x3, x4, x5 on id columns a, b, c, d, e into a single wide-format dataframe.")
    attach(L, name="jesse-R-test")
}