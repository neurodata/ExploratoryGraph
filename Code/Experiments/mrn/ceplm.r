con <- url("http://www.cis.jhu.edu/~parky/MRN/cci.txt")
cci <- scan(con)
close(con)
cci

con <- url("http://www.cis.jhu.edu/~parky/MRN/fibergraph.Rbin")
load(con)
close(con)
(m <- length(fibergraph.list))
