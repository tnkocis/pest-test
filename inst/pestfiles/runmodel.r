#! C:\R\R-3.4.4\bin\x64\Rscript.exe

args <- commandArgs(TRUE)

paramfile = args[1]
outfile = args[2]

devtools::load_all("C:/repository/pestest")
LV.out = run_model(param.file = paramfile)
write_outputs(LV.out, outfile)
