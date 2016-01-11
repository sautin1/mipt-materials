rm -r matrix_out
hadoop fs -get matrix_out
cat matrix_out/part* | less
