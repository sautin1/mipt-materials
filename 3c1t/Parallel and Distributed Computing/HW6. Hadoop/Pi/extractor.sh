rm -r dir_out
hadoop fs -get dir_out
cat dir_out/part* | less