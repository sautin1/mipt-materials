rm -r output
hadoop fs -get output
cat output/part* | less