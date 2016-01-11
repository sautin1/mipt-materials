mv Pi.java src/
javac -cp `yarn classpath` -d  ./build ./src/Pi.java
rm pi.jar
jar cf pi.jar -C ./build ru
hadoop fs -rm -r dir_out
hadoop fs -rm -r pi.jar pi_input.txt
hadoop fs -put pi.jar
hadoop fs -put pi_input.txt
hadoop jar pi.jar ru.jiht.Pi pi_input.txt dir_out
