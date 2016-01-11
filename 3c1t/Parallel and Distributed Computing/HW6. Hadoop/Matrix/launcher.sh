mv MatrixMultiplication.java src/
javac -cp `yarn classpath` -d  ./build ./src/MatrixMultiplication.java
rm matrix.jar
jar cf matrix.jar -C ./build ru
hadoop fs -rm -r matrix_out
hadoop fs -rm -r matrix.jar matrix_input.txt
hadoop fs -put matrix.jar
hadoop fs -put matrix_input.txt
hadoop jar matrix.jar ru.jiht.MatrixMultiplication matrix_input.txt matrix_out
