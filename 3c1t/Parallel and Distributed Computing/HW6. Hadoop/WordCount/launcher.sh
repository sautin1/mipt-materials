chmod +x mapper.py
chmod +x reducer.py
hadoop fs -rm -r output
hadoop fs -rm -r mapper.py reducer.py
hadoop fs -put mapper.py
hadoop fs -put reducer.py
hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-mapreduce/hadoop-streaming.jar -input input/ -output output/ -mapper "mapper.py" -reducer "reducer.py" -file ./mapper.py -file ./reducer.py -file /opt/cloudera/parcels/CDH/lib/hadoop-mapreduce/hadoop-streaming.jar -numReduceTasks 1

