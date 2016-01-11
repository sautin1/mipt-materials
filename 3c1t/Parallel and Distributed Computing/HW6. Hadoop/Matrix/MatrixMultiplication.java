package ru.jiht;
import java.io.IOException;
import java.util.*;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

public class MatrixMultiplication {
    public static final String delim = ", ";

    public static class Map extends Mapper<LongWritable, Text, Text, Text> {
        // A: m * n; B: n * p.
        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            Configuration conf = context.getConfiguration();
            int m = Integer.parseInt(conf.get("m"));
            int p = Integer.parseInt(conf.get("p"));
            String line = value.toString();
            String[] profile = line.split(delim);
            String matr = profile[0];
            if ("A".equals(matr)) {
                String i   = profile[1];
                String j   = profile[2];
                String aij = profile[3];
                for (int k = 0; k < p; ++k) {
                    context.write(new Text(i + delim + k), 
                                  new Text(matr + delim + j + delim + aij));
                }
            } else {
                String j   = profile[1];
                String k   = profile[2];
                String bjk = profile[3];
                for (int i = 0; i < m; ++i) {
                    context.write(new Text(i + delim + k), 
                                  new Text(matr + delim + j + delim + bjk));
                }
            }
        }
    }

    public static class Reduce extends Reducer<Text, Text, Text, Text> {
        public void reduce(Text key, Iterable<Text> values, Context context) 
                throws IOException, InterruptedException {
            Configuration conf = context.getConfiguration();
            int n = Integer.parseInt(conf.get("n"));
            List<Long> rowA = new ArrayList<>(Collections.nCopies(n, 0L));
            List<Long> colB = new ArrayList<>(Collections.nCopies(n, 0L));

            for (Text textValue : values) {
                String[] value = textValue.toString().split(delim);
                Integer index = Integer.parseInt(value[1]);
                Long matrElem = Long.parseLong(value[2]);
                if ("A".equals(value[0])) {
                    rowA.set(index, matrElem);
                } else {
                    colB.set(index, matrElem);
                }
            }
            long result = 0L;
            for (int j = 0; j < n; ++j) {
                result += rowA.get(j) * colB.get(j);
            }
            context.write(new Text(key.toString()), new Text(Long.toString(result)));
        }
    }

    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("m", "2");
        conf.set("n", "5");
        conf.set("p", "3");
        Job job = new Job(conf, "Matrix");
        job.setJarByClass(MatrixMultiplication.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setMapperClass(Map.class);
        job.setReducerClass(Reduce.class);

        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        job.setNumReduceTasks(1);
        job.waitForCompletion(true);
    }
}