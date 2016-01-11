package ru.jiht;
import java.io.IOException;
import java.util.*;
import java.util.Random;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

public class Pi {
    public static class Map extends Mapper<LongWritable, Text, IntWritable, LongWritable> {
        static Random rand = new Random();
        
        public void map(LongWritable key, Text value, Context context) throws 
                IOException, InterruptedException {
            long pointsQuantity = 100000L;

            long outsidePoints = 0L;
            long insidePoints = 0L;
            for (long i = 0L; i < pointsQuantity; ++i) {
                double x = rand.nextDouble();
                double y = rand.nextDouble();
                double dist = (x - 0.5)*(x - 0.5) + (y - 0.5) * (y - 0.5);
                if (dist > 0.5 * 0.5) {
                    ++outsidePoints;
                } else {
                    ++insidePoints;
                }
            }           
            context.write(new IntWritable(0), new LongWritable(outsidePoints));
            context.write(new IntWritable(1), new LongWritable( insidePoints));
        }
    }
    public static class Reduce extends Reducer<IntWritable, LongWritable, LongWritable, DoubleWritable> {
        static long insidePoints  = -1L;
        static long outsidePoints = -1L;

        public void reduce(IntWritable key, Iterable<LongWritable> values, Context context)
                throws IOException, InterruptedException {
            long sum = 0L;
            for (LongWritable value : values) {
                sum += value.get();
            }
            if (key.get() == 0) {
                outsidePoints = sum;
            } else {
                insidePoints  = sum;
            }
            if (insidePoints != -1L && outsidePoints != -1L) {
                context.write(new LongWritable(insidePoints + outsidePoints), 
                    new DoubleWritable(insidePoints * 4.0 / (insidePoints + outsidePoints)));
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        Job job = new Job(conf, "Pi");
        job.setJarByClass(Pi.class);

        job.setOutputKeyClass(IntWritable.class);
        job.setOutputValueClass(LongWritable.class);

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