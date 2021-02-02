import java.util.Scanner;
import java.io.*;

public class ExpressionCalculator{
    public static void main(String[] args){
        Evaluator ev = new Evaluator("");
        long sum = 0;
        try (FileReader reader = new FileReader("input.txt"); Scanner parser = new Scanner(reader)){
            while (parser.hasNextLine()){
                String input = parser.nextLine();
                input = input.replaceAll("\\s","");
                ev.changeString(input);
                sum += ev.getExpressionValue();
            }
            System.out.println(sum);
        }
        catch (IOException e){

        }
    }
}