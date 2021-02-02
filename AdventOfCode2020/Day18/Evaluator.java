public class Evaluator{
    private ExpressionTokenizer tokenizer;

    public Evaluator(String expression){
        tokenizer = new ExpressionTokenizer(expression);
    }

    public long getExpressionValue(){
        long value = getTermValue();
        boolean done = false;
        while (!done){
            String next = tokenizer.peekToken();
            if ("*".equals(next)){
                tokenizer.nextToken();
                value *= getTermValue();
            }
            else{
                done = true;
            }
        }
        return value;
    }

    public long getTermValue(){
        long value = getFactorValue();
        boolean done = false;
        while (!done){
            String next = tokenizer.peekToken();
            if ("+".equals(next)){
                tokenizer.nextToken();
                value += getFactorValue();
            }
            else{
                done = true;
            }
        }
        return value;
    }

    public long getFactorValue(){
        long value;
        String next = tokenizer.peekToken();
        if ("(".equals(next)){
            tokenizer.nextToken();
            value = getExpressionValue();
            tokenizer.nextToken();
        }
        else{
            value = Long.parseLong(tokenizer.nextToken());
        }
        return value;
    }

    // public long getTermValue(){
    //     long value = getFactorValue();
    //     boolean done = false;
    //     while (!done){
    //         String next = tokenizer.peekToken();
    //         if ("*".equals(next) || "+".equals(next)){
    //             tokenizer.nextToken();
    //             long value2 = getFactorValue();
    //             if ("*".equals(next)){
    //                 value *= value2;
    //             }
    //             else{
    //                 value += value2;
    //             }
    //         }
    //         else{
    //             done = true;
    //         }
    //     }
    //     return value;
    // }

    // public long getFactorValue(){
    //     long value;
    //     String next = tokenizer.peekToken();
    //     if ("(".equals(next)){
    //         tokenizer.nextToken();
    //         value = getTermValue();
    //         tokenizer.nextToken();
    //     }
    //     else{
    //         value = Long.parseLong(tokenizer.nextToken());
    //     }
    //     return value;
    // }

    public void changeString(String str){
        tokenizer = new ExpressionTokenizer(str);
    }
}