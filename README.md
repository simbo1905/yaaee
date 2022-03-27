# yaaee

Yet Another Arithmetic Expression Evaluator

# Specification

 * A simple integer arithmetic expression to give an integer result, e.g. "1 + 2 * 3" should yield the answer 7
 * Must be able to be run over a file with one expression per line and output "expr = answer"
 * Supports: 
   1. operators +, -, *, /
   2. integers
   3. parentheses
   
# Running In Docker

Pull the mozilla sbt docker image with `docker pull mozilla/sbt` then run it on mac/linux with: 

```shell
docker run -it --rm -v ~/.ivy2:/root/.ivy2 -v ~/.sbt:/root/.sbt -v $PWD:/app -w /app mozilla/sbt sbt shell
```

Then tell `sbt shell` to compile and run the application to process an input file with: 

```shell
runMain YetAnotherArithmeticExpressionEvaluator example.txt
```

See also the GitHub Action build pipeline that runs the unit tests to check it all works. 
