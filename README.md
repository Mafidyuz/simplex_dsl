# Simplex DSL

DSL fatto in scala. Per eseguirlo compilare i diversi file insieme con il comando

```scalac TableauBuilder.scala Tableau.scala SimplexParser.scala SimplexEvaluator.scala Matrix.scala```

Scrivere un problema di programmazione lineare in un file ad esempio `test.simplex`
```
max z = 3x1 + 2x2

vincoli
8x1 + 4x2 <= 64
4x1 + 6x2 <= 54
x1 + x2 <= 10
```

eseguire il tutto con `scala SimplexEvaluator test.simplex`
