# Haskell-Integral-Derivative-Calculator

A compact Haskell-based Computer Algebra System featuring a custom symbolic language, a hand written recursive-descent parser, rule driven simplification, structural differentiation, pattern-based antiderivatives, adaptive numerical integration, an interactive plotting layer, and a LLM module that explains results.
All actual math is computed locally by the system.

I built this as part of my **Logical & Functional Programming course**, mainly to understand how CAS tools work internally and how far a clean, modular Haskell design can be pushed without relying on big external libraries.

---

## Architecture

The project is split into straightforward modules, each doing one job

### Expression language (Expr)
Core symbolic data type used everywhere.

 ### Parser (Parse)
A fully manual recursive-descent parser. It handles implicit multiplication, nested functions, exponent precedence, and weird edge cases that show up when students type in random math.

### Simplifier (Simplify)
A set of algebraic rewrite rules — constant folding, combining factors, flattening multiplications, removing useless parentheses, etc.

### Differentiator (Differentiate)
Full structural derivative rules: product, quotient, chain rule, plus trig, hyperbolic, and even the general u^v case.

### Symbolic Integrator (Integrate)
A pattern-based antiderivative engine. It covers polynomials, exponentials, trig/hyperbolic forms with linear inner functions, and a few special identities.
If no rule matches, it fails cleanly instead of pretending.

### Numerical Integrator (Integrate)
Adaptive Simpson’s Rule with recursion limits and stability checks. Good for definite integrals and plotting.

### Evaluator (Eval)
Turns symbolic expressions into actual numeric functions.

### Pretty Printer (Pretty)
Outputs LaTeX-style expressions so MathJax can render them nicely.

### Web UI (Main)
Scotty server, Plotly graphs, interactive sliders, and MathJax rendering.

### LLM explainer (LLM)
Only generates short natural-language explanations of the results.
It does not compute derivatives or integrals — it just explains what the system already computed.

---

## What it can do

- Parse real mathematical expressions like sin(x)*exp(x) or atan x + cosh(e*x)

- Simplify and normalize the expression

- Compute symbolic derivatives

- Compute symbolic antiderivatives (when rules apply)

- Perform adaptive numerical integration for definite integrals

- Plot f(x) and f'(x) interactively
  
- Render all expressions cleanly using LaTeX

- Provide a short explanation via the LLM (optional and clearly isolated)

---

## How to run it


1.First clone this repository (or download each file in the same folder)

Then use the commands(assuming you have all neded pacakges to run haskell and use cabal):

```bash

cabal build
cabal run
```

Then visit:

```arduino

http://localhost:3000

```

Type an expression, adjust the bounds using the sliders, and the system updates the plots and symbolic results instantly.

---

## Licence

This project is licensed under the MIT License.


Maintained by @irfanuruchi
