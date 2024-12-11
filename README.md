# FOL-Visualizer
The code for Adi and Burhan's CS560 project: a website that helps visualize First Order Logic proofs.

## How to run this project
You will need Python, OCaml, opam, and dune installed. As for libraries, our requirements.txt lists the Python required libraries, which is only Flask. For OCaml libraries, you will need the menhir library.

Once you have everything, from the outermost project directory, run "flask run" in the terminal and it should tell you the place your website is running locally!

## How to use this project

There are only two points of user interaction in our website - entering the FOL formula and pressing the "see next step" button to follow along with the knowledge base expanding, the latter of which is self explanatory.

 The allowed symbols for the FOL formulas users can enter are:

 1. Functions and vars - these can be made up of any string of (any case) English letters.

 2. Parentheses - they are allowed, lol.

 3. Negations - use ~.

 3. Conjunctions - use &&.

 4. Disjunctions - use ||.

 5. Implications - use ->.

 6. Bidirectional implications - use <->.

 7. Universal Quantifiers - use "forall {var}, {statement}." Note the required comma.

 8. Existential Quantifiers - use "exists {var}, {statement}."

 So an example allowed formula would be:

 ~(exists y, (forall z, ((p(z,y) <-> ~(exists x, (p(z,x) && p(x,z))))))).