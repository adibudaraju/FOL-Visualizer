import subprocess
import os
from flask import Flask, render_template, request, jsonify

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')


def unicodify(arr):
    arr_new = []
    for a in arr:
        b = a.replace("forall", chr(0x2200))
        b = b.replace("exists", chr(0x2203))
        b = b.replace("||", chr(0x2228))
        b = b.replace("&&", chr(0x2227))
        b = b.replace("<->", chr(0x21D4))
        b = b.replace("->", chr(0x21D2))
        b = b.replace("~", chr(0x00AC))
        
        arr_new.append(b)
    return arr_new

@app.route('/proof', methods=['POST'])
def proof():
    data = request.json
    statement = data['statement']
    result = run_ocaml_proof_checker(statement).splitlines()
    result.insert(0, statement)

    num_steps = 8
    if len(result) < num_steps:
        if result[1] == "timeout":
            return jsonify({"error": True, "msg": "Our algorithm timed out while evaluating your statement. This means it likely got stuck resolving an unhelpful chain - your statement's validity could not be determined."})
        else:
            return jsonify({"error": True, "msg": "There was an error parsing your logic statement."})
    
    
    first_part = result[0:num_steps]
    descriptions = [
        "Initial Statement: ",
        "Step 0: Convert all implications and remove free vars",
        "Step 1: Negate the statement",
        "Step 2: Convert to Negation Normal Form",
        "Step 3: Convert to Prenex Normal Form",
        "Step 4: Skolemize",
        "Step 5: Convert to Clausal Normal Form",
        "Step 6: Drop quantifiers and list out the clauses"
    ]
    
    initial_kb = result[num_steps - 1]
    initial_kb = [a.strip() for a in initial_kb.split(";")]
    initial_kb.insert(0, "Knowledge Base:")

    valid = None
    if("invalid" in result[-1]):
        valid = False
    else:
        valid = True

    summary = result[-1]
    unify = result[num_steps:-1]
    unify_steps = []
    kb = []
    for stmt in unify:
        parts = stmt.split(";")
        parts = [a.strip() for a in parts]
        if parts[0] == "Factor":
            subst = "substitution " + parts[4]
            if parts[4] == "NONE":
                subst = "no substitution"
            unify_steps.append(f"Factoring clause {parts[1]} by unifying {parts[2]} and {parts[3]} with {subst}, resulting in new clause {parts[5]}")
            kb.append(parts[5])
        else:
            subst = "substitution " + parts[5]
            if parts[5] == "NONE":
                subst = "no substitution"
            unify_steps.append(f"Resolving clauses {parts[1]}; {parts[2]} by unifying {parts[3]} and {parts[4]} with {subst}, resulting in new clause {parts[6]}")
            kb.append(parts[6])
        
    first_part = unicodify(first_part)
    unify_steps = unicodify(unify_steps)
    kb = unicodify(kb)
    initial_kb = unicodify(initial_kb)

    return jsonify({"error": False, "msg": "bruh", "valid": valid, "steps": first_part, "descriptions": descriptions, "unify": unify_steps, "summary": summary, "kb": kb, "initial_kb": initial_kb})

def run_ocaml_proof_checker(statement):
    dir_path = os.path.dirname(os.path.realpath(__file__))
    folprover_dir = os.path.join(dir_path, 'folprover')
    command = f"dune exec folprover \"{statement}\""

    try:
        result = subprocess.run(
            command, 
            shell=True, 
            cwd=folprover_dir, 
            capture_output=True, 
            text=True, 
            timeout=7
        )
        if result.returncode == 0:
            output = result.stdout
        else:
            output = f"Error: {result.stderr}"
    except subprocess.TimeoutExpired:
        return "timeout"
    except Exception as e:
        output = f"An error occurred: {e}"

    return output

if __name__ == '__main__':
    app.run(debug=True)
