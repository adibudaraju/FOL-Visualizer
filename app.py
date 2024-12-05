import subprocess
import os
from flask import Flask, render_template, request, jsonify

app = Flask(__name__)
from flask_cors import CORS
CORS(app)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/proof', methods=['POST'])
def proof():
    num_steps = 7
    data = request.json
    statement = data['statement']
    result = run_ocaml_proof_checker(statement).splitlines()
    valid = None
    initial_kb = result[num_steps - 1]
    initial_kb = [a.strip() for a in initial_kb.split(";")]
    initial_kb.insert(0, "Knowledge Base")
    kb = []
    if("invalid" in result[-1]):
        valid = False
    else:
        valid = True
    summary = result[-1]
    first_part = result[0:num_steps]
    descriptions = [
        "Initial Statement: "
        "Step 1: Negate the statement",
        "Step 2: Convert to Negation Normal Form",
        "Step 3: Convert to Prenex Normal Form",
        "Step 4: Skolemize",
        "Step 5: Convert to Clausal Normal Form",
        "Step 6: Drop quantifiers and list out the clauses"
    ]
    unify = result[num_steps:-1]

    unify_steps = []
    cur_kb = None
    for stmt in unify:
        parts = stmt.split(";")
        parts = [a.strip() for a in parts]
        if parts[0] == "Factor":
            subst = "substitution " + parts[4]
            if parts[4] == "NONE":
                subst = "no substitution"
            unify_steps.append(f"Factoring clause {parts[1]} by unifying {parts[2]} and {parts[3]} with {subst} - resulting in new clause {parts[5]}")
            kb.append(parts[5])
        else:
            subst = "substitution " + parts[5]
            if parts[5] == "NONE":
                subst = "no substitution"
            unify_steps.append(f"Resolving clauses {parts[1]} and {parts[2]} by unifying {parts[3]} and {parts[4]} with {subst} - resulting in new clause {parts[6]}")
            kb.append(parts[6])
        
        
    return jsonify({"valid": valid, "steps": first_part, "descriptions": descriptions, "unify": unify_steps, "summary": summary, "kb": kb, "initial_kb": initial_kb})

def run_ocaml_proof_checker(statement):
    dir_path = os.path.dirname(os.path.realpath(__file__))
    folprover_dir = os.path.join(dir_path, 'folprover')
    command = f"dune exec folprover \"{statement}\""
    try:
        result = subprocess.run(command, shell=True, cwd=folprover_dir, capture_output=True, text=True)
        if result.returncode == 0:
            return result.stdout  # Return the command output
        else:
            return f"Error: {result.stderr}"
    except Exception as e:
        return f"An error occurred: {e}"

if __name__ == '__main__':
    app.run(debug=True)
