import subprocess
import os
from flask import Flask, render_template, request, jsonify

app = Flask(__name__)

@app.route('/')
def index():
    os.system("ocamlopt -o proof proof.ml")
    return render_template('index.html')

@app.route('/proof', methods=['POST'])
def proof():
    data = request.json
    statement = data['statement']
    result = run_ocaml_proof_checker(statement)

    is_valid = result.strip() == "Valid"
    return jsonify({"valid": is_valid, "steps": ["Step 1", "Step 2", "Conclusion"]})

def run_ocaml_proof_checker(statement):
    try:
        output = subprocess.check_output(['./proof', statement], universal_newlines=True)
        return output
    except subprocess.CalledProcessError as e:
        return f"Error: {e}"

if __name__ == '__main__':
    app.run(debug=True)
