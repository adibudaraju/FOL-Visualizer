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
    data = request.json
    statement = data['statement']
    result = run_ocaml_proof_checker(statement).splitlines()
    print(result)
    return jsonify({"valid": True, "steps": result})

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
