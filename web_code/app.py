from flask import Flask, render_template, request, jsonify
import os
from dotenv import load_dotenv

app = Flask(__name__)

# Load environment variables from .env file
load_dotenv()

# AWS credentials from environment variables (if still needed for other parts of the app)
AWS_ACCESS_KEY = os.getenv("AWS_ACCESS_KEY_ID")
AWS_SECRET_KEY = os.getenv("AWS_SECRET_ACCESS_KEY")
AWS_REGION = os.getenv("AWS_DEFAULT_REGION")
BUCKET_NAME = os.getenv("S3_BUCKET_NAME")


@app.route("/")
def home():
    return render_template("home.html")


@app.route("/project")
def project():
    return render_template("project.html")


@app.route("/analysis", methods=["GET"])
def analysis():
    return render_template("analysis.html")


@app.route("/contact")
def contact():
    return render_template("contact.html")


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, debug=True)
    # local run, uncommand
    # app.run(port=8080)
