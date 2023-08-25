import argparse
import chevron
from pathlib import Path


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--name", help="Name", default="completion-pod")
    parser.add_argument("--template", help="Template file", default="completion-pod.yaml.mustache")
    parser.add_argument("--max-total-tokens", help="Max total tokens", default=8192)
    parser.add_argument("--max-concurrent-requests", help="Max concurrent requests", default=200)
    parser.add_argument("--host-port", help="Host port", default=8080)
    parser.add_argument("--model-dir", help="Model directory", required=True)
    parser.add_argument("--data-dir", help="Data directory", required=True)
    parser.add_argument("--prompt-file", help="Prompt file", required=True)
    parser.add_argument("--output-file", help="Output file", required=True)
    parser.add_argument("--num-connections", help="Number of connections", default=100)
    parser.add_argument("--num-runners", help="Number of runners", default=80)
    parser.add_argument("--attempt-limit", help="Attempt limit", default=20)
    parser.add_argument("--gpu", help="GPU number", default=0)

    args = parser.parse_args()
    with open(args.template) as f:
        template = f.read()
    yaml_text = chevron.render(template, args.__dict__)
    output_path = Path(args.name + ".yaml")
    output_path.write_text(yaml_text)


