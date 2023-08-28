from pathlib import Path
import chevron
import shutil
import argparse

def build_single_experiment(
    exp_root: Path,    
    exp_name: str,
    py_template: Path,
    model: str,
    training_items: int,
    multiple_repo: str,
    gpu_num: int,
    lr: float,
    bs: int,
    sched: str,
    epochs: int,
    warmup_steps: int, 
    test_data: Path, 
    train_data: Path,
    slurm: bool = False,
    slurm_args: dict = {}
    ):
    exp_dir = exp_root / Path(f"{exp_name}_lr_{lr:.0e}_bs_{bs}_sched_{sched}_epochs_{epochs}_warmup_{warmup_steps}_items_{training_items}")
    exp_dir.mkdir(parents=True, exist_ok=True)
    with open(py_template, "r") as f:
        py_text = chevron.render(
            f, 
            {
                "model": model,
                "learning_rate": lr,
                "batch_size": bs,
                "schedule": sched,
                "epochs": epochs,
                "warmup_steps": warmup_steps,
                "test_data": test_data,
                "train_data": train_data,
                "training_items": training_items
            }
        )
    with open("run-exp-sh.mustache", "r") as f:
        run_exp_text = chevron.render(
            f,
            {
                "gpu_num": gpu_num,
                "pass_k": multiple_repo / "pass_k.py",
            }
        )
    with open(exp_dir / Path("train.py"), "w") as f:
        f.write(py_text)
    with open(exp_dir / Path("run-exp.sh"), "w") as f:
        f.write(run_exp_text)

    if slurm:
        with open("slurm/launch-sh.mustache", "r") as f:
            launch_text = chevron.render(f, slurm_args)
        with open(exp_dir / Path("launch.sh"), "w") as f:
            f.write(launch_text)
        shutil.copy("slurm/train.sbatch", exp_dir / Path("train.sbatch"))
        shutil.copy("slurm/eval_checkpoint.sbatch", exp_dir / Path("eval_checkpoint.sbatch"))
        shutil.copy("slurm/executions.sbatch", exp_dir / Path("executions.sbatch"))
        shutil.copy("slurm/run_completions.sbatch", exp_dir / Path("run_completions.sbatch"))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, required=True, help="Model name/path")
    parser.add_argument("--train-template", type=str, required=True, help="Path to train.py template")
    parser.add_argument("--train-data", type=str, required=True, help="Path to train data")
    parser.add_argument("--test-data", type=str, required=True, help="Path to test data")
    parser.add_argument("--training-items", type=int, required=True)
    parser.add_argument("--exp-name", type=str, required=True)
    parser.add_argument("--exp-root", type=str, default="experiments")
    parser.add_argument("--multiple-repo", type=str, required=True)
    parser.add_argument("--gpu-num", type=int, default=0)
    parser.add_argument("--learning-rate", type=str, default="3e-5")
    parser.add_argument("--batch-size", type=str, default="8")
    parser.add_argument("--schedule", type=str, default="cosine")
    parser.add_argument("--epochs", type=int, default=1)
    parser.add_argument("--warmup-steps", type=int, default=10)
    # Slurm args
    parser.add_argument("--slurm", action="store_true")
    parser.add_argument("--slurm-multiple-singularity", type=str)
    parser.add_argument("--slurm-venv-activate", type=str)
    parser.add_argument("--slurm-hf-cache", type=str, default="./cache/")
    parser.add_argument("--slurm-eval-dataset", type=str)
    parser.add_argument("--slurm-tokenizer", type=str)

    args = parser.parse_args()
    exp_root = Path(args.exp_root).absolute()
    if args.slurm:
        if args.slurm_multiple_singularity is None:
            raise ValueError("Must specify --slurm-multiple-singularity")
        if args.slurm_venv_activate is None:
            raise ValueError("Must specify --slurm-venv-activate")
        if args.slurm_eval_dataset is None:
            raise ValueError("Must specify --slurm-eval-dataset")
        if args.slurm_tokenizer is None:
            raise ValueError("Must specify --slurm-tokenizer")
        slurm_args = {
            "multiple_repo": args.multiple_repo,
            "multiple_image": args.slurm_multiple_singularity,
            "venv_activate": args.slurm_venv_activate,
            "hf_cache": args.slurm_hf_cache,
            "dataset": args.slurm_eval_dataset,
            "tokenizer": args.slurm_tokenizer
        }
    else:
        slurm_args = {}
    if args.single:
        build_single_experiment(
            exp_root,
            exp_name=args.exp_name,
            model=args.model,
            py_template=Path(args.train_template).absolute(),
            gpu_num=args.gpu_num,
            multiple_repo=Path(args.multiple_repo),
            lr=float(args.learning_rate),
            bs=int(args.batch_size),
            training_items=args.training_items,
            sched=args.schedule,
            epochs=args.epochs,
            warmup_steps=args.warmup_steps,
            train_data=Path(args.train_data).absolute(),
            test_data=Path(args.test_data).absolute(),
            slurm=args.slurm,
            slurm_args=slurm_args
        ) 
