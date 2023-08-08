from pathlib import Path
import chevron
import shutil
import argparse

def build_single_experiment(
    exp_root: Path,
    lr: float,
    bs: int,
    sched: str,
    epochs: int,
    warmup_steps: int, 
    test_data: Path, 
    train_data: Path,
    ):
    exp_dir = exp_root / Path(f"lr_{lr:.0e}_bs_{bs}_sched_{sched}_epochs_{epochs}_warmup_{warmup_steps}")
    exp_dir.mkdir(parents=True, exist_ok=True)
    with open("train-py.mustache", "r") as f:
        py_text = chevron.render(
            f, 
            {
                "learning_rate": lr,
                "batch_size": bs,
                "schedule": sched,
                "epochs": epochs,
                "warmup_steps": warmup_steps,
                "test_data": test_data,
                "train_data": train_data,
            }
        )
    with open(exp_dir / Path("train.py"), "w") as f:
        f.write(py_text)
    shutil.copy("train.sbatch", exp_dir / Path("train.sbatch"))
    shutil.copy("eval_checkpoint.sbatch", exp_dir / Path("eval_checkpoint.sbatch"))
    shutil.copy("executions.sbatch", exp_dir / Path("executions.sbatch"))
    shutil.copy("launch.sh", exp_dir / Path("launch.sh"))

def all_build_experiments(exp_root: Path):
    for lr in [1e-5, 2e-5, 3e-5, 4e-5, 5e-5, 6e-5, 7e-5, 8e-5, 9e-5, 1e-4]:
        for bs in [8, 16, 32]:
            for sched in ["cosine", "constant"]:
                build_single_experiment(
                    exp_root,
                    lr=lr,
                    bs=bs,
                    sched=sched,
                    epochs=1,
                    warmup_steps=10,
                    train_data=Path("racket_10k_train.jsonl").absolute(),
                    test_data=Path("humaneval_10_racket.jsonl").absolute(),
                )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--exp-root", type=str, default="experiments")
    parser.add_argument("--test", action="store_true")
    args = parser.parse_args()
    exp_root = Path(args.exp_root).absolute()
    if args.test:
        build_single_experiment(
            exp_root,
            lr=1e-5,
            bs=8,
            sched="cosine",
            epochs=1,
            warmup_steps=10,
            train_data=Path("racket_10k_train.jsonl").absolute(),
            test_data=Path("humaneval_10_racket.jsonl").absolute(),
        )
        exit(0)
    all_build_experiments(Path(args.exp_root)) 