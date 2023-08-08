from pathlib import Path
import chevron
import shutil
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
    with open(exp_root / Path("train.py"), "w") as f:
        f.write(py_text)
    shutil.copy("train.sbatch", exp_root / Path("train.sbatch"))
    shutil.copy("eval_checkpoint.sbatch", exp_root / Path("eval_checkpoint.sbatch"))
    shutil.copy("executions.sbatch", exp_root / Path("eval_final.sbatch"))
    shutil.copy("launch.sh", exp_root / Path("launch.sh"))