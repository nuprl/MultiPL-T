from pathlib import Path
import chevron
import shutil
import argparse

def build_single_experiment(
    exp_root: Path,    
    exp_name: str,
    template: Path,
    training_items: int,
    lr: float,
    bs: int,
    sched: str,
    epochs: int,
    warmup_steps: int, 
    test_data: Path, 
    train_data: Path,
    slurm: bool = True
    ):
    exp_dir = exp_root / Path(f"{exp_name}_lr_{lr:.0e}_bs_{bs}_sched_{sched}_epochs_{epochs}_warmup_{warmup_steps}_items_{training_items}")
    exp_dir.mkdir(parents=True, exist_ok=True)
    with open(template, "r") as f:
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
                "training_items": training_items
            }
        )
    with open(exp_dir / Path("train.py"), "w") as f:
        f.write(py_text)

    if slurm:
        shutil.copy("slurm/train.sbatch", exp_dir / Path("train.sbatch"))
        shutil.copy("slurm/eval_checkpoint.sbatch", exp_dir / Path("eval_checkpoint.sbatch"))
        shutil.copy("slurm/executions.sbatch", exp_dir / Path("executions.sbatch"))
        shutil.copy("slurm/run_completions.sbatch", exp_dir / Path("run_completions.sbatch"))
        shutil.copy("slurm/launch.sh", exp_dir / Path("launch.sh"))

def grid_build_experiments(
    exp_root: Path, 
    exp_name: str,
    template: Path,
    training_items: int,
    lrs, 
    bss, 
    scheds, 
    epochs, 
    warmup_steps, 
    test_data: Path, 
    train_data: Path,
    slurm: bool = True
    ):
    for lr in lrs:
        for bs in bss:
            for sched in scheds:
                build_single_experiment(
                    exp_root,
                    exp_name=exp_name,
                    training_items=training_items,
                    template=template,
                    lr=lr,
                    bs=bs,
                    sched=sched,
                    epochs=epochs,
                    warmup_steps=warmup_steps,
                    train_data=train_data,
                    test_data=test_data,
                    slurm=slurm
                )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--train-template", type=str, required=True, help="Path to train.py template")
    parser.add_argument("--train-data", type=str, required=True, help="Path to train data")
    parser.add_argument("--test-data", type=str, required=True, help="Path to test data")
    parser.add_argument("--training-items", type=int, required=True)
    parser.add_argument("--exp-name", type=str, required=True)
    parser.add_argument("--exp-root", type=str, default="experiments")
    parser.add_argument("--single", action="store_true", )
    parser.add_argument("--grid", action="store_true")
    parser.add_argument("--learning-rate", type=str, default="3e-5")
    parser.add_argument("--batch-size", type=str, default="8")
    parser.add_argument("--schedule", type=str, default="cosine")
    parser.add_argument("--epochs", type=int, default=1)
    parser.add_argument("--warmup-steps", type=int, default=10)
    parser.add_argument("--slurm", action="store_true")

    args = parser.parse_args()
    exp_root = Path(args.exp_root).absolute()
    if args.single and args.grid:
        raise ValueError("Cannot specify both single and grid")
    if not (args.single or args.grid): 
        raise ValueError("Must specify either single or grid")
    if args.single:
        build_single_experiment(
            exp_root,
            exp_name=args.exp_name,
            template=Path(args.train_template).absolute(),
            lr=float(args.learning_rate),
            bs=int(args.batch_size),
            training_items=args.training_items,
            sched=args.schedule,
            epochs=args.epochs,
            warmup_steps=args.warmup_steps,
            train_data=Path(args.train_data).absolute(),
            test_data=Path(args.test_data).absolute(),
            slurm=args.slurm
        ) 
    elif args.grid:
        lrs = [float(lr) for lr in args.learning_rate.split(",")]
        bss = [int(bs) for bs in args.batch_size.split(",")]
        scheds = args.schedule.split(",")
        grid_build_experiments(
            exp_root,
            exp_name=args.exp_name,
            template=Path(args.train_template).absolute(),
            lrs=lrs,
            bss=bss,
            training_items=args.training_items,
            scheds=scheds,
            epochs=args.epochs,
            warmup_steps=args.warmup_steps,
            train_data=Path(args.train_data).absolute(),
            test_data=Path(args.test_data).absolute(),
            slurm=args.slurm
        )
