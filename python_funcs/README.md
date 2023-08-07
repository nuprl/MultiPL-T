## Test Generation Instructions

#### 0. Configure environment

```
pip3 -m venv venv
source venv/bin/activate
pip3 install -r requirements.txt
accelerate config # and follow the instructions
```

#### 1. Run the code execution docker container

```
pushd ./code_exec_server/
./build_and_run.sh
popd
```

#### 2. Run the dataset aggregator server

This will start a server that will listen for incoming requests to add to the dataset. Open a new terminal and run:

```
python3 dataset_aggregator.py --name resulting-dataset-name
```

#### 3. Run the test generator script

```
accelerate launch test_gen.py
```
