# KaMinParLogParser

## Expected Raw Result Organization

The raw results of the test runs are expected to be in following hierarchical organization:
````
all_runs
    |___ run-1
            |___ configuration-1
                    |___ graph-1
                            |___ log_1.log
                            |___ log_2.log
                    |___ graph-2
                    |___ graph-3
                    |___ graph-4
                    |___ ...
            |___ configuration-2
            |___ configuration-3
            |___ ...
    |___ run-2
    |___ run-3
    |___ ...
````

The meaning of the single folders/files are as follows:
   - `run-x`: All result data created during one complete test run. 
        Normally a new run-dir is created when the code changed and everything has to be recalculated.
   - `run-x/configuration-y`: One configuration that has been tested during run x.
        Configuration normally differ in the arguments passed for calculations.
        `configuration-y` should be the name of the configuration.
   - `run-x/configuration-y/graph-z`: One directory for every graph tested, where `graph-z` is the name of the graph.
   - `run-x/configuration-y/graph-z/log_i.log`: One log file per each individual run with the same configuration and on the same graph.
        The log files contain the complete console output of this individual run.