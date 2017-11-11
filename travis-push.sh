#!/usr/bin/env bash
set -e

# Script for pushing updates to collaborative ensemble repo and reichlab/flusight
if [[ $(git diff --name-only HEAD~ inst/submissions/ | wc -l) == 0 ]]; then
    echo "No change, not triggering builds."
else
    echo "Change detected in submissions. Triggering builds."
    wget https://raw.githubusercontent.com/reichlab/xpull/31c3a025a1e7df08de4817c900b3f837ed484bca/trigger.sh
    # Trigger xpull build on flusight
    bash trigger.sh reichlab/flusight travis-xpull.sh
    # Trigger xpull build on cdc-flusight-contest
    bash trigger.sh FluSightNetwork/cdc-flusight-ensemble travis-xpull.sh
fi;
