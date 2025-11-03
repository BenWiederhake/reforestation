#!/usr/bin/env python3

import datetime
import subprocess
import random



def determine_last_commit_timestamp():
    result = subprocess.run(["git", "log", "--date=raw", "--format=%ad", "-n1"], check=True, capture_output=True, encoding="UTF-8")
    timestamp_str = result.stdout.split(" ")[0]
    return int(timestamp_str)


def generate_suggested_timestamps_after(last_commit_timestamp):
    dt = datetime.datetime.fromtimestamp(last_commit_timestamp)
    end_dt = datetime.datetime.now()
    while True:
        dt += datetime.timedelta(days=1)
        dt += datetime.timedelta(seconds=random.gauss(mu=-240, sigma=3600))
        if dt > end_dt:
            break
        yield dt.strftime("%Y-%m-%d %H:%M:%S")


def run():
    last_commit_timestamp = determine_last_commit_timestamp()
    for next_timestamp in generate_suggested_timestamps_after(last_commit_timestamp):
        subprocess.run(["faketime", next_timestamp, "./reforest.py", "", "--commit"], check=True)


if __name__ == "__main__":
    run()
