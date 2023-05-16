#!/bin/bash
nbfc status -a | grep "Temperature" | sed 's/.*: //'
