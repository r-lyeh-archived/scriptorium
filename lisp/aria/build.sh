#!/bin/bash
gcc aria.c -o aria -DAR_STANDALONE -Wall -Wextra -std=c89 -pedantic -O3
strip aria
