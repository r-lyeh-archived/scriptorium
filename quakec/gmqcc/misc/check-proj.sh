#!/bin/sh

host="gmqcc.qc.to"
location="$host/files"
list="$location/files"
hashes="$location/hashes"
options="$location/options"

#download required things
download_list=$(wget -qO- ${list})
download_hashes=$(wget -qO- ${hashes})
download_options=$(wget -qO- ${options})

download() {
    local old="$PWD"
    cd ~/.gmqcc/testsuite
    echo "$download_list" | while read -r line
    do
        echo "downloading $line ..."
        wget -q "${location}/$line"
    done

    echo "$download_hashes" > ~/.gmqcc/testsuite/hashes
    echo "$download_options" > ~/.gmqcc/testsuite/options

    cd "$old"
}

if [ -z "$download_list" -o -z "$download_hashes" -o -z "$download_options" ]; then
    echo "failed to download required information to check projects."

    if [ "$(ping -q -c1 "${host}")" ]; then
        echo "host ${host} seems to be up but missing required files."
        echo "please file bug report at: github.com/graphitemaster/gmqcc"
    else
        echo "host ${host} seems to be down, please try again later."
    fi

    echo "aborting"
    exit 1
fi

# we have existing contents around
if [ -f ~/.gmqcc/testsuite/hashes -a -f ~/.gmqcc/testsuite/options ]; then
    echo "$download_hashes" > /tmp/gmqcc_download_hashes
    echo "$download_options" > /tmp/gmqcc_download_options

    diff -u ~/.gmqcc/testsuite/hashes /tmp/gmqcc_download_hashes > /dev/null
    check_hash=$?
    diff -u ~/.gmqcc/testsuite/options /tmp/gmqcc_download_options > /dev/null
    check_opts=$?

    if [ $check_hash -ne 0 -o $check_opts -ne 0 ]; then
        echo "consistency errors in hashes (possible update), obtaining fresh contents"
        rm -rf ~/.gmqcc/testsuite/projects
        rm ~/.gmqcc/testsuite/*.zip

        download
    fi
else
    # do we even have the directory
    echo "preparing project testsuite for the first time"
    if [ ! -d ~/.gmqcc/testsuite ]; then
        mkdir -p ~/.gmqcc/testsuite
    fi

    download
fi

if [ ! -d ~/.gmqcc/testsuite/projects ]; then
    mkdir -p ~/.gmqcc/testsuite/projects
    old="$PWD"
    cd ~/.gmqcc/testsuite/projects
    echo "$(ls ../ | grep -v '^hashes$' | grep -v '^projects$' | grep -v '^options$')" | while read -r line
    do
        echo "extracting project $line"
        mkdir "$(echo "$line" | sed 's/\(.*\)\..*/\1/')"
        unzip -qq "../$line" -d $(echo "$line" | sed 's/\(.*\)\..*/\1/')
    done
    cd "$old"
else
    echo "previous state exists, using it"
fi

# compile projects in those directories
gmqcc_bin="gmqcc"
env -i type gmqcc 1>/dev/null 2>&1 || {
    if [ -f ../gmqcc ]; then
        echo "previous build of gmqcc exists, using it"
        gmqcc_bin="$(pwd)/../gmqcc"
    elif [ -f ./gmqcc ]; then
        echo "previous build of gmqcc exists, using it"
        gmqcc_bin="$(pwd)/gmqcc"
    else
        echo "gmqcc not installed and previous build doesn't exist"
        echo "please run make, or make install"
        exit 1
    fi
}

end_dir="$PWD"
cd ~/.gmqcc/testsuite/projects
start="$PWD"
find . -maxdepth 1 -mindepth 1 -type d | while read -r line
do
    line="${line#./}"
    echo -n "compiling $line... "
    cd "${start}/${line}"

    # does the project have multiple subprojects?
    if [ -f dirs ]; then
        echo ""
        cat dirs | while read -r dir
        do
            # change to subproject
            echo -n "    compiling $dir... "
            old="$PWD"
            cd "$dir"
            cmd="$(cat ../../../options | grep "$line:" | awk '{print substr($0, index($0, $2))}')"
            "$gmqcc_bin" $cmd > /dev/null 2>&1
            if [ $? -ne 0 ]; then
                echo "error"
            else
                echo "success"
            fi
            cd "$old"
        done
    # nope only one project
    else
        cmd="$(cat ../../options | grep "$line:" | awk '{print substr($0, index($0, $2))}')"
        "$gmqcc_bin" $cmd > /dev/null 2>&1
        if [ $? -ne 0 ]; then
            echo "error"
        else
            echo "success"
        fi
    fi
done

cd "$end_dir"
