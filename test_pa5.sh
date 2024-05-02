# test_pa5.sh
# Test script for Tiger compiler type-checking phase.
# Author: Amittai Aviram - aviram@cslab.bc.edu.

VALID_DIR=test_valid
ERROR_DIR=test_error
ERROR_COUNT=0

echo "Building ..."
make clean
make
if [[ $? -ne 0 ]]
then
    echo -e "\nBuilding failed.\n"
    exit 1
fi
for FILE in $VALID_DIR/*.tig
do
    RESULT=$(./parse $FILE 2>&1)
    if [[ $RESULT = *failed* ]]
    then
        echo "Parsing failed on $FILE"
        ERROR_COUNT=$((ERROR_COUNT+1))
    fi
    if [[ $RESULT = *$VALID_DIR* ]]
    then
        echo $RESULT
        ERROR_COUNT=$((ERROR_COUNT+1))
    fi
done
for FILE in $ERROR_DIR/*.tig
do
    RESULT=$(./parse $FILE 2>&1)
    if [[ $RESULT = *failed* ]]
    then
        if [[ "$FILE" != *test49.tig* ]]
        then
            echo "Parsing failed for $FILE"
            ERROR_COUNT=$((ERROR_COUNT+1))
        fi
    fi
    if [[ $RESULT != *$ERROR_DIR* ]]
    then
        echo "Failed to detect type-checking issue in $FILE"
        ERROR_COUNT=$((ERROR_COUNT+1))
    fi
done
if [[ $ERROR_COUNT -eq 0 ]]
then
    printf "\nCORRECT\n\n"
fi
make clean

