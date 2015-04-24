START=$(date +%s.%N)
./chess data/db.pgn
END=$(date +%s.%N)
DIFF=$(echo "$END - $START" | bc)
echo $DIFF
