# Housekeeper

## Build 
```shell
git clone git@github.com:xh-dev/housekeeping.git
cd housekeeper
sbt assembly
```

## Command Line Usage
```shell
housekeeper 0.0.2
Usage: housekeeper [options] source

  --execute          actually run the removing processing
  --debug <value>    activate debugging log
  --pattern <value>  Pattern to match
  source             The source directory or source file
```

## Housekeeping Strategy
| Day range           | Backup Strategy                                    |
|---------------------|----------------------------------------------------|
| 0-7 days            | Keep all records                                   |
| 8 days - 6 months   | Keep 3 copy of each month (1, 15, 29 or 30 or 31 ) |
| 7 months - 10 years | Keep last copy of each month                       |



## Demo
```shell
# pwd = current git root directory
cd test_data

# Run test script to generate data, will generate 1 year dummy backup file
./gen_test_data.sh

# the backup files is with format "x_{year}-{month}-{day}.sql"
# the pattern should "x_<year>-<month>-<day>\.sql" ('.' should be marked as '\.' )

# without --execute flag, the program only should the result of planned action of which file will be deleted
java -jar housekeeper --pattern x_<year>-<month>-<day>\.sql ./test-data

# with --execute flag, the delete operation will actually perform
java -jar housekeeper --pattern x_<year>-<month>-<day>\.sql ./test-data --execute
```
