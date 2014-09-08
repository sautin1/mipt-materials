#ifndef PLANNER_H
#define PLANNER_H

#include <string>
#include <vector>
#include <ctime>
#include <functional>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <iomanip>
#include "binary_heap_ref.h"

// Commands
const std::string COMMAND_SAVE   = "save"    ;
const std::string COMMAND_LOAD   = "load"    ;
const std::string COMMAND_NEW    = "new"     ;
const std::string COMMAND_TOP    = "top"     ;
const std::string COMMAND_EXIT   = "exit"    ;
const std::string COMMAND_INSERT = "insert"  ;
const std::string COMMAND_ERASE  = "erase"   ;
const std::string COMMAND_UPDATE = "update"  ;
const std::string COMMAND_HELP   = "help"    ;

const std::string COMP_NAME      = "name"    ;
const std::string COMP_SUBJECT   = "subject" ;
const std::string COMP_DEADLINE  = "deadline";
const std::string COMP_TASKQ     = "taskq"   ;
const std::string COMP_CTASKQ    = "ctaskq"  ;

const size_t OUTPUT_STRING_WIDTH  = 10;
const size_t OUTPUT_NUMBER_WIDTH  = 3;

bool fileExists(const std::string& filename);

struct PlannerTask
{
    typedef enum {TOFfile, TOFuser} TaskOutputFormat;
    std::string name;
    std::string subject;
    time_t deadline;
    size_t taskQ;
    size_t completedTaskQ;
    PlannerTask();
    PlannerTask(const std::string& name, const std::string& subject, const time_t& deadline,
                const size_t taskQ, const size_t completedTaskQ);
    bool operator > (const PlannerTask& right) const;
    bool operator == (const PlannerTask& right) const;
    void print(std::ostream& fout, TaskOutputFormat format) const;
};

class PlannerHasher
{
    std::hash<std::string> str_hash;
public:
    size_t operator() (const PlannerTask& task) const;
};

class Planner
{
private:
    typedef enum {CTsave, CTload, CTnew, CTtop, CTexit, CTinsert, CTerase, CTupdate, CThelp, CTerror} CommandT;
    typedef enum {PTCname, PTCsubject, PTCdeadline, PTCtaskq, PTCctaskq, PTCempty} PlannerTaskComponent;

    std::string filename;
    binary_heap_ref< PlannerTask, std::greater<PlannerTask>, PlannerHasher > taskHeap;
    std::vector<PlannerTask> taskVector;
    bool modified;

    // small planner functions
    size_t size() const;
    bool is_empty() const;
    size_t lastTopSize() const;
    void clear();
    void printHelp() const;

    // small auxiliary functions
    bool checkTaskNumber(int taskNumber) const;
    bool checkTaskTime(time_t taskTime) const;
    PlannerTaskComponent makePTC(const std::string& component) const;
    CommandT makeCommandT(const std::string& command) const;
    PlannerTask parseTask(std::istream& fin) const;
    time_t readDeadline() const;

    // tasks
    void safeExit();
    void printFewTasks(std::ostream& fout, size_t quantity, PlannerTask::TaskOutputFormat format);
    void loadTasks(const std::string& loadFilename);
    void saveTasks();
    void insertTask();
    void updateTask(const size_t taskNumber);
    void eraseTask(const size_t taskNumber);

    // main planner functions
    void processCommand(const CommandT command, const std::string& arg);
public:
    Planner(const std::string& new_filename);
    void communicate();
};

#endif // PLANNER_H
