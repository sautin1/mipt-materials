#include "planner.h"

bool fileExists(const std::string& filename)
{
    return std::ifstream(filename);
}

// __________________________________________________________________ //

size_t PlannerHasher::operator ()(const PlannerTask& task) const
{
    return str_hash(task.subject+task.name);
}

// __________________________________________________________________ //

PlannerTask::PlannerTask()
{}

PlannerTask::PlannerTask(const std::string& name_, const std::string& subject_, const time_t& deadline_,
            const size_t taskQ_, const size_t completedTaskQ_)
    :name(name_), subject(subject_), deadline(deadline_), taskQ(taskQ_),
     completedTaskQ(completedTaskQ_)
{}

bool PlannerTask::operator > (const PlannerTask& right) const
{
    time_t currentTime = time(NULL);
    double leftHoursRemaining = (difftime(deadline, currentTime) / 3600);
    double rightHoursRemaining = (difftime(right.deadline, currentTime) / 3600);
    return (((taskQ - completedTaskQ) / leftHoursRemaining) > ((right.taskQ - right.completedTaskQ) / rightHoursRemaining));
}

bool PlannerTask::operator == (const PlannerTask& right) const
{
    return ((subject == right.subject) && (name == right.name));
}

void PlannerTask::print(std::ostream& fout, TaskOutputFormat format) const
{
    tm taskTime = *localtime(&deadline);
    if (format == TOFfile) {
        fout << subject << " " << name << " " << taskTime.tm_hour << " " << taskTime.tm_min << " " << taskTime.tm_mday << " " << taskTime.tm_mon << " " << taskTime.tm_year + 1900 << " " << completedTaskQ << " " << taskQ;
    } else {
        fout << std::setw(OUTPUT_STRING_WIDTH) << std::left << subject << ": " << std::setw(OUTPUT_STRING_WIDTH) << std::left << name << " (till ";
        if (taskTime.tm_hour < 10) {
            fout << "0";
        }
        fout << taskTime.tm_hour << ":";
        if (taskTime.tm_min < 10) {
            fout << "0";
        }
        fout << taskTime.tm_min << " ";
        if (taskTime.tm_mday < 10) {
            fout << "0";
        }
        fout << taskTime.tm_mday << ".";
        if (taskTime.tm_mon+1 < 10) {
            fout << "0";
        }
        fout << taskTime.tm_mon+1 << "." << taskTime.tm_year + 1900 << "). ";
        fout << "Completed: " << std::setw(OUTPUT_NUMBER_WIDTH) << std::left << completedTaskQ << " / " << std::setw(OUTPUT_NUMBER_WIDTH) << std::left << taskQ << "\n";
    }
}
// __________________________________________________________________ //

Planner::Planner(const std::string& new_filename)
    :modified(false)
{
    std::cout << "Welcome to HomeWorkPlanner (hwp), created by Andrew Sautin!\n";
    if (fileExists(new_filename)) {
        filename = new_filename;
        loadTasks(filename);
    }
}

size_t Planner::size() const
{
    return taskHeap.size();
}

bool Planner::is_empty() const
{
    return (size() == 0);
}


size_t Planner::lastTopSize() const
{
    return taskVector.size();
}

void Planner::clear()
{
    taskHeap.clear();
    taskVector.clear();
    filename.clear();
    modified = false;
}

void Planner::printHelp() const
{
    std::cout << "Available commands: \n";
    std::cout << "\t\"help\"              - print the list of all available commands.\n";
    std::cout << "\t\"load <fileName>\"   - load all tasks from file <filename> to planner.\n";
    std::cout << "\t\"save <fileName>\"   - save all tasks from planner to file <filename>.\n";
    std::cout << "\t\"top <quantity>\"    - print <quantity> top rated tasks from planner.\n";
    std::cout << "\t\"top\"               - print all tasks from planner in priority order.\n";
    std::cout << "\t\"insert\"            - insert new task to planner.\n";
    std::cout << "\t\"erase <taskNumber>  - erase task #<taskNumber> from planner.\n";
    std::cout << "\t\"update <taskNumber> - update a field of task #<taskNumber> in planner.\n";
    std::cout << "\t\"exit\"              - close planner and exit program.\n";
}

bool Planner::checkTaskNumber(int taskNumber) const
{
    return (taskNumber >= 0 && taskNumber < (int)lastTopSize());
}

bool Planner::checkTaskTime(time_t taskTime) const
{
    return difftime(taskTime, time(NULL)) > 0;
}

Planner::PlannerTaskComponent Planner::makePTC(const std::string& component) const
{
    PlannerTaskComponent ptc;
    if (component == COMP_NAME) {
        ptc = PTCname;
    } else if (component == COMP_SUBJECT) {
        ptc = PTCsubject;
    } else if (component == COMP_DEADLINE) {
        ptc = PTCdeadline;
    } else if (component == COMP_TASKQ) {
        ptc = PTCtaskq;
    } else if (component == COMP_CTASKQ) {
        ptc = PTCctaskq;
    } else {
        ptc = PTCempty;
    }
    return ptc;
}

Planner::CommandT Planner::makeCommandT(const std::string& command) const
{
    CommandT result;
    if (command == COMMAND_NEW) {
        result = CTnew;
    } else if (command == COMMAND_LOAD) {
        result = CTload;
    } else if (command == COMMAND_SAVE) {
        result = CTsave;
    } else if (command == COMMAND_EXIT) {
        result = CTexit;
    } else if (command == COMMAND_TOP) {
        result = CTtop;
    } else if (command == COMMAND_INSERT) {
        result = CTinsert;
    } else if (command == COMMAND_ERASE) {
        result = CTerase;
    } else if (command == COMMAND_UPDATE) {
        result = CTupdate;
    } else if (command == COMMAND_HELP) {
        result = CThelp;
    } else {
        result = CTerror;
    }
    return result;
}

PlannerTask Planner::parseTask(std::istream& fin) const
{
    PlannerTask task;
    tm taskTime;
    fin >> task.subject >> task.name >> taskTime.tm_hour >> taskTime.tm_min >> taskTime.tm_mday >> taskTime.tm_mon >> taskTime.tm_year >> task.completedTaskQ >> task.taskQ;

    taskTime.tm_sec = 0;
    taskTime.tm_year -= 1900;
    --taskTime.tm_mon;
    task.deadline = mktime(&taskTime);

    return task;
}

time_t Planner::readDeadline() const
{
    tm newTime;
    newTime.tm_sec = 0;
    std::string timeString;
    std::cin >> timeString;
    std::stringstream sstream(timeString, std::ios_base::in);
    char tmp;
    sstream >> newTime.tm_hour >> tmp >> newTime.tm_min;
    std::cin >> timeString;
    std::stringstream sstream2(timeString, std::ios_base::in);
    sstream2 >> newTime.tm_mday >> tmp >> newTime.tm_mon >> tmp >> newTime.tm_year;
    newTime.tm_year -= 1900;
    --newTime.tm_mon;
    return mktime(&newTime);
}

void Planner::safeExit()
{
    std::cout << "Would you like to save your current plan? (y/n) ";
    char answer;
    std::cin >> answer;
    if (answer != 'n' && answer != 'N') {
        while (filename.empty()) {
            std::cout << "\tFilename: ";
            std::cin >> filename;
            if (fileExists(filename)) {
                std::cout << "\n\tFile with such name already exists. Overwrite? (y/n) ";
                std::cin >> answer;
                if (answer == 'n' || answer == 'N') {
                    filename.clear();
                }
            }
        }
        saveTasks();
    }
    clear();
}


void Planner::printFewTasks(std::ostream& fout, size_t quantity, PlannerTask::TaskOutputFormat format)
{
    taskVector.clear();
    quantity = std::min(taskHeap.size(), quantity);
    for (size_t task_index = 0; task_index < quantity; ++task_index) {
        taskVector.push_back(taskHeap.extractMin());
        if (format == PlannerTask::TOFuser) {
            fout << task_index + 1 << ") ";
        }
        taskVector.back().print(fout, format);
        if (task_index != quantity-1 && format == PlannerTask::TOFfile) {
            fout << "\n";
        }
    }
    for (size_t task_index = 0; task_index < taskVector.size(); ++task_index) {
        taskHeap.insert(taskVector[task_index]);
    }
}

void Planner::loadTasks(const std::string& loadFilename)
{
    PlannerTask newTask;
    std::ifstream fin(loadFilename);
    while (fin.good()) {
        newTask = parseTask(fin);
        taskHeap.insert(newTask);
    }
    std::cout << "Tasks are loaded to planner!\n";
}

void Planner::saveTasks()
{
    modified = false;
    std::ofstream fout(filename);
    printFewTasks(fout, size(), PlannerTask::TOFfile);
    std::cout << "Tasks are saved!\n";
}

void Planner::insertTask()
{
    PlannerTask newTask;
    std::cout << "\tTask name: ";
    std::cin >> newTask.name;
    std::cout << "\tTask subject: ";
    std::cin >> newTask.subject;
    while (true) {
        std::cout << "\tTask deadline (hh:mm dd.mm.yyyy): ";
        newTask.deadline = readDeadline();
        if (!checkTaskTime(newTask.deadline)) {
            std::cout << "Error: time has already passed.\n";
        } else {
            break;
        }
    }
    std::cout << "\tTask quantity: ";
    std::cin >> newTask.taskQ;
    while (true) {
        std::cout << "\tCompleted task quantity: ";
        std::cin >> newTask.completedTaskQ;
        if (newTask.completedTaskQ >= newTask.taskQ) {
            std::cout << "Error: completed task quantity must be in range [" << 0 << " : " << newTask.taskQ-1 << "].\n";
        } else {
            break;
        }
    }

    bool success = taskHeap.insert(newTask);
    if (!success) {
        std::cout << "Task cannot be inserted!\n";
    } else {
        std::cout << "Task is inserted!\n";
    }
}

void Planner::updateTask(size_t taskNumber)
{
    PlannerTask plannerTask;
    plannerTask = taskVector[taskNumber];
    PlannerTaskComponent taskComp(PTCempty);
    while (taskComp == PTCempty) {
        std::string taskCompString;
        std::cout << "Enter fieldName (name, subject, deadline, taskq, ctaskq): ";
        std::cin >> taskCompString;
        taskComp = makePTC(taskCompString);
        if (taskComp == PTCempty) {
            std::cerr << "Error: wrong fieldName. It must be equal to \"name\", \"subject\", \"deadline\", \"taskq\", \"ctaskq\".\n\n";
        }
    }
    switch (taskComp) {
        case PTCname: {
            std::cout << "\tNew name: ";
            std::cin >> plannerTask.name;
        } break;
        case PTCsubject: {
            std::cout << "\tNew subject: ";
            std::cin >> plannerTask.subject;
        } break;
        case PTCdeadline: {
            while (true) {
                std::cout << "\tNew deadline (hh:mm dd.mm.yyyy): ";
                plannerTask.deadline = readDeadline();
                if (!checkTaskTime(plannerTask.deadline)) {
                    std::cout << "Error: time has already passed.\n";
                } else {
                    break;
                }
            }
        } break;
        case PTCtaskq: {
            std::cout << "\t New task quantity: ";
            std::cin >> plannerTask.taskQ;
            if (plannerTask.taskQ <= plannerTask.completedTaskQ) {
                std::cout << "Task \"" << plannerTask.subject << " : " << plannerTask.name << "\" is completed!\n";
                eraseTask(taskNumber);
                return;
            }
        } break;
        case PTCctaskq: {
            std::cout << "\t New completed task quantity: ";
            std::cin >> plannerTask.completedTaskQ;
            if (plannerTask.taskQ <= plannerTask.completedTaskQ) {
                std::cout << "Task \"" << plannerTask.subject << ": " << plannerTask.name << "\" is completed!\n";
                eraseTask(taskNumber);
                return;
            }
        } break;
        default: break;
    }

    taskHeap.updateValue(taskVector[taskNumber], plannerTask);
    taskVector[taskNumber] = plannerTask;
    std::cout << "Task is updated!\n";
}

void Planner::eraseTask(const size_t taskNumber)
{
    bool success = taskHeap.erase(taskVector[taskNumber]);
    if (success) {
        std::cout << "Task #" << taskNumber+1 << " is erased!\n";
    } else {
        std::cout << "Error: task #" << taskNumber+1 << " was already erased!\n";
    }
    return;
}

void Planner::processCommand(const CommandT command, const std::string& arg)
{
    // safeExit check
    if ((command == CTnew || command == CTexit) && modified) {
        safeExit();
    }
    // check lost parameters
    if (arg.empty()) {
        switch (command) {
            case CTload: {
                std::cout << "Error: wrong number of parameters. Call: \"" << COMMAND_LOAD << " <fileName>\"\n";
                return;
            } break;
            case CTsave: {
                if (filename.empty()) {
                    std::cout << "Error: wrong number of parameters. Call: \"" << COMMAND_SAVE << " <fileName>\"\n";
                    return;
                }
            } break;
            case CTerase: {
                std::cout << "Error: wrong number of parameters. Call: \"" << COMMAND_ERASE << " <taskNumber>\"\n";
                return;
            } break;
            case CTupdate: {
                std::cout << "Error: wrong number of parameters. Call: \"" << COMMAND_UPDATE << " <taskNumber>\"\n";
                return;
            } break;
            default: break;
        }
    }

    // process command
    std::stringstream sstream(arg, std::ios_base::in);

    switch (command) {
        case CTnew: {
            // planner's empty
            clear();
            if (!arg.empty()) {
                sstream >> filename;
                if (!fileExists(filename)) {
                    std::cout << "Error: such file does not exist.\n";
                    filename.clear();
                }
            }
            std::cout << "New planner is created!\n";
        } break;
        case CTload: {
            std::string loadFile;
            sstream >> loadFile;
            if (fileExists(loadFile)) {
                if (is_empty()) {
                    filename = loadFile;
                } else {
                    modified = true;
                }
                loadTasks(loadFile);
            } else {
                std::cout << "Error: such file does not exist.\n";
                filename.clear();
            }
        } break;
        case CTsave: {
            if (!arg.empty()) {
                sstream >> filename;
                char answer;
                if (fileExists(filename)) {
                    std::cout << "\n\tFile with such name already exists. Overwrite? (y/n) ";
                    std::cin >> answer;
                    if (answer == 'n' || answer == 'N') {
                        filename.clear();
                    }
                }
            }
            saveTasks();
        } break;
        case CTexit: {
            // planner's empty
            std::cout << "Bye!\n";
            return;
        } break;
        case CTtop: {
            int quantity = 0;
            sstream >> quantity;
            if (quantity == 0) {
                quantity = size();
            }
            printFewTasks(std::cout, quantity, PlannerTask::TOFuser);
        } break;
        case CTinsert: {
            insertTask();
            modified = true;
        } break;
        case CTerase: {
            int taskNumber;
            sstream >> taskNumber;
            --taskNumber;
            if (!checkTaskNumber(taskNumber)) {
                std::cout << "Error: wrong parameter. TaskNumber must be in range [" << 1 << " : " << lastTopSize() << "]\n";
            } else {
                eraseTask(taskNumber);
                modified = true;
            }
        } break;
        case CTupdate: {
            int taskNumber;
            sstream >> taskNumber;
            --taskNumber;
            if (!checkTaskNumber(taskNumber)) {
                std::cout << "Error: wrong parameter. TaskNumber must be in range [" << 1 << " : " << lastTopSize() << "]\n";
            } else {
                updateTask(taskNumber);
                modified = true;
            }
        } break;
        case CThelp: {
            printHelp();
        }
        case CTerror: {
            std::cout << "Error: wrong command. Call \"help\" for the list of all available commands.\n";
        } break;
    }
}

void Planner::communicate()
{
    // read & process commands
    while (true) {
        std::string command;
        std::string argument;
        std::cout << ">> ";
        std::cin >> command;
        std::getline(std::cin, argument);
        if (argument[0] == ' ') {
            argument = argument.substr(1);
        }
        CommandT commandT = makeCommandT(command);
        processCommand(commandT, argument);
        std::cout << "\n";
        if (commandT == CTexit) {
            break;
        }
    }
}
