#pragma once

enum class TTaskLaunchMode : char {
    SYNC,       // synchronously
    ASYNC,      // asynchronously, start at once
    DEFERRED,   // asynchronously, start deferred
    CHOOSE      // let function decide between ASYNC and SYNC
};
