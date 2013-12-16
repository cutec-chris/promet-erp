    // In memory tasks store
    tasks = [];
    // This will change the view if localStorage isn't available. It
    // returns true or false
    function verifyLocalStorage() {
      if (!window.localStorage) {
        document.querySelector('.section').style.display="none";
        document.querySelector('#not-supported').style.display="block";
        return false;
      }
      return true;
    }
    function DeleteDoneTasks(){
      var i = 0;
      var list = document.querySelector('#task-list');
      list.innerHTML = "";
      var aTask = tasks[i];
      while (aTask) {
        var bTask = tasks[i+1];
        if (aTask.completed=='Y'){
          tasks.splice(i,1);
        } else {
          i++;
        }
        aTask = bTask;
      }
      localStorage['tasks'] = JSON.stringify(tasks);
      delete tasks;
      tasks = new Array();
      var jsoncode = localStorage.getItem('tasks');
      var oldTasks = JSON.parse(jsoncode || '[]');
      for (var i=0;i<=oldTasks.length-1;i++){
        oldTasks[i].lpriority = i;
        addTask(oldTasks[i]);
      }
    }
    function TaskDone(event){
      e = event.target;
      if (e.checked)
        tasks[e.parentElement.id].completed='Y';
      else
        tasks[e.parentElement.id].completed='N';
      localStorage['tasks'] = JSON.stringify(tasks);
    }
    // This adds a task
    function addTask(task){
      // To the view
      var nli=document.createElement("li");
      var list = document.querySelector('#task-list');
      list.appendChild(nli);
      var ndiv = document.createElement("div");
      nli.appendChild(ndiv);
      ndiv.setAttribute('id',task.lpriority);
      ndiv.innerHTML = '<input type="checkbox" style="width:auto;display:inline;"><label style="margin-left:5px;">'+task.subject+'</label>';
      ndiv.firstElementChild.checked = (task.completed=="Y");
      // To memory
      tasks[tasks.length] = task;
      ndiv.firstElementChild.addEventListener('CheckboxStateChange',TaskDone);
    }
    // Set it all up
    // Notify the user if local storage isn't supported
    if (verifyLocalStorage() == true) {
        // Get the last stored tasks and restore them to the UI.
        // Default to an empty array
        var jsoncode = localStorage.getItem('tasks');
        var oldTasks = JSON.parse(jsoncode || '[]');
        for (var i=0;i<=oldTasks.length-1;i++){
          oldTasks[i].lpriority = i;
          addTask(oldTasks[i]);
        }
        // Set up a handler for submission
        document.querySelector('#submitForm').onsubmit=function(){
          // Add the new task
          var aTaskName = document.querySelector('#task-name');
          var newTask = { 'subject' : aTaskName.value,
                          'sql_id' : undefined,
                          'completed' : 'N',
                          'lpriority' : 0
                        };
          newTask.lpriority = tasks.length;
          addTask(newTask);
          // In storage
          localStorage['tasks'] = JSON.stringify(tasks);
          // Clear the input
          document.querySelector('#task-name').value = '';
          // Don't post
          return false;
      }
    }

